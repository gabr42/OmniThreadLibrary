unit OtlPlatform.Tasks;
{$I OtlOptions.inc}
interface
uses OtlPlatform.Extras, System.Classes, OtlPlatform.Pipe,
     System.Generics.Collections,
     System.SysUtils, System.SyncObjs,
     OtlPlatform.Sync,
     OtlPlatform.Sync.Intf,
     OtlPlatform.Sync.Modular
     // Ensure that Atomic is last in the uses list, so that its record
     //  helpers (in the case of NDEF USE_SLACKSPACE_ALIGNMENT) are in scope.
     , OtlPlatform.Atomic
     ;

type

  /// <remarks>This is an internal class. Do not use.</remarks>
  TTasking = class
    class function CreateWorkFactory(
      MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventPool: integer;
      const SharedLock: ILock = nil): IWorkFactory;

    class function CreateFuture<T>( const AFactory: IWorkFactory; Func: TFutureFunc<T>): IFuture<T>;
    class function Join( const WorkFactory: IWorkFactory; const Tasks: array of ITask): ITask;
  end;

  TTaskThreadStatus = (ttsIdle, ttsInWork, ttsTerminated);

  TWorkFactory = class;
  TTaskThread = class( TThread)
  private const
    TaskQueueTimeOutPeriod = 10000; // 10 s

  private
    function  GetStatus: TTaskThreadStatus;
    procedure SetStatus( Value: TTaskThreadStatus);
    function  IsLocallyTerminated: boolean;
    function  IsGloballyTerminated: boolean;
    function  IsTerminated: boolean;

  protected
    FWorkFactory: TWorkFactory;
    FLock: ILock;
    [Volatile] FStatus: TVolatileUint32; // (TTaskThreadStatus
    [Volatile] FisTerminated: TVolatileUint32;
    FHouseKeeperAlert: IEvent; // Shared. Auto-reset.
    FWorkAlert: IEvent;        // Shared. Auto-reset

    procedure Execute; override;

  public
    constructor Create( AOwner: TWorkFactory);
    destructor Destroy; override;

    property Status: TTaskThreadStatus   read GetStatus write SetStatus;
  end;


  ITaskEx = interface;
  TJobPipe = class( TPipe<ITaskEx>) end;

  RSchedJob = record
    FMaturity: TDateTime;
    FJob: ITaskEx;
  end;

  TSchedQueue = class( TList<RSchedJob>)
  public
    constructor Create;
    function OrderedAdd( const Addend: RSchedJob): integer;
    function PopMature( NowTime: TDateTime; var PopJob: ITaskEx): boolean;
    function NextMaturity( var Value: TDateTime): boolean;
  end;

  THouseKeeperThread = class( TThread)
  public
    const HouseKeepingPeriod = 1000; // ms

  private
    FWorkFactory: TWorkFactory;
    FLock: ILock;
    FHouseKeeperAlert: IEvent; // Shared. Auto-reset.
    FWorkAlert: IEvent;        // Shared. Auto-reset
    [Volatile] FisTerminated: TVolatileUint32;
    FAllThreadsDone: ICountDown;
    FMaxThreadCount: cardinal;     // Maximum allowable count of threads.
    FMinIdleCount: cardinal;       // Minimum count of idle threads that the house-keeper is to maintain, if permitted by Max properties.
    FMaxIdleCount: cardinal;       // Maximum count of idle threads that  the house-keeper is to maintain.
    procedure ComputeHowManyNewThreadsToCreate( var More: boolean; var Delta: integer);
  protected
    procedure Execute; override;
  public
    constructor Create( AOwner: TWorkFactory);
    destructor Destroy; override;
  end;

  TTask = class;

  TBaseFuture = class;
  TBaseFutureClass = class of TBaseFuture;
  TComputeFutureProc = reference to procedure( const Task: TBaseFuture);
{$REGION 'Private declarations - do not use'}
// These are in the interface section due to the limitations of generics.
  TCoForEachTaskFactory = class abstract
    private
      FTemplate    : ITask;
    public
      FCompositeTask: ITask;

      constructor Create( WF: TWorkFactory; ThreadCount: integer);
      function CreateTemplate     ( WF: TWorkFactory                                 ): ITask;   virtual; abstract;
      function CreateCompositeTask( WF: TWorkFactory; const Iterators: array of ITask): ITask;   virtual;
    end;

  TTaskFactFunc = reference to function( AWF: TWorkFactory; AIntVal: integer): TCoForEachTaskFactory;
{$ENDREGION}

  TWorkFactory = class( TInterfacedObject, IWorkFactory)
  public const
    MaxCoForEachThreadCount = 10;

  private
    FLock: ILock;
    [Volatile] FThreadCount: TVolatileUint32; // Total number of threads either in-work, terminated or idle.
    [Volatile] FIdleCount: TVolatileUint32;   // Count of idle threads available for tasking.
    FMaxThreadCount: cardinal;     // Maximum allowable count of threads.
    FMinIdleCount: cardinal;       // Minimum count of idle threads that the house-keeper is to maintain, if permitted by Max properties.
    FMaxIdleCount: cardinal;       // Maximum count of idle threads that  the house-keeper is to maintain.
    [Volatile] FisTerminated: TVolatileUint32;
    [Volatile] FWorkEnqueued: TVolatileUint32;
    [Volatile] FWorkCapacity: TVolatileUint32;
    FTaskQueue: TJobPipe;
    FReturnQueue: TJobPipe;
    FTaskThreads: TObjectList<TTaskThread>;
    FHouseKeepAlert: IEvent;  // Auto-reset
    FWorkAlert: IEvent;       // Auto-reset
    FHouseKeeper: THouseKeeperThread;
    FShareLock: ILock;
    FSynchroPool: TSynchroFactory;
    FAllThreadsDone: ICountDown;
    FSchedQueue:   TSchedQueue;

    function  FactoryLock: ILock;
    function  AsObject: TObject;
    function  ProcessJobReturn( TimeOut: cardinal): boolean;
    function  QueueJob( const Task: ITask): boolean;
    procedure SchedQueueJob( AMaturity: TDateTime; const Task: ITaskEx);
    procedure StartTask( const TaskIntf: ITask);
    function  CanStart( TaskObj: TTask): boolean;

    function NewLock( Locking: TLockingMechanism): ILock;
    function Event            ( ManualReset, InitialState: boolean): IEvent;                        // Interface level, Reusable.
    function LightEvent       ( ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;     // Interface level, Reusable.
    function CountDown( InitialValue: cardinal): ICountDown;                                        // Interface level, Reusable.
    function CompositeSynchro_WaitAll( const AFactors: TSynchroArray; APropagation: TWaitPropagation): ICompositeSynchro;
    function CompositeSynchro_WaitAny( const AFactors: TSynchroArray; APropagation: TWaitPropagation): ICompositeSynchro;
    function ModularEvent    ( ManualReset, InitialState: boolean): IEvent;        overload;
    function ModularEvent    ( const ABase: IEvent               ): IEvent;        overload;
    function ModularSemaphore( AInitialCount: cardinal): ISemaphore;    overload;
    function ModularSemaphore( const ABase: ISemaphore): ISemaphore;    overload;
    function ModularCountDown( AInitialValue: cardinal): ICountDown;    overload;
    function ModularCountDown( const ABase: ICountDown): ICountDown;    overload;
    function Join( const Tasks: array of ITask                ): ITask;                         overload;
    function Join( const Procs: array of System.SysUtils.TProc): ITask;                         overload;
    function Join( const Procs: array of TTaskProc            ): ITask;                         overload;
    function WaitForAny( const AFactors: TSynchroArray; APropagation: TWaitPropagation; var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;
    function WaitForAll( const AFactors: TSynchroArray; APropagation: TWaitPropagation;                            TimeOut: cardinal = FOREVER): TWaitResult;
    function CoForEach( LowIdx, HighIdx, StepIdx: integer; ThreadCount: integer; Proc: TForEachProc): ITask;                                      overload;
    function CoForEachBase( ThreadCount: integer; TaskFact: TTaskFactFunc): ITask;
    function Abandonable( const BaseTask: ITask; Timeout: integer): IFuture<boolean>;
    procedure AbandonTask( const Task: ITask);
    function GeometricRetry( BaseFunc: TFutureFunc<boolean>; InitialRetryPause: integer; PauseGrowth: double; MaxRetryPause: integer; MaxTryCount: integer): IFuture<boolean>;
    function SelfJoin( const BaseTask: ITask; ThreadCount: integer): ITask;

  public
    /// <remarks> DO NOT USE. This constructor is for internal use only.
    ///  Use instead either TSBDParallel.CreateWorkFactory. </remarks>
    constructor CreateWorkFactory( MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventCount1: integer; const SharedLock: ILock = nil);
    destructor Destroy; override;
    function CoBeginTask( Proc: System.SysUtils.TProc): ITask;  overload;
    function CoBeginTask( Proc: TTaskProc            ): ITask;  overload;
    function CoForEach<T>( Collection: IEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;     overload;
    function CoForEach<T>( Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;     overload;
    function ShareLock: ILock;
    function SynchroPool: TSynchroFactory;
  end;


  ITaskEx = interface ['{BF5FF699-E6E0-4993-BB16-C623D122E781}']
    function  GetStatus: TTaskStatus;
    procedure SetStatus( Value: TTaskStatus);
    procedure BeforeExecute;
    procedure Execute;
    procedure ReleaseExecuteProc;
    procedure AfterExecute;
    procedure ExecuteReturn;
    procedure MarkExceptionThrown;
    function  DoCaptureException: boolean;
    procedure Clear;
    function  QueueIfCan: boolean;
    procedure Signal;
    procedure DereferenceParent;
    procedure AddChild( const Addend: ITaskEx);
    procedure AddChildren( const Addends: array of ITaskEx);
    procedure EndAddingChildren;
    procedure QueueChildren;
    procedure SetParent( const ATask: TTask);
    procedure CleanUpAfterDoneOrCancelled;
    procedure AttachThread( AThread: TTaskThread);
    procedure DetachThread;
    function  AttachedThread: TTaskThread;

    property Status: TTaskStatus  read GetStatus write SetStatus;
  end;

  TTask = class( TInterfacedObject, ITask, ITaskEx)
  private
    [Volatile] FStatus: TVolatileUint32;
    [Volatile] FDatum: TVolatileObject;
    [Volatile] FAttachedThread: TVolatileObject;
    FWorkFactory: TWorkFactory;
    FJob: TTaskProc;
    FReturnJob: TTaskProc;
    FhasThrownException: boolean;
    FParent: TTask;
    FParentLock: TSBDSpinLock;
    FChildrenTasks: TArray<ITaskEx>;
    FTermination: ICountDown;

    function  AsObject: TObject;
    function  Queue: ITask;
    function  CanStart: boolean;
    function  QueueIfCan: boolean;
    procedure SetFinalStatus( Value: TTaskStatus);
    procedure SetParent( const ATask: TTask);
    procedure AttachThread( AThread: TTaskThread);
    procedure DetachThread;
    function  AttachedThread: TTaskThread;

  protected
    function  GetDatum: TObject;                                 virtual;
    procedure SetDatum( Value: TObject);                         virtual;
    function  GetStatus: TTaskStatus;                            virtual;
    function  ITask.Status = GetStatus;
    procedure SetStatus( Value: TTaskStatus);                    virtual;
    function  IsDone: boolean;                                   virtual;
    function  SetOnDone( Proc: TTaskProc): ITask;                virtual;
    function  FundamentalClone: TTask;                           virtual;
    function  Clone: ITask;                                      virtual;
    procedure DereferenceParent;                                 virtual;
    procedure AddChildren( const Addends: array of ITaskEx);     virtual;
    procedure EndAddingChildren;                      virtual;
    procedure AddChild( const Addend: ITaskEx);       virtual;
    procedure Signal;                                 virtual;
    procedure BeforeExecute;                          virtual;
    procedure Execute;                                virtual;
    procedure ReleaseExecuteProc;                     virtual;
    procedure AfterExecute;                           virtual;
    procedure ExecuteReturn;                          virtual;
    procedure MarkExceptionThrown;                    virtual;
    function  DoCaptureException: boolean;            virtual;
    procedure Clear;                                  virtual;
    procedure Cancel;                                 virtual;
    function  TerminationSynchro: ISynchro;           virtual;
    procedure QueueChildren;                          virtual;
    procedure CleanUpAfterDoneOrCancelled;            virtual;
    function  TaskLoad: integer;                      virtual;

  public
    constructor Create( AWorkFactory: TWorkFactory; AJob: TTaskProc; AParent: TTask);  virtual;
    destructor Destroy; override;
  end;
  TTaskClass = class of TTask;

  TDecoratedTask = class( TTask)
  protected
    FBase: ITaskEx;
    function  GetDatum: TObject;                                 override;
    procedure SetDatum( Value: TObject);                         override;
    function  GetStatus: TTaskStatus;                            override;
    procedure SetStatus( Value: TTaskStatus);                    override;
    function  IsDone: boolean;                                   override;
    function  SetOnDone( Proc: TTaskProc): ITask;                override;
    function  Clone: ITask;                                      override;
    procedure DereferenceParent;                                 override;
    procedure AddChildren( const Addends: array of ITaskEx);     override;
    procedure EndAddingChildren;                      override;
    procedure AddChild( const Addend: ITaskEx);       override;
    procedure Signal;                                 override;
    procedure BeforeExecute;                          override;
    procedure Execute;                                override;
    procedure ReleaseExecuteProc;                     override;
    procedure AfterExecute;                           override;
    procedure ExecuteReturn;                          override;
    procedure MarkExceptionThrown;                    override;
    function  DoCaptureException: boolean;            override;
    procedure Clear;                                  override;
    procedure Cancel;                                 override;
    function  TerminationSynchro: ISynchro;           override;
    procedure CleanUpAfterDoneOrCancelled;            override;

  public
    constructor CreateDecorated( AWorkFactory: TWorkFactory; const ABase: ITaskEx);
    destructor Destroy; override;
  end;

  TBaseFuture = class abstract( TTask)
  protected
    FCapturedException: Exception;
    FReturnLock: TSBDSpinLock;

    function AsIFuture: IInterface;   virtual; abstract;

  public
    constructor Create( AWorkFactory: TWorkFactory; AJob: TTaskProc; AParent: TTask);  override;
    destructor Destroy; override;
  end;


  {$WARN HIDDEN_VIRTUAL OFF}
  // Suppressed Warning: SetOnDone() hides vmethod of base type.
  TFuture<T> = class( TBaseFuture, IFuture<T>)
  private
    FValue: T;
    FFunc: TFutureFunc<T>;
    FDoneProc: TProcessValueProc<T>;

    function  QueueFuture: IFuture<T>;
    function  TryValue( var Value: T; TimeOut: cardinal = FOREVER): boolean;
    function  SetOnDone( OnDoneProc: TProcessValueProc<T>): IFuture<T>;   overload;
    function  GetValue: T;

    function  GetReturnAction( var ReturnProc: TProcessValueProc<T>; var PropagatedException: Exception): boolean;
    procedure ComputeValueOrCaptureException( Task: TBaseFuture);
    procedure CallReturnProcOrPropageException( Task: TBaseFuture);

  protected
    function  AsIFuture: IInterface;                  override;
    procedure ReleaseExecuteProc;                     override;
    procedure Clear;                                  override;
    procedure Execute;                                override;
    procedure ExecuteReturn;                          override;
    function  FundamentalClone: TTask;                override;

  public
    constructor Create( AWorkFactory: TWorkFactory; AFunc: TFutureFunc<T>; AParent: TTask);    virtual;
    destructor Destroy; override;
  end;
  {$WARN HIDDEN_VIRTUAL ON}

  TFutureBoolean = class( TFuture<boolean>) end;

  TJoinTask = class( TTask)
  public
    constructor CreateJoin( AWorkFactory: TWorkFactory; const ATasks: array of ITask);
  protected
    procedure Execute;                                override;
    procedure Cancel;                                 override;
  end;

  TJoinIntegerTask = class( TJoinTask)
  public
    [Volatile] FValue: TVolatileInt32;
    constructor CreateIntegerJoin( AWorkFactory: TWorkFactory; const ATasks: array of ITask; InitialValue: integer);
  protected
    procedure CleanUpAfterDoneOrCancelled; override;
  end;

  TJoinObjectOwningTask = class( TJoinTask)
  private
    FObj: TObject;
  public
    constructor CreateObjectJoin( AWorkFactory: TWorkFactory; const ATasks: array of ITask; AObj: TObject);
    destructor Destroy; override;
  protected
    procedure CleanUpAfterDoneOrCancelled; override;
  end;

  TValuePipelineBase = class abstract( TInterfacedObject, IValuePipelineBase)
  protected
    procedure Start; virtual; abstract;
  end;

  TStageTaskFactory = reference to function( AFactory: TWorkFactory; Stage: TObject) : ITask;

  TValuePipeline<T> = class( TValuePipelineBase, IValuePipeline<T>)
  private type
    TStage<U> = class
    private
      FPipeline: TValuePipeline<U>;
      FIdx: integer;
      FStagerTasks: TList<ITask>;
      FInData: IPipe<U>;
      FOutData: IPipe<U>;
      [Volatile] FIncompleteCount: TVolatileUint32;

      procedure Start;
      constructor CreateStage( AOwner: TValuePipeline<U>; StageTaskFactory: TStageTaskFactory; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal);

    public
      constructor CreateStageFromSingleFunc( AOwner: TValuePipeline<U>; StageTask: TSingleFunc<U>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal);
      constructor CreateStageFromPipeToPipe( AOwner: TValuePipeline<U>; StageTask: TPipeToPipe<U>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal);
      destructor Destroy; override;
    end;

  private type
    TSingleFuncStager<U> = class( TTask)
    private
      FStageFunc: TSingleFunc<U>;
      FInData, FOutData: IPipe<U>;
      FStage: TStage<U>;

    protected
      procedure Execute;                                override;
      procedure ReleaseExecuteProc;                     override;
      procedure Clear;                                  override;

    public
      constructor CreateSingleStager( AFactory: TWorkFactory; AStage: TStage<U>; StageTask: TSingleFunc<U>; const AInData, AOutData: IPipe<U>);
    end;

    TPipeToPipeStager<U> = class( TTask)
    private
      FStageFunc: TPipeToPipe<U>;
      FInData, FOutData: IPipe<U>;
      FStage: TStage<U>;

    protected
      procedure Execute;                                override;
      procedure ReleaseExecuteProc;                     override;
      procedure Clear;                                  override;

    public
      constructor CreatePipeStager( AFactory: TWorkFactory; AStage: TStage<U>; StageTask: TPipeToPipe<U>; const AInData, AOutData: IPipe<U>);
    end;

  private
    FFactory: TWorkFactory;
    FInputQueue: IPipe<T>;
    FOutputQueue: IPipe<T>;
    FStages: TObjectList<TStage<T>>;

    function InputQueue: IPipe<T>;
    function OutputQueue: IPipe<T>;
    function Stage( StageTask: TSingleFunc<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>; overload;
    function Stage( StageTask: TPipeToPipe<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>; overload;

  protected
    procedure Start; override;

  public
    constructor Create( AFactory: TWorkFactory; Capacity, LowWaterMark, HighWaterMark: cardinal);
    destructor Destroy; override;
  end;


  TForkJoin<T> = class( TFuture<T>)
  private type
    TForkJoinProblemStatus = (UntestedHolistic, ForkableHolistic, InWorkForked, JoinableForked, Solved);

  private type
    TProc_Bool = reference to procedure( Value: boolean);

  private type
    TForkJoinProblem = class;
    IForkJoinProblem = interface ['{81B15F6C-C47B-4A31-86BD-CE06BD182D00}']
      function AsObject: TObject;
    end;

    TForkJoinProblem = class( TInterfacedObject, IForkJoinProblem)
    private
      function AsObject: TObject;
    public
      FStatus: TForkJoinProblemStatus;
      FDatum: T;      // Default(T) when forked.
      FChildren: TArray<IForkJoinProblem>;  // The array is empty when holistic or solved.
      FCountOfUnsolvedChildren: cardinal; // only can be non-zero when ForkedWithUnsolvedChildren, then it is 1+
      FForkParent: TForkJoinProblem;  // nil when it is the seed.

      constructor Create( AStatus: TForkJoinProblemStatus; const ADatum: T; AForkParent: TForkJoinProblem);
    end;

  private type
    TProblemStack = class( TStack<IForkJoinProblem>)
    end;

  private type
    TForkJoiner = class( TTask)
    private
      FOwner: TForkJoin<T>;
    protected
      procedure Execute;                                override;
    public
      constructor CreateForker( AOwner: TForkJoin<T>);
    end;

  private
    FDoFork: TTestFunc<T>;
    FFork  : TForkFunc<T>;
    FJoin  : TJoinFunc<T>;
    FSeed  : IForkJoinProblem;       // Seed is the root problem

    FJoinStack: TProblemStack;
    FForkStack: TProblemStack;
    FTestStack: TProblemStack;

    FhasCancelled: boolean;
    [Volatile] FCapturedException: TVolatileObject; // Exception
    FActionable: TEvent; // manual-reset event.
    FUnfinishedChildTasks: ICountDown;
    FIsActionable: boolean;
    FCountOfProblemsInWork: integer;
    FQueueLock: TSBDSpinLock;

    function  Act( Action: TForkJoinerAction; Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
    function  Test( Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
    function  Fork( Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
    function  Join( Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
    procedure MarkAsSolved( Problem: TForkJoinProblem; EnterLockProc: TProc_Bool);

  public
    constructor CreateForkJoin( const AFactory: TWorkFactory; const ASeed: T; NumTasks: cardinal; ADoFork: TTestFunc<T>; AFork: TForkFunc<T>; AJoin: TJoinFunc<T>);
    destructor Destroy; override;
  end;

{$REGION 'Private declarations - do not use'}
// These are in the interface section due to the limitations of generics.
TCoForEachIntegerTaskFactory = class sealed( TCoForEachTaskFactory)
  public
    constructor Create( WF: TWorkFactory; ThreadCount: integer; LowIdx, HighIdx, StepIdx: integer; Proc: TForEachProc);
    function CreateTemplate     ( WF: TWorkFactory                                 ): ITask;   override;
    function CreateCompositeTask( WF: TWorkFactory; const Iterators: array of ITask): ITask;   override;

  protected
    FLowIdx: integer;
    FHighIdx: integer;
    FStepIdx: integer;
    FCompositeTaskObj: TJoinIntegerTask;
    FProc: TForEachProc;
  end;

TCoForEachEnumerableTaskFactory<T> = class abstract( TCoForEachTaskFactory)
  public
    constructor Create( WF: TWorkFactory; ThreadCount: integer; CollectionCursorIsThreadSafe: boolean; Proc: TForEachItemProc<T>);
  protected
    FIsThreadSafe: boolean;
    FProc        : TForEachItemProc<T>;
    FCursorLock  : ILock;
  end;

TCoForEachEnumerableObjTaskFactory<T> = class sealed( TCoForEachEnumerableTaskFactory<T>)
  public
    constructor Create( WF: TWorkFactory; ThreadCount: integer; Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; Proc: TForEachItemProc<T>);
    function CreateTemplate( WF: TWorkFactory): ITask;   override;
    function CreateCompositeTask( WF: TWorkFactory; const Iterators: array of ITask): ITask;   override;
  protected
    FCursor: TEnumerator<T>;
  end;

TCoForEachEnumerableIntfTaskFactory<T> = class sealed( TCoForEachEnumerableTaskFactory<T>)
  public
    constructor Create( WF: TWorkFactory; ThreadCount: integer; Collection: IEnumerable<T>; CollectionCursorIsThreadSafe: boolean; Proc: TForEachItemProc<T>);
    function CreateTemplate( WF: TWorkFactory): ITask;   override;
  protected
    FCursor: IEnumerator<T>;
  end;

procedure EnterLock( var SpinLock: TSBDSpinLock; var EnteredFlag: boolean; Value: boolean);
{$ENDREGION}

implementation






uses System.Types, System.Generics.Defaults;



type
  TSchedJobComparer = class( TInterfacedObject, IComparer<RSchedJob>)
  private
    function Compare( const Left, Right: RSchedJob): integer;
  end;

  IGeometricFuture = interface ['{41093AE2-0EA2-4044-AAD2-233A30D02DE9}'] end;
  TGeometricFuture = class( TFuture<boolean>, IGeometricFuture)
  protected
    function  TaskLoad: integer;                      override;
    function  FundamentalClone: TTask;                override;

  public
    FTrialIndex: integer;
    FPause   : integer;
    FGrowth  : double;
    FMaxPause: integer;
    FTryCount: integer;

    constructor CreateGeo( AWorkFactory: TWorkFactory; AFunc: TFutureFunc<boolean>;
      AInitialRetryPause: integer; APauseGrowth: double; AMaxRetryPause, AMaxTryCount: integer);
    procedure IncrementTryCount;
    function  MaxedOut: boolean;
    procedure BumpPausePeriod;
    function  NextTrialMatures: TDateTime;
  end;


constructor TWorkFactory.CreateWorkFactory(
  MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventCount1: integer; const SharedLock: ILock);
var
  Capacity: integer;
begin
   //SiAuto.SiMain
   FSynchroPool      := TSynchroFactory.Create( MaxEventCount1);
   FLock             := TSynchroFactory.AcquireCriticalSection;
   FShareLock := SharedLock;
   if not assigned( FShareLock) then
     FShareLock := TSynchroFactory.AcquireCriticalSection;
   FThreadCount .Initialize(0);
   FIdleCount   .Initialize(0);
   FisTerminated.Initialize( 0);
   FWorkEnqueued.Initialize( 0);
   FMaxThreadCount := MaxThreadCount1;
   if FMaxThreadCount = 0 then
     FMaxThreadCount := 1;
   Capacity := FMaxThreadCount * 2;
   FTaskQueue := TJobPipe.Create( self, Capacity-1, 1, Capacity);
   FTaskQueue._AddRef;
   FSchedQueue   := TSchedQueue.Create;
   FMaxIdleCount := MaxIdleCount1;
   if FMaxIdleCount >= FMaxThreadCount then
     FMaxIdleCount := FMaxThreadCount - 1;
   FMinIdleCount := MinIdleCount1;
   if FMinIdleCount > FMaxIdleCount then
     FMinIdleCount := FMaxIdleCount;
   FTaskThreads := TObjectList<TTaskThread>.Create( False);
   FReturnQueue := TJobPipe.Create( self, Capacity-1, 1, Capacity);
   FReturnQueue._AddRef;
   FWorkCapacity.Initialize( FMaxThreadCount);
   FHouseKeepAlert := TSBDParallel.Event( False, False);
   FWorkAlert      := TSBDParallel.Event( False, False);
   FAllThreadsDone := FSynchroPool.AcquireCountDown( 1, False);
   FHouseKeeper    := THouseKeeperThread.Create( self)
end;

destructor TWorkFactory.Destroy;
begin
   FLock.Enter;
   FisTerminated.Write( 1);
   if assigned( FHouseKeeper) then
     FHouseKeeper.FisTerminated.Write( 1);
   FWorkAlert.Signal;
   FTaskQueue.Complete;
   FReturnQueue.Complete;
   FLock.Leave;
   FAllThreadsDone.WaitFor;
   FAllThreadsDone := nil;
   FTaskQueue._Release;
   FReturnQueue._Release;
   FTaskThreads.Free;
   FSchedQueue.Free;
   FLock := nil;
   if FSynchroPool.CanDestroy then
       FSynchroPool.Free
     else
       raise Exception.Create( 'Allocated locks must be all released before TWorkFactory is destroyed.');
   inherited
end;


function TWorkFactory.Abandonable(
  const BaseTask: ITask; Timeout: integer): IFuture<boolean>;
begin
  result := TTasking.CreateFuture<boolean>( self,
    function( const Future: IFuture<boolean>): boolean
    begin
      result := BaseTask.Queue.TerminationSynchro.WaitFor( Timeout) = wrSignaled;
      if not result then
        AbandonTask( BaseTask)
    end)
end;


procedure TWorkFactory.AbandonTask( const Task: ITask);
var
  Thread: TTaskThread;
begin
  Task.Cancel;
  FLock.Enter;
  Thread := (Task as ITaskEx).AttachedThread;
  if Thread.Status = ttsInWork then
    Thread.Status := ttsTerminated;
  FLock.Leave
end;

function TWorkFactory.AsObject: TObject;
begin
  result := self
end;

function TWorkFactory.CountDown( InitialValue: cardinal): ICountDown;
begin
  result := FSynchroPool.AcquireCountDown( InitialValue, True)
end;

function TWorkFactory.Event( ManualReset, InitialState: boolean): IEvent;
begin
  result := FSynchroPool.AcquireKernelEvent( ManualReset, InitialState, True)
end;

function TWorkFactory.LightEvent(
  ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;
begin
  result := FSynchroPool.AcquireLightEvent( ManualReset, InitialState, SpinMax, True)
end;



function TWorkFactory.CanStart( TaskObj: TTask): boolean;
begin
  FLock.Enter;
  result := (FisTerminated.Read = 0) and
            (TaskObj.GetStatus = tsConfigurable) and
            (FWorkCapacity.Read > 0);
  FLock.Leave
end;

function TWorkFactory.CoBeginTask( Proc: TTaskProc): ITask;
var
  Task: TTask;
begin
  Task := TTask.Create( self, Proc, nil);
  Task.EndAddingChildren;
  result := Task
end;


constructor TCoForEachTaskFactory.Create( WF: TWorkFactory; ThreadCount: integer);
var
  Iterators: array of ITask;
  IteratorCount: integer;
  i: integer;
begin
  FTemplate := CreateTemplate( WF);
  IteratorCount := ThreadCount;
  if IteratorCount < 1 then
    IteratorCount := 1;
  if IteratorCount > TWorkFactory.MaxCoForEachThreadCount then
    IteratorCount := TWorkFactory.MaxCoForEachThreadCount;
  SetLength( Iterators, IteratorCount);
  Iterators[0] := FTemplate;
  for i := 1 to IteratorCount - 1 do
    Iterators[i] := FTemplate.Clone;
  FCompositeTask := CreateCompositeTask( WF, Iterators)
end;


function TCoForEachTaskFactory.CreateCompositeTask(
  WF: TWorkFactory; const Iterators: array of ITask): ITask;
begin
  result := TJoinTask.CreateJoin( WF, Iterators)
end;


constructor TCoForEachEnumerableIntfTaskFactory<T>.Create(WF: TWorkFactory;
  ThreadCount: integer; Collection: IEnumerable<T>;
  CollectionCursorIsThreadSafe: boolean; Proc: TForEachItemProc<T>);
begin
  FCursor := Collection.GetEnumerator;
  inherited Create( WF, ThreadCount, CollectionCursorIsThreadSafe, Proc)
end;

function TCoForEachEnumerableIntfTaskFactory<T>.CreateTemplate(
  WF: TWorkFactory): ITask;
begin
  if FIsThreadSafe then
      result := WF.CoBeginTask(
        procedure( const Task: ITask)
          begin
          while FCursor.MoveNext do
            FProc( FCursor.Current, FCompositeTask)
          end)
    else
      result := WF.CoBeginTask(
        procedure( const Task: ITask)
          var
            doMoveNext: boolean;
            Current   : T;
          begin
          repeat
            FCursorLock.Enter;
              doMoveNext := FCursor.MoveNext;
              if doMoveNext then
                  Current := FCursor.Current
                else
                  Current := Default( T);
            FCursorLock.Leave;
            if doMoveNext then
              FProc( Current, FCompositeTask)
          until not doMoveNext
          end)
end;

constructor TCoForEachIntegerTaskFactory.Create( WF: TWorkFactory; ThreadCount: integer; LowIdx, HighIdx, StepIdx: integer; Proc: TForEachProc);
begin
  FLowIdx  := LowIdx;
  FHighIdx := HighIdx;
  FStepIdx := StepIdx;
  FProc    := Proc;
  FCompositeTaskObj := nil;
  // FCompositeTaskObj is created later.
  inherited Create( WF, ThreadCount)
end;

function TCoForEachIntegerTaskFactory.CreateTemplate( WF: TWorkFactory): ITask;
begin
  result := WF.CoBeginTask(
    procedure( const Task: ITask)
      var
        OldValue, NewValue: integer;

        function IsWithinRange( IndexValue: integer): boolean;
        begin
          // If StepIdx is +ve, HighIdx is an upper bound.
          // If StepIdx is -ve, HighIdx is an lower bound.
          // If StepIdx is zero (edge case), we shouldn't do any iteration.
          result := ((IndexValue <= FHighIdx) and (FStepIdx > 0))  or
                    ((IndexValue >= FHighIdx) and (FStepIdx < 0))
        end;

      begin
      repeat
        NewValue := FCompositeTaskObj.FValue.Add( FStepIdx);
        OldValue := NewValue - FStepIdx;
        if ((NewValue < OldValue) and (FStepIdx > 0)) or
           ((NewValue > OldValue) and (FStepIdx < 0)) then
          // This can only occur if there is an integer overflow.
          // If it is an overflow, it is time to cease iteration.
          // This should be a rare edge case.
          break;
        if IsWithinRange( OldValue) then
          FProc( OldValue, FCompositeTask)
      until not IsWithinRange( NewValue)
      end);
end;

function TCoForEachIntegerTaskFactory.CreateCompositeTask( WF: TWorkFactory; const Iterators: array of ITask): ITask;
begin
  FCompositeTaskObj := TJoinIntegerTask.CreateIntegerJoin( WF, Iterators, FLowIdx);
  result := FCompositeTaskObj
end;


constructor TCoForEachEnumerableTaskFactory<T>.Create(WF: TWorkFactory;
  ThreadCount: integer; CollectionCursorIsThreadSafe: boolean;
  Proc: TForEachItemProc<T>);
begin
  FIsThreadSafe := CollectionCursorIsThreadSafe;
  FProc         := Proc;
  if not FIsThreadSafe then
    FCursorLock := WF.NewLock( KernelLocking);
  inherited Create( WF, ThreadCount)
end;



constructor TCoForEachEnumerableObjTaskFactory<T>.Create( WF: TWorkFactory; ThreadCount: integer; Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; Proc: TForEachItemProc<T>);
begin
  FCursor := Collection.GetEnumerator;
  inherited Create( WF, ThreadCount, CollectionCursorIsThreadSafe, Proc)
end;

function TCoForEachEnumerableObjTaskFactory<T>.CreateCompositeTask(
  WF: TWorkFactory; const Iterators: array of ITask): ITask;
begin
  result := TJoinObjectOwningTask.CreateObjectJoin( WF, Iterators, FCursor)
  // The task owns the cursor object, and will destroy it when it cleans up.
end;

function TCoForEachEnumerableObjTaskFactory<T>.CreateTemplate( WF: TWorkFactory): ITask;
begin
  if FIsThreadSafe then
      result := WF.CoBeginTask(
        procedure( const Task: ITask)
          begin
          while FCursor.MoveNext do
            FProc( FCursor.Current, FCompositeTask)
          end)
    else
      result := WF.CoBeginTask(
        procedure( const Task: ITask)
          var
            doMoveNext: boolean;
            Current   : T;
          begin
          repeat
            FCursorLock.Enter;
              doMoveNext := FCursor.MoveNext;
              if doMoveNext then
                  Current := FCursor.Current
                else
                  Current := Default( T);
            FCursorLock.Leave;
            if doMoveNext then
              FProc( Current, FCompositeTask)
          until not doMoveNext
          end)
end;


function TWorkFactory.CoForEachBase( ThreadCount: integer; TaskFact: TTaskFactFunc): ITask;
var
  Factory: TCoForEachTaskFactory;
begin
  Factory := TaskFact( self, ThreadCount);
  try
    result  := Factory.FCompositeTask
  finally
    Factory.Free;
    end
end;

function TWorkFactory.CoForEach(
  LowIdx, HighIdx, StepIdx, ThreadCount: integer; Proc: TForEachProc): ITask;
begin
  result := CoForEachBase( ThreadCount,
    function( AWF: TWorkFactory; AThreadCount: integer): TCoForEachTaskFactory
    begin
      result := TCoForEachIntegerTaskFactory.Create( AWF, AThreadCount, LowIdx, HighIdx, StepIdx, Proc)
    end)
end;


function TWorkFactory.CoForEach<T>(
  Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;
begin
  result := CoForEachBase( ThreadCount,
    function( AWF: TWorkFactory; AThreadCount: integer): TCoForEachTaskFactory
    begin
      result := TCoForEachEnumerableObjTaskFactory<T>.Create( self, ThreadCount, Collection, CollectionCursorIsThreadSafe, Proc)
    end)
end;

function TWorkFactory.CoForEach<T>(
  Collection: IEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer;
  Proc: TForEachItemProc<T>): ITask;
begin
  result := CoForEachBase( ThreadCount,
    function( AWF: TWorkFactory; AThreadCount: integer): TCoForEachTaskFactory
    begin
      result := TCoForEachEnumerableIntfTaskFactory<T>.Create( self, ThreadCount, Collection, CollectionCursorIsThreadSafe, Proc)
    end)
end;

function TWorkFactory.CoBeginTask( Proc: System.SysUtils.TProc): ITask;
begin
  result := CoBeginTask(
    procedure( const Task: ITask)
      begin
      Proc();
      end)
end;


function TWorkFactory.QueueJob( const Task: ITask): boolean;
var
  willEnqueue, hasEnqueued: boolean;
  doHouseKeep: boolean;
  TaskEx: ITaskEx;
begin
  result := True;
  if not Supports( Task, ITaskEx, TaskEx) then
    TaskEx := nil;
  if not assigned( TaskEx) then
    raise Exception.Create( 'TWorkFactory.StartTask() - no task.');
  doHouseKeep := False;
  willEnqueue := False;
  FLock.Enter;
  try
    if FisTerminated.Read = 1 then
      raise Exception.Create( 'TWorkFactory.StartTask() when terminated');
    if TaskEx.Status <> tsConfigurable then exit;
    willEnqueue := FWorkCapacity.Read > 0;
    doHouseKeep := (FIdleCount.Read = 0) and willEnqueue;
    if willEnqueue then
      begin
      FWorkCapacity.Decrement;
      FWorkEnqueued.Increment;
      TaskEx.Status := tsEnqueued
      end;
  finally
    FLock.Leave
    end;
  hasEnqueued := willEnqueue and (FTaskQueue.Enqueue( TaskEx, 0) = wrSignaled);
  if doHouseKeep then
    FHouseKeepAlert.Signal;
  result := hasEnqueued or willEnqueue;
  if not result then exit;
  if not hasEnqueued then
    begin
    FTaskQueue.Enqueue( TaskEx, FOREVER);
    hasEnqueued := True
    end;
  if hasEnqueued then
    FWorkAlert.Signal // This is an auto-reset event.
end;

procedure TWorkFactory.SchedQueueJob( AMaturity: TDateTime; const Task: ITaskEx);
var
  Addend: RSchedJob;
  doSignal: boolean;
  Next: TDateTime;
  isQueueable: boolean;
begin
  Addend.FMaturity := AMaturity;
  Addend.FJob      := Task;
  FLock.Enter;
  isQueueable := Task.Status = tsConfigurable;
  doSignal := isQueueable and (not FSchedQueue.NextMaturity( Next)) or (AMaturity < Next);
  if isQueueable then
    FSchedQueue.OrderedAdd( Addend);
  FLock.Leave;
  if doSignal then
    FHouseKeepAlert.Signal
end;

procedure TWorkFactory.StartTask( const TaskIntf: ITask);
begin
  if not QueueJob( TaskIntf) then
    raise Exception.Create( 'TWorkFactory.StartTask() - WorkFactory has exceeded maximum thread count.')
end;


function TWorkFactory.SynchroPool: TSynchroFactory;
begin
  result := FSynchroPool
end;

function TWorkFactory.ProcessJobReturn( TimeOut: cardinal): boolean;
var
  TaskEx: ITaskEx;
begin
  result := FReturnQueue.Dequeue( TaskEx, TimeOut) = wrSignaled;
   if not result then exit;
   try
     TaskEx.ExecuteReturn
   finally
     TaskEx.Clear
     end
end;

function TWorkFactory.FactoryLock: ILock;
begin
  result := FLock
end;

constructor TGeometricFuture.CreateGeo( AWorkFactory: TWorkFactory; AFunc: TFutureFunc<boolean>;
      AInitialRetryPause: integer; APauseGrowth: double; AMaxRetryPause, AMaxTryCount: integer);
begin
  inherited Create( AWorkFactory, AFunc, nil);
  FTrialIndex := 0;
  FPause      := AInitialRetryPause;
  FGrowth     := APauseGrowth;
  FMaxPause   := AMaxRetryPause;
  FTryCount   := AMaxTryCount
end;

function TGeometricFuture.TaskLoad: integer;
begin
  result := inherited + 1
end;

function TGeometricFuture.FundamentalClone: TTask;
var
  Newbie: TGeometricFuture;
begin
  result := inherited FundamentalClone;
  Newbie := result as TGeometricFuture;
  Newbie.FTrialIndex := 0;
  Newbie.FPause      := FPause;
  Newbie.FGrowth     := FGrowth;
  Newbie.FMaxPause   := FMaxPause;
  Newbie.FTryCount   := FTryCount
end;


procedure TGeometricFuture.IncrementTryCount;
begin
  Inc( FTrialIndex)
end;

function TGeometricFuture.MaxedOut: boolean;
begin
  result := FTrialIndex >= FTryCount
end;

procedure TGeometricFuture.BumpPausePeriod;
begin
  if FTrialIndex >= 2 then
    FPause := Round( FPause * FGrowth);
  if FPause  > FMaxPause then
     FPause := FMaxPause
end;

function TGeometricFuture.NextTrialMatures: TDateTime;
begin
  result := Now + FPause
end;

function TWorkFactory.GeometricRetry(
        BaseFunc: TFutureFunc<boolean>; InitialRetryPause: integer; PauseGrowth: double;
        MaxRetryPause, MaxTryCount: integer): IFuture<boolean>;
var
  ChildTemplate: TFuture<boolean>;
  StaticParentFuture: TGeometricFuture;
  ChildFunc, ParentFunc: TFutureFunc<boolean>;
  BaseFuncLcl: TFutureFunc<boolean>;
begin
  BaseFuncLcl := BaseFunc;
  ChildFunc :=
    function( const Future: IFuture<boolean>): boolean
    var
      NextDynamicChild: ITaskEx;
    begin
      result := (not assigned( BaseFuncLcl)) or BaseFuncLcl( StaticParentFuture);
      StaticParentFuture.FReturnLock.Enter;
      StaticParentFuture.IncrementTryCount;
      StaticParentFuture.FValue := result;
      StaticParentFuture.FReturnLock.Leave;
      if result or StaticParentFuture.MaxedOut then
          // TGeometricFuture.SetFinalStatus() adds one count.
          // This takes it away, telling the client of the parent future, we are done.
          StaticParentFuture.FTermination.Signal
        else
          begin
          StaticParentFuture.BumpPausePeriod;
          NextDynamicChild := ChildTemplate.Clone as ITaskEx;
          // Wait the pause time before trying again.
          SchedQueueJob( StaticParentFuture.NextTrialMatures, NextDynamicChild)
          end
    end;
  ChildTemplate := TFutureBoolean.Create( self, ChildFunc, nil);
  ChildTemplate.EndAddingChildren;

  ParentFunc :=
    function( const Future: IFuture<boolean>): boolean
    var
      DynamicChild: IFuture<boolean>;
    begin
      DynamicChild := ChildTemplate.Clone.AsObject as TFutureBoolean;
      QueueJob( DynamicChild);
      result := False
    end;
  StaticParentFuture := TGeometricFuture.CreateGeo( self, ParentFunc, InitialRetryPause, PauseGrowth, MaxRetryPause, MaxTryCount);
  // Overload TaskLoad to add one to the load count.
  StaticParentFuture.EndAddingChildren;
  result := StaticParentFuture
end;



function TWorkFactory.NewLock( Locking: TLockingMechanism): ILock;
begin
  case Locking of
    KernelLocking: result := FSynchroPool.AcquireCriticalSection( True); //  _CreateCritLockIntf( FSynchroPool);
    BusLocking   : result := FSynchroPool.AcquireSpinLock;               // _CreateSpinLockIntf;
  end
end;


function TWorkFactory.CompositeSynchro_WaitAll(
  const AFactors: TSynchroArray; APropagation: TWaitPropagation): ICompositeSynchro;
begin
  result := TCompositeSynchro.Create(
    AFactors, FSynchroPool, APropagation, TCompositeSynchro.AllTest(), TestAll, False)
end;

function TWorkFactory.CompositeSynchro_WaitAny(
  const AFactors: TSynchroArray; APropagation: TWaitPropagation): ICompositeSynchro;
begin
  result := TCompositeSynchro.Create(
    AFactors, FSynchroPool, APropagation, TCompositeSynchro.AnyTest(), TestAny, False)
end;

function TWorkFactory.WaitForAll(
  const AFactors: TSynchroArray; APropagation: TWaitPropagation; TimeOut: cardinal): TWaitResult;
var
  Dummy: integer;
begin
  result := CompositeSynchro_WaitAll( AFactors, APropagation).WaitFor( Dummy, Timeout)
end;

function TWorkFactory.WaitForAny(const AFactors: TSynchroArray;
  APropagation: TWaitPropagation; var SignallerIdx: integer;
  TimeOut: cardinal): TWaitResult;
begin
  result := CompositeSynchro_WaitAny( AFactors, APropagation).WaitFor( SignallerIdx, Timeout)
end;

function TWorkFactory.Join( const Tasks: array of ITask): ITask;
begin
  result := TTasking.Join( self, Tasks)
end;

function TWorkFactory.Join( const Procs: array of TTaskProc): ITask;
var
  L, i: integer;
  Tasks: array of ITask;
begin
  L := Length( Procs);
  SetLength( Tasks, L);
  for i := 0 to L - 1 do
    Tasks[i] := CoBeginTask( Procs[i]);
  result := Join( Tasks)
end;

function TWorkFactory.Join( const Procs: array of System.SysUtils.TProc): ITask;
var
  L, i: integer;
  Tasks: array of ITask;
begin
  L := Length( Procs);
  SetLength( Tasks, L);
  for i := 0 to L - 1 do
    Tasks[i] := CoBeginTask( Procs[i]);
  result := Join( Tasks)
end;

function TWorkFactory.SelfJoin(
  const BaseTask: ITask; ThreadCount: integer): ITask;
var
  SubTasks: array of ITask;
  Count, j: integer;
begin
  Count := ThreadCount;
  if Count < 1 then
    Count := 1;
  if Count = 1 then
      result := BaseTask.Clone
    else
      begin
      SetLength( SubTasks, Count);
      for j := 0 to Count - 1 do
        SubTasks[j] := BaseTask.Clone;
      result := Join( SubTasks)
      end
end;


function TWorkFactory.ShareLock: ILock;
begin

end;

function TWorkFactory.ModularCountDown( AInitialValue: cardinal): ICountDown;
begin
  result := ModularCountDown( CountDown( AInitialValue))
end;

function TWorkFactory.ModularCountDown( const ABase: ICountDown): ICountDown;
begin
  result := TModularCountDown.Create( ABase, FShareLock, FSynchroPool)
end;

function TWorkFactory.ModularEvent( const ABase: IEvent): IEvent;
begin
  result := TModularEvent.Create( ABase, FShareLock, FSynchroPool)
end;

function TWorkFactory.ModularEvent( ManualReset, InitialState: boolean): IEvent;
begin
  result := ModularEvent( Event( ManualReset, InitialState))
end;

function TWorkFactory.ModularSemaphore( const ABase: ISemaphore): ISemaphore;
begin
  result := TModularSemaphore.Create( ABase, FShareLock, FSynchroPool)
end;

function TWorkFactory.ModularSemaphore( AInitialCount: cardinal): ISemaphore;
begin
  result := ModularSemaphore( _CreateTestableSemaphoreIntf( nil, AInitialCount))
end;


constructor TTaskThread.Create( AOwner: TWorkFactory);
begin
  inherited Create( False);
  FWorkFactory := AOwner;
  FreeOnTerminate := True;
  FLock := FWorkFactory.FLock;
  FLock.Enter;
  FWorkAlert := FWorkFactory.FWorkAlert;
  FHouseKeeperAlert := FWorkFactory.FHouseKeepAlert;
  FWorkFactory.FTaskThreads.Add( self);
  FStatus.Initialize( Ord( ttsIdle));
  FisTerminated.Initialize( 0);
  FWorkFactory.FIdleCount.Increment;
  FWorkFactory.FThreadCount.Increment;
  FWorkFactory.FWorkCapacity.Increment;
  FWorkFactory.FAllThreadsDone.CounterSignal;
  FLock.Leave
end;


destructor TTaskThread.Destroy;
begin
  if assigned( FLock) then
    begin
    FLock.Enter;
    Status := ttsTerminated;
    FWorkFactory := nil;
    FLock.Leave;
    FLock := nil
    end;
  FisTerminated.Finalize;
  FStatus.Finalize;
  inherited
end;


function TTaskThread.GetStatus: TTaskThreadStatus;
begin
  result := TTaskThreadStatus( FStatus.Read)
end;

function TTaskThread.IsGloballyTerminated: boolean;
begin
  result := (not assigned( FWorkFactory)) or (FWorkFactory.FisTerminated.Read = 1)
end;

function TTaskThread.IsLocallyTerminated: boolean;
begin
  result := (FisTerminated.Read = 1) or Terminated
end;

function TTaskThread.IsTerminated: boolean;
begin
  result := IsLocallyTerminated or IsGloballyTerminated
end;

procedure TTaskThread.SetStatus( Value: TTaskThreadStatus);
//  Old-value  \  New-value | Idle                         |  In-work   |  Terminated
//  ------------------------+------------------------------+------------+---------------
//  Idle                    | FThreadCount     NOP         |   NOP      |    -1
//                          | FIdleCount       NOP         |   -1       |    -1
//                          | FisTerminated    NOP         |   NOP      |    set
//                          | FWorkCapacity    NOP         |   -1       |    -1
//                          | FTaskThreads     NOP         |   NOP      |    remove
//                          | FAllThreadsDone  NOP         |   NOP      |    -1
//  ------------------------+------------------------------+------------+---------------
//  In-work                 | FThreadCount     NOP         |   NOP      |    -1
//                          | FIdleCount       +1          |   NOP      |    0
//                          | FisTerminated    NOP         |   NOP      |    set
//                          | FWorkCapacity    +1          |   NOP      |    0
//                          | FTaskThreads     NOP         |   NOP      |    remove
//                          | FAllThreadsDone  NOP         |   NOP      |    -1
//  ------------------------+------------------------------+------------+---------------
//  Terminated              | (all)            NOP         |    NOP     |      NOP
//  ------------------------+------------------------------+------------+---------------
var
  OldValue: TTaskThreadStatus;
begin
  OldValue := TTaskThreadStatus( FStatus.Exchange( Ord( Value)));
  if (Value = OldValue) or (OldValue = ttsTerminated) then exit;
  FStatus.Write( Ord( Value));

  case Value of
    ttsIdle:
      begin
      FWorkFactory.FIdleCount.Increment;
      FWorkFactory.FWorkCapacity.Increment
      end;

    ttsInWork:
      begin
      FWorkFactory.FIdleCount.Decrement;
      // Don't decrement work capacity because we dequeued a task,
      //  but the capacity will not truly return until the thread is idle again.
      end;

    ttsTerminated:
      begin
      FisTerminated.Write( 1);
      FWorkFactory.FThreadCount.Decrement;
      FWorkFactory.FTaskThreads.Remove( self);
      {$IFDEF DONT_TRUST_EMBARCADERO_THREADING_LIBRARY}
        FWorkFactory.FAllThreadsDone.Signal;
      {$ELSE}
        FWorkFactory.FAllThreadsDone.AddResource( -1);
      {$ENDIF}
      FWorkFactory := nil
      end
  end
end;



procedure TTaskThread.Execute;
var
   TaskEx: ITaskEx;
   TimedOut: boolean;
   isGlobalTerminated1: boolean;
   isLocalTerminated1 : boolean;
   isTerminated1   : boolean;
   hasTask: boolean;
   doHouseKeep: boolean;
   CapturedException: Exception;
   isInsideLock: boolean;

   function SenseTerminated: boolean;
   begin
     isGlobalTerminated1 := isGloballyTerminated();
     isLocalTerminated1  := IsLocallyTerminated();
     isTerminated1       := isTerminated();
     result              := isTerminated1
   end;

   procedure CheckHouseWork;
   var
     IdleCount: cardinal;
   begin
   if doHouseKeep or (not assigned( FWorkFactory)) then exit;
   IdleCount := FWorkFactory.FIdleCount.Read;
   if (not isTerminated1) and
      ((IdleCount < FWorkFactory.FMinIdleCount) or
       (IdleCount > FWorkFactory.FMaxIdleCount) or
       (FWorkFactory.FThreadCount.Read > FWorkFactory.FMaxThreadCount)) then
     doHouseKeep := True
   end;

   procedure EnterLock;
   begin
     if isInsideLock then exit;
     isInsideLock := True;
     FLock.Enter
   end;

   procedure ExitLock;
   begin
     if not isInsideLock then exit;
     isInsideLock := False;
     FLock.Leave
   end;

begin
  doHouseKeep  := False;
  isInsideLock := False;
  repeat
    TimedOut := FWorkAlert.WaitFor( TaskQueueTimeOutPeriod) <> wrSignaled; // This is auto-reset
    hasTask  := (not SenseTerminated) and (not TimedOut) and (FWorkFactory.FTaskQueue.Dequeue( TaskEx, 0) = wrSignaled);
    EnterLock;
    if hasTask then
      begin
      FWorkFactory.FWorkEnqueued.Decrement;
      hasTask := TaskEx.Status <> tsCancelled;
      // ITask instances MUST descend from TTask. This is a requirement.
      end;
    if isTerminated1 then
      Status := ttsTerminated;
    if isLocalTerminated1 and not isGlobalTerminated1 then
      doHouseKeep := True;
    CheckHouseWork;
    if isTerminated1 or (FWorkFactory.FWorkEnqueued.Read  > 0) then
      FWorkAlert.Signal;
    if hasTask then
      begin
      Status := ttsInWork;
      TaskEx.BeforeExecute;
      TaskEx.AttachThread( self);
      ExitLock;
      // The default BeforeExecute() sets the task status to InWork and records the actual job being executed.
        try
          TaskEx.Execute
        except on Exception do
          begin
            TaskEx.MarkExceptionThrown;
            if TaskEx.DoCaptureException then
                begin
                CapturedException := Exception( AcquireExceptionObject);
                if FWorkFactory.FReturnQueue.EnqueueException( CapturedException, 0) <> wrSignaled then
                    CapturedException.Free
                end
          end
        end;
      EnterLock;
      TaskEx.AfterExecute;
      // The default AfterExecute() will set the task status to done,
      //  signal the ternination synchro and post the execution package to the output queue.
      if not isTerminated1 then
        Status := ttsIdle;
      TaskEx.DetachThread;
      CheckHouseWork;
      end;
    ExitLock;
    if doHouseKeep then
      begin
      doHouseKeep := False;
      FHouseKeeperAlert.Signal
      end;
    TaskEx     := nil
  until isTerminated
end;



constructor THouseKeeperThread.Create( AOwner: TWorkFactory);
begin
  inherited Create( False);
  FWorkFactory := AOwner;
  FLock := FWorkFactory.FLock;
  FHouseKeeperAlert := FWorkFactory.FHouseKeepAlert;
  FWorkAlert := FWorkFactory.FWorkAlert;
  FisTerminated.Initialize( 0);
  FAllThreadsDone := FWorkFactory.FAllThreadsDone;
  FMaxThreadCount := FWorkFactory.FMaxThreadCount;
  FMinIdleCount   := FWorkFactory.FMinIdleCount;
  FMaxIdleCount   := FWorkFactory.FMaxIdleCount;
  FreeOnTerminate := True
end;

destructor THouseKeeperThread.Destroy;
begin
  FisTerminated.Finalize;
  inherited
end;


function AddIntegerToCardinal( base: Cardinal; Addend: integer): cardinal; inline;
begin
  // Assume that the sum (base + addend) is within the valid range for a cardinal.
  if Addend >= 0 then
      result := base + cardinal( Addend)
    else
      result := base - cardinal( -Addend)
end;

procedure THouseKeeperThread.ComputeHowManyNewThreadsToCreate( var More: boolean; var Delta: integer);
var
  WorkEnqueued, IdleCount: cardinal;
  QueueDemand: cardinal;
  FutureIdle: cardinal;
  MinGuideDemand: cardinal;
  Count: cardinal;
  NewCount: cardinal;
begin
   FLock.Enter;
   WorkEnqueued := FWorkFactory.FWorkEnqueued.Read;
   IdleCount    := FWorkFactory.FIdleCount.Read;
   if WorkEnqueued > IdleCount then
       QueueDemand := WorkEnqueued - IdleCount
     else
       QueueDemand := 0;
   FutureIdle := IdleCount + QueueDemand;
   if FMinIdleCount > FutureIdle then
       MinGuideDemand := FMinIdleCount - FutureIdle
     else
       MinGuideDemand := 0;
   FutureIdle := FutureIdle + MinGuideDemand;
   if FutureIdle  > FMaxIdleCount then
      FutureIdle := FMaxIdleCount;
   Delta      := FutureIdle - IdleCount;
   Count      := FWorkFactory.FThreadCount.Read;
   NewCount   := AddIntegerToCardinal( Count, Delta);
   if NewCount  > FMaxThreadCount then
      NewCount := FMaxThreadCount;
   Delta      := NewCount - Count;
   if (Delta < 0) and (cardinal(-Delta) > IdleCount) then
     Delta := - IdleCount;
   More := False;
   if Delta > 1 then
       begin
       Delta := 1;
       More  := True
       end
     else if Delta < -1 then
       begin
       Delta := -1;
       More  := True
       end;
   if (not More) and (QueueDemand > 0) and (Delta <= 0) then
     More := True;
   FLock.Leave
end;

procedure THouseKeeperThread.Execute;
var
  More: boolean;
  Delta: integer;
  doSignalWork: boolean;
  Thread: TTaskThread;
  j: integer;
  NowTime, Next: TDateTime;
  SleepTime: double;
  SchedJob: ITaskEx;
begin
   NowTime := Now;
   More    := False;
   repeat
     if Terminated then break;
     if More then
         begin
         SleepTime := 0.0;
         Sleep( 1);
         TThread.Yield;
         More := False
         end
       else
         begin
         FLock.Enter;
         if FWorkFactory.FSchedQueue.NextMaturity( Next) then
             begin
             NowTime := Now;
             SleepTime := Next - NowTime;
             if SleepTime < 0.0 then
               SleepTime := 0.0;
             if SleepTime > HouseKeepingPeriod then
               SleepTime := HouseKeepingPeriod
             end
           else
             SleepTime := HouseKeepingPeriod;
         FLock.Leave;
         FHouseKeeperAlert.WaitFor( Round( SleepTime))
         end;
     if Terminated then break;
     if SleepTime > 0.0 then
       NowTime := Now;
     FLock.Enter;
     while FWorkFactory.FSchedQueue.PopMature( NowTime, SchedJob) do
       FWorkFactory.QueueJob( SchedJob as ITask);
     FLock.Leave;
     ComputeHowManyNewThreadsToCreate( More, Delta);
     if Delta = 0 then continue;
     doSignalWork := False;
     FLock.Enter;
       begin
       if Delta > 0 then
           TTaskThread.Create( FWorkFactory)
         else
           begin
           Thread := nil;
           for j := FWorkFactory.FTaskThreads.Count - 1 downto 0 do
             begin
             if TTaskThreadStatus( FWorkFactory.FTaskThreads[j].FStatus.Read) <> ttsIdle then continue;
             Thread := FWorkFactory.FTaskThreads[j];
             break
             end;
           Thread.Status := ttsTerminated;
           doSignalWork  := True
           end
       end;
     FLock.Leave;
     if doSignalWork then
       FWorkAlert.Signal;
   until (FisTerminated.Read = 1) or Terminated;
   {$IFDEF DONT_TRUST_EMBARCADERO_THREADING_LIBRARY}
     FAllThreadsDone.Signal;
   {$ELSE}
     FAllThreadsDone.AddResource( -1);
   {$ENDIF}
   FWorkFactory.FHouseKeeper := nil
end;


constructor TTask.Create( AWorkFactory: TWorkFactory; AJob: TTaskProc; AParent: TTask);
begin
  FStatus.Initialize( Ord( tsConfigurable));
  FDatum.Initialize;
  FAttachedThread.Initialize;
  FWorkFactory := AWorkFactory;
  FJob := AJob;
  FReturnJob := nil;
  FhasThrownException := False;
  FParent := AParent;
  FParentLock.Initialize;
  SetLength( FChildrenTasks, 0);
  FTermination := nil
end;


function TTask.FundamentalClone: TTask;
begin
  result := TTaskClass( ClassType).Create( FWorkFactory, FJob, FParent);
  result.FDatum.Write( FDatum.ReadObj);
  result.AddChildren( FChildrenTasks);
  if GetStatus = tsConfigurable then
    result.EndAddingChildren
end;

function TTask.Clone: ITask;
var
  Newbie: TTask;
begin
  FWorkFactory.FLock.Enter;
  if GetStatus = tsConfigurable then
      begin
      Newbie := FundamentalClone;
      result := Newbie
      end
    else
      result := nil;
  FWorkFactory.FLock.Leave
end;


destructor TTask.Destroy;
begin
  FStatus.Finalize;
  FDatum.Finalize;
  FParentLock.Finalize;
  FAttachedThread.Finalize;
  inherited
end;

function TTask.AsObject: TObject;
// Static method. Cannot be decorated
begin
  result := self
end;


function TTask.AttachedThread: TTaskThread;
begin
  result := FAttachedThread.ReadObj as TTaskThread
end;

procedure TTask.AttachThread( AThread: TTaskThread);
begin
  FAttachedThread.Write( AThread)
end;

procedure TTask.DetachThread;
begin
  FAttachedThread.Write( nil)
end;

function TTask.DoCaptureException: boolean;
begin
  result := True
end;



function TTask.GetDatum: TObject;
begin
  result := FDatum.ReadObj
end;

function TTask.GetStatus: TTaskStatus;
begin
  result := TTaskStatus( FStatus.Read)
end;

function TTask.IsDone: boolean;
begin
  result := FTermination.IsSignalled
end;

procedure TTask.MarkExceptionThrown;
begin
  FhasThrownException := True
end;

function TTask.Queue: ITask;
begin
  result := self;
  FWorkFactory.StartTask( result)
end;

procedure TTask.QueueChildren;
var
  Child: ITaskEx;
begin
  for Child in FChildrenTasks do
    FWorkFactory.StartTask( Child as ITask)
end;

procedure TTask.SetDatum( Value: TObject);
begin
  FDatum.Write( Value)
end;

function TTask.SetOnDone( Proc: TTaskProc): ITask;
begin
  FReturnJob := Proc
end;

procedure TTask.SetParent( const ATask: TTask);
begin
  FParentLock.Enter;
  FParent := ATask;
  FParentLock.Leave
end;

procedure TTask.SetFinalStatus( Value: TTaskStatus);
// Static method. Cannot be decorated.
// Only called from set status.
// Value = Cancelled or Done
var
  OldValue: TTaskStatus;
  isFinalised: boolean;
begin
  FWorkFactory.FLock.Enter;
  repeat
    OldValue := TTaskStatus( FStatus.Read);
    if OldValue in [tsCancelled, tsDone] then
        isFinalised := True
      else
        isFinalised := FStatus.CompareAndExchange( Ord( OldValue), Ord( Value))
  until isFinalised;
  CleanUpAfterDoneOrCancelled;
  FWorkFactory.FLock.Leave
end;


procedure TTask.SetStatus( Value: TTaskStatus);
begin
  if Value in [tsCancelled, tsDone] then
      SetFinalStatus( Value)
    else
      FStatus.Write( Ord( Value))
end;


function TTask.TerminationSynchro: ISynchro;
begin
  result := FTermination as ISynchro
end;


procedure TTask.BeforeExecute;
begin
  SetStatus( tsInWork);
  FhasThrownException := False
end;


procedure TTask.Cancel;
begin
  SetStatus( tsCancelled)
end;

function TTask.CanStart: boolean;
// Static method. Cannot be decorated.
begin
  result := assigned( FWorkFactory) and FWorkFactory.CanStart( self)
end;


procedure TTask.AddChild( const Addend: ITaskEx);
var
  Child: ITaskEx;
  isRedundant: boolean;
  L: integer;
begin
  //isRedundant := False;                 // Warning, Android, XE7: is never used
  for Child in FChildrenTasks do
    begin
    isRedundant := Child = Addend;
    if isRedundant then break
    end;
  L := Length( FChildrenTasks);
  SetLength( FChildrenTasks, L + 1);
  FChildrenTasks[L] := Addend;
  Addend.SetParent( self)
end;

procedure TTask.AddChildren( const Addends: array of ITaskEx);
var
  Addend: ITaskEx;
begin
  for Addend in Addends do
    AddChild( Addend)
end;


function TTask.TaskLoad: integer;  // overrideable.
begin
  result := Length( FChildrenTasks) + 1
end;

procedure TTask.EndAddingChildren;
begin
  FTermination := FWorkFactory.CountDown( TaskLoad)
end;

procedure TTask.AfterExecute;
begin
  Signal
end;


procedure TTask.Execute;
var
  Job: TTaskProc;
begin
  Job  := FJob;
  FJob := nil;
  if assigned( Job) then
    Job( self)
end;

procedure TTask.ExecuteReturn;
var
  Job: TTaskProc;
begin
  Job        := FReturnJob;
  FReturnJob := nil;
  if assigned( Job) then
    Job( self)
end;

procedure TTask.ReleaseExecuteProc;
begin
  FJob := nil
end;

function TTask.QueueIfCan: boolean;
// Static method. Cannot be decorated.
begin
  result := FWorkFactory.QueueJob( self)
end;

procedure TTask.DereferenceParent;
begin
  FParentLock.Enter;
  FParent := nil;
  FParentLock.Leave
end;

procedure TTask.Signal;
var
  ReturnEnqueued: boolean;
begin
  ReturnEnqueued := False;
  FParentLock.Enter;
  if FTermination.SignalHit then
    begin
    SetStatus( tsDone);
    if (not FhasThrownException) and assigned( FReturnJob) and (GetStatus <> tsCancelled) then
      ReturnEnqueued := FWorkFactory.FReturnQueue.Enqueue( self, 0) = wrSignaled;
    if assigned( FParent) then
      FParent.Signal;
    if not ReturnEnqueued then
      Clear
    end;
  FParentLock.Leave
end;

procedure TTask.CleanUpAfterDoneOrCancelled;
begin
end;


procedure TTask.Clear;
var
  Child: ITaskEx;
begin
  FParentLock.Enter;
  for Child in FChildrenTasks do
    Child.Clear;
  SetLength( FChildrenTasks, 0);
  FJob := nil;
  FReturnJob := nil;
  FParent := nil;
  FParentLock.Leave
end;




class function TTasking.CreateFuture<T>(
  const AFactory: IWorkFactory; Func: TFutureFunc<T>): IFuture<T>;
var
  Future: TFuture<T>;
begin
  Future := TFuture<T>.Create( TWorkFactory( AFactory.AsObject), Func, nil);
  Future.EndAddingChildren;
  result := Future
end;


class function TTasking.CreateWorkFactory( MaxThreadCount1, MinIdleCount1,
  MaxIdleCount1, MaxEventPool: integer; const SharedLock: ILock): IWorkFactory;
begin
  result := TWorkFactory.CreateWorkFactory( MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventPool, SharedLock);
end;





class function TTasking.Join(
  const WorkFactory: IWorkFactory; const Tasks: array of ITask): ITask;
var
  JoinTask: TJoinTask;
begin
  if Length( Tasks) < 2 then
    raise Exception.Create( 'TTasking.Join() needs at least 2 child tasks.');
  JoinTask := TJoinTask.CreateJoin( TWorkFactory( WorkFactory.AsObject), Tasks);
  result   := JoinTask
end;



constructor TBaseFuture.Create( AWorkFactory: TWorkFactory; AJob: TTaskProc; AParent: TTask);
begin
  inherited Create( AWorkFactory, AJob, AParent);
  FReturnLock.Initialize;
  FCapturedException := nil
end;

destructor TBaseFuture.Destroy;
begin
  FReturnLock.Finalize;
  inherited
end;

constructor TFuture<T>.Create( AWorkFactory: TWorkFactory; AFunc: TFutureFunc<T>; AParent: TTask);
begin
  inherited Create( AWorkFactory, nil, AParent);
  FValue    := Default( T);
  FFunc     := AFunc;
  FDoneProc := nil
end;

procedure TFuture<T>.ComputeValueOrCaptureException( Task: TBaseFuture);
var
  E: Exception;
begin
  if assigned( FFunc) then
      try
        FValue := FFunc( IFuture<T>( Task.AsIFuture))
      except on Exception do
        begin
        FReturnLock.Enter;
        FCapturedException := Exception( AcquireExceptionObject);
        FReturnLock.Leave
        end
      end
    else
      FValue := Default( T)
end;

function TFuture<T>.AsIFuture: IInterface;
var
  Ret: IFuture<T>;
begin
  Ret    := self;
  result := IInterface( Ret)
end;

procedure TFuture<T>.CallReturnProcOrPropageException( Task: TBaseFuture);
var
  ReturnProc: TProcessValueProc<T>;
  PropagatedException: Exception;
begin
  if GetReturnAction( ReturnProc, PropagatedException) then
      begin
      if assigned( ReturnProc) then
        ReturnProc( IFuture<T>( Task.AsIFuture), FValue)
      end
    else
      raise PropagatedException
end;


procedure TFuture<T>.Clear;
begin
  inherited;
  FFunc     := nil;
  FDoneProc := nil
end;

function TFuture<T>.FundamentalClone: TTask;
var
  Newbie: TBaseFuture;
begin
  Newbie := TFuture<T>( ClassType).Create( FWorkFactory, FFunc, FParent);
  Newbie.FDatum.Write( FDatum.ReadObj);
  Newbie.AddChildren( FChildrenTasks);
  if GetStatus = tsConfigurable then
    Newbie.EndAddingChildren;
end;

destructor TFuture<T>.Destroy;
begin
  FReturnLock.Finalize;
  inherited
end;

procedure TFuture<T>.Execute;
begin
  ComputeValueOrCaptureException( self)
end;

procedure TFuture<T>.ExecuteReturn;
begin
  CallReturnProcOrPropageException( self)
end;

function TFuture<T>.GetReturnAction(
  var ReturnProc: TProcessValueProc<T>; var PropagatedException: Exception): boolean;
begin
  FReturnLock.Enter;
  PropagatedException := FCapturedException;
  result := not assigned( PropagatedException);
  if not result then
    FCapturedException := nil;
  ReturnProc := FDoneProc;
  FDoneProc  := nil;
  FReturnLock.Leave
end;

function TFuture<T>.GetValue: T;
begin
  TryValue( result, FOREVER)
end;

function TFuture<T>.QueueFuture: IFuture<T>;
begin
  result := self;
  Queue
end;

procedure TFuture<T>.ReleaseExecuteProc;
begin
  inherited;
  FFunc := nil
end;

function TFuture<T>.SetOnDone( OnDoneProc: TProcessValueProc<T>): IFuture<T>;
begin
  FReturnLock.Enter;
  FDoneProc := OnDoneProc;
  FReturnLock.Leave
end;

function TFuture<T>.TryValue( var Value: T; TimeOut: cardinal): boolean;
begin
  Queue;
  result := TerminationSynchro.WaitFor( TimeOut) = wrSignaled;
  if result then
    begin
    CallReturnProcOrPropageException( self);
    Value := FValue
    end
end;





constructor TDecoratedTask.CreateDecorated(
  AWorkFactory: TWorkFactory; const ABase: ITaskEx);
begin
  FBase := ABase;
  inherited Create( AWorkFactory, nil, nil)
end;


procedure TDecoratedTask.AddChild( const Addend: ITaskEx);
begin
  FBase.AddChild( Addend)
end;

procedure TDecoratedTask.AddChildren( const Addends: array of ITaskEx);
begin
  FBase.AddChildren( Addends)
end;

procedure TDecoratedTask.AfterExecute;
begin
  FBase.AfterExecute
end;

procedure TDecoratedTask.BeforeExecute;
begin
  FBase.BeforeExecute
end;

procedure TDecoratedTask.Cancel;
begin
  (FBase as ITask).Cancel
end;

procedure TDecoratedTask.CleanUpAfterDoneOrCancelled;
begin
  FBase.CleanUpAfterDoneOrCancelled
end;

procedure TDecoratedTask.Clear;
begin
  FBase.Clear
end;

function TDecoratedTask.Clone: ITask;
begin
  result := inherited Clone;
  if assigned( result) and assigned( FBase) then
    (result.AsObject as TDecoratedTask).FBase := (FBase as ITask).Clone as ITaskEx
end;

procedure TDecoratedTask.DereferenceParent;
begin
  FBase.DereferenceParent
end;

destructor TDecoratedTask.Destroy;
begin
  FBase.Clear;
  FBase := nil;
  inherited
end;

function TDecoratedTask.DoCaptureException: boolean;
begin
  result := FBase.DoCaptureException
end;

procedure TDecoratedTask.EndAddingChildren;
begin
  FBase.EndAddingChildren
end;

procedure TDecoratedTask.Execute;
begin
  FBase.Execute
end;

procedure TDecoratedTask.ExecuteReturn;
begin
  FBase.ExecuteReturn
end;

function TDecoratedTask.GetDatum: TObject;
begin
  result := (FBase as ITask).Datum
end;

function TDecoratedTask.GetStatus: TTaskStatus;
begin
  result := FBase.Status
end;

function TDecoratedTask.IsDone: boolean;
begin
  result := (FBase as ITask).isDone
end;

procedure TDecoratedTask.MarkExceptionThrown;
begin
  FBase.MarkExceptionThrown
end;


procedure TDecoratedTask.ReleaseExecuteProc;
begin
  FBase.ReleaseExecuteProc
end;

procedure TDecoratedTask.SetDatum(Value: TObject);
begin
  (FBase as ITask).Datum := Value
end;

function TDecoratedTask.SetOnDone( Proc: TTaskProc): ITask;
begin
  (FBase as ITask).SetOnDone( Proc)
end;

procedure TDecoratedTask.SetStatus( Value: TTaskStatus);
begin
  FBase.Status := Value
end;

procedure TDecoratedTask.Signal;
begin
  FBase.Signal
end;

function TDecoratedTask.TerminationSynchro: ISynchro;
begin
  result := (FBase as ITask).TerminationSynchro
end;


constructor TJoinTask.CreateJoin(
  AWorkFactory: TWorkFactory; const ATasks: array of ITask);
var
  SubTasks: TArray<ITaskEx>;
  j, L: integer;
begin
  inherited Create( AWorkFactory, nil, nil);
  L := Length( ATasks);
  SetLength( SubTasks, L);
  for j := 0 to L - 1 do
    SubTasks[j] := ATasks[j] as ITaskEx;
  AddChildren( SubTasks);
  EndAddingChildren
end;


procedure TJoinTask.Execute;
begin
  QueueChildren
end;

procedure TJoinTask.Cancel;
var
  Child: ITaskEx;
begin
  inherited;
  for Child in FChildrenTasks do
    (Child as ITask).Cancel
end;



constructor TValuePipeline<T>.Create(
  AFactory: TWorkFactory; Capacity, LowWaterMark, HighWaterMark: cardinal);
begin
  FFactory     := AFactory;
  FInputQueue  := TPipe<T>.Create( FFactory, LowWaterMark, HighWaterMark, Capacity);
  FOutputQueue := FInputQueue;
  FStages      := TObjectList<TStage<T>>.Create( True)
end;


destructor TValuePipeline<T>.Destroy;
begin
  FInputQueue := nil;
  FStages.Free;
  inherited
end;

function TValuePipeline<T>.InputQueue: IPipe<T>;
begin
  result := FInputQueue
end;

function TValuePipeline<T>.OutputQueue: IPipe<T>;
begin
  result := FOutputQueue
end;

function TValuePipeline<T>.Stage(
  StageTask: TSingleFunc<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>;
var
  Addend: TStage<T>;
begin
  if ThreadCount = 0 then
    ThreadCount := 1;
  Addend := TStage<T>.CreateStageFromSingleFunc( self, StageTask, Capacity, LowWaterMark, HighWaterMark, ThreadCount);
  FStages.Add( Addend);
  FOutputQueue := Addend.FOutData
end;

function TValuePipeline<T>.Stage(
  StageTask: TPipeToPipe<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>;
var
  Addend: TStage<T>;
begin
  if ThreadCount = 0 then
    ThreadCount := 1;
  Addend := TStage<T>.CreateStageFromPipeToPipe( self, StageTask, Capacity, LowWaterMark, HighWaterMark, ThreadCount);
  FStages.Add( Addend);
  FOutputQueue := Addend.FOutData
end;


procedure TValuePipeline<T>.Start;
var
  Stage: TStage<T>;
begin
  for Stage in FStages do
    Stage.Start
end;

constructor TValuePipeline<T>.TStage<U>.CreateStage(
  AOwner: TValuePipeline<U>; StageTaskFactory: TStageTaskFactory; Capacity, LowWaterMark, HighWaterMark,
  ThreadCount: cardinal);
var
  ThreadIdx: integer;
  StagerTask: ITask;
begin
  FPipeline := AOwner;
  FIdx   := FPipeline.FStages.Count;
  TObjectList<TStage<U>>( FPipeline.FStages).Add( self);
  if FIdx = 0 then
      FInData := FPipeline.FInputQueue
    else
      FInData := FPipeline.FStages[ FIdx - 1].FOutData;
  FOutData := TPipe<U>.Create( FPipeline.FFactory, LowWaterMark, HighWaterMark, Capacity);
  for ThreadIdx := 1 to ThreadCount do
    begin
    StagerTask := StageTaskFactory( FPipeline.FFactory, self);
    FStagerTasks.Add( StagerTask)
    end;
  FIncompleteCount.Initialize( ThreadCount)
end;


constructor TValuePipeline<T>.TStage<U>.CreateStageFromSingleFunc(
  AOwner: TValuePipeline<U>; StageTask: TSingleFunc<U>; Capacity, LowWaterMark,
  HighWaterMark, ThreadCount: cardinal);
begin
  CreateStage( AOwner,
    function( AFactory: TWorkFactory; Stage: TObject) : ITask
      begin
      result := TSingleFuncStager<U>.CreateSingleStager( AFactory, TStage<U>( Stage), StageTask, FInData, FOutData)
      end,
    Capacity, LowWaterMark, HighWaterMark, ThreadCount)
end;

constructor TValuePipeline<T>.TStage<U>.CreateStageFromPipeToPipe(
  AOwner: TValuePipeline<U>; StageTask: TPipeToPipe<U>; Capacity, LowWaterMark,
  HighWaterMark, ThreadCount: cardinal);
begin
  CreateStage( AOwner,
    function( AFactory: TWorkFactory; Stage: TObject) : ITask
      begin
      result := TPipeToPipeStager<U>.CreatePipeStager( AFactory, TStage<U>( Stage), StageTask, FInData, FOutData)
      end,
    Capacity, LowWaterMark, HighWaterMark, ThreadCount)
end;

destructor TValuePipeline<T>.TStage<U>.Destroy;
begin
  FStagerTasks.Free;
  FOutData := nil;
  FIncompleteCount.Finalize;
  inherited;
end;

procedure TValuePipeline<T>.TStage<U>.Start;
var
  Task: ITask;
begin
  for Task in FStagerTasks do
    Task.Queue
end;


constructor TValuePipeline<T>.TSingleFuncStager<U>.CreateSingleStager(
  AFactory: TWorkFactory; AStage: TStage<U>; StageTask: TSingleFunc<U>; const AInData,
  AOutData: IPipe<U>);
begin
  inherited Create( AFactory, nil, nil);
  FStage     := AStage;
  FStageFunc := StageTask;
  FInData    := AInData;
  FOutData   := AOutData
end;

procedure TValuePipeline<T>.TSingleFuncStager<U>.Execute;
var
  Item: RWaitItem<U>;
  OutDatum: U;
  Ex: Exception;
begin
  try
    for Item in FInData.Enumerable( FOREVER) do
      begin
      if assigned( Item.FExcptn) then
        raise Item.FExcptn;
      if assigned( FStageFunc) then
          OutDatum := FStageFunc( Item.FItem)
        else
          OutDatum := Item.FItem;
      FOutData.Enqueue( OutDatum)
      end
  finally
    if FStage.FIncompleteCount.DecrementIfAboveZero then
      FOutData.Complete
  end
end;

procedure TValuePipeline<T>.TSingleFuncStager<U>.Clear;
begin
  inherited;
  FStageFunc := nil;
  FInData    := nil;
  FOutData   := nil
end;

procedure TValuePipeline<T>.TSingleFuncStager<U>.ReleaseExecuteProc;
begin
  inherited;
  FStageFunc := nil
end;

constructor TValuePipeline<T>.TPipeToPipeStager<U>.CreatePipeStager(
  AFactory: TWorkFactory; AStage: TStage<U>; StageTask: TPipeToPipe<U>;
  const AInData, AOutData: IPipe<U>);
begin
  inherited Create( AFactory, nil, nil);
  FStage     := AStage;
  FStageFunc := StageTask;
  FInData    := AInData;
  FOutData   := AOutData
end;


procedure TValuePipeline<T>.TPipeToPipeStager<U>.Execute;
begin
  try
    FStageFunc( FInData, FOutData)
  finally
    if FStage.FIncompleteCount.DecrementIfAboveZero then
      FOutData.Complete
    end
end;

procedure TValuePipeline<T>.TPipeToPipeStager<U>.ReleaseExecuteProc;
begin
  inherited;
  FStageFunc := nil
end;

procedure TValuePipeline<T>.TPipeToPipeStager<U>.Clear;
begin
  inherited;
  FStageFunc := nil;
  FInData    := nil;
  FOutData   := nil
end;


constructor TForkJoin<T>.CreateForkJoin(
  const AFactory: TWorkFactory; const ASeed: T; NumTasks: cardinal; ADoFork: TTestFunc<T>; AFork: TForkFunc<T>; AJoin: TJoinFunc<T>);
var
  Idx: integer;
begin
  inherited Create( AFactory,
    function( const Future: IFuture<T>): T
      var
        Exc: Exception;
      begin
      (Future as ITaskEx).QueueChildren;
      FUnfinishedChildTasks.WaitFor;
      Exc := FCapturedException.Read<Exception>;
      FCapturedException.Write( nil);
      if assigned( Exc) then
          raise Exc
        else
          result := TForkJoinProblem( FSeed.AsObject).FDatum
      end
    , nil);
  FSeed   := TForkJoinProblem.Create( UntestedHolistic, ASeed, nil);
  FDoFork := ADoFork;
  FFork   := AFork;
  FJoin   := AJoin;
  if NumTasks = 0 then
    NumTasks := 1;
  FJoinStack := TProblemStack.Create;
  FForkStack := TProblemStack.Create;
  FTestStack := TProblemStack.Create;
  FhasCancelled := False;
  FCapturedException.Initialize;
  FActionable := TEvent.Create( nil, True, True, '', False); // manual-reset event.
  FUnfinishedChildTasks := AFactory.Countdown( NumTasks);
  FIsActionable := True;
  FCountOfProblemsInWork := 0;
  FQueueLock.Initialize;
  for Idx := 1 to NumTasks do
    AddChild( TForkJoiner.CreateForker( self));
  FTestStack.Push( FSeed)
end;


destructor TForkJoin<T>.Destroy;
begin
  FSeed   := nil;
  FDoFork := nil;
  FFork   := nil;
  FJoin   := nil;
  FJoinStack.Free;
  FForkStack.Free;
  FTestStack.Free;
  FCapturedException.Finalize;
  FActionable.Free;
  FUnfinishedChildTasks := nil;
  FQueueLock.Finalize;
  inherited;
end;

function TForkJoin<T>.TForkJoinProblem.AsObject: TObject;
begin
  result := self
end;

constructor TForkJoin<T>.TForkJoinProblem.Create(
  AStatus: TForkJoinProblemStatus; const ADatum: T;
  AForkParent: TForkJoinProblem);
begin
  FStatus := AStatus;
  FDatum  := ADatum;
  SetLength( FChildren, 0);
  FCountOfUnsolvedChildren := 0;
  FForkParent := AForkParent
end;

constructor TForkJoin<T>.TForkJoiner.CreateForker( AOwner: TForkJoin<T>);
begin
  FOwner := AOwner;
  inherited Create( FOwner.FWorkFactory, nil, FOwner)
end;

procedure EnterLock( var SpinLock: TSBDSpinLock; var EnteredFlag: boolean; Value: boolean);
begin
  if EnteredFlag = Value then exit;
  if Value then
      begin
      SpinLock.Enter;
      EnteredFlag := True
      end
    else
      begin
      EnteredFlag := False;
      SpinLock.Leave
      end
end;

procedure TForkJoin<T>.TForkJoiner.Execute;
var
  WorkLoad: integer;
  NewIsActionable: boolean;
  Action: TForkJoinerAction;
  Problem: IForkJoinProblem;
  Entered: boolean;
  EnterLockProc: TProc_Bool;

begin
  Action  := actNothing;
  Entered := False;
  EnterLockProc := procedure( Value: boolean)
    begin
      EnterLock( FOwner.FQueueLock, Entered, Value)
    end;
  while (not (Action in [actCancel, actException])) and (FOwner.FActionable.WaitFor <> wrSignaled) do
    begin
    EnterLockProc( True);
    WorkLoad := FOwner.FJoinStack.Count +
                FOwner.FForkStack.Count +
                FOwner.FTestStack.Count +
                FOwner.FCountOfProblemsInWork;
    NewIsActionable := (WorkLoad > 0) or FOwner.FhasCancelled or FOwner.FCapturedException.IsAssigned;
    if NewIsActionable <> FOwner.FIsActionable then
      begin
      FOwner.FIsActionable := NewIsActionable;
      if FOwner.FIsActionable then
          FOwner.FActionable.SetEvent
        else
          FOwner.FActionable.ResetEvent
      end;
    Action := actNothing;
    if FOwner.FisActionable then
      begin
      if FOwner.FCapturedException.IsAssigned then
          Action := actException
        else if FOwner.FhasCancelled then
          Action := actCancel

        // Action priorities are ...
        //  1. Join if you can.
        //  2. Otherwise Test if you can.
        //  3. Otherwise Fork.

        else if FOwner.FJoinStack.Count > 0 then
          Action := actDequeueJoinStack
        else if FOwner.FTestStack.Count > 0 then
          Action := actDequeueTestStack
        else if FOwner.FForkStack.Count > 0 then
          Action := actDequeueForkStack
      end;

    Problem := nil;
    case Action of
      actDequeueJoinStack: Problem := FOwner.FJoinStack.Pop;
      actDequeueForkStack: Problem := FOwner.FForkStack.Pop;
      actDequeueTestStack: Problem := FOwner.FTestStack.Pop;
      end;
    if Action in [actDequeueJoinStack, actDequeueForkStack, actDequeueTestStack] then
      Inc( FOwner.FCountOfProblemsInWork);
    FOwner.Act( Action, TForkJoinProblem( Problem.AsObject), EnterLockProc);
    EnterLockProc( False)
    end;
  EnterLockProc( False);
  FOwner.FUnfinishedChildTasks.Signal
end;



function TForkJoin<T>.Act(
  Action: TForkJoinerAction; Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
begin
  result := True;
  case Action of
    actDequeueJoinStack: result := Join( Problem, EnterLockProc);
    actDequeueForkStack: result := Fork( Problem, EnterLockProc);
    actDequeueTestStack: result := Test( Problem, EnterLockProc);
    end;
  if not result then
    Action := actException
end;



function TForkJoin<T>.Test(
  Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
var
  doFork: boolean;
begin
  EnterLockProc( False);
  result := True;
  if assigned( FDoFork) then
      try
        doFork := FDoFork( Problem.FDatum)
      except
          begin
          result := False;
          EnterLockProc( True);
          TAtomic.InitializeVolatile( FCapturedException,
            function: TObject
            begin
              result := TObject( AcquireExceptionObject)
            end)
          end
        end
    else
      doFork := True;
  if result then
    begin
    if doFork then
        MarkAsSolved( Problem, EnterLockProc)
      else
        begin
        EnterLockProc( True);
        Problem.FStatus := ForkableHolistic;
        FForkStack.Push( Problem)
        end
    end;
  EnterLockProc( True);
  Dec( FCountOfProblemsInWork)
end;


procedure TForkJoin<T>.MarkAsSolved(
  Problem: TForkJoinProblem; EnterLockProc: TProc_Bool);
begin
  EnterLockProc( True);
  Problem.FStatus := Solved;
  if assigned( Problem.FForkParent) then
     begin
     Dec( Problem.FForkParent.FCountOfUnsolvedChildren);
     if Problem.FForkParent.FCountOfUnsolvedChildren = 0 then
       begin
       Problem.FForkParent.FStatus := JoinableForked;
       FJoinStack.Push( Problem.FForkParent)
       end
     end
end;

function TForkJoin<T>.Fork(
  Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
var
  Children: TArray<T>;
  L, Idx: integer;
  ForkChild: TForkJoinProblem;
begin
  EnterLockProc( False);
  result := True;
  if assigned( FFork) then
      begin
      try
        Children := FFork( Problem.FDatum)
      except
          begin
          result := False;
          EnterLockProc( True);
          TAtomic.InitializeVolatile( FCapturedException,
            function: TObject
            begin
              result := TObject( AcquireExceptionObject)
            end)
          end
        end;
      if Length( Children) = 0 then
        raise Exception.Create( 'TForkJoin.Fork() must yield at least one subproblem.');
      end
    else
      begin
      SetLength( Children, 1);
      Children[0] := Problem.FDatum
      end;
  Problem.FStatus := InWorkForked;
  L := Length( Children);
  Problem.FCountOfUnsolvedChildren := L;
  SetLength( Problem.FChildren, L);
  EnterLockProc( True);
  for Idx := 0 to L-1 do
    begin
    ForkChild := TForkJoinProblem.Create( UntestedHolistic, Children[Idx], Problem);
    Problem.FChildren[ Idx] := ForkChild;
    FTestStack.Push( ForkChild)
    end;
  Dec( FCountOfProblemsInWork)
end;




function TForkJoin<T>.Join(
  Problem: TForkJoinProblem; EnterLockProc: TProc_Bool): boolean;
var
  Children: TArray<T>;
  L, Idx: integer;
begin
  result := True;
  L := Length( Problem.FChildren);
  SetLength( Children, L);
  for Idx := 0 to L-1 do
    Children[Idx] := TForkJoinProblem( Problem.FChildren[Idx].AsObject).FDatum;
  if assigned( FJoin) then
      try
        Problem.FDatum := FJoin( Problem.FDatum, Children)
      except
        result := False;
        EnterLockProc( True);
        TAtomic.InitializeVolatile( FCapturedException,
          function: TObject
          begin
            result := TObject( AcquireExceptionObject)
          end)
        end
    else
      begin
      L := Length( Children);
      if L >= 2 then
          Problem.FDatum := Children[1]
        else
          Problem.FDatum := Default(T)
      end;
  MarkAsSolved( Problem, EnterLockProc);
  Dec( FCountOfProblemsInWork)
end;



constructor TJoinIntegerTask.CreateIntegerJoin(
  AWorkFactory: TWorkFactory; const ATasks: array of ITask; InitialValue: integer);
begin
  FValue.Initialize( InitialValue);
  inherited CreateJoin( AWorkFactory, ATasks)
end;


procedure TJoinIntegerTask.CleanUpAfterDoneOrCancelled;
begin
  inherited;
  FValue.Finalize
end;




constructor TJoinObjectOwningTask.CreateObjectJoin( AWorkFactory: TWorkFactory;
  const ATasks: array of ITask; AObj: TObject);
begin
  FObj := AObj;
  inherited CreateJoin( AWorkFactory, ATasks)
end;

destructor TJoinObjectOwningTask.Destroy;
begin
  FreeAndNil( FObj);
  inherited
end;

procedure TJoinObjectOwningTask.CleanUpAfterDoneOrCancelled;
begin
  inherited;
  FreeAndNil( FObj)
end;


constructor TSchedQueue.Create;
var
  Cmp: IComparer<RSchedJob>;
begin
  Cmp := TSchedJobComparer.Create;
  inherited Create( Cmp)
end;


function TSchedJobComparer.Compare( const Left, Right: RSchedJob): integer;
var
  Diff: double;
begin
  Diff := Right.FMaturity - Left.FMaturity;
  if Diff > 0.0 then
      result := 1
  else if Diff < 0.0 then
      result := -1
  else
      result := 0
end;

function TSchedQueue.NextMaturity( var Value: TDateTime): boolean;
begin
  result := Count > 0;
  if result then
      Value := Items[0].FMaturity
    else
      Value := -1.0
end;

function TSchedQueue.OrderedAdd( const Addend: RSchedJob): integer;
begin
  BinarySearch( Addend, result);
  Insert( result, Addend)
end;

function TSchedQueue.PopMature(
  NowTime: TDateTime; var PopJob: ITaskEx): boolean;
begin
  result := (Count > 0) and (Items[0].FMaturity <= NowTime);
  if result then
      begin
      PopJob := Items[0].FJob;
      Delete( 0)
      end
    else
      PopJob := nil
end;


end.
