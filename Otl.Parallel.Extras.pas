unit Otl.Parallel.Extras;
{$I OtlOptions.inc}

interface



uses System.SyncObjs, System.Classes, System.SysUtils, System.Generics.Collections
   , Otl.Parallel.SynchroPrimitives.InterfaceLevel
   , Otl.Parallel.SynchroPrimitives.BasicLevel
   , Otl.Parallel.SynchroPrimitives.ModularLevel
 {$IFDEF MSWINDOWS}
   , Winapi.Windows
 {$ENDIF}
   , Otl.Parallel.Atomic;

const
  FOREVER = INFINITE;
  MaxCardinal: cardinal = cardinal( -1);

type
  ITask = interface;
  TTaskProc = reference to procedure( const Task: ITask);
  TTaskStatus = (tsConfigurable, tsEnqueued, tsInWork, tsCancelled, tsDone);

  ITask = interface ['{622CB06C-4C87-4039-8547-9C0CE73C0D76}']
    {$REGION 'property accessors'}
    function GetDatum: TObject;
    procedure SetDatum( Value: TObject);
    {$ENDREGION}
    function  AsObject: TObject;
    function  CanStart: boolean;
    procedure Cancel;
    function  Status: TTaskStatus;
    function  IsDone: boolean;
    function  TerminationSynchro: ISynchro;
    function  Queue: ITask;
    function  SetOnDone( Proc: TTaskProc): ITask;
    function  Clone: ITask;
    property  Datum: TObject         read GetDatum write SetDatum;
  end;

  IFuture<T> = interface;
  TFutureFunc<T> = reference to function( const Future: IFuture<T>): T;
  TProcessValueProc<T> = reference to procedure(
    const Future: IFuture<T>; const Value: T);

  IFuture<T> = interface( ITask)
    function  QueueFuture: IFuture<T>;
    function  TryValue( var Value: T; TimeOut: cardinal = FOREVER): boolean;
    function  SetOnDone( OnDoneProc: TProcessValueProc<T>): IFuture<T>;
    function  GetValue: T;
    property  Value: T read GetValue;
  end;

  IValuePipelineBase = interface ['{40D5B259-4E28-48B1-AC10-0E23810FC2F0}']
    procedure Start;
  end;

  RPipeItem<T> = record
    FItem: T;
    FExcptn: Exception;
  end;

  RWaitItem<T> = record
    FItem: T;
    FExcptn: Exception;
    FWaitResult: TWaitResult;
    procedure Release;
  end;

  RWaitItemObj = RWaitItem<TObject>;
  TPipeStatus = (Normal, Filling, Draining, Finished);

  IPipe<T> = interface
    function  Enqueue( const Addend: T; TimeOut: cardinal = FOREVER): TWaitResult;
    function  EnqueueException( Addend: Exception; TimeOut: cardinal = FOREVER): TWaitResult;
    procedure Complete;
    function  isCompleted: boolean;
    function  isFinished: boolean; // Finished means completed AND empty.
    procedure ClearAndReset;
    function  Count: cardinal;
    /// <remarks>Dequeue() will return wrAbandoned if completed and empty.</remarks>
    function  Dequeue( var Obj: T; TimeOut: cardinal = FOREVER): TWaitResult;  overload;
    function  Dequeue: T;                                                      overload;
    function  PeekDequeue( var Obj: T; var Completed: boolean): boolean;
    function  EnqueueSynchro: ISynchro;
    function  DequeueSynchro: ISynchro;
    function  CompletedSynchro: ISynchro;
    function  CanEnqueue: boolean;
    /// <remarks> CanDequeue returns True iff dequeuing will not block AND
    ///              (the queue is not both complete and empty).</remarks>
    function  CanDequeue: boolean;
    function  Enumerable( Timeout: cardinal): IEnumerable<RWaitItem<T>>;
    function  Status: TPipeStatus;
    /// <remarks>Once enqueueing becomes blocked, it won't unblock until the pipe has been dequeued at or below the Low water mark.
    ///  LowWaterMark ranges from zero to capacity minus 1. The default and most liberal value is capacity minus 1.
    ///  An efficient value might be about 20% of capacity.
    ///  </remarks>
    function  LowWaterMark: cardinal;
    /// <remarks>Once dequeueing becomes blocked, it won't unblock until the pipe has been enqueued at or above the Low high mark.
    ///  HighWaterMark ranges from 1 to capacity. The default and most liberal value is 1.
    ///  An efficient value might be about 80% of capacity.
    ///  There is no requirement for LowWaterMark to be less that HighWaterMark.
    ///  </remarks>
    function  HighWaterMark: cardinal;
    function  MaximumCapacity: cardinal;
    function  AsObject: TObject;
  end;
  IObjectPipe = IPipe<TObject>;


  TSingleFunc<T> = reference to function( const Value: T): T;
  TPipeToPipe<T> = reference to procedure( const InData, OutData: IPipe<T>);

  IValuePipeline<T> = interface( IValuePipelineBase)
    function InputQueue: IPipe<T>;
    function OutputQueue: IPipe<T>;
    function Stage( StageTask: TSingleFunc<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>; overload;
    function Stage( StageTask: TPipeToPipe<T>; Capacity, LowWaterMark, HighWaterMark, ThreadCount: cardinal): IValuePipeline<T>; overload;
  end;

  TTestFunc<T> = reference to function( var Datum: T        ): boolean;
  TForkFunc<T> = reference to function( var Datum: T        ): TArray<T>;
  TJoinFunc<T> = reference to function( const ParentDatum: T; const Data : TArray<T>): T;
  TForEachProc = reference to procedure( Idx: integer; const Task: ITask);
  TForEachItemProc<T> = reference to procedure( const Item: T; const Task: ITask);
  IWorkFactory = interface ['{86D1CAD6-8756-47D3-9286-F6186B46AE16}']
    function  FactoryLock: ILock;
    function  ProcessJobReturn( TimeOut: cardinal): boolean;
    function  QueueJob( const Task: ITask): boolean;
    procedure StartTask( const TaskIntf: ITask);

    function NewLock( Locking: TLockingMechanism): ILock;
    function Event            ( ManualReset, InitialState: boolean): IEvent;                        // Interface level, Reusable.
    function LightEvent       ( ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;     // Interface level, Reusable.
    function CountDown( InitialValue: cardinal): ICountDown;                                        // Interface level, Reusable.
    function AsObject: TObject;
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
    function SelfJoin( const BaseTask: ITask; ThreadCount: integer): ITask;

    function CoBeginTask( Proc: System.SysUtils.TProc): ITask;  overload;
    function CoBeginTask( Proc: TTaskProc            ): ITask;  overload;

    // <ist be at least 2 factors. Each factor must support ISynchroExInternal, such as Modular synchronisation primitives
    function WaitForAny( const AFactors: TSynchroArray; APropagation: TWaitPropagation; var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;
    function WaitForAll( const AFactors: TSynchroArray; APropagation: TWaitPropagation;                            TimeOut: cardinal = FOREVER): TWaitResult;

    function CoForEach( LowIdx, HighIdx, StepIdx: integer; ThreadCount: integer; Proc: TForEachProc): ITask;
    function Abandonable( const BaseTask: ITask; Timeout: integer): IFuture<boolean>;
    function GeometricRetry( BaseFunc: TFutureFunc<boolean>; InitialRetryPause: integer; PauseGrowth: double; MaxRetryPause: integer; MaxTryCount: integer): IFuture<boolean>;
    function ShareLock: ILock;
    function SynchroPool: TSynchroFactory;
  end;

  /// <remarks>TForkJoinerAction is a private type. Do not use.</remarks>
  TForkJoinerAction = (actNothing, actDequeueJoinStack, actDequeueForkStack, actDequeueTestStack, actCancel, actException);

  TSBDParallel = class
  public
     // Basic level constructors
    class function EventObj     ( ManualReset, InitialState: boolean): TSBDEvent;
    class function LightEventObj( ManualReset, InitialState: boolean; SpinMax: cardinal): TSBDEvent;
    class function SemaphoreObj ( AInitialCount: cardinal): TSBDSemaphore;
    class function CountDownObj ( AInitialValue: cardinal): TCountDown;
    class function CountUpObj   ( AInitialValue, AMaxValue: cardinal): TCountUp;
    class function CritSect: TCriticalSection;

  public
     // Interface level constructors. Non-reusable.
     // For re-usable interface level synchronisation primitives, see the TSynchroFactory public methods.
    class function NewLock( Locking: TLockingMechanism): ILock;
    class function Semaphore    ( AInitialCount: cardinal): ISemaphore;
    class function Event        ( ManualReset, InitialState: boolean): IEvent;
    class function LightEvent   ( ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;
    class function CountDown    ( InitialValue: cardinal): ICountDown;
    class function CountUp      ( InitialValue, AMaxValue: cardinal): ICountUp;

  public
    // Pipes
    /// <param name="ALowwater">Once enqueueing becomes blocked, it won't unblock until the pipe has been dequeued at or below the Low water mark.
    ///  LowWaterMark ranges from zero to capacity minus 1. The default and most liberal value is capacity minus 1.
    ///  An efficient value might be about 20% of capacity.
    ///  </param>
    /// <param name="AHighWater">Once dequeueing becomes blocked, it won't unblock until the pipe has been enqueued at or above the Low high mark.
    ///  HighWaterMark ranges from 1 to capacity. The default and most liberal value is 1.
    ///  An efficient value might be about 80% of capacity.
    ///  There is no requirement for LowWaterMark to be less that HighWaterMark.
    ///  </param>
    /// <remarks>For a free-wheeling pipe, set LowWater=Capacity-1; and HighWater=1</remarks>
    /// <remarks>For a pipe with less state blocking, set LowWater=20% * Capacity; and HighWater=80% * Capacity</remarks>
    class function Pipe<T>   ( const WorkFactory: IWorkFactory; ALowwater, AHighWater, ACapacity: cardinal): IPipe<T>;
    class function ObjectPipe( const WorkFactory: IWorkFactory; ALowwater, AHighWater, ACapacity: cardinal): IObjectPipe;
    class function UnboundedPipe<T>   ( const WorkFactory: IWorkFactory): IPipe<T>;
    class function UnboundedObjectPipe( const WorkFactory: IWorkFactory): IObjectPipe;

  public
    // work factory
    class function WorkFactory(
      MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventPool: integer;
      const SharedLock: ILock = nil): IWorkFactory;

  public
    // Generic class methods to support tasking.
    class function Future<T>( const WF: IWorkFactory; Func: TFutureFunc<T>): IFuture<T>;
    class function ValuePipeline<T>( const WF: IWorkFactory; Capacity, LowWaterMark, HighWaterMark: cardinal): IValuePipeline<T>;
    class function SolveByForkJoin<T>( const WF: IWorkFactory; const Seed: T; NumTasks: cardinal; DoFork: TTestFunc<T>; Fork: TForkFunc<T>; Join: TJoinFunc<T>): IFuture<T>;
    class function CoForEach<T>( const WF: IWorkFactory; const Collection: IEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;    overload;
    class function CoForEach<T>( const WF: IWorkFactory; const Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;    overload;
  end;





implementation




uses System.Diagnostics, Otl.Parallel.Pipe, System.RTLConsts, TypInfo,
     Otl.Parallel.Tasks;




procedure InitUnit_Parallel;
begin
  TStopWatch.Create
end;


procedure DoneUnit_Parallel;
begin
end;





procedure RWaitItem<T>.Release;
var
  Obj: TObject;
begin
  FreeAndNil( FExcptn);
  if PTypeInfo(TypeInfo(T))^.Kind = tkClass then
      begin
      Obj   := PObject( @FItem)^;
      FItem := Default( T);
      Obj.Free
      end
    else
      FItem := Default( T)
end;



class function TSBDParallel.CoForEach<T>(
 const WF: IWorkFactory; const Collection: IEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;
begin
  result := (WF.AsObject as TWorkFactory).CoForEach<T>( Collection, CollectionCursorIsThreadSafe, ThreadCount, Proc)
end;

class function TSBDParallel.CoForEach<T>(
  const WF: IWorkFactory; const Collection: TEnumerable<T>; CollectionCursorIsThreadSafe: boolean; ThreadCount: integer; Proc: TForEachItemProc<T>): ITask;
begin
  result := (WF.AsObject as TWorkFactory).CoForEach<T>( Collection, CollectionCursorIsThreadSafe, ThreadCount, Proc)
end;

class function TSBDParallel.CountDown( InitialValue: cardinal): ICountDown;
begin
  result := _CreateCountDownIntf( nil, InitialValue)
end;


class function TSBDParallel.CountDownObj( AInitialValue: cardinal): TCountDown;
begin
  result := TCountDown.Create( AInitialValue)
end;


class function TSBDParallel.CountUp( InitialValue, AMaxValue: cardinal): ICountUp;
begin
  result := _CreateCountUpIntf( nil, InitialValue, AMaxValue)
end;

class function TSBDParallel.CountUpObj( AInitialValue, AMaxValue: cardinal): TCountUp;
begin
  result := TCountUp.Create( AInitialValue, AMaxValue)
end;

class function TSBDParallel.CritSect: TCriticalSection;
begin
  result := TFixedCriticalSection.Create
end;

class function TSBDParallel.Event(
  ManualReset, InitialState: boolean): IEvent;
begin
  result := _CreateKernalEventIntf( nil, ManualReset, InitialState)
end;


class function TSBDParallel.EventObj(
  ManualReset, InitialState: boolean): TSBDEvent;
begin
  result := TKernelEvent.Create( ManualReset, InitialState)
end;


class function TSBDParallel.Future<T>(
  const WF: IWorkFactory; Func: TFutureFunc<T>): IFuture<T>;
begin
  result := TTasking.CreateFuture<T>( WF, Func)
end;

class function TSBDParallel.LightEvent(
  ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;
begin
  result := _CreateLightEventIntf( nil, ManualReset, InitialState, SpinMax)
end;


class function TSBDParallel.LightEventObj(
  ManualReset, InitialState: boolean; SpinMax: cardinal): TSBDEvent;
begin
  result := TLightEvent.Create( ManualReset, InitialState, SpinMax)
end;


class function TSBDParallel.WorkFactory(
  MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventPool: integer;
  const SharedLock: ILock): IWorkFactory;
begin
  result := TWorkFactory.CreateWorkFactory( MaxThreadCount1, MinIdleCount1, MaxIdleCount1, MaxEventPool, SharedLock)
end;

class function TSBDParallel.NewLock( Locking: TLockingMechanism): ILock;
begin
  case Locking of
    KernalLocking: result := _CreateCritLockIntf( nil);
    BusLocking   : result := _CreateSpinLockIntf;
  end;
end;


class function TSBDParallel.ObjectPipe(
  const WorkFactory: IWorkFactory;
  ALowwater, AHighWater, ACapacity: cardinal): IObjectPipe;
begin
  result := TObjectPipe.Create( WorkFactory, ALowwater, AHighWater, ACapacity)
end;

class function TSBDParallel.Pipe<T>(
  const WorkFactory: IWorkFactory; ALowwater,
  AHighWater, ACapacity: cardinal): IPipe<T>;
begin
  result := TPipe<T>.Create( WorkFactory, ALowwater, AHighWater, ACapacity)
end;

class function TSBDParallel.Semaphore( AInitialCount: cardinal): ISemaphore;
begin
  result := _CreateNativeSemaphoreIntf( AInitialCount)
end;

class function TSBDParallel.SemaphoreObj( AInitialCount: cardinal): TSBDSemaphore;
begin
  result := TSBDSemaphore.Create( AInitialCount);
end;

class function TSBDParallel.UnboundedObjectPipe( const WorkFactory: IWorkFactory): IObjectPipe;
begin
  result := ObjectPipe( WorkFactory, MaxCardinal-1, 1, MaxCardinal)
end;

class function TSBDParallel.UnboundedPipe<T>( const WorkFactory: IWorkFactory): IPipe<T>;
begin
  result := Pipe<T>( WorkFactory, MaxCardinal-1, 1, MaxCardinal)
end;

class function TSBDParallel.ValuePipeline<T>(
  const WF: IWorkFactory; Capacity, LowWaterMark, HighWaterMark: cardinal): IValuePipeline<T>;
begin
  result := TValuePipeline<T>.Create( WF.AsObject as TWorkFactory, Capacity, LowWaterMark, HighWaterMark)
end;

class function TSBDParallel.SolveByForkJoin<T>(
  const WF: IWorkFactory; const Seed: T; NumTasks: cardinal; DoFork: TTestFunc<T>; Fork: TForkFunc<T>;
  Join: TJoinFunc<T>): IFuture<T>;
begin
  result := TForkJoin<T>.CreateForkJoin( WF.AsObject as TWorkFactory, Seed, NumTasks, DoFork, Fork, Join)
end;


{ TODO
 =========

Executive summary
====================
5. PipeServer abstraction
6. HeavyPool abstraction
7. MREW primitives (basic level + interfaced level)
8. Unit header and general unit descriptions
9. Descriptions for Parallel Programming abstractions



 
PipeServer
=====================
Overview
A pipe server allows multiple copies of a task to process input data from a pipe.
  It is equivalent to a one stage value pipeline with no output, and an explicit
  specification of the task count.
Declaration
TParallel = class
  class function PipeServer<T>( const InPipe: IPipe<T>;
     Proc: TProcessValueProc<T>; TaskCount: integer): ITask;
end;

Heavy Pool
===============
Overview
A heavy pool is a pool of re-usable heavy weight objects. The pool can be constrained by:
•	A maximum number of created objects (available or in-use)
•	Available (that is to say, not in current use) pooled objects can have a maximum age. If they exceed this age, then are destroyed.
•	A minimum number of available objects. When less, a background house keeping pool will slowly grow the pool.
The user supplies functions to create/destroy the objects, or take action on the event of Acquire from/ release back to the pool. Removing too-old pool objects, or growing the pool to the specified minimum are house-keeping jobs performed by a task on a periodic basis.
}


initialization
InitUnit_Parallel;

finalization
DoneUnit_Parallel;


end.
