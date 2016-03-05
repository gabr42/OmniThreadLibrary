unit Otl.Parallel.SynchroPrimitives.InterfaceLevel;
// IMPORTANT!
//  READ THE COMMENTS IN UNIT Otl.Parallel.SynchroPrimitives .

{$I OtlOptions.inc}

interface
uses System.SyncObjs, Otl.Parallel.SynchroPrimitives.BasicLevel,
     Generics.Collections, Otl.Parallel.Atomic;

type
  TLockingMechanism = (KernalLocking, BusLocking);

  TSignalState = (esSignalled, esNotSignalled, esUnknown);

  ISynchro = interface ['{EF480FC2-7BBB-40B9-A9F7-246A7834CFA9}']
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}

    procedure Signal;
    /// <remarks> ISynchro.isSignalled() is not supported for auto-reset events and semaphores.</remarks>
    function  isSignalled: boolean;
    function  isSignalled_IsSupported: boolean;
    function  SignalState: TSignalState;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;
    function  AsObject: TObject;
    function  AsMWObject: TObject;
    function  IsPoolManaged: boolean;
    /// <remarks> Only modular synchros can be combined into a composite synchro. </remarks>
    function  IsModular: boolean;

    {$IFDEF MSWINDOWS}
      /// <remarks> ISynchro.Handle is not supported for:
      ///   Non-windows platforms;
      ///   Light events;
      ///   Testable semaphores;
      ///   CountDowns/Ups  nor
      ///   Functional events.
      ///  Basically, it is only for heavy events and native semaphores, all on a windows platform.
      /// </remarks>
      property Handle: THandle   read GetHandle;
    {$ENDIF}
  end;
  TSynchroArray = TArray<ISynchro>;

  IEvent = interface( ISynchro) ['{3E6E1606-88E5-4898-9F79-613085CAE090}']
    procedure SetEvent;
    procedure ResetEvent;
    function  isManualEvent: boolean;
    function  isLight: boolean;
  end;

  ISemaphore = interface( ISynchro) ['{4ACCD111-3E22-4902-B397-3309D84BBDD9}']
    function InitialValue: cardinal;
    /// <remarks> ISemaphore.Value() is not supported for native semaphores.</remarks>
    function Value: cardinal;
    /// <remarks> ISemaphore.Reset() is not supported for native semaphores.</remarks>
    procedure Reset;
    /// <remarks> Returns True iff it is safe to call Value() and Reset().</remarks>
    function isValueTesting_IsSupported: boolean;
  end;

  ILock = interface ['{38665699-5A91-4930-8AB8-AC4AD36B7182}']
    procedure Enter;
    procedure Leave;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  IsKernalMode: boolean;
    function  AsSpinLock: PSBDSpinLock;
    function  AsCriticalSection: TCriticalSection;
    function  LockCount: integer;
  end;

  ICountDown = interface( ISynchro) ['{23D82B7E-9670-4B24-835C-D1E879CF36C9}']
    function InitialValue: cardinal;
    function Value: cardinal;
    function SignalHit: boolean;
    function Allocate: cardinal; // Signal and return the value.
    procedure CounterSignal;
  end;

  ICountUp = interface( ISynchro) ['{C3AD39CC-7918-44E3-AE08-B5E8F2F9EDB5}']
    function InitialValue: cardinal;
    function MaxValue: cardinal;
    function Value: cardinal;
    function SignalHit: boolean;
  end;

  TObjectList = class( TObjectList<TObject>) end;
  TObjectQueue = class( TQueue<TObject>) end;

  TSynchroFactory = class
  public
    function AcquireKernelEvent( AManual, AInitialState, AReUse: boolean): IEvent;
    function AcquireLightEvent( AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IEvent;
    function AcquireNativeSemaphore  ( AInitialCount: cardinal): ISemaphore; // Native semaphores are never pooled.
    function AcquireTestableSemaphore( AInitialCount: cardinal; AReUse: boolean): ISemaphore;
    /// <remarks> When re-using critical sections, Enter/Leave calls must be balanced before releasing the last reference. </remarks>
    function AcquireCriticalSection( AReUse: boolean): ILock;  overload;
    class function AcquireCriticalSection: ILock;              overload;
    function AcquireSpinLock: ILock; // Spinlocks are never pooled.
    function AcquireCountDown( InitialValue: cardinal; AReUse: boolean): ICountDown;
    function AcquireCountUp( InitialValue, MaxValue: cardinal; AReUse: boolean): ICountUp;
    /// <remarks> To pulse a functional event, call Signal() on it. </remarks>
    function AcquireFunctionalEvent( ASignalTest: TEventFunction; APLock: PSBDSpinLock; AReUse: boolean): ISynchro;
    procedure PurgeAllPools;
    function  CanDestroy: boolean;

    constructor Create( APoolMax: cardinal);
    destructor Destroy; override;

  private
    FManualKernelEvents: TObjectQueue;
    FAutoKernelEvents  : TObjectQueue;
    FLightEvents       : TObjectQueue;
    FSemaphores        : TObjectQueue; // TSimulatedSemaphores only. Native semaphores cannot be pooled.
    FCrits             : TObjectQueue;
    FCountDowns        : TObjectQueue;
    FCountUps          : TObjectQueue;
    FFunctionals       : TObjectQueue;
    [Volatile]
    FAquisitionCount   : TVolatileUint32;
    FPoolMax           : cardinal;
    //FSemaphoreCount    : cardinal;
  private
    [Volatile] FLock: TSBDSpinLock;
    function Queues: TArray<TObjectQueue>;
  end;



{$REGION 'For internal use only.'}
/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountDownIntf( const APool: TSynchroFactory; InitialValue: cardinal): ICountDown;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountUpIntf( const APool: TSynchroFactory; InitialValue, MaxValue: cardinal): ICountUp;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateKernalEventIntf( const APool: TSynchroFactory; ManualReset, InitialState: boolean): IEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateLightEventIntf( const APool: TSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCritLockIntf( const APool: TSynchroFactory): ILock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateSpinLockIntf: ILock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateTestableSemaphoreIntf( const APool: TSynchroFactory; AInitialCount: cardinal): ISemaphore;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateNativeSemaphoreIntf( AInitialCount: cardinal): ISemaphore;
{$ENDREGION}

implementation



uses  Otl.Parallel.Errors;

type

  TRecycleObject = class abstract( TObject, IInterface)
  private const
    objPooledFlag = Integer( $80000000);

  private
    {$IFDEF AUTOREFCOUNT} [weak] {$ENDIF}
    FPool: TSynchroFactory;
    [Volatile] FRefCount: TVolatileInt32;
    FAcquired: boolean;

    function  QueryInterface( const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  GetRefCount: integer;  inline;

  protected
    function Pool: TObjectQueue; virtual; abstract;
    procedure ReleaseConfiguration; virtual;
    function  SignalState: TSignalState;                 virtual; abstract;

  public
    constructor Create( const APool: TSynchroFactory);
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;

    property RefCount: integer read GetRefCount;
  end;

  TRecycleSynchro = class( TRecycleObject, ISynchro)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;           virtual;
    {$ENDIF}

    procedure Signal;                                                virtual; abstract;
    function  isSignalled: boolean;                                  virtual; abstract;
    function  isSignalled_IsSupported: boolean;                      virtual;
    function  SignalState: TSignalState;                             override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   virtual; abstract;
    function  IsModular: boolean;                                    virtual;
    function  AsMWObject: TObject;                                   virtual;
  end;

  TRecycleEvent = class( TRecycleSynchro, IEvent)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;           override;
    {$ENDIF}

    procedure Signal;                                                override;
    function  isSignalled: boolean;                                  override;
    function  isSignalled_IsSupported: boolean;                      override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   override;
    function  Pool: TObjectQueue;                                    override;
    function  SignalState: TSignalState;                             override;
    function  AsMWObject: TObject;                                   override;

  private
    FBase: TSBDEvent;
    FManual: boolean;
    FShadow: TSignalState;
    FWaiters: cardinal;
    FisLight: boolean;
    [Volatile] FLock: TSBDSpinLock; // Only used for manual events.
    FAsyncClear: boolean;

    procedure SetEvent;
    procedure ResetEvent;
    function  isManualEvent: boolean;
    function  isLight: boolean;

  public
    constructor CreateAsKernal( const APool: TSynchroFactory; AManual, AInitialState: boolean);
    constructor CreateAsLight( const APool: TSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
    destructor Destroy; override;

  public
    /// <remarks>Although syntactically "public", semantically this is "private"
    ///  and for internal use only. Please do not use. </remarks>
    procedure Reconfigure( Manual: boolean; Value: cardinal);        virtual;
  end;

  TRecycleSemaphore = class( TRecycleSynchro, ISemaphore)
  public
    constructor Create( const APool: TSynchroFactory; AInitialCount: cardinal);
    destructor Destroy; override;

  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;           override;
    {$ENDIF}
    procedure Signal;                                                override;
    function  isSignalled: boolean;                                  override;
    function  isSignalled_IsSupported: boolean;                      override;
    function  SignalState: TSignalState;                             override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   override;
    function  Pool: TObjectQueue;                                    override;
    function  AsMWObject: TObject;                                   override;

  private type
    TSimulatedSemaphore = class( TFunctionalEvent)
      public
        constructor Create( AInitialValue: cardinal);
        destructor Destroy; override;
        procedure Reset;
        procedure ReconfigureInitial( Value: cardinal);
        function SignalState: TSignalState;

      protected
        [Volatile]
        FCurrentValue: TVolatileUint32;
        FInitialValue: cardinal;

        procedure SignalTest( doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);   override;

      private
        [Volatile] FLock: TSBDSpinLock;
      end;

  private
    FBase: TSimulatedSemaphore;

    function InitialValue: cardinal;
    function Value: cardinal;
    procedure Reset;
    function isValueTesting_IsSupported: boolean;

  public
    /// <remarks>Although syntactically "public", semantically this is "private"
    ///  and for internal use only. Please do not use. </remarks>
    procedure Reconfigure( Value: cardinal);        virtual;
  end;


  TNativeSemaphore = class( TInterfacedObject, ISemaphore)
  public
    constructor Create( AInitialCount: cardinal);
    destructor Destroy; override;
  private
    FBase: TSBDSemaphore;
    FInitial: cardinal;
    FWaiters: cardinal;
    FShadow: cardinal;
    [Volatile] FLock: TSBDSpinLock;
    procedure Signal;
    function  isSignalled: boolean;
    function  isSignalled_IsSupported: boolean;
    function  SignalState: TSignalState;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  InitialValue: cardinal;
    function  Value: cardinal;
    procedure Reset;
    function  isValueTesting_IsSupported: boolean;
    function  IsModular: boolean;
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}
  protected
    function AsMWObject: TObject;
  end;


  TRecycleCrit = class( TRecycleObject, ILock)
  private
    [Volatile] FLockCount: TVolatileInt32;

  protected
    function Pool: TObjectQueue;                override;
    function SignalState: TSignalState;         override;
    function LockCount: integer;

  public
    constructor Create( const APool: TSynchroFactory);
    destructor Destroy; override;

  private
    FCrit: TCriticalSection;
    procedure Enter;
    procedure Leave;
    function  IsKernalMode: boolean;
    function  AsSpinLock: PSBDSpinLock;
    function  AsCriticalSection: TCriticalSection;
  end;

  TSpinIntf = class( TInterfacedObject, ILock)
  public
    constructor Create;
    destructor Destroy; override;

  private
    [Volatile] FLock: TSBDSpinLock;
    function  LockCount: integer;
    procedure Enter;
    procedure Leave;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  IsKernalMode: boolean;
    function  AsSpinLock: PSBDSpinLock;
    function  AsCriticalSection: TCriticalSection;
  end;


  TRecycleCountDown = class( TRecycleSynchro, ICountDown)
  protected
    function  Pool: TObjectQueue;                                    override;
    procedure Signal;                                                override;
    function  isSignalled: boolean;                                  override;
    function  isSignalled_IsSupported: boolean;                      override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   override;
  public
    constructor Create( const APool: TSynchroFactory; AInitialValue: cardinal);
    destructor Destroy; override;
  private
    FBase: TCountDown;
    function InitialValue: cardinal;
    function Value: cardinal;
    function SignalHit: boolean;
    function Allocate: cardinal; // Signal and return the value.
    procedure Reconfigure( AInitial: cardinal);
    procedure CounterSignal;
  end;

  TRecycleCountUp = class( TRecycleSynchro, ICountUp)
  protected
    function  Pool: TObjectQueue;                                    override;
    procedure Signal;                                                override;
    function  isSignalled: boolean;                                  override;
    function  isSignalled_IsSupported: boolean;                      override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   override;
  public
    constructor Create( const APool: TSynchroFactory; AInitial, AMaxValue: cardinal);
    destructor Destroy; override;
  private
    FBase: TCountUp;
    function InitialValue: cardinal;
    function MaxValue: cardinal;
    function Value: cardinal;
    function SignalHit: boolean;
    procedure Reconfigure( AInitial, AMaxValue: cardinal);
  end;

  TRecycleFunctional = class( TRecycleSynchro)
  public
    constructor Create( const APool: TSynchroFactory; ASignalTest: TEventFunction; APLock: PSBDSpinLock);
    destructor Destroy; override;

  protected
    procedure Signal;                                                override;
    function  isSignalled: boolean;                                  override;
    function  isSignalled_IsSupported: boolean;                      override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;   override;
    function  Pool: TObjectQueue;                                    override;
    procedure ReleaseConfiguration;                                  override;

  private
    FBase: TFunctionalEvent;
    procedure Reconfigure( ASignalTest: TEventFunction; APLock: PSBDSpinLock);
  end;


function TSynchroFactory.AcquireCountDown(
  InitialValue: cardinal; AReUse: boolean): ICountDown;
var
  Pool: TSynchroFactory;
  Addend: TRecycleCountDown;
  Queue: TObjectQueue;
begin
  Queue  := FCountDowns;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleCountDown;
      Addend.FRefCount.Write( 0);
      Addend.Reconfigure( InitialValue)
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleCountDown.Create( Pool, InitialValue)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;

function TSynchroFactory.AcquireCountUp(
  InitialValue, MaxValue: cardinal; AReUse: boolean): ICountUp;
var
  Pool: TSynchroFactory;
  Addend: TRecycleCountUp;
  Queue: TObjectQueue;
begin
  Queue  := FCountUps;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleCountUp;
      Addend.FRefCount.Write( 0);
      Addend.Reconfigure( InitialValue, MaxValue)
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleCountUp.Create( Pool, InitialValue, MaxValue)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;

class function TSynchroFactory.AcquireCriticalSection: ILock;
begin
  result := TRecycleCrit.Create( nil)
end;

function TSynchroFactory.AcquireCriticalSection( AReUse: boolean): ILock;
var
  Pool: TSynchroFactory;
  Addend: TRecycleCrit;
  Queue: TObjectQueue;
begin
  Queue := FCrits;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleCrit;
      Addend.FRefCount.Write( 0)
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleCrit.Create( Pool)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;

function TSynchroFactory.AcquireFunctionalEvent(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock; AReUse: boolean): ISynchro;
var
  Pool: TSynchroFactory;
  Addend: TRecycleFunctional;
  Queue: TObjectQueue;
  doPulse: boolean;
begin
  doPulse := False;
  Queue := FFunctionals;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleFunctional;
      Addend.FRefCount.Write( 0);
      Addend.Reconfigure( ASignalTest, APlock);
      doPulse := True;
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleFunctional.Create( Pool, ASignalTest, APLock)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave;
  if doPulse then
    Addend.Signal
end;


function TSynchroFactory.AcquireKernelEvent(
  AManual, AInitialState, AReUse: boolean): IEvent;
var
  Pool: TSynchroFactory;
  Addend: TRecycleEvent;
  Queue: TObjectQueue;
begin
  if AManual then
      Queue := FManualKernelEvents
    else
      Queue := FAutoKernelEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleEvent;
      Addend.FRefCount.Write( 0);
      if (not (Addend.isSignalled_IsSupported)) or
         (Addend.isSignalled <> AInitialState) then
            begin
            if AInitialState then
                Addend.SetEvent
              else
                Addend.ResetEvent
            end
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleEvent.CreateAsKernal( Pool, AManual, AInitialState)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;

function TSynchroFactory.AcquireLightEvent(
  AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IEvent;
var
  Pool: TSynchroFactory;
  Addend: TRecycleEvent;
  Queue: TObjectQueue;
begin
  Queue := FLightEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleEvent;
      Addend.Reconfigure( AManual, SpinMax);
      Addend.FRefCount.Write( 0);
      if (not (Addend.isSignalled_IsSupported)) or
         (Addend.isSignalled <> AInitialState) then
            begin
            if AInitialState then
                Addend.SetEvent
              else
                Addend.ResetEvent
            end
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleEvent.CreateAsLight( Pool, AManual, AInitialState, SpinMax)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;



function TSynchroFactory.AcquireNativeSemaphore( AInitialCount: cardinal): ISemaphore;
begin
  result := TNativeSemaphore.Create( AInitialCount)
end;


function TSynchroFactory.AcquireTestableSemaphore( AInitialCount: cardinal; AReUse: boolean): ISemaphore;
var
  Pool: TSynchroFactory;
  Addend: TRecycleSemaphore;
  Queue: TObjectQueue;
begin
  Queue := FSemaphores;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then
      begin
      Addend := Queue.Dequeue as TRecycleSemaphore;
      Addend.Reconfigure( AInitialCount);
      Addend.FRefCount.Write( 0);
      end
    else
      begin
      if AReUse then
          Pool := self
        else
          Pool := nil;
      Addend := TRecycleSemaphore.Create( Pool, AInitialCount)
      end;
  result := Addend;
  if AReUse then
    begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment
    end;
  FLock.Leave
end;

function TSynchroFactory.AcquireSpinLock: ILock;
begin
  result := TSpinIntf.Create
end;

function TSynchroFactory.CanDestroy: boolean;
begin
  result := FAquisitionCount.Read = 0
end;


constructor TSynchroFactory.Create( APoolMax: cardinal);
begin
  FLock.Initialize;
  FPoolMax := APoolMax;
  FManualKernelEvents := TObjectQueue.Create;
  FAutoKernelEvents   := TObjectQueue.Create;
  FLightEvents        := TObjectQueue.Create;
  FSemaphores         := TObjectQueue.Create;
  FCrits              := TObjectQueue.Create;
  FCountDowns         := TObjectQueue.Create;
  FCountUps           := TObjectQueue.Create;
  FFunctionals        := TObjectQueue.Create
end;

destructor TSynchroFactory.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  Queue: TObjectQueue;
{$ENDIF}
begin
  if FAquisitionCount.Read <> 0 then
    raise TParallelException.Create( ECannotDestroySynchroFactory);
  PurgeAllPools;
  {$IFNDEF AUTOREFCOUNT}
  for Queue in Queues do
    Queue.Free;
  {$ENDIF}
  inherited;
  FLock.Finalize
end;

procedure TSynchroFactory.PurgeAllPools;
begin
  FLock.WithinLock( procedure
    var
      Queue: TObjectQueue;
    begin
    for Queue in Queues do
      while Queue.Count > 0 do
       {$IFDEF AUTOREFCOUNT}
        Queue.Dequeue
       {$ELSE}
        Queue.Dequeue.Free
       {$ENDIF}
    end)
end;

function TSynchroFactory.Queues: TArray<TObjectQueue>;
begin
  result := [FManualKernelEvents, FAutoKernelEvents, FLightEvents, FSemaphores, FCrits, FCountDowns, FCountUps, FFunctionals];
end;


procedure TRecycleObject.AfterConstruction;
begin
  FRefCount.Decrement
end;

function TRecycleObject.AsObject: TObject;
begin
  result := self
end;

class function TRecycleObject.NewInstance: TObject;
begin
  result := inherited NewInstance;
  TRecycleObject( result).FRefCount.Initialize( 1)
end;

constructor TRecycleObject.Create( const APool: TSynchroFactory);
begin
  FPool := APool
end;


function TRecycleObject.GetRefCount: integer;
begin
  result := FRefCount.Read;
  if ((result and objPooledFlag) <> 0) or (result <= -2) then
    result := -1
end;

function TRecycleObject.IsPoolManaged: boolean;
begin
  result := assigned( FPool)
end;

function TRecycleObject.QueryInterface( const IID: TGUID; out Obj): HResult;
begin
  if GetInterface( IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE
end;

procedure TRecycleObject.ReleaseConfiguration;
begin
end;

function TRecycleObject._AddRef: Integer;
begin
  result := FRefCount.Increment
end;

function TRecycleObject._Release: Integer;
var
  OldValue: integer;
  PooledByOther: boolean;
  Pl: TObjectQueue;
begin
  result := FRefCount.Decrement;
  if result <> 0 then exit;
  repeat
    OldValue := FRefCount.Read;
    PooledByOther := (OldValue and objPooledFlag) = objPooledFlag;
  until PooledByOther or FRefCount.CompareAndExchange( OldValue, OldValue or objPooledFlag);
  if PooledByOther then exit;
  ReleaseConfiguration;
  if assigned( FPool) then
      begin
      if FAcquired then
        begin
        FAcquired := False;
        FPool.FAquisitionCount.Decrement
        end;
      Pl := Pool;
      if cardinal( Pl.Count) < FPool.FPoolMax then
          Pool.Enqueue( self)
        else
          begin
          FPool := nil;
          Destroy
          end
      end
    else
      Destroy
end;


function TRecycleEvent.AsMWObject: TObject;
begin
  result := FBase.AsMWObject
end;

constructor TRecycleEvent.CreateAsKernal( const APool: TSynchroFactory; AManual, AInitialState: boolean);
begin
  FLock.Initialize;
  inherited Create( APool);
  FManual := AManual;
  FisLight := False;
  if AInitialState then
      FShadow := esSignalled
    else
      FShadow := esNotSignalled;
  FWaiters  := 0;
  FAsyncClear := False;
  FBase   := TKernelEvent.Create( AManual, AInitialState)
end;

constructor TRecycleEvent.CreateAsLight(
  const APool: TSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
begin
  FLock.Initialize;
  inherited Create( APool);
  FManual := AManual;
  FisLight := True;
  if AInitialState then
      FShadow := esSignalled
    else
      FShadow := esNotSignalled;
  FWaiters  := 0;
  FAsyncClear := False;
  FBase   := TLightEvent.Create( AManual, AInitialState, SpinMax)
end;

destructor TRecycleEvent.Destroy;
begin
  FBase.Free;
  FLock.Finalize;
  inherited
end;

{$IFDEF MSWINDOWS}
function TRecycleEvent.GetHandle: THandle;
begin
  if FisLight then
      result := 0
    else
      result := FBase.Handle
end;
{$ENDIF}

function TRecycleEvent.isLight: boolean;
begin
  result := FisLight
end;

function TRecycleEvent.isManualEvent: boolean;
begin
  result := FManual
end;

function TRecycleEvent.isSignalled: boolean;
var
  Ok: boolean;
begin
  if FisLight then
      result := FBase.isSignalled
  else if FManual then
      result := FShadow = esSignalled
  else
    begin
    FLock.Enter;
    Ok := FShadow <> esUnknown;
    result := FShadow = esSignalled;
    FLock.Leave;
    if not Ok then
      raise TParallelException.Create( EIsSignalledNotSupported)
    end
end;

function TRecycleEvent.isSignalled_IsSupported: boolean;
begin
  result := FisLight or FManual
end;

function TRecycleEvent.Pool: TObjectQueue;
begin
  if not assigned( FPool) then
      result := nil
  else if isLight then
      result := FPool.FLightEvents
  else if FManual then
      result := FPool.FManualKernelEvents
  else
      result := FPool.FAutoKernelEvents

end;

procedure TRecycleEvent.ResetEvent;
begin
  if not FisLight then
    FLock.Enter;
  FBase.ResetEvent;
  if not FisLight then
    begin
    if FManual or (FWaiters = 0) then
        FShadow := esNotSignalled
      else
        FAsyncClear := True;
    FLock.Leave
    end
end;

procedure TRecycleEvent.SetEvent;
begin
  if not FisLight then
    FLock.Enter;
  FBase.SetEvent;
  if not FisLight then
    begin
    if FManual or (FWaiters = 0) then
        FShadow := esSignalled
      else
        FAsyncClear := False;
    FLock.Leave
    end
end;

procedure TRecycleEvent.Reconfigure( Manual: boolean; Value: cardinal);
begin
  if FBase is TLightEvent then
    TLightEvent( FBase).Reconfigure( Manual, Value)
end;

procedure TRecycleEvent.Signal;
begin
  SetEvent
end;

function TRecycleEvent.SignalState: TSignalState;
begin
  if isSignalled then
      result := esSignalled
    else
      result := esNotSignalled
end;

function TRecycleEvent.WaitFor( Timeout: cardinal): TWaitResult;
begin
  if (not FisLight) and (not FManual) then
    begin
    FLock.Enter;
    if FWaiters = 0 then
      FAsyncClear := False;
    Inc( FWaiters);
    FShadow := esUnknown;
    FLock.Leave
    end;
  result := FBase.WaitFor( Timeout);
  if (not FisLight) and (not FManual) then
    begin
    FLock.Enter;
    Dec( FWaiters);
    if (FWaiters = 0) and FAsyncClear then
      begin
      FAsyncClear := False;
      FShadow     := esNotSignalled
      end;
    FLock.Leave
    end
end;


function TRecycleSynchro.IsModular: boolean;
begin
  result := False
end;

function TRecycleSynchro.isSignalled_IsSupported: boolean;
begin
  result := True
end;


function TRecycleSynchro.AsMWObject: TObject;
begin
  result := nil
end;

{$IFDEF MSWINDOWS}
function TRecycleSynchro.GetHandle: THandle;
begin
  result := 0
end;
{$ENDIF}

function TRecycleSynchro.SignalState: TSignalState;
begin
  if isSignalled then
      result := esSignalled
    else
      result := esNotSignalled
end;



constructor TRecycleSemaphore.Create(
  const APool: TSynchroFactory; AInitialCount: cardinal);
begin
  inherited Create( APool);
  FBase := TSimulatedSemaphore.Create( AInitialCount)
end;

destructor TRecycleSemaphore.Destroy;
begin
  FBase.Free;
  inherited
end;

{$IFDEF MSWINDOWS}
function TRecycleSemaphore.GetHandle: THandle;
begin
  result := 0
end;
{$ENDIF}

function TRecycleSemaphore.AsMWObject: TObject;
begin
  result := nil
end;


function TRecycleSemaphore.InitialValue: cardinal;
begin
  result := FBase.FInitialValue
end;

function TRecycleSemaphore.isSignalled: boolean;
begin
  result := FBase.isSignalled
end;

function TRecycleSemaphore.SignalState: TSignalState;
begin
  result := FBase.SignalState
end;

function TRecycleSemaphore.isSignalled_IsSupported: boolean;
begin
  result := True
end;

function TRecycleSemaphore.isValueTesting_IsSupported: boolean;
begin
  result := True
end;

function TRecycleSemaphore.Pool: TObjectQueue;
begin
  if not assigned( FPool) then
    result := nil
  else
    result := FPool.FSemaphores
end;

procedure TRecycleSemaphore.Reconfigure( Value: cardinal);
begin
  FBase.ReconfigureInitial( Value)
end;

procedure TRecycleSemaphore.Reset;
begin
  FBase.Reset
end;

procedure TRecycleSemaphore.Signal;
begin
  FBase.FLock.Enter;
  FBase.FCurrentValue.Increment;
  FBase.FLock.Leave;
  FBase.Pulse
end;

function TRecycleSemaphore.Value: cardinal;
begin
  FBase.FLock.Enter;
  result := FBase.FCurrentValue.Read;
  FBase.FLock.Leave
end;

function TRecycleSemaphore.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FBase.WaitFor( Timeout)
end;


constructor TRecycleSemaphore.TSimulatedSemaphore.Create( AInitialValue: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitialValue;
  FCurrentValue.Initialize( FInitialValue);
  inherited Create( nil, @FLock)
end;

destructor TRecycleSemaphore.TSimulatedSemaphore.Destroy;
begin
  FLock.Finalize;
  FCurrentValue.Finalize;
  inherited
end;

procedure TRecycleSemaphore.TSimulatedSemaphore.ReconfigureInitial( Value: cardinal);
begin
  FInitialValue := Value;
  Reset
end;

procedure TRecycleSemaphore.TSimulatedSemaphore.Reset;
begin
  FLock.WithinLock( procedure
    begin
    FCurrentValue.Write( FInitialValue);
    if FInitialValue > 0 then
      Pulse
    end)
end;

function TRecycleSemaphore.TSimulatedSemaphore.SignalState: TSignalState;
begin
  if FCurrentValue.Read > 0 then
      result := esSignalled
    else
      result := esNotSignalled
end;

procedure TRecycleSemaphore.TSimulatedSemaphore.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  wasSuccessfullyAcquired := doAcquire and FCurrentValue.DecrementIfAboveZero;
  isInSignalledState := FCurrentValue.Read > 0
end;




constructor TNativeSemaphore.Create( AInitialCount: cardinal);
begin
  FInitial := AInitialCount;
  FBase    := TSBDSemaphore.Create( FInitial);
  FWaiters := 0;
  FShadow  := FInitial;
  FLock.Initialize;
end;

destructor TNativeSemaphore.Destroy;
begin
  FLock.Finalize;
  FBase.Free;
  inherited
end;

function TNativeSemaphore.AsMWObject: TObject;
begin
  result := FBase.AsMWObject
end;

function TNativeSemaphore.AsObject: TObject;
begin
  result := self
end;

{$IFDEF MSWINDOWS}
function TNativeSemaphore.GetHandle: THandle;
begin
  result := FBase.Handle
end;
{$ENDIF}

function TNativeSemaphore.InitialValue: cardinal;
begin
  result := FInitial
end;

function TNativeSemaphore.IsPoolManaged: boolean;
begin
  result := False
end;

function TNativeSemaphore.isSignalled: boolean;
var
  State: TSignalState;
begin
  State := SignalState;
  result := State = esSignalled;
  if State = esUnknown then
    raise TParallelException.Create( EIsSignalledNotSupported)
end;


function TNativeSemaphore.SignalState: TSignalState;
begin
  FLock.Enter;
  if (FWaiters = 0) or (FShadow = 0) then
      begin
      if FShadow > 0 then
          result := esSignalled
        else
          result := esNotSignalled
      end
    else
      result := esUnknown;
  FLock.Leave
end;

function TNativeSemaphore.isSignalled_IsSupported: boolean;
begin
  result := False
end;

function TNativeSemaphore.isValueTesting_IsSupported: boolean;
begin
  result := False
end;

function TNativeSemaphore.IsModular: boolean;
begin
{$IFDEF MSWINDOWS}
  result := True
{$ELSE}
  result := False
{$ENDIF}
end;

procedure TNativeSemaphore.Reset;
begin
  raise TParallelException.Create( EIsSignalledNotSupported)
end;

procedure TNativeSemaphore.Signal;
begin
  FLock.Enter;
  FBase.Signal;
  Inc( FShadow);
  FLock.Leave;
end;

function TNativeSemaphore.Value: cardinal;
var
  Ok: boolean;
begin
  result := 0;
  FLock.Enter;
  Ok := FWaiters = 0;
  if Ok then
    result := FShadow;
  FLock.Leave;
  if not Ok then
    raise TParallelException.Create( EIsSignalledNotSupported)
end;

function TNativeSemaphore.WaitFor( Timeout: cardinal): TWaitResult;
begin
  FLock.Enter;
  Inc( FWaiters);
  FLock.Leave;
  result := FBase.WaitFor( Timeout);
  FLock.Enter;
  if result = wrSignaled then
    Dec( FShadow);
  Dec( FWaiters);
  FLock.Leave;
end;


constructor TRecycleCrit.Create(const APool: TSynchroFactory);
begin
  inherited Create( APool);
  FCrit := TFixedCriticalSection.Create;
  FLockCount.Initialize( 0);
end;

destructor TRecycleCrit.Destroy;
begin
  FCrit.Free;
  FLockCount.Finalize;
  inherited
end;

function TRecycleCrit.AsCriticalSection: TCriticalSection;
begin
  result := FCrit
end;

function TRecycleCrit.AsSpinLock: PSBDSpinLock;
begin
  result := nil
end;

procedure TRecycleCrit.Enter;
begin
  FCrit.Enter;
  FLockCount.Increment
end;

function TRecycleCrit.IsKernalMode: boolean;
begin
  result := True
end;


procedure TRecycleCrit.Leave;
begin
  FLockCount.Decrement;
  FCrit.Leave
end;

function TRecycleCrit.LockCount: integer;
begin
  result := FLockCount.Read
end;

function TRecycleCrit.Pool: TObjectQueue;
begin
  if assigned( FPool) then
      result := FPool.FCrits
    else
      result := nil
end;


function TRecycleCrit.SignalState: TSignalState;
begin
  result := esUnknown
end;

constructor TSpinIntf.Create;
begin
  FLock.Initialize
end;

destructor TSpinIntf.Destroy;
begin
  FLock.Finalize;
  inherited
end;

function TSpinIntf.AsCriticalSection: TCriticalSection;
begin
  result := nil
end;

function TSpinIntf.AsObject: TObject;
begin
  result := self
end;

function TSpinIntf.AsSpinLock: PSBDSpinLock;
begin
  result := @FLock
end;

procedure TSpinIntf.Enter;
begin
  FLock.Enter
end;

function TSpinIntf.IsKernalMode: boolean;
begin
  result := False
end;

function TSpinIntf.IsPoolManaged: boolean;
begin
  result := False
end;

procedure TSpinIntf.Leave;
begin
  FLock.Leave
end;


constructor TRecycleCountDown.Create( const APool: TSynchroFactory; AInitialValue: cardinal);
begin
  inherited Create( APool);
  FBase := TCountDown.Create( AInitialValue)
end;

function TSpinIntf.LockCount: integer;
begin
  result := FLock.FEntryCount.Read
end;

function TRecycleCountDown.Allocate: cardinal;
begin
  result := FBase.Allocate
end;

procedure TRecycleCountDown.CounterSignal;
begin
  FBase.CounterSignal
end;

destructor TRecycleCountDown.Destroy;
begin
  FBase.Free;
  inherited
end;

function TRecycleCountDown.InitialValue: cardinal;
begin
  result := FBase.FullCount
end;

function TRecycleCountDown.isSignalled: boolean;
begin
  result := FBase.isSignalled
end;

function TRecycleCountDown.isSignalled_IsSupported: boolean;
begin
  result := True
end;

function TRecycleCountDown.Pool: TObjectQueue;
begin
  if assigned( FPool) then
      result := FPool.FCountDowns
    else
      result := nil
end;

procedure TRecycleCountDown.Reconfigure( AInitial: cardinal);
begin
  FBase.Reconfigure( AInitial);
end;

procedure TRecycleCountDown.Signal;
begin
  FBase.Signal
end;

function TRecycleCountDown.SignalHit: boolean;
begin
  result := FBase.SignalHit
end;

function TRecycleCountDown.Value: cardinal;
begin
  result := FBase.Value
end;

function TRecycleCountDown.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FBase.WaitFor( Timeout)
end;


constructor TRecycleCountUp.Create(
  const APool: TSynchroFactory; AInitial, AMaxValue: cardinal);
begin
  inherited Create( APool);
  FBase := TCountUp.Create( AInitial, AMaxValue)
end;

destructor TRecycleCountUp.Destroy;
begin
  FBase.Free;
  inherited
end;

function TRecycleCountUp.InitialValue: cardinal;
begin
  result := FBase.InitialValue
end;

function TRecycleCountUp.isSignalled: boolean;
begin
  result := FBase.isSignalled
end;

function TRecycleCountUp.isSignalled_IsSupported: boolean;
begin
  result := True
end;

function TRecycleCountUp.MaxValue: cardinal;
begin
  result := FBase.MaxValue
end;

function TRecycleCountUp.Pool: TObjectQueue;
begin
  if assigned( FPool) then
      result := FPool.FCountUps
    else
      result := nil
end;

procedure TRecycleCountUp.Reconfigure( AInitial, AMaxValue: cardinal);
begin
  FBase.Reconfigure( AInitial, AMaxValue)
end;

procedure TRecycleCountUp.Signal;
begin
  FBase.Signal
end;

function TRecycleCountUp.SignalHit: boolean;
begin
  result := FBase.SignalHit
end;

function TRecycleCountUp.Value: cardinal;
begin
  result := FBase.Value
end;

function TRecycleCountUp.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FBase.WaitFor( Timeout)
end;


constructor TRecycleFunctional.Create(
  const APool: TSynchroFactory; ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  inherited Create( APool);
  FBase := TFunctionalEvent.Create( ASignalTest, APLock)
end;

destructor TRecycleFunctional.Destroy;
begin
  FBase.Free;
  inherited
end;

function TRecycleFunctional.isSignalled: boolean;
begin
  result := FBase.isSignalled
end;

function TRecycleFunctional.isSignalled_IsSupported: boolean;
begin
  result := True
end;

function TRecycleFunctional.Pool: TObjectQueue;
begin
  if assigned( FPool) then
      result := FPool.FFunctionals
    else
      result := nil
end;

procedure TRecycleFunctional.Reconfigure(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  FBase.Reconfigure( ASignalTest, APLock)
end;

procedure TRecycleFunctional.ReleaseConfiguration;
begin
  FBase.Reconfigure( nil, nil)
end;

procedure TRecycleFunctional.Signal;
begin
  FBase.Pulse
end;

function TRecycleFunctional.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FBase.WaitFor( Timeout)
end;


function _CreateCountDownIntf( const APool: TSynchroFactory; InitialValue: cardinal): ICountDown;
begin
  result := TRecycleCountDown.Create( APool, InitialValue)
end;

function _CreateCountUpIntf( const APool: TSynchroFactory; InitialValue, MaxValue: cardinal): ICountUp;
begin
  result := TRecycleCountUp.Create( APool, InitialValue, MaxValue)
end;

function _CreateKernalEventIntf( const APool: TSynchroFactory; ManualReset, InitialState: boolean): IEvent;
begin
  result := TRecycleEvent.CreateAsKernal( APool, ManualReset, InitialState)
end;

function _CreateLightEventIntf( const APool: TSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;
begin
  result := TRecycleEvent.CreateAsLight( APool, ManualReset, InitialState, SpinMax)
end;

function _CreateCritLockIntf( const APool: TSynchroFactory): ILock;
begin
  result := TRecycleCrit.Create( APool)
end;


function _CreateSpinLockIntf: ILock;
begin
  result := TSpinIntf.Create
end;


function _CreateTestableSemaphoreIntf( const APool: TSynchroFactory; AInitialCount: cardinal): ISemaphore;
begin
  result := TRecycleSemaphore.Create( APool, AInitialCount)
end;


function _CreateNativeSemaphoreIntf( AInitialCount: cardinal): ISemaphore;
begin
  result := TNativeSemaphore.Create( AInitialCount)
end;


end.
