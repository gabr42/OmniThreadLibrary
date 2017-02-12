unit OtlParallel.Platform.Pipe;
{$I OtlOptions.inc}
interface
uses SysUtils, System.SyncObjs, Generics.Collections,
     OtlSync.Platform.Interfaced,
     OtlSync.Platform.Atomic,
     OtlParallel.Platform.Interfaces;

type

  IPipeBase_Internal = interface ['{4278834F-E84E-4EA8-B124-2475225B5C8A}']
    function  EnqueueException( Addend: Exception; TimeOut: cardinal = FOREVER): TWaitResult;
    procedure Complete;
    function  isCompleted: boolean;
    procedure ClearAndReset;
    function  Count: cardinal;
    function  EnqueueSynchro: IOmniSynchro;
    function  DequeueSynchro: IOmniSynchro;
    function  CanEnqueue: boolean;
    function  CanDequeue: boolean;
    function  Status: TPipeStatus;
    function  LowWaterMark: cardinal;
    function  HighWaterMark: cardinal;
    function  MaximumCapacity: cardinal;
    function  AsObject: TObject;
  end;

  TPipeBase = class abstract( TInterfacedObject, IPipeBase_Internal)
  protected
    function  EnqueueException( Addend: Exception; TimeOut: cardinal = FOREVER): TWaitResult;     virtual; abstract;
    procedure Complete;                              virtual; abstract;
    function  isCompleted: boolean;                  virtual; abstract;
    function  isFinished: boolean;                   virtual; abstract;
    procedure ClearAndReset;                         virtual; abstract;
    function  Count: cardinal;                       virtual; abstract;
    function  EnqueueSynchro: IOmniSynchro;              virtual; abstract;
    function  DequeueSynchro: IOmniSynchro;              virtual; abstract;
    function  CanEnqueue: boolean;                   virtual; abstract;
    function  CanDequeue: boolean;                   virtual; abstract;
    function  Status: TPipeStatus;                   virtual; abstract;
    function  LowWaterMark: cardinal;                virtual; abstract;
    function  HighWaterMark: cardinal;               virtual; abstract;
    function  MaximumCapacity: cardinal;             virtual; abstract;
    function  AsObject: TObject;
  end;


  TPipe<T> = class;
  TEnumeratorPipeItem<T> = class( TInterfacedObject, IEnumerator<RWaitItem<T>>)
  private
    FPipe: TPipe<T>;
    FHold: IPipeBase_Internal;
    FCurrent: RWaitItem<T>;
    function  GetCurrent: TObject;
    function  GetCurrentIntf: RWaitItem<T>;
    function  IEnumerator<RWaitItem<T>>.GetCurrent = GetCurrentIntf;
    function  MoveNext: Boolean;
    procedure Reset;
  public
    constructor Create( AOwner: TPipe<T>);
  end;

  TEnumerablePipeItem<T> = class( TInterfacedObject, IEnumerable<RWaitItem<T>>)
  private
    FPipe: TPipe<T>;
    FHold: IPipeBase_Internal;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorIntf: IEnumerator<RWaitItem<T>>;
    function IEnumerable<RWaitItem<T>>.GetEnumerator = GetEnumeratorIntf;
  public
    constructor Create( AOwner: TPipe<T>);
  end;

{$ALIGN 8}
  TPipe<T> = class( TPipeBase, IPipe<T>)
  private
    function  EnqueueItem( const Addend: RPipeItem<T>; TimeOut: cardinal = FOREVER): TWaitResult;
  protected
    function  Enqueue( const Addend: T; TimeOut: cardinal = FOREVER): TWaitResult;
    function  EnqueueException( Addend: Exception; TimeOut: cardinal = FOREVER): TWaitResult;      override;
    function  Dequeue( var Obj: T; TimeOut: cardinal = FOREVER): TWaitResult;  overload;
    function  Dequeue: T;                                                      overload;

    procedure Complete;                               override;
    function  isCompleted: boolean;                   override;
    function  isFinished: boolean;                    override;
    procedure ClearAndReset;                          override;
    function  Count: cardinal;                        override;
    function  CanEnqueue: boolean;                    override;
    function  CanDequeue: boolean;                    override;
    function  EnqueueSynchro: IOmniSynchro;               override;
    function  DequeueSynchro: IOmniSynchro;               override;
    function  Enumerable( Timeout: cardinal): IEnumerable<RWaitItem<T>>;
    function  Status: TPipeStatus;                     override;
    function  LowWaterMark: cardinal;                  override;
    function  HighWaterMark: cardinal;                 override;
    function  MaximumCapacity: cardinal;               override;
    function  PeekDequeue( var Obj: T; var Finished: boolean): boolean;
    function  CompletedSynchro: IOmniSynchro;
    // FInished means completed and empty

  private
    FQueue: TQueue<RPipeItem<T>>;
    FLowwater, FHighWater, FCapacity: cardinal;
    FSlowLock: IOmniLock;

    // FQuickLock MUST be aligned to SizeOf( TThreadId). This could be 4 or 8 depending on platform.
    [Volatile] FQuickLock : TOmniAtomicSpinLock;
    [Volatile] FCanDequeue: boolean;
    [Volatile] FCanEnqueue: boolean;
    [Volatile] FisComplete: boolean;
    FEnqueuable: IOmniEvent;
    FDequeuable: IOmniEvent;
    [Volatile] FUseSlowLock: boolean;
    FLastOpWasPop: boolean;
    FCompleted: IOmniEvent;

  public type
    // Read this as strict private. It only marked as public in order to
    //  work around a compiler limitation.
    TOpResult = (orSucceeded, orTimeOut, OrInternalError, OrPopCompletedEmptyError, orTryAgain);
    TShieldedFunc2 = function( InitialCount: cardinal; const InDatum:  RPipeItem<T>; var OutDatum:  RPipeItem<T>): TOpResult of object;

  private
    procedure HandleCountBumpEvent( NewCount: cardinal);
    procedure ShortShieldedOperation( Operation: TProc);
    function  Inner_Enqueue( InitialCount: cardinal; const InDatum:  RPipeItem<T>; var OutDatum:  RPipeItem<T>): TOpResult;
    function  Inner_Dequeue( InitialCount: cardinal; const InDatum:  RPipeItem<T>; var OutDatum:  RPipeItem<T>): TOpResult;

    function  ShieldedOperation(
                const InDatum: RPipeItem<T>;
                var   EventMirrorFlag: boolean;
                const ControllingEvent: IOmniEvent;
                      TimeOut: cardinal;
                      Operation: TShieldedFunc2;
                var   OutDatum: RPipeItem<T>)
                     : TOpResult;

  public
    constructor Create( const WorkFactory: IWorkFactory; ALowwater, AHighWater, ACapacity: cardinal);
    destructor Destroy; override;
  end;

  TObjectPipe = class( TPipe< TObject>)
  end;


implementation




uses System.Diagnostics;
const
  MinCapacity = 1;

constructor TPipe<T>.Create(
  const WorkFactory: IWorkFactory; ALowwater, AHighWater, ACapacity: cardinal);
begin
  Assert( assigned( WorkFactory));
  FQueue     := TQueue<RPipeItem<T>>.Create;
  FLowwater  := ALowwater;
  FHighWater := AHighWater;
  FCapacity  := ACapacity;
  FQuickLock.Initialize;
  if FCapacity  < MinCapacity then
     FCapacity := MinCapacity;
  if Fhighwater <= 1 then
     Fhighwater := 1;
  if Fhighwater > FCapacity then
     Fhighwater := FCapacity;
  if FLowwater >= FCapacity then
     FLowwater := FCapacity - 1;
  FCanDequeue := False;
  FCanEnqueue := True;
  FisComplete := False;
  if assigned( WorkFactory) then
      begin
      FSlowLock   := WorkFactory.NewLock( KernelLocking);
      FEnqueuable := WorkFactory.ModularEvent( True, FCanEnqueue);
      FDequeuable := WorkFactory.ModularEvent( True, FCanDequeue);
      FCompleted  := WorkFactory.ModularEvent( True, False      )
      end
    else
      begin
      FSlowLock   := TSBDParallel.NewLock( KernelLocking);
      FEnqueuable := TSBDParallel.Event( True, FCanEnqueue);
      FDequeuable := TSBDParallel.Event( True, FCanDequeue);
      FCompleted  := TSBDParallel.Event( True, False      )
      end;
  FUseSlowLock := True;
  FLastOpWasPop := False
end;

destructor TPipe<T>.Destroy;
var
  Item: RPipeItem<T>;
begin
  FSlowLock.Enter;
  while FQueue.Count > 0 do
    begin
    Item := FQueue.Dequeue;
    Item.FExcptn.Free
    end;
  FEnqueuable := nil;
  FDequeuable := nil;
  FCompleted  := nil;
  FQueue.Free;
  FSlowLock.Leave;
  FSlowLock := nil;
  FQuickLock.Finalize;
  inherited;
end;

constructor TEnumerablePipeItem<T>.Create( AOwner: TPipe<T>);
begin
  FPipe := AOwner;
  FHold := FPipe
end;

function TPipeBase.AsObject: TObject;
begin
  result := self
end;


constructor TEnumeratorPipeItem<T>.Create( AOwner: TPipe<T>);
begin
  FPipe := AOwner;
  FHold := FPipe
end;

function TEnumeratorPipeItem<T>.GetCurrent: TObject;
begin
  result := nil
end;

function TEnumeratorPipeItem<T>.GetCurrentIntf: RWaitItem<T>;
begin
  result := FCurrent
end;

function TEnumeratorPipeItem<T>.MoveNext: Boolean;
begin
  FCurrent.FWaitResult := FPipe.Dequeue( FCurrent.FItem, FOREVER);
  result := FCurrent.FWaitResult <> wrAbandoned
end;

procedure TEnumeratorPipeItem<T>.Reset;
begin
end;


function TEnumerablePipeItem<T>.GetEnumerator: IEnumerator;
begin
  result := nil
end;

function TEnumerablePipeItem<T>.GetEnumeratorIntf: IEnumerator<RWaitItem<T>>;
begin
  result := TEnumeratorPipeItem<T>.Create( FPipe)
end;


function TPipe<T>.CanDequeue: boolean;
var
  CanDequeueResult: boolean;
begin
  ShortShieldedOperation(
    procedure
      begin
        CanDequeueResult := FCanDequeue and (FQueue.Count > 0)
      end);
  result := CanDequeueResult
end;


function TPipe<T>.CanEnqueue: boolean;
var
  CanEnqueueResult: boolean;
begin
  ShortShieldedOperation(
    procedure
      begin
        CanEnqueueResult := FCanEnqueue
      end);
  Result := CanEnqueueResult
end;


procedure TPipe<T>.ClearAndReset;
var
  Item: RPipeItem<T>;
begin
  FSlowLock.Enter;
  FQuickLock.Enter;
  try
    while FQueue.Count > 0 do
      begin
      Item := FQueue.Dequeue;
      if assigned( Item.FExcptn) then
        Item.FExcptn.Free
      end;
    FCanDequeue := False;
    FCanEnqueue := True;
    FisComplete := False;
    FCompleted.ResetEvent;
    if FCanEnqueue then
        FEnqueuable.SetEvent
      else
        FEnqueuable.ResetEvent;
    if FCanDequeue then
        FDequeuable.SetEvent
      else
        FDequeuable.ResetEvent;
    FUseSlowLock := True;
    FLastOpWasPop := False
  finally
    FQuickLock.Leave;
    FSlowLock.Leave
    end
end;

function TPipe<T>.Count: cardinal;
var
  CountResult: integer;
begin
  ShortShieldedOperation(
    procedure
      begin
        CountResult := FQueue.Count
      end);
  result := CountResult
end;


function TPipe<T>.Enumerable( Timeout: cardinal): IEnumerable<RWaitItem<T>>;
begin
  result := TEnumerablePipeItem<T>.Create( self)
end;

procedure TPipe<T>.Complete;
begin
  if FisComplete then exit;
  ShortShieldedOperation(
    procedure
      begin
        if FisComplete then exit;
        FisComplete := True;
        FCompleted.SetEvent;
        FCanDequeue := True; // Even when Count = 0 !
        FDequeuable.SetEvent;
        FCanEnqueue := False;
        FEnqueuable.ResetEvent
      end)
end;


function TPipe<T>.CompletedSynchro: IOmniSynchro;
begin
  result := FCompleted
end;

procedure TPipe<T>.HandleCountBumpEvent( NewCount: cardinal);
var
  PriorCanEnqueue: boolean;
  PriorCanDequeue: boolean;
begin
  PriorCanEnqueue := FCanEnqueue;
  PriorCanDequeue := FCanDequeue;
  if NewCount >= FCapacity then
    begin
    FCanEnqueue := False;
    if PriorCanEnqueue then
      FEnqueuable.ResetEvent
    end
  else if (NewCount <= FLowwater) and (not FisComplete) then
    begin
    FCanEnqueue := True;
    if not PriorCanEnqueue then
      FEnqueuable.SetEvent
    end;

  if (NewCount >= FHighwater) or FisComplete then
    begin
    FCanDequeue := True;
    if not PriorCanDequeue then
      FDequeuable.SetEvent
    end
  else if NewCount = 0 then
    begin
    FCanDequeue := False;
    if PriorCanDequeue then
      FDequeuable.ResetEvent
    end;
end;



function TPipe<T>.HighWaterMark: cardinal;
begin
  result := FHighwater
end;

function TPipe<T>.isCompleted: boolean;
begin
  result := FisComplete
end;

function TPipe<T>.isFinished: boolean;
var
  isFinishedResult: boolean;
begin
  ShortShieldedOperation(
    procedure
      begin
        isFinishedResult := FisComplete and (FQueue.Count = 0)
      end);
  result := isFinishedResult
end;


function TPipe<T>.LowWaterMark: cardinal;
begin
  result := FLowwater
end;

function TPipe<T>.MaximumCapacity: cardinal;
begin
  result := FCapacity
end;


function TPipe<T>.ShieldedOperation(
  const InDatum: RPipeItem<T>;
  var   EventMirrorFlag: boolean;
  const ControllingEvent: IOmniEvent;
        TimeOut: cardinal;
        Operation: TShieldedFunc2;
  var   OutDatum: RPipeItem<T>)
       : TOpResult;
var
  Timer: TStopWatch;
  TimeLeft: cardinal;
  LoopCount: int64;
  Count1, Count2: integer;
  Elapsed: int64;
  usedSlow: boolean;
  WasPop: boolean;
begin
  LoopCount := -1;
  TimeLeft  := TimeOut;
  if (TimeLeft <> FOREVER) and (TimeLeft > 0) and assigned( ControllingEvent) then
    begin
    Timer.Reset;
    Timer.Start
    end;
  repeat
    Inc( LoopCount);
    if EventMirrorFlag and assigned( ControllingEvent) then
      begin
        if (LoopCount > 0) and (TimeLeft <> FOREVER) and (TimeLeft <> 0) then
          begin
          Elapsed := Timer.ElapsedMilliseconds;
          if Elapsed > 0 then
            begin
            if (Int64Rec( Elapsed).Hi > 0) or (Int64Rec( Elapsed).Lo >= TimeOut) then
                TimeLeft := 0
              else
                TimeLeft := TimeOut - Int64Rec( Elapsed).Lo
            end
          end;
        if ControllingEvent.WaitFor( TimeLeft) = wrSignaled then
            result := orSucceeded
          else
            result := orTimeOut
      end
      else result := orSucceeded;
    if result = orSucceeded then
      begin
        usedSlow := FUseSlowLock;
        if usedSlow then
          FSlowLock.Enter;
        FQuickLock.Enter;
        Count1 := FQueue.Count;
        result := Operation( Count1, InDatum, OutDatum);
        Count2 := FQueue.Count;
        if Count2 <> Count1 then
          HandleCountBumpEvent( Count2);
        case result of
          orSucceeded:
            begin
              if not FisComplete then
                begin
                if Count2 < Count1 then
                    WasPop := True
                else if Count2 > Count1 then
                    WasPop := False
                else
                    WasPop := FLastOpWasPop;
                if WasPop <> FLastOpWasPop then
                    begin
                    FUseSlowLock  := FCanEnqueue and FCanDequeue;
                    FLastOpWasPop := WasPop
                    end
                  else
                    FUseSlowLock := False
                end
            end;

          orTimeOut               : result := OrInternalError;

          OrInternalError,
          OrPopCompletedEmptyError: ;

          orTryAgain              :
            begin
            FUseSlowLock := True;
            if TimeLeft = 0 then
              result := orTimeOut
            end
          end;
        FQuickLock.Leave;
        if usedSlow then
          FSlowLock.Leave
      end
  until result <> orTryAgain
end;


procedure TPipe<T>.ShortShieldedOperation( Operation: TProc);
var
  usedSlow: boolean;
begin
  usedSlow := FUseSlowLock;
  if usedSlow then
    FSlowLock.Enter;
  FQuickLock.Enter;
  Operation;
  FQuickLock.Leave;
  if usedSlow then
    FSlowLock.Leave
end;


function TPipe<T>.Inner_Dequeue(
  InitialCount: cardinal; const InDatum: RPipeItem<T>; var OutDatum: RPipeItem<T>): TOpResult;
begin
  if (InitialCount = 0) and FisComplete then
      begin
      result       := OrPopCompletedEmptyError;
      FUseSlowLock := True
      end
    else if FCanDequeue then
      begin
      if InitialCount = 0 then
          begin
          result       := OrInternalError;
          FUseSlowLock := True
          end
        else
          begin
          OutDatum := FQueue.Dequeue;
          result   := orSucceeded
          end;
      end
    else
      begin
      result       := orTryAgain;
      FUseSlowLock := True
      end
end;


function TPipe<T>.Dequeue( var Obj: T; TimeOut: cardinal): TWaitResult;
var
  OpRes: TOpResult;
  DummyIn: RPipeItem<T>;
  OutDatum: RPipeItem<T>;
begin
  OpRes := ShieldedOperation( DummyIn, FCanDequeue, FDequeuable, TimeOut, Inner_Dequeue, OutDatum);
  case OpRes of
    orSucceeded:
      begin
      result := wrSignaled;
      if assigned( OutDatum.FExcptn) then
          raise OutDatum.FExcptn
        else
          Obj := OutDatum.FItem
      end;

    orTimeOut:
      result := wrTimeOut;

    OrInternalError, orTryAgain:
      raise Exception.Create( 'TPipe<T> internal error');

    OrPopCompletedEmptyError:
      result := wrAbandoned
  end
end;




function TPipe<T>.Dequeue: T;
begin
  if Dequeue( result, INFINITE) = wrAbandoned then
    raise Exception.Create( 'TPipe<T>.Pop() attempt to pop a completed empty pipe.');
end;

function TPipe<T>.PeekDequeue( var Obj: T; var Finished: boolean): boolean;
begin
  case Dequeue( Obj, 0) of
    wrSignaled : begin
                 Finished := False;
                 result   := True
                 end;

    wrTimeOut  : begin
                 Finished := False;
                 result   := False
                 end;

    wrAbandoned: begin
                 Finished := True;
                 result   := False
                 end;
  end;
end;



function TPipe<T>.DequeueSynchro: IOmniSynchro;
begin
  result := FDequeuable
end;

function TPipe<T>.Enqueue( const Addend: T; TimeOut: cardinal): TWaitResult;
var
  AddendRec: RPipeItem<T>;
begin
  AddendRec.FItem   := Addend;
  AddendRec.FExcptn := nil;
  result := EnqueueItem( AddendRec, TimeOut)
end;

function TPipe<T>.EnqueueException(
  Addend: Exception; TimeOut: cardinal): TWaitResult;
var
  AddendRec: RPipeItem<T>;
begin
  AddendRec.FItem   := Default(T);
  AddendRec.FExcptn := Addend;
  result := EnqueueItem( AddendRec, TimeOut)
end;


function TPipe<T>.Inner_Enqueue(
  InitialCount: cardinal; const InDatum: RPipeItem<T>; var OutDatum: RPipeItem<T>): TOpResult;
begin
 if FCanEnqueue then
      begin
      if InitialCount >= FCapacity then
          begin
          result       := OrInternalError;
          FUseSlowLock := True
          end
        else
          begin
          FQueue.Enqueue( InDatum);
          result   := orSucceeded
          end
      end
    else
      begin
      result       := orTryAgain;
      FUseSlowLock := True
      end
end;



function TPipe<T>.EnqueueItem( const Addend: RPipeItem<T>; TimeOut: cardinal = FOREVER): TWaitResult;
var
  OpRes: TOpResult;
  DummyOut: RPipeItem<T>;
begin
  OpRes := ShieldedOperation( Addend, FCanEnqueue, FEnqueuable, TimeOut, Inner_Enqueue, DummyOut);
  case OpRes of
    orSucceeded: result := wrSignaled;

    orTimeOut: result := wrTimeOut;

    OrInternalError, orTryAgain, OrPopCompletedEmptyError:
      raise Exception.Create( 'TPipe<T> internal error');
  end
end;



function TPipe<T>.EnqueueSynchro: IOmniSynchro;
begin
  result := FEnqueuable
end;

function TPipe<T>.Status: TPipeStatus;
var
  StatusResult: TPipeStatus;
begin
  ShortShieldedOperation(
    procedure
      begin
      if FisComplete and (FQueue.Count = 0) then
          StatusResult := Finished
      else if FisComplete then
          StatusResult := Finished
      else if not FCanDequeue then
          StatusResult := Filling
      else if not FCanEnqueue then
          StatusResult := Draining
      else
          StatusResult := Normal
      end);
  result := StatusResult
end;

procedure InitUnit_Pipe;
begin
  TStopWatch.Create
end;


procedure DoneUnit_Pipe;
begin
end;



initialization
InitUnit_Pipe;

finalization
DoneUnit_Pipe;

end.
