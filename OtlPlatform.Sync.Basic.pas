unit OtlPlatform.Sync.Basic;

// IMPORTANT!
//  READ THE COMMENTS IN UNIT OtlPlatform.SynchroPrimitives .

{$I OtlOptions.inc}

interface

uses
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  OtlPlatform.Atomic,
  OtlPlatform.Sync.Intf;

type

  /// <remarks>Base class for low-level synchronisation primitives
  //   at the basic level.</remarks>
  TOtlSynchro = class abstract
    {$IFDEF MSWINDOWS}
    protected
      function GetHandle: THandle; virtual;
    {$ENDIF}

    public
      procedure Signal;               virtual;
      function  isSignalled: boolean; virtual; abstract;
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; virtual; abstract;
      function  AsMWObject: TObject;  virtual; abstract;

    {$IFDEF MSWINDOWS}
      property Handle: THandle   read GetHandle;
    {$ENDIF}
    end;

  TOtlEvent = class abstract( TOtlSynchro)
    public
      procedure SetEvent;       virtual; abstract;
      procedure ResetEvent;     virtual; abstract;
    end;

  TKernelEvent = class( TOtlEvent)
    public
      procedure SetEvent;             override;
      procedure ResetEvent;           override;
      procedure Signal;               override;

      /// <remarks>TOtlEvent.isSignalled() is not supported .</remarks>
      function  isSignalled: boolean; override;
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;

    public
      /// <remarks> DO NOT USE. This constructor is for internal use only.
      ///  Use instead either TSBDParallel.CreateKernelEventObj. </remarks>
      constructor Create( AManual, AInitialState: boolean);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    {$IFDEF MSWINDOWS}
    protected
      function GetHandle: THandle; override;
    {$ENDIF}

    private
      FEvent: System.SyncObjs.TEvent;
    end;


  TLightEvent = class( TOtlEvent)
    public
      procedure SetEvent;             override;
      procedure ResetEvent;           override;
      procedure Signal;               override;
      function  isSignalled: boolean; override;
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;

    public
      class constructor Create;
      /// <remarks> DO NOT USE. This constructor is for internal use only.
      ///  Use instead either TSBDParallel.CreateLightEventObj. </remarks>
      constructor Create( AManual, AInitialState: boolean; ASpinMax: cardinal = 100);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    private
      [Volatile] FLock: TSBDSpinLock;
      FPulsar: System.SyncObjs.TEvent; // Auto-reset
      FKernelUsers: cardinal;
      FIsSignalled: boolean;
      FManual: boolean;
      FSpinMax: cardinal;

    public
      /// <remarks>Although syntactically "public", semantically this is "private"
      ///  and for internal use only. Please do not use. </remarks>
      procedure Reconfigure( Manual: boolean; Value: cardinal);        virtual;
    end;



  TOtlSemaphore = class( TOtlSynchro)
    public
      procedure Signal;               override;
      /// <remarks>TOtlSemaphore.isSignalled() is not supported .</remarks>
      function  isSignalled: boolean; override;
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;

    public
      /// <remarks> DO NOT USE. This constructor is for internal use only.
      ///  Use instead either TSBDParallel.CreateSemaphoreObj. </remarks>
      constructor Create( AInitialCount: cardinal);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    {$IFDEF MSWINDOWS}
    protected
      function GetHandle: THandle; override;
    {$ENDIF}

    private
      FSem: System.SyncObjs.TSemaphore;
    end;

  /// <remarks>
  ///  The TFixedCriticalSection declaration was copied from the OmniThread Library.
  ///  The concept originates from Eric Grange.
  ///    Possible url for the original idea is http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  ///   however at the time of writing of this remark, this url appears broken.
  ///   For an explanation of the fix, see http://grandruru.blogspot.com.au/2012/04/fixing-tcriticalsection.html
  /// </remarks>
  TFixedCriticalSection = class( TCriticalSection)
  strict protected
    FDummy: array [0..95] of byte;
  end;

  // Spin Lock
  // ====================
  //  Please find TSBDSpinLock in unit SBD.Parallel.Atomic

  TFunctionalEvent = class( TOtlSynchro)
    public
      procedure Signal;               override;
      function  isSignalled: boolean; override;
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;
      procedure Pulse;

    public
      class constructor Create;
      constructor Create( ASignalTest: TEventFunction; APLock: PSBDSpinLock);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    protected
      FSignalTest: TEventFunction;
      FPulsar: TEvent; // Manual reset.
      FPulsarIsSignalled: boolean;
      [Volatile] FLock: TSBDSpinLock;
      FPLock: PSBDSpinLock;

      procedure SignalTest( doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);   virtual;

    public
      /// <remarks>Although syntactically "public", semantically this is "private"
      ///  and for internal use only. Please do not use. </remarks>
      procedure Reconfigure( ASignalTest: TEventFunction; APLock: PSBDSpinLock);        virtual;
    end;

  TCountDown = class( TOtlSynchro)
    public
      /// <remarks>Signal() decrements the count. Raises exception if was zero.</remarks>
      procedure Signal;               override;

      /// <remarks>CounterSignal() increments the count. Raises exception if was MaxCardinal. Does not effect FullCount.</remarks>
      procedure CounterSignal;

      /// <remarks>Like Signal(), but if this call was responsible for the count going to zero, then return True.</remarks>
      function  SignalHit: boolean;

      /// <remarks>Like Signal(), but returns the value.</remarks>
      function  Allocate: cardinal;

      /// <remarks>True iff the count is zero.</remarks>
      function  isSignalled: boolean; override;

      function  Value: cardinal;

      /// <remarks>Blocks until count is zero.</remarks>
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;

      /// <remarks>Resets the count to the initial value.</remarks>
      procedure Reset;

      /// <remarks>Returns the initial value.</remarks>
      function  FullCount: cardinal;

    public
      /// <remarks> DO NOT USE. This constructor is for internal use only.
      ///  Use instead either TSBDParallel..CreateCountDownObj. </remarks>
      constructor Create( AInitial: cardinal);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    private
      FInitialValue: cardinal;
      [Volatile] FValue: TVolatileUInt32;

    private type
      TCountDownFunction = class( TFunctionalEvent)
      protected
        procedure SignalTest( doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean); override;
      private
        FOwner: TCountDown;
        constructor Create( AOwner: TCountDown);
      end;

    private
      FLock: TSBDSpinLock;
      FCountDownFunc: TCountDownFunction;

    public
      /// <remarks>Although syntactically "public", semantically this is "private"
      ///  and for internal use only. Please do not use. </remarks>
      procedure Reconfigure( AInitial: cardinal);
    end;

  TCountUp = class( TOtlSynchro)
    public
      /// <remarks>Signal() increments the count. Raises exception if the max was breached.</remarks>
      procedure Signal;               override;

      /// <remarks>Like Signal(), but if this call was responsible for the count going hitting the max, then return True.</remarks>
      function  SignalHit: boolean;

      /// <remarks>True iff the count is at the max.</remarks>
      function  isSignalled: boolean; override;

      function  Value: cardinal;

      /// <remarks>Blocks until count is zero.</remarks>
      function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;

      /// <remarks>Resets the count to the initial value.</remarks>
      procedure Reset;

      /// <remarks>Returns the initial value.</remarks>
      function  InitialValue: cardinal;

      /// <remarks>Returns the max value.</remarks>
      function  MaxValue: cardinal;

    public
      constructor Create( AInitial, AMaxValue: cardinal);
      destructor Destroy; override;
      function  AsMWObject: TObject;  override;

    private
      FInitialValue: cardinal;
      FMaxValue: cardinal;
      FCountDown: TCountDown;

    public
      /// <remarks>Although syntactically "public", semantically this is "private"
      ///  and for internal use only. Please do not use. </remarks>
      procedure Reconfigure( AInitial, AMaxValue: cardinal);
    end;


  TOtlMREW = class // Multi-read, exclusive write
  public
    function  EnterRead( Timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitRead;
    function  EnterWrite( Timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitWrite;
    function  ReaderCount: integer; // if +ve, this is the number of entered readers,
                                    // if -ve, there is an entered writer.

  private
    [Volatile] FReaders: TVolatileInt32;
    FZeroOrAbove: System.SyncObjs.TEvent;
    FZero       : System.SyncObjs.TEvent;
    FWriterThread: TThreadId;
    FGate: TCriticalSection;

  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation












uses OtlPlatform.Errors, System.Diagnostics;

{$IFDEF MSWINDOWS}
function TOtlSynchro.GetHandle: THandle;
begin
  result := 0
end;
{$ENDIF}


procedure TOtlSynchro.Signal;
begin
end;


function TKernelEvent.AsMWObject: TObject;
begin
  result := FEvent
end;

constructor TKernelEvent.Create( AManual, AInitialState: boolean);
begin
  FEvent := System.SyncObjs.TEvent.Create( nil, AManual, AInitialState, '', False)
end;

destructor TKernelEvent.Destroy;
begin
  FEvent.Free;
  inherited
end;

{$IFDEF MSWINDOWS}
function TKernelEvent.GetHandle: THandle;
begin
  result := FEvent.Handle
end;
{$ENDIF}

function TKernelEvent.isSignalled: boolean;
begin
  raise TParallelException.Create( EIsSignalledNotSupported);
end;

procedure TKernelEvent.ResetEvent;
begin
  FEvent.ResetEvent
end;

procedure TKernelEvent.SetEvent;
begin
  FEvent.SetEvent
end;

procedure TKernelEvent.Signal;
begin
  FEvent.SetEvent
end;

function TKernelEvent.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FEvent.WaitFor( Timeout)
end;



class constructor TLightEvent.Create;
begin
  TStopwatch.Create
end;

function TLightEvent.AsMWObject: TObject;
begin
  result := nil
end;

constructor TLightEvent.Create(
  AManual, AInitialState: boolean; ASpinMax: cardinal);
begin
  FLock.Initialize;
  FIsSignalled := AInitialState;
  FPulsar      := System.SyncObjs.TEvent.Create( nil, False, FIsSignalled, '', False);
  FKernelUsers := 0;
  FManual      := AManual;
  FSpinMax     := ASpinMax
end;

procedure TLightEvent.Reconfigure( Manual: boolean; Value: cardinal);
begin
  FSpinMax := Value;
  FManual  := Manual
end;



destructor TLightEvent.Destroy;
begin
  FLock.Finalize;
  FPulsar.Free;
  inherited
end;

function TLightEvent.isSignalled: boolean;
begin
  FLock.Enter;
  result := FIsSignalled;
  FLock.Leave
end;

procedure TLightEvent.ResetEvent;
begin
  FLock.Enter;
  FisSignalled := False;
  FLock.Leave
end;

procedure TLightEvent.SetEvent;
begin
  FLock.Enter;
  FisSignalled := True;
  if FKernelUsers > 0 then
    FPulsar.SetEvent;
  FLock.Leave
end;

procedure TLightEvent.Signal;
begin
  SetEvent
end;

const
  MaxCardinal      : cardinal = Cardinal( $FFFFFFFF);
  MaxFiniteCardinal: cardinal = Cardinal( $FFFFFFFE);

function TLightEvent.WaitFor( Timeout: cardinal): TWaitResult;
var
  Timer: TStopWatch;
  TimeOutRemaining: cardinal;
  Elapsed: int64;
  SpinCount: integer;
  hasBumpedEntryCount: boolean;
  doPulse: boolean;
begin
  if (TimeOut <> INFINITE) and (TimeOut <> 0) then
    begin
    Timer.Reset;
    Timer.Start
    end;
  TimeOutRemaining    := TimeOut;
  SpinCount           := 0;
  hasBumpedEntryCount := False;
  result              := wrIOCompletion;
  doPulse             := False;
  repeat
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then
      begin
      Elapsed := Timer.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then
        Elapsed := MaxCardinal;
      if TimeOut > Elapsed then
          TimeOutRemaining := TimeOut - cardinal( Elapsed)
        else
          TimeOutRemaining := 0
      end;
    FLock.Enter;

    if (TimeOutRemaining = 0) or FisSignalled then
        begin
        if FisSignalled then
            begin
            result := wrSignaled;
            if not FManual then
              FisSignalled := False
            end
          else
            result := wrTimeOut;
        if hasBumpedEntryCount then
          Dec( FKernelUsers);
        if FisSignalled and (FKernelUsers > 0) then
          doPulse := True;
        FLock.Leave
        end

      else if (FKernelUsers > 0) or (cardinal( SpinCount) >= FSpinMax) then    // Warning, XE7: widen
        begin
        if not hasBumpedEntryCount then
          begin
          hasBumpedEntryCount := True;
          Inc( FKernelUsers)
          end;
        FLock.Leave;
        FPulsar.WaitFor( TimeOutRemaining)
        end

      else
        begin
        FLock.Leave;
        Inc( SpinCount);
        TThread.Yield;
        end
  until result <> wrIOCompletion;
  if doPulse then
    FPulsar.SetEvent
end;


function TOtlSemaphore.AsMWObject: TObject;
begin
  result := FSem
end;

constructor TOtlSemaphore.Create( AInitialCount: cardinal);
begin
  FSem := System.SyncObjs.TSemaphore.Create( nil, AInitialCount, MaxInt, '', False)
end;

destructor TOtlSemaphore.Destroy;
begin
  FSem.Free;
  inherited
end;

{$IFDEF MSWINDOWS}
function TOtlSemaphore.GetHandle: THandle;
begin
  result := FSem.Handle
end;
{$ENDIF}

function TOtlSemaphore.isSignalled: boolean;
begin
  raise TParallelException.Create( EIsSignalledNotSupported);
end;

procedure TOtlSemaphore.Signal;
begin
  FSem.Release
end;

function TOtlSemaphore.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FSem.WaitFor( Timeout)
end;



function TFunctionalEvent.AsMWObject: TObject;
begin
  result := nil
end;

constructor TFunctionalEvent.Create( ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  if assigned( APLock) then
      FPLock := APLock
    else
      begin
      FLock.Initialize;
      FPLock := @FLock
      end;
  FSignalTest        := ASignalTest;
  FPulsarIsSignalled := True;
  FPulsar            := TEvent.Create( nil, True, FPulsarIsSignalled, '', False)
end;

class constructor TFunctionalEvent.Create;
begin
  TStopwatch.Create
end;

destructor TFunctionalEvent.Destroy;
begin
  if FPLock = @FLock then
    FLock.Finalize;
  FPulsar.Free;
  inherited
end;

procedure TFunctionalEvent.Reconfigure(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  if not assigned( ASignalTest) then
      begin
      // Deconfigure in preparation for return to the object pool
      if FPLock = @FLock then
        FLock.Finalize;
      FPLock := nil
      end
    else
      begin
      // Refurbishing for re-use.
      if assigned( APLock) then
          FPLock := APLock
        else
          begin
          FLock.Initialize;
          FPLock := @FLock
          end
      end;
  FSignalTest := ASignalTest
end;


function TFunctionalEvent.isSignalled: boolean;
var
  LocalIsSig: boolean;
begin
  FPLock^.WithinLock( procedure
    var
      Dummy: boolean;
    begin
     SignalTest( False, Dummy, LocalIsSig)
    end);
  result := LocalIsSig
end;

procedure TFunctionalEvent.Signal;
begin
  Pulse
end;


procedure TFunctionalEvent.SignalTest( doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);
begin
  FSignalTest( doAcquire, wasSuccessfullyAcquired, isInSignalledState)
end;


procedure TFunctionalEvent.Pulse;
begin
  FPLock^.WithinLock( procedure
    begin
    if not FPulsarIsSignalled then
      begin
      FPulsarIsSignalled := True;
      FPulsar.SetEvent
      end
    end)
end;

function TFunctionalEvent.WaitFor( Timeout: cardinal): TWaitResult;
var
  Timer: TStopWatch;
  TimeOutRemaining: cardinal;
  Elapsed: int64;
  Clipped: boolean;
  Acquired: boolean;
begin
  if (TimeOut <> INFINITE) and (TimeOut <> 0) then
    begin
    Timer.Reset;
    Timer.Start
    end;
  TimeOutRemaining    := TimeOut;
  repeat
    Clipped := False;
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then
      begin
      Elapsed := Timer.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then
        begin
        Elapsed := MaxCardinal;
        Clipped := True
        end;
      if TimeOut > Elapsed then
          TimeOutRemaining := TimeOut - cardinal( Elapsed)
        else
          TimeOutRemaining := 0
      end;
    if TimeOutRemaining = 0 then
        result := wrSignaled
      else
        result := FPulsar.WaitFor( TimeOutRemaining);
    if (result = wrTimeOut) and Clipped then
      result := wrIOCompletion;
    if result = wrSignaled then
      begin
      Acquired := False;
      FPLock^.WithinLock( procedure
        var
          NewValue: boolean;
        begin
        SignalTest( True, Acquired, NewValue);
        if NewValue <> FPulsarIsSignalled then
          begin
          FPulsarIsSignalled := NewValue;
          if FPulsarIsSignalled then
              FPulsar.SetEvent
            else
              FPulsar.ResetEvent
          end
        end);
      if Acquired then
          result := wrSignaled
        else if TimeOutRemaining = 0 then
          result := wrTimeOut
        else
          result := wrIOCompletion
      end
  until result <> wrIOCompletion
end;



constructor TCountDown.Create( AInitial: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitial;
  FValue.Initialize( FInitialValue);
  FCountDownFunc := TCountDownFunction.Create( self);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse
end;

destructor TCountDown.Destroy;
begin
  FCountDownFunc.Free;
  FValue.Finalize;
  inherited
end;

function TCountDown.FullCount: cardinal;
begin
  result := FInitialValue
end;

function TCountDown.isSignalled: boolean;
begin
  result := FValue.Read = 0
end;

procedure TCountDown.Reconfigure( AInitial: cardinal);
begin
  FInitialValue := AInitial;
  FValue.Write( FInitialValue);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse
end;

procedure TCountDown.Reset;
begin
  FLock.WithinLock( procedure
    begin
    FValue.Write( FInitialValue)
    end);
  FCountDownFunc.Pulse
end;

procedure TCountDown.Signal;
begin
  Allocate
end;

function TCountDown.Allocate: cardinal;
var
  Bonkers: boolean;
begin
  FLock.Enter;
  Bonkers := FValue.Read = 0;
  if not Bonkers then
      result := FValue.Decrement
    else
      result := 0;
  FLock.Leave;
  if Bonkers then
      raise TParallelException.Create( ESignalCountUpDownRange)
    else
      FCountDownFunc.Pulse
end;

function TCountDown.SignalHit: boolean;
begin
  result := Allocate = 0
end;

function TCountDown.AsMWObject: TObject;
begin
  result := nil
end;

procedure TCountDown.CounterSignal;
var
  Bonkers: boolean;
  Val: cardinal;
begin
  FLock.Enter;
  Val := FValue.Read;
  Bonkers := Val = MaxCardinal;
  if not Bonkers then
    Val := FValue.Increment;
  FLock.Leave;
  if Bonkers then
      raise TParallelException.Create( ESignalCountUpDownRange)
    else if Val = 1 then
      FCountDownFunc.Pulse
end;


function TCountDown.Value: cardinal;
begin
  result := FValue.Read
end;

function TCountDown.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FCountDownFunc.WaitFor( Timeout)
end;


constructor TCountDown.TCountDownFunction.Create( AOwner: TCountDown);
begin
  FOwner := AOwner;
  inherited Create( nil, @FOwner.FLock)
end;

procedure TCountDown.TCountDownFunction.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  isInSignalledState := FOwner.FValue.Read = 0;
  wasSuccessfullyAcquired := doAcquire and isInSignalledState
end;


function TCountUp.AsMWObject: TObject;
begin
  result := nil
end;

constructor TCountUp.Create( AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  if FMaxValue > FInitialValue then
      FCountDown := TCountDown.Create( FMaxValue - FInitialValue)
    else
      begin
      FCountDown := nil;
      raise TParallelException.Create( ESignalCountUpDownRange)
      end
end;

destructor TCountUp.Destroy;
begin
  FCountDown.Free;
  inherited
end;

function TCountUp.InitialValue: cardinal;
begin
  result := FInitialValue
end;

function TCountUp.isSignalled: boolean;
begin
  result := FCountDown.isSignalled
end;

function TCountUp.MaxValue: cardinal;
begin
  result := FMaxValue
end;

procedure TCountUp.Reconfigure( AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  Reset
end;

procedure TCountUp.Reset;
begin
  FCountDown.Reset
end;

procedure TCountUp.Signal;
begin
  FCountDown.Signal
end;

function TCountUp.SignalHit: boolean;
begin
  result := FCountDown.SignalHit
end;

function TCountUp.Value: cardinal;
begin
  result := FMaxValue - FCountDown.Value
end;

function TCountUp.WaitFor( Timeout: cardinal): TWaitResult;
begin
  result := FCountDown.WaitFor( Timeout)
end;

procedure InitUnit_BasicLevel;
begin
  TStopWatch.Create
end;


procedure DoneUnit_BasicLevel;
begin
end;





constructor TOtlMREW.Create;
begin
  FReaders.Initialize( 0);
  FZeroOrAbove  := System.SyncObjs.TEvent.Create( nil, False, False, '');
  FZero         := System.SyncObjs.TEvent.Create( nil, False, False, '');
  FWriterThread := 0;
  // This is an auto-reset event.
  FGate := TFixedCriticalSection.Create
end;

destructor TOtlMREW.Destroy;
begin
  FReaders.Finalize;
  FZeroOrAbove.Free;
  FZero.Free;
  FGate.Free;
  inherited
end;

function TOtlMREW.EnterRead( Timeout: cardinal): TWaitResult;
var
  Watch: TStopWatch;
  Elapsed: int64;
  TimeoutRemaining: cardinal;
  Readers: integer;
  isStable: boolean;
begin
  Readers := FReaders.Read;
  if (Readers >= 0) and
     (FReaders.CompareAndExchange( Readers, Readers + 1)) then
      result := wrSignaled
      // It is possible that FZero will now be in a false positive state.
      // If this is the case, it will be corrected on the next EnterWrite()
    else
      result := wrIOCompletion;
  if result <> wrIOCompletion then exit;
  TimeoutRemaining := Timeout;
  if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then
    begin
    Watch.Reset;
    Watch.Start
    end;
  repeat
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then
      begin
      Elapsed := Watch.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then
        Elapsed := MaxCardinal;
      if TimeOut > Elapsed then
          TimeOutRemaining := TimeOut - cardinal( Elapsed)
        else
          TimeOutRemaining := 0
      end;
    if (TimeOutRemaining > 0) and (FReaders.Read < 0) then
      FZeroOrAbove.WaitFor( TimeOutRemaining);
    FGate.Enter;
    isStable := False;
    repeat
      Readers := FReaders.Read;
      if Readers >= 0 then
          begin
          if FReaders.CompareAndExchange( Readers, Readers + 1) then
              begin
              isStable := True;
              Inc( Readers)
              end
          end
        else
          isStable := True
    until isStable;
    if Readers >= 1 then
        begin
        result := wrSignaled;
        FZeroOrAbove.SetEvent
        end
      else if Readers < 0 then
        FZeroOrAbove.ResetEvent;
    FGate.Leave;
    if (result = wrIOCompletion) and (TimeOutRemaining = 0) then
      result := wrTimeout
  until result <> wrIOCompletion
end;



procedure TOtlMREW.ExitRead;
var
  Readers: integer;
begin
  Readers := FReaders.Decrement;
  if Readers >= 0 then
    FZeroOrAbove.SetEvent;
  if Readers = 0 then
    FZero.SetEvent
end;


function TOtlMREW.EnterWrite( Timeout: cardinal): TWaitResult;
var
  Watch: TStopWatch;
  Elapsed: int64;
  TimeoutRemaining: cardinal;
  Readers: integer;
  isStable: boolean;
  This: TThreadId;
  doWait: boolean;
  Hit: boolean;
begin
  This := TThread.CurrentThread.ThreadID;
  FGate.Enter;
  Readers := FReaders.Read;
  if (Readers < 0) and (This = FWriterThread) and
     (FReaders.CompareAndExchange( Readers, Readers - 1)) then
      result := wrSignaled
    else
      result := wrIOCompletion;
  FGate.Leave;
  if result <> wrIOCompletion then exit;
  TimeoutRemaining := Timeout;
  Watch.Reset;
  Watch.Start;
  repeat
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then
      begin
      Elapsed := Watch.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then
        Elapsed := MaxCardinal;
      if TimeOut > Elapsed then
          TimeOutRemaining := TimeOut - cardinal( Elapsed)
        else
          TimeOutRemaining := 0
      end;
    FGate.Enter;
    Readers := FReaders.Read;
    doWait := (TimeOutRemaining > 0) and
              (Readers <> 0) and
              ((Readers >= 0) or (FWriterThread <> This));
    FGate.Leave;
    if doWait then
      FZero.WaitFor( TimeOutRemaining);
    FGate.Enter;
    Hit := False;
    isStable := False;
    repeat
      Readers := FReaders.Read;
      if (Readers = 0) or ((Readers < 0) and (FWriterThread = This)) then
          begin
          if FReaders.CompareAndExchange( Readers, Readers - 1) then
              begin
              Hit := True;
              isStable := True;
              Dec( Readers);
              if Readers = -1 then
                FWriterThread := This
              end
          end
        else
          isStable := True
    until isStable;
    if Hit then
      result := wrSignaled;
    if Readers = 0 then
        FZero.SetEvent
      else
        FZero.ResetEvent;
    FGate.Leave;
    if (result = wrIOCompletion) and (TimeOutRemaining = 0) then
      result := wrTimeout
  until result <> wrIOCompletion
end;

procedure TOtlMREW.ExitWrite;
var
  Readers: integer;
begin
  FGate.Enter;
  Readers := FReaders.Increment;
  if Readers >= 0 then
    FZeroOrAbove.SetEvent;
  if Readers = 0 then
    begin
    FWriterThread := 0;
    FZero.SetEvent
    end;
  FGate.Leave
end;

function TOtlMREW.ReaderCount: integer;
begin
  result := FReaders.Read;
  if result < -1 then
    result := -1
end;

initialization
InitUnit_BasicLevel;

finalization
DoneUnit_BasicLevel;
end.
