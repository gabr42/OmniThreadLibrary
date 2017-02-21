unit Otl.Parallel.SynchroPrimitives.ConditionVariables;

{$I OtlOptions.inc}

interface
uses Otl.Parallel.SynchroPrimitives.InterfaceLevel, System.SyncObjs
   , System.Classes, System.SysUtils, System.Generics.Collections
 {$IFDEF MSWINDOWS}
   , Winapi.Windows
 {$ENDIF}
   , Otl.Parallel.Atomic;

const
  FOREVER = INFINITE;
  MaxCardinal: cardinal = cardinal( -1);

type
  TSBDConditionVariable = class
  public
    FLock: ILock;
    {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
      FEventFactory: TSynchroFactory;
    {$ENDIF} {$ENDIF}

    procedure Pulse;

    /// <remarks>Call from within lock. Sleeps outside of the lock.</remarks>
    function  WaitUntilPulse( TimeOut: cardinal = FOREVER): TWaitResult;

    /// <remarks>Call outside of lock. Condition is tested within the lock, but it sleeps outside of the lock.</remarks>
    /// <param name="Condition">The condition to wait for. Must not raise an exception.</param>
    /// <param name="UpdateStateBasedOnConditionChange">If the condition test fails, but it detects a state change, and we are about to enter a sleep state, call this procedure. May be nil. Must not raise an exception..</param>
    /// <param name="CommitAcceptanceOfCondition">If the condition test passes, but it requires a commital step, call this function. If it returns wrSignaled, the commit was successful.
    ///   If unsuccesful, but could roll back, then return wrAbandoned, otherwise return wrError. May be nil. Must not raise an exception..</param>
    function  WaitFor(
                TimeOut: cardinal;
                Condition                        : System.SysUtils.TFunc<boolean,boolean>;
                UpdateStateBasedOnConditionChange: System.SysUtils.TProc)
                  : TWaitResult;

    class constructor Create;
    constructor Create(
      {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
        AEventFactory: TSynchroFactory; AdoSerializePulses: boolean;
      {$ENDIF} {$ENDIF}
        const ALock: ILock);
    destructor  Destroy; override;

  private
    {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
    FCritSect: TCriticalSection;
    FCondVar: TConditionVariableCS;
    {$ELSE}

    {$IFDEF MSWINDOWS}
    FCritSect: TRTLCriticalSection;
    FCondVar : RTL_CONDITION_VARIABLE;

    {$ELSE}
  private
    FWaiters: TList<IEvent>;
    FdoSerializePulses: boolean;
    {$ENDIF}
    {$ENDIF}
  end;


implementation








uses Diagnostics;


{$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFDEF MSWINDOWS}
  type TCriticalSection_AccessProtected = class( TCriticalSection) end;
{$ENDIF} {$ENDIF}

class constructor TSBDConditionVariable.Create;
begin
  TStopwatch.Create
end;

constructor TSBDConditionVariable.Create(
      {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
        AEventFactory: TSynchroFactory;
        AdoSerializePulses: boolean;
      {$ENDIF} {$ENDIF}
      const ALock: ILock);
begin
  FLock := ALock;

  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCritSect := FLock.AsCriticalSection;
  FCondVar  := TConditionVariableCS.Create;
  Assert( assigned( FCritSect));
  {$ELSE}

  {$IFDEF MSWINDOWS}
  Assert( assigned( FLock.AsCriticalSection));
  FCritSect := TCriticalSection_AccessProtected( FLock.AsCriticalSection).FSection;
  WinApi.Windows.InitializeConditionVariable( FCondVar);

  {$ELSE}
  FWaiters      := TList<IEvent>.Create;
  FEventFactory := AEventFactory;
  FdoSerializePulses := AdoSerializePulses
  {$ENDIF}
  {$ENDIF}
end;


destructor TSBDConditionVariable.Destroy;
begin
  FLock.Enter;

  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCondVar.Free;
  {$ELSE}

  {$IFDEF MSWINDOWS}
  // NOP

  {$ELSE}
  FWaiters.Free;
  {$ENDIF}
  {$ENDIF}

  FLock.Leave;
  FLock := nil;
  inherited;
end;


procedure TSBDConditionVariable.Pulse;
{$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
var
  Ev: IEvent;
{$ENDIF} {$ENDIF}
begin
  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCondVar.ReleaseAll
  {$ELSE}

  {$IFDEF MSWINDOWS}
  WinApi.Windows.WakeAllConditionVariable( FCondVar);

  {$ELSE}
  FLock.Enter;
  for Ev in FWaiters do
    begin
    Ev.Signal;
    if FdoSerializePulses then
      break
    end;
  FLock.Leave
  {$ENDIF}
  {$ENDIF}
end;

function TSBDConditionVariable.WaitFor(
                TimeOut: cardinal;
                Condition                        : System.SysUtils.TFunc<boolean,boolean>;
                UpdateStateBasedOnConditionChange: System.SysUtils.TProc)
                  : TWaitResult;
var
  TimeOutRemaining: cardinal;
  WR: TWaitResult;
  Timer: TStopWatch;
  Elapsed: cardinal;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  doSignalNext: boolean;
  Ev: IEvent;
  {$ENDIF} {$ENDIF}
begin
  {$IFDEF POSIX}
    result := wrError;
  {$ENDIF}
  if (TimeOut <> INFINITE) and (TimeOut <> 0) then
    begin
    Timer.Reset;
    Timer.Start
    end;
  TimeOutRemaining := TimeOut;
  FLock.Enter;
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
    if Condition( True) then
        WR := wrSignaled
      else if TimeOutRemaining = 0 then
        WR := wrTimeOut
      else
        begin
          if assigned( UpdateStateBasedOnConditionChange) then
            UpdateStateBasedOnConditionChange;
          case WaitUntilPulse( TimeOutRemaining) of
            wrSignaled: begin
                        if Condition( True) then
                            WR := wrSignaled
                          else
                            WR := wrAbandoned
                            // Meaning: this was a spurious wake-up, so loop back.
                        end;
            wrTimeOut : WR := wrTimeOut;
            else        WR := wrError
            end
        end
  until (WR in [wrSignaled, wrError]) or ((WR = wrTimeOut) and (TimeOutRemaining = 0));
  if assigned( UpdateStateBasedOnConditionChange) then
    UpdateStateBasedOnConditionChange;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  doSignalNext := (result = wrSignaled) and FdoSerializePulses and Condition( False);
  {$ENDIF} {$ENDIF}
  FLock.Leave;
  result := WR;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  if doSignalNext then
    begin
    FLock.Enter;
    for Ev in FWaiters do
      begin
      Ev.Signal;
      break
      end;
    FLock.Leave
    end
  {$ENDIF} {$ENDIF}
end;



function TSBDConditionVariable.WaitUntilPulse( TimeOut: cardinal): TWaitResult;
{$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
{$IFNDEF MSWINDOWS}
var
  Ev: IEvent;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  result := FCondVar.WaitFor( TimeOut)
  {$ELSE}

  {$IFDEF MSWINDOWS}
  if WinApi.Windows.SleepConditionVariableCS( FCondVar, FCritSect, TimeOut) then
      result := wrSignaled
    else
      case GetLastError of
        ERROR_TIMEOUT : result := wrTimeout;
        WAIT_ABANDONED: result := wrAbandoned;
        else            result := wrError;
        end;

  {$ELSE}
  Ev := FEventFactory.AcquireKernelEvent( True, False, True);  // Manual. Starts clear. Re-use.
  FWaiters.Add( Ev);
  FLock.Leave;
  result := Ev.WaitFor( TimeOut);
  FLock.Enter;
  FWaiters.Remove( Ev)
  {$ENDIF}
  {$ENDIF}
end;



end.
