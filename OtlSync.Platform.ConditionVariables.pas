///<summary>Platform independent condition variables.
///    Part of the OmniThreadLibrary project. Requires Delphi XE7.</summary>
///<author>Sean B. Durkin</author>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017 Sean B. Durkin, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://plus.google.com/communities/112307748950248514961
///   Authors           : Seam B. Durkin, Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Creation date     : 2017-02-12
///   Last modification : 2017-02-12
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2017-02-12
///       - Imported from mobile/Otl.Parallel.ConditionVariables.pas.

unit OtlSync.Platform.ConditionVariables;

{$I OtlOptions.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SyncObjs, System.Classes, System.SysUtils, System.Generics.Collections,
  OtlSync.Platform.Atomic,
  OtlSync.Platform.Interfaced;

const
  FOREVER = INFINITE;

type
  TOmniConditionVariable = class
  strict private
    {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
    FCritSect         : TCriticalSection;
    FCondVar          : TConditionVariableCS;
    {$ELSEIF defined(MSWINDOWS)}
    FCritSect         : TRTLCriticalSection;
    FCondVar          : RTL_CONDITION_VARIABLE;
    {$ELSE}
    FWaiters          : TList<IEvent>;
    FDoSerializePulses: boolean;
    {$ENDIF}
  protected
    FLock        : IOmniLock;
    {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
    FEventFactory: TSynchroFactory;
    {$ENDIF} {$ENDIF}
  public
    class constructor Create;
    constructor Create(
      {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
      AEventFactory: TSynchroFactory; AdoSerializePulses: boolean;
      {$ENDIF} {$ENDIF}
      const ALock: IOmniLock);
    destructor  Destroy; override;
    procedure Pulse;
    /// <remarks>Call from within lock. Sleeps outside of the lock.</remarks>
    function  WaitUntilPulse(timeout: cardinal = FOREVER): TWaitResult;
    /// <remarks>Call outside of lock. Condition is tested within the lock, but it sleeps outside of the lock.</remarks>
    /// <param name="Condition">The condition to wait for. Must not raise an exception.</param>
    /// <param name="UpdateStateBasedOnConditionChange">If the condition test fails, but it detects a state change, and we are about to enter a sleep state, call this procedure. May be nil. Must not raise an exception..</param>
    /// <param name="CommitAcceptanceOfCondition">If the condition test passes, but it requires a commital step, call this function. If it returns wrSignaled, the commit was successful.
    ///   If unsuccesful, but could roll back, then return wrAbandoned, otherwise return wrError. May be nil. Must not raise an exception..</param>
    function  WaitFor(
                timeout                          : cardinal;
                condition                        : System.SysUtils.TFunc<boolean,boolean>;
                updateStateBasedOnConditionChange: System.SysUtils.TProc)
                : TWaitResult;
  end; { TOmniConditionVariable }

implementation

uses
  System.Diagnostics;

{$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFDEF MSWINDOWS}
type
  TCriticalSection_AccessProtected = class(TCriticalSection) end;
{$ENDIF} {$ENDIF}

const
  MaxCardinal: cardinal = cardinal(-1);

{ TOmniConditionVariable }

class constructor TOmniConditionVariable.Create;
begin
  TStopwatch.Create;
end; { TOmniConditionVariable.Create }

constructor TOmniConditionVariable.Create(
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
  AEventFactory: TSynchroFactory;
  AdoSerializePulses: boolean;
  {$ENDIF} {$ENDIF}
  const ALock: IOmniLock);
begin
  FLock := ALock;

  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCritSect := FLock.AsCriticalSection;
  FCondVar  := TConditionVariableCS.Create;
  Assert( assigned( FCritSect));
  {$ELSEIF defined(MSWINDOWS)}
  Assert( assigned( FLock.AsCriticalSection));
  FCritSect := TCriticalSection_AccessProtected( FLock.AsCriticalSection).FSection;
  WinApi.Windows.InitializeConditionVariable( FCondVar);
  {$ELSE}
  FWaiters      := TList<IEvent>.Create;
  FEventFactory := AEventFactory;
  FDoSerializePulses := AdoSerializePulses;
  {$ENDIF}
end; { TOmniConditionVariable.Create }

destructor TOmniConditionVariable.Destroy;
begin
  FLock.Enter;

  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCondVar.Free;
  {$ELSEIF not defined(MSWINDOWS)}
  FWaiters.Free;
  {$ENDIF}

  FLock.Leave;
  FLock := nil;
  inherited;
end; { TOmniConditionVariable.Destroy }

procedure TOmniConditionVariable.Pulse;
{$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
var
  event: IEvent;
{$ENDIF} {$ENDIF}
begin
  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  FCondVar.ReleaseAll;
  {$ELSEIF defined(MSWINDOWS)}
  WinApi.Windows.WakeAllConditionVariable(FCondVar);
  {$ELSE}
  FLock.Enter;
  for event in FWaiters do begin
    event.Signal;
    if FDoSerializePulses then
      break;
  end;
  FLock.Leave;
  {$ENDIF}
end; { TOmniConditionVariable.Pulse }

function TOmniConditionVariable.WaitFor(
           timeout                          : cardinal;
           condition                        : System.SysUtils.TFunc<boolean,boolean>;
           updateStateBasedOnConditionChange: System.SysUtils.TProc)
           : TWaitResult;
var
  timeoutremaining: cardinal;
  wr              : TWaitResult;
  timer           : TStopWatch;
  elapsed         : cardinal;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  doSignalNext    : boolean;
  event           : IEvent;
  {$ENDIF} {$ENDIF}
begin
  Result := wrError;
  if (timeout <> INFINITE) and (timeout <> 0) then begin
    timer.Reset;
    timer.Start
  end;
  timeoutremaining := timeout;
  FLock.Enter;
  repeat
    if (timeoutremaining <> INFINITE) and (timeoutremaining <> 0) then begin
      elapsed := timer.ElapsedMilliseconds;
      if elapsed > MaxCardinal then
        elapsed := MaxCardinal;
      if timeout > elapsed then
        timeoutremaining := timeout - cardinal( elapsed)
      else
        timeoutremaining := 0;
    end;
    if condition(True) then
      wr := wrSignaled
    else if timeoutremaining = 0 then
      wr := wrTimeOut
    else begin
      if assigned(updateStateBasedOnConditionChange) then
        updateStateBasedOnConditionChange;
      case WaitUntilPulse(timeoutremaining) of
        wrSignaled:
          begin
            if condition(True) then
              wr := wrSignaled
            else
              wr := wrAbandoned
              // Meaning: this was a spurious wake-up, so loop back.
          end;
        wrTimeOut : wr := wrTimeOut;
        else        wr := wrError
      end;
    end;
  until (wr in [wrSignaled, wrError]) or ((wr = wrTimeOut) and (timeoutremaining = 0));
  if assigned(updateStateBasedOnConditionChange) then
    updateStateBasedOnConditionChange;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  doSignalNext := (Result = wrSignaled) and FDoSerializePulses and condition(False);
  {$ENDIF} {$ENDIF}
  FLock.Leave;
  Result := wr;
  {$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
  {$IFNDEF MSWINDOWS}
  if doSignalNext then begin
    FLock.Enter;
    for event in FWaiters do begin
      event.Signal;
      break;
    end;
    FLock.Leave;
  end;
  {$ENDIF} {$ENDIF}
end; { TOmniConditionVariable.WaitFor }

function TOmniConditionVariable.WaitUntilPulse(timeout: cardinal): TWaitResult;
{$IFNDEF USE_EMBARCADERO_TConditionVariableCS}
{$IFNDEF MSWINDOWS}
var
  event: IEvent;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
  Result := FCondVar.WaitFor(timeout);
  {$ELSEIF defined(MSWINDOWS)}
  if WinApi.Windows.SleepConditionVariableCS(FCondVar, FCritSect, timeout) then
    Result := wrSignaled
  else case GetLastError of
    ERROR_TIMEOUT : Result := wrTimeout;
    WAIT_ABANDONED: Result := wrAbandoned;
    else            Result := wrError;
  end;
  {$ELSE}
  event := FEventFactory.AcquireKernelEvent(True, False, True);  // Manual. Starts clear. Re-use.
  FWaiters.Add(event);
  FLock.Leave;
  Result := event.WaitFor(timeout);
  FLock.Enter;
  FWaiters.Remove(event);
  {$ENDIF}
end; { TOmniConditionVariable.WaitUntilPulse }

end.
