///<summary>Basic platform independant synchronization primitives.
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
///   Creation date     : 2017-02-11
///   Last modification : 2017-02-11
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2017-02-11
///       - Imported from mobile/Otl.Parallel.SynchroPrimitives.BasicLevel.pas.

unit OtlSync.Platform.Basic;

// IMPORTANT!
//  READ THE COMMENTS IN UNIT OtlSync.Platform.

{$I OtlOptions.inc}

interface

uses
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  OtlSync.Platform,
  OtlSync.Platform.Atomic;

type
  /// <remarks>Base class for low-level synchronisation primitives
  //   at the basic level.</remarks>
  TOmniPlatformSynchro = class abstract
  {$IFDEF MSWINDOWS}
  protected
    function GetHandle: THandle; virtual;
  {$ENDIF}
  public
    function  AsMWObject: TObject; virtual; abstract;
    function  IsSignalled: boolean; virtual; abstract;
    procedure Signal; virtual;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; virtual; abstract;
  {$IFDEF MSWINDOWS}
    property Handle: THandle read GetHandle;
  {$ENDIF}
  end; { TOmniPlatformSynchro }

  TOmniPlatformEvent = class abstract(TOmniPlatformSynchro)
  public
    procedure SetEvent; virtual; abstract;
    procedure ResetEvent; virtual; abstract;
  end; { TOmniPlatformEvent }

  TOmniKernelEvent = class(TOmniPlatformEvent)
  strict private
    FEvent: System.SyncObjs.TEvent;
  {$IFDEF MSWINDOWS}
  protected
    function GetHandle: THandle; override;
  {$ENDIF}
  public
    /// <remarks> DO NOT USE. This constructor is for internal use only.
    ///  Use instead either TSBDParallel.CreateKernelEventObj. </remarks>
    constructor Create(AManual, AInitialState: boolean);
    destructor  Destroy; override;
    function  AsMWObject: TObject; override;
    /// <remarks>TOmniPlatformEvent.IsSignalled() is not supported .</remarks>
    function  IsSignalled: boolean; override;
    procedure ResetEvent; override;
    procedure SetEvent; override;
    procedure Signal; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniKernelEvent }

  TOmniLightEvent = class(TOmniPlatformEvent)
  strict private
    FIsSignalled    : boolean;
    FKernelUsers    : cardinal;
    [Volatile] FLock: TOmniAtomicSpinLock;
    FManual         : boolean;
    FPulsar         : System.SyncObjs.TEvent; // Auto-reset
    FSpinMax        : cardinal;
  protected
    procedure Reconfigure( Manual: boolean; Value: cardinal); virtual;
  public
    class constructor Create;
    /// <remarks> DO NOT USE. This constructor is for internal use only.
    ///  Use instead either TSBDParallel.CreateLightEventObj. </remarks>
    constructor Create(AManual, AInitialState: boolean; ASpinMax: cardinal = 100);
    destructor  Destroy; override;
    function  AsMWObject: TObject;  override;
    function  IsSignalled: boolean; override;
    procedure ResetEvent; override;
    procedure SetEvent; override;
    procedure Signal; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniLightEvent }

  TOmniPlatformSemaphore = class(TOmniPlatformSynchro)
  strict private
    FSem: System.SyncObjs.TSemaphore;
  {$IFDEF MSWINDOWS}
  protected
    function GetHandle: THandle; override;
  {$ENDIF}
  public
    /// <remarks> DO NOT USE. This constructor is for internal use only.
    ///  Use instead either TSBDParallel.CreateSemaphoreObj. </remarks>
    constructor Create( AInitialCount: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject;  override;
    function  IsSignalled: boolean; override;
    procedure Signal; override;
    /// <remarks>TOmniPlatformSemaphore.IsSignalled() is not supported .</remarks>
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniPlatformSemaphore }

  /// <remarks>
  ///  The TFixedCriticalSection declaration was copied from the OmniThread Library.
  ///  The concept originates from Eric Grange.
  ///    Possible url for the original idea is http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  ///   however at the time of writing of this remark, this url appears broken.
  ///   For an explanation of the fix, see http://grandruru.blogspot.com.au/2012/04/fixing-tcriticalsection.html
  /// </remarks>
  TFixedCriticalSection = class(TCriticalSection)
  strict protected
    FDummy: array [0..95] of byte;
  end; { TFixedCriticalSection }

  TOmniFunctionalEvent = class(TOmniPlatformSynchro)
  strict protected
    [Volatile] FLock  : TOmniAtomicSpinLock;
    FPLock            : POmniAtomicSpinLock;
    FPulsar           : TEvent; // Manual reset
    FPulsarIsSignalled: boolean;
    FSignalTest       : TOmniEventFunction;
  protected
    procedure Reconfigure(ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock); virtual;
    procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean); virtual;
  public
    class constructor Create;
    constructor Create(ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
    destructor Destroy; override;
    function  AsMWObject: TObject;  override;
    function  IsSignalled: boolean; override;
    procedure Pulse;
    procedure Signal; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniFunctionalEvent }

  TOmniCountDown = class(TOmniPlatformSynchro)
  strict private type
    TCountDownFunction = class(TOmniFunctionalEvent)
    strict private
      FOwner: TOmniCountDown;
    protected
      procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean); override;
    public
      constructor Create(AOwner: TOmniCountDown);
    end; { TCountDownFunction }
  var
    FCountDownFunc   : TCountDownFunction;
    [Volatile] FValue: TOmniVolatileUInt32;
    FInitialValue    : cardinal;
    FLock            : TOmniAtomicSpinLock;
  protected
    procedure Reconfigure(AInitial: cardinal);
  public
    /// <remarks> DO NOT USE. This constructor is for internal use only.
    ///  Use instead either TSBDParallel..CreateCountDownObj. </remarks>
    constructor Create( AInitial: cardinal);
    destructor Destroy; override;
    function  AsMWObject: TObject;  override;
    /// <remarks>Like Signal(), but returns the value.</remarks>
    function  Allocate: cardinal;
    /// <remarks>CounterSignal() increments the count. Raises exception if was MaxCardinal. Does not effect FullCount.</remarks>
    procedure CounterSignal;
    /// <remarks>Returns the initial value.</remarks>
    function  FullCount: cardinal;
    /// <remarks>True iff the count is zero.</remarks>
    function  IsSignalled: boolean; override;
    /// <remarks>Signal() decrements the count. Raises exception if was zero.</remarks>
    procedure Signal; override;
    /// <remarks>Like Signal(), but if this call was responsible for the count going to zero, then return True.</remarks>
    function  SignalHit: boolean;
    function  Value: cardinal;
    /// <remarks>Blocks until count is zero.</remarks>
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
    /// <remarks>Resets the count to the initial value.</remarks>
    procedure Reset;
  end; { TOmniCountDown }

  TOmniCountUp = class(TOmniPlatformSynchro)
  strict private
    FCountDown   : TOmniCountDown;
    FInitialValue: cardinal;
    FMaxValue    : cardinal;
  protected
    procedure Reconfigure(AInitial, AMaxValue: cardinal);
  public
    constructor Create(AInitial, AMaxValue: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject;  override;
    /// <remarks>Returns the initial value.</remarks>
    function  InitialValue: cardinal;
    /// <remarks>True iff the count is at the max.</remarks>
    function  IsSignalled: boolean; override;
    /// <remarks>Returns the max value.</remarks>
    function  MaxValue: cardinal;
    /// <remarks>Resets the count to the initial value.</remarks>
    procedure Reset;
    /// <remarks>Signal() increments the count. Raises exception if the max was breached.</remarks>
    procedure Signal; override;
    /// <remarks>Like Signal(), but if this call was responsible for the count going hitting the max, then return True.</remarks>
    function  SignalHit: boolean;
    function  Value: cardinal;
    /// <remarks>Blocks until count is zero.</remarks>
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniCountUp }

  TOmniPlatformMREW = class
  strict private
    FGate              : TCriticalSection;
    [Volatile] FReaders: TOmniVolatileInt32;
    FWriterThread      : TThreadId;
    FZero              : System.SyncObjs.TEvent;
    FZeroOrAbove       : System.SyncObjs.TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    function  EnterRead(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitRead;
    function  EnterWrite(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitWrite;
    function  ReaderCount: integer; // if +ve, this is the number of entered readers,
                                    // if -ve, there is an entered writer.
  end; { TOmniPlatformMREW }

implementation

uses
  System.Diagnostics;

const
  MaxCardinal      : cardinal = Cardinal($FFFFFFFF);
  MaxFiniteCardinal: cardinal = Cardinal($FFFFFFFE);

{ TOmniPlatformSynchro }

{$IFDEF MSWINDOWS}
function TOmniPlatformSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TOmniPlatformSynchro.GetHandle }
{$ENDIF}

procedure TOmniPlatformSynchro.Signal;
begin
  // do nothing
end; { TOmniPlatformSynchro.Signal }

{ TOmniKernelEvent }

function TOmniKernelEvent.AsMWObject: TObject;
begin
  Result := FEvent;
end; { TOmniKernelEvent.AsMWObject }

constructor TOmniKernelEvent.Create(AManual, AInitialState: boolean);
begin
  FEvent := System.SyncObjs.TEvent.Create( nil, AManual, AInitialState, '', False);
end; { TOmniKernelEvent.Create }

destructor TOmniKernelEvent.Destroy;
begin
  FEvent.Free;
  inherited;
end; { TOmniKernelEvent.Destroy }

{$IFDEF MSWINDOWS}
function TOmniKernelEvent.GetHandle: THandle;
begin
  Result := FEvent.Handle;
end; { TOmniKernelEvent.GetHandle }
{$ENDIF}

function TOmniKernelEvent.IsSignalled: boolean;
begin
  raise TOmniSynchroException.Create( EIsSignalledNotSupported);
end; { TOmniKernelEvent.IsSignalled }

procedure TOmniKernelEvent.ResetEvent;
begin
  FEvent.ResetEvent;
end; { TOmniKernelEvent.ResetEvent }

procedure TOmniKernelEvent.SetEvent;
begin
  FEvent.SetEvent;
end; { TOmniKernelEvent.SetEvent}

procedure TOmniKernelEvent.Signal;
begin
  FEvent.SetEvent;
end; { TOmniKernelEvent.Signal }

function TOmniKernelEvent.WaitFor(timeout: cardinal): TWaitResult;
begin;
  Result := FEvent.WaitFor(timeout);
end; { TOmniKernelEvent.WaitFor }

{ TOmniLightEvent }

class constructor TOmniLightEvent.Create;
begin
  TStopwatch.Create;
end; { TOmniLightEvent.Create }

function TOmniLightEvent.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniLightEvent.AsMWObject }

constructor TOmniLightEvent.Create(
  AManual, AInitialState: boolean; ASpinMax: cardinal);
begin
  FLock.Initialize;
  FIsSignalled := AInitialState;
  FPulsar      := System.SyncObjs.TEvent.Create(nil, False, FIsSignalled, '', False);
  FKernelUsers := 0;
  FManual      := AManual;
  FSpinMax     := ASpinMax;
end; { TOmniLightEvent.Create }

destructor TOmniLightEvent.Destroy;
begin
  FLock.Finalize;
  FPulsar.Free;
  inherited;
end; { TOmniLightEvent.Destroy }

procedure TOmniLightEvent.Reconfigure(Manual: boolean; Value: cardinal);
begin
  FSpinMax := Value;
  FManual  := Manual;
end; { TOmniLightEvent.Reconfigure }

function TOmniLightEvent.IsSignalled: boolean;
begin
  FLock.Enter;
  Result := FIsSignalled;
  FLock.Leave;
end; { TOmniLightEvent.IsSignalled }

procedure TOmniLightEvent.ResetEvent;
begin
  FLock.Enter;
  FIsSignalled := False;
  FLock.Leave;
end; { TOmniLightEvent.ResetEvent }

procedure TOmniLightEvent.SetEvent;
begin
  FLock.Enter;
  FIsSignalled := True;
  if FKernelUsers > 0 then
    FPulsar.SetEvent;
  FLock.Leave;
end; { TOmniLightEvent.SetEvent }

procedure TOmniLightEvent.Signal;
begin
  SetEvent;
end; { TOmniLightEvent.Signal }

function TOmniLightEvent.WaitFor(timeout: cardinal): TWaitResult;
var
  Timer: TStopWatch;
  TimeOutRemaining: cardinal;
  Elapsed: int64;
  SpinCount: integer;
  hasBumpedEntryCount: boolean;
  doPulse: boolean;
begin
  if (timeout <> INFINITE) and (timeout <> 0) then begin
    Timer.Reset;
    Timer.Start;
  end;
  TimeOutRemaining    := timeout;
  SpinCount           := 0;
  hasBumpedEntryCount := False;
  Result              := wrIOCompletion;
  doPulse             := False;
  repeat
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then begin
      Elapsed := Timer.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then
        Elapsed := MaxCardinal;
      if timeout > Elapsed then
          TimeOutRemaining := timeout - cardinal(Elapsed)
        else
          TimeOutRemaining := 0;
    end;

    FLock.Enter;

    if (TimeOutRemaining = 0) or FIsSignalled then begin
      if FIsSignalled then begin
        Result := wrSignaled;
        if not FManual then
          FIsSignalled := False;
      end
      else
        Result := wrTimeOut;
      if hasBumpedEntryCount then
        Dec(FKernelUsers);
      if FIsSignalled and (FKernelUsers > 0) then
        doPulse := True;
      FLock.Leave;
    end

    else if (FKernelUsers > 0) or (cardinal(SpinCount) >= FSpinMax) then begin   // Warning, XE7: widen
      if not hasBumpedEntryCount then begin
        hasBumpedEntryCount := True;
        Inc(FKernelUsers);
      end;
      FLock.Leave;
      FPulsar.WaitFor(TimeOutRemaining);
    end

    else begin
      FLock.Leave;
      Inc(SpinCount);
      TThread.Yield;
    end;
  until Result <> wrIOCompletion;
  if doPulse then
    FPulsar.SetEvent;
end; { TOmniLightEvent.WaitFor }

{ TOmniPlatformSemaphore }

constructor TOmniPlatformSemaphore.Create(AInitialCount: cardinal);
begin
  FSem := System.SyncObjs.TSemaphore.Create(nil, AInitialCount, MaxInt, '', False);
end; { TOmniPlatformSemaphore.Create }

destructor TOmniPlatformSemaphore.Destroy;
begin
  FSem.Free;
  inherited;
end; { TOmniPlatformSemaphore.Destroy }

function TOmniPlatformSemaphore.AsMWObject: TObject;
begin
  Result := FSem;
end; { TOmniPlatformSemaphore.AsMWObject }

{$IFDEF MSWINDOWS}
function TOmniPlatformSemaphore.GetHandle: THandle;
begin
  Result := FSem.Handle;
end; { TOmniPlatformSemaphore.GetHandle }
{$ENDIF}

function TOmniPlatformSemaphore.IsSignalled: boolean;
begin
  raise TOmniSynchroException.Create(EIsSignalledNotSupported);
end; { TOmniPlatformSemaphore.IsSignalled }

procedure TOmniPlatformSemaphore.Signal;
begin
  FSem.Release;
end; { TOmniPlatformSemaphore.Signal }

function TOmniPlatformSemaphore.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FSem.WaitFor(timeout);
end; { TOmniPlatformSemaphore.WaitFor }

{ TOmniFunctionalEvent }

constructor TOmniFunctionalEvent.Create(ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
begin
  if assigned(APLock) then
    FPLock := APLock
  else begin
    FLock.Initialize;
    FPLock := @FLock;
  end;
  FSignalTest        := ASignalTest;
  FPulsarIsSignalled := True;
  FPulsar            := TEvent.Create(nil, True, FPulsarIsSignalled, '', False);
end; { TOmniFunctionalEvent.Create }

class constructor TOmniFunctionalEvent.Create;
begin
  TStopwatch.Create;
end; { TOmniFunctionalEvent.Create }

destructor TOmniFunctionalEvent.Destroy;
begin
  if FPLock = @FLock then
    FLock.Finalize;
  FPulsar.Free;
  inherited;
end; { TOmniFunctionalEvent.Destroy }

function TOmniFunctionalEvent.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniFunctionalEvent.AsMWObject }

procedure TOmniFunctionalEvent.Reconfigure(
  ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
begin
  if not assigned(ASignalTest) then begin
    // Deconfigure in preparation for return to the object pool
    if FPLock = @FLock then
      FLock.Finalize;
    FPLock := nil;
  end
  else begin
    // Refurbishing for re-use.
    if assigned(APLock) then
      FPLock := APLock
    else begin
      FLock.Initialize;
      FPLock := @FLock;
    end
  end;
  FSignalTest := ASignalTest;
end; { TOmniFunctionalEvent.Reconfigure }

function TOmniFunctionalEvent.IsSignalled: boolean;
var
  LocalIsSig: boolean;
begin
  FPLock^.WithinLock(
    procedure
    var
      Dummy: boolean;
    begin
      SignalTest(False, Dummy, LocalIsSig);
    end);
  Result := LocalIsSig;
end; { TOmniFunctionalEvent.IsSignalled }

procedure TOmniFunctionalEvent.Signal;
begin
  Pulse;
end; { TOmniFunctionalEvent.Signal }

procedure TOmniFunctionalEvent.SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);
begin
  FSignalTest(doAcquire, wasSuccessfullyAcquired, isInSignalledState);
end; { TOmniFunctionalEvent.SignalTest }

procedure TOmniFunctionalEvent.Pulse;
begin
  FPLock^.WithinLock(
    procedure
    begin
      if not FPulsarIsSignalled then begin
        FPulsarIsSignalled := True;
        FPulsar.SetEvent
      end
    end);
end; { TOmniFunctionalEvent.Pulse }

function TOmniFunctionalEvent.WaitFor(timeout: cardinal): TWaitResult;
var
  Acquired        : boolean;
  Clipped         : boolean;
  Elapsed         : int64;
  TimeOutRemaining: cardinal;
  Timer           : TStopWatch;
begin
  if (timeout <> INFINITE) and (timeout <> 0) then begin
    Timer.Reset;
    Timer.Start
  end;
  TimeOutRemaining := timeout;
  repeat
    Clipped := False;
    if (TimeOutRemaining <> INFINITE) and (TimeOutRemaining <> 0) then begin
      Elapsed := Timer.ElapsedMilliseconds;
      if Elapsed > MaxCardinal then begin
        Elapsed := MaxCardinal;
        Clipped := True
      end;
      if timeout > Elapsed then
        TimeOutRemaining := timeout - cardinal(Elapsed)
      else
        TimeOutRemaining := 0;
    end;
    if TimeOutRemaining = 0 then
      Result := wrSignaled
    else
      Result := FPulsar.WaitFor(TimeOutRemaining);
    if (Result = wrTimeOut) and Clipped then
      Result := wrIOCompletion;
    if Result = wrSignaled then begin
      Acquired := False;
      FPLock^.WithinLock(
        procedure
        var
          NewValue: boolean;
        begin
          SignalTest(True, Acquired, NewValue);
          if NewValue <> FPulsarIsSignalled then begin
            FPulsarIsSignalled := NewValue;
            if FPulsarIsSignalled then
                FPulsar.SetEvent
              else
                FPulsar.ResetEvent
          end
        end);
      if Acquired then
        Result := wrSignaled
      else if TimeOutRemaining = 0 then
        Result := wrTimeOut
      else
        Result := wrIOCompletion
    end;
  until Result <> wrIOCompletion;
end; { TOmniFunctionalEvent.WaitFor }

{ TOmniCountDown.TCountDownFunction }

constructor TOmniCountDown.TCountDownFunction.Create(AOwner: TOmniCountDown);
begin
  FOwner := AOwner;
  inherited Create(nil, @FOwner.FLock);
end; { TOmniCountDown.TCountDownFunction.Create }

procedure TOmniCountDown.TCountDownFunction.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  isInSignalledState := FOwner.FValue.Read = 0;
  wasSuccessfullyAcquired := doAcquire and isInSignalledState;
end; { TOmniCountDown.TCountDownFunction.SignalTest }

{ TOmniCountDown }

constructor TOmniCountDown.Create(AInitial: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitial;
  FValue.Initialize(FInitialValue);
  FCountDownFunc := TCountDownFunction.Create(self);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse;
end; { TOmniCountDown.Create }

destructor TOmniCountDown.Destroy;
begin
  FCountDownFunc.Free;
  FValue.Finalize;
  inherited;
end; { TOmniCountDown.Destroy }

function TOmniCountDown.FullCount: cardinal;
begin
  Result := FInitialValue;
end; { TOmniCountDown.FullCount }

function TOmniCountDown.IsSignalled: boolean;
begin
  Result := FValue.Read = 0;
end; { TOmniCountDown.IsSignalled }

procedure TOmniCountDown.Reconfigure(AInitial: cardinal);
begin
  FInitialValue := AInitial;
  FValue.Write(FInitialValue);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse;
end; { TOmniCountDown.Reconfigure }

procedure TOmniCountDown.Reset;
begin
  FLock.WithinLock(
    procedure
    begin
      FValue.Write(FInitialValue)
    end);
  FCountDownFunc.Pulse;
end; { TOmniCountDown.Reset }

procedure TOmniCountDown.Signal;
begin
  Allocate;
end; { TOmniCountDown.Signal }

function TOmniCountDown.Allocate: cardinal;
var
  bonkers: boolean;
begin
  FLock.Enter;
  bonkers := FValue.Read = 0;
  if not bonkers then
    Result := FValue.Decrement
  else
    Result := 0;
  FLock.Leave;
  if bonkers then
    raise TOmniSynchroException.Create(ESignalCountUpDownRange)
  else
    FCountDownFunc.Pulse;
end; { TOmniCountDown.Allocate }

function TOmniCountDown.SignalHit: boolean;
begin
  Result := Allocate = 0;
end; { TOmniCountDown.SignalHit }

function TOmniCountDown.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniCountDown.AsMWObject }

procedure TOmniCountDown.CounterSignal;
var
  bonkers: boolean;
  val    : cardinal;
begin
  FLock.Enter;
  val := FValue.Read;
  bonkers := val = MaxCardinal;
  if not bonkers then
    val := FValue.Increment;
  FLock.Leave;
  if bonkers then
    raise TOmniSynchroException.Create(ESignalCountUpDownRange)
  else if val = 1 then
    FCountDownFunc.Pulse;
end; { TOmniCountDown.CounterSignal }

function TOmniCountDown.Value: cardinal;
begin
  Result := FValue.Read;
end; { TOmniCountDown.Value }

function TOmniCountDown.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FCountDownFunc.WaitFor(timeout);
end; { TOmniCountDown.WaitFor }

{ TOmniCountUp }

constructor TOmniCountUp.Create(AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  if FMaxValue > FInitialValue then
    FCountDown := TOmniCountDown.Create(FMaxValue - FInitialValue)
  else begin
    FCountDown := nil;
    raise TOmniSynchroException.Create(ESignalCountUpDownRange)
  end;
end; { TOmniCountUp.Create }

destructor TOmniCountUp.Destroy;
begin
  FCountDown.Free;
  inherited;
end; { TOmniCountUp.Destroy }

function TOmniCountUp.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniCountUp.AsMWObject }

function TOmniCountUp.InitialValue: cardinal;
begin
  Result := FInitialValue;
end; { TOmniCountUp.InitialValue }

function TOmniCountUp.IsSignalled: boolean;
begin
  Result := FCountDown.IsSignalled;
end; { TOmniCountUp.IsSignalled }

function TOmniCountUp.MaxValue: cardinal;
begin
  Result := FMaxValue;
end; { TOmniCountUp.MaxValue }

procedure TOmniCountUp.Reconfigure(AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  Reset;
end; { TOmniCountUp.Reconfigure }

procedure TOmniCountUp.Reset;
begin
  FCountDown.Reset;
end; { TOmniCountUp.Reset }

procedure TOmniCountUp.Signal;
begin
  FCountDown.Signal;
end; { TOmniCountUp.Signal }

function TOmniCountUp.SignalHit: boolean;
begin
  Result := FCountDown.SignalHit;
end; { TOmniCountUp.SignalHit }

function TOmniCountUp.Value: cardinal;
begin
  Result := FMaxValue - FCountDown.Value;
end; { TOmniCountUp.Value }

function TOmniCountUp.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FCountDown.WaitFor(timeout);
end; { TOmniCountUp.WaitFor }

{ TOmniPlatformMREW }

constructor TOmniPlatformMREW.Create;
begin
  FReaders.Initialize(0);
  FZeroOrAbove  := System.SyncObjs.TEvent.Create(nil, False, False, '');
  FZero         := System.SyncObjs.TEvent.Create(nil, False, False, '');
  FWriterThread := 0;
  // This is an auto-reset event.
  FGate := TFixedCriticalSection.Create;
end; { TOmniPlatformMREW.Create }

destructor TOmniPlatformMREW.Destroy;
begin
  FReaders.Finalize;
  FZeroOrAbove.Free;
  FZero.Free;
  FGate.Free;
  inherited;
end; { TOmniPlatformMREW.Destroy }

function TOmniPlatformMREW.EnterRead(timeout: cardinal): TWaitResult;
var
  elapsed         : int64;
  isStable        : boolean;
  readers         : integer;
  timeoutRemaining: cardinal;
  watch           : TStopWatch;
begin
  readers := FReaders.Read;
  if (readers >= 0)
     and (FReaders.CompareAndExchange(readers, readers + 1))
  then
    Result := wrSignaled
    // It is possible that FZero will now be in a false positive state.
    // If this is the case, it will be corrected on the next EnterWrite()
  else
    Result := wrIOCompletion;
  if Result <> wrIOCompletion then
    Exit;
  timeoutRemaining := timeout;
  if (timeoutRemaining <> INFINITE) and (timeoutRemaining <> 0) then begin
    watch.Reset;
    watch.Start
  end;
  repeat
    if (timeoutRemaining <> INFINITE) and (timeoutRemaining <> 0) then begin
      elapsed := watch.ElapsedMilliseconds;
      if elapsed > MaxCardinal then
        elapsed := MaxCardinal;
      if timeout > elapsed then
        timeoutRemaining := timeout - cardinal(elapsed)
      else
        timeoutRemaining := 0;
    end;
    if (timeoutRemaining > 0) and (FReaders.Read < 0) then
      FZeroOrAbove.WaitFor(timeoutRemaining);
    FGate.Enter;
    isStable := False;
    repeat
      readers := FReaders.Read;
      if readers >= 0 then begin
        if FReaders.CompareAndExchange(readers, readers + 1) then begin
          isStable := True;
          Inc(readers);
        end
      end
      else
        isStable := True;
    until isStable;
    if readers >= 1 then begin
      Result := wrSignaled;
      FZeroOrAbove.SetEvent
    end
    else if readers < 0 then
      FZeroOrAbove.ResetEvent;
    FGate.Leave;
    if (Result = wrIOCompletion) and (timeoutRemaining = 0) then
      Result := wrTimeout;
  until Result <> wrIOCompletion;
end; { TOmniPlatformMREW.EnterRead }

procedure TOmniPlatformMREW.ExitRead;
var
  readers: integer;
begin
  readers := FReaders.Decrement;
  if readers >= 0 then
    FZeroOrAbove.SetEvent;
  if readers = 0 then
    FZero.SetEvent;
end; { TOmniPlatformMREW.ExitRead }

function TOmniPlatformMREW.EnterWrite(timeout: cardinal): TWaitResult;
var
  doWait          : boolean;
  elapsed         : int64;
  hit             : boolean;
  isStable        : boolean;
  readers         : integer;
  this            : TThreadId;
  timeoutRemaining: cardinal;
  watch           : TStopWatch;
begin
  this := TThread.CurrentThread.ThreadID;
  FGate.Enter;
  readers := FReaders.Read;
  if (readers < 0) and (this = FWriterThread)
     and (FReaders.CompareAndExchange(readers, readers - 1))
  then
    Result := wrSignaled
  else
    Result := wrIOCompletion;
  FGate.Leave;
  if Result <> wrIOCompletion then
    Exit;
  timeoutRemaining := timeout;
  watch.Reset;
  watch.Start;
  repeat
    if (timeoutRemaining <> INFINITE) and (timeoutRemaining <> 0) then begin
      elapsed := watch.ElapsedMilliseconds;
      if elapsed > MaxCardinal then
        elapsed := MaxCardinal;
      if timeout > elapsed then
        timeoutRemaining := timeout - cardinal(elapsed)
      else
        timeoutRemaining := 0;
    end;
    FGate.Enter;
    readers := FReaders.Read;
    doWait := (timeoutRemaining > 0)
              and (readers <> 0)
              and ((readers >= 0) or (FWriterThread <> this));
    FGate.Leave;
    if doWait then
      FZero.WaitFor(timeoutRemaining);
    FGate.Enter;
    hit := False;
    isStable := False;
    repeat
      readers := FReaders.Read;
      if (readers = 0) or ((readers < 0) and (FWriterThread = this)) then begin
        if FReaders.CompareAndExchange(readers, readers - 1) then begin
          hit := True;
          isStable := True;
          Dec(readers);
          if readers = -1 then
            FWriterThread := this;
        end
      end
      else
        isStable := True;
    until isStable;
    if hit then
      Result := wrSignaled;
    if readers = 0 then
      FZero.SetEvent
    else
      FZero.ResetEvent;
    FGate.Leave;
    if (Result = wrIOCompletion) and (timeoutRemaining = 0) then
      Result := wrTimeout;
  until Result <> wrIOCompletion;
end; { TOmniPlatformMREW.EnterWrite }

procedure TOmniPlatformMREW.ExitWrite;
var
  readers: integer;
begin
  FGate.Enter;
  readers := FReaders.Increment;
  if readers >= 0 then
    FZeroOrAbove.SetEvent;
  if readers = 0 then begin
    FWriterThread := 0;
    FZero.SetEvent;
  end;
  FGate.Leave;
end; { TOmniPlatformMREW.ExitWrite }

function TOmniPlatformMREW.ReaderCount: integer;
begin
  Result := FReaders.Read;
  if Result < -1 then
    Result := -1;
end; { TOmniPlatformMREW.ReaderCount }

{ initialization }

procedure InitUnit_BasicLevel;
begin
  TStopWatch.Create;
end; { InitUnit_BasicLevel }

procedure DoneUnit_BasicLevel;
begin
  // do nothing
end; { DoneUnit_BasicLevel }

initialization
  InitUnit_BasicLevel;
finalization
  DoneUnit_BasicLevel;
end.
