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
  TOtlSynchro = class abstract
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
  end; { TOtlSynchro }

  TOtlEvent = class abstract(TOtlSynchro)
  public
    procedure SetEvent; virtual; abstract;
    procedure ResetEvent; virtual; abstract;
  end; { TOtlEvent }

  TKernelEvent = class(TOtlEvent)
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
    /// <remarks>TOtlEvent.IsSignalled() is not supported .</remarks>
    function  IsSignalled: boolean; override;
    procedure ResetEvent; override;
    procedure SetEvent; override;
    procedure Signal; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TKernelEvent }

  TLightEvent = class(TOtlEvent)
  strict private
    FIsSignalled    : boolean;
    FKernelUsers    : cardinal;
    [Volatile] FLock: TSBDSpinLock;
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
  end; { TLightEvent }

  TOtlSemaphore = class(TOtlSynchro)
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
    /// <remarks>TOtlSemaphore.IsSignalled() is not supported .</remarks>
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOtlSemaphore }

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

  TFunctionalEvent = class(TOtlSynchro)
  strict protected
    [Volatile] FLock  : TSBDSpinLock;
    FPLock            : PSBDSpinLock;
    FPulsar           : TEvent; // Manual reset
    FPulsarIsSignalled: boolean;
    FSignalTest       : TEventFunction;
  protected
    procedure Reconfigure(ASignalTest: TEventFunction; APLock: PSBDSpinLock); virtual;
    procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean); virtual;
  public
    class constructor Create;
    constructor Create(ASignalTest: TEventFunction; APLock: PSBDSpinLock);
    destructor Destroy; override;
    function  AsMWObject: TObject;  override;
    function  IsSignalled: boolean; override;
    procedure Pulse;
    procedure Signal; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TFunctionalEvent }

  TCountDown = class(TOtlSynchro)
  strict private type
    TCountDownFunction = class(TFunctionalEvent)
    strict private
      FOwner: TCountDown;
    protected
      procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean); override;
    public
      constructor Create(AOwner: TCountDown);
    end; { TCountDownFunction }
  var
    FCountDownFunc   : TCountDownFunction;
    [Volatile] FValue: TVolatileUInt32;
    FInitialValue    : cardinal;
    FLock            : TSBDSpinLock;
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
  end; { TCountDown }

  TCountUp = class(TOtlSynchro)
  strict private
    FCountDown   : TCountDown;
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
  end; { TCountUp }

  TOtlMREW = class // Multi-read, exclusive write
  strict private
    FGate              : TCriticalSection;
    [Volatile] FReaders: TVolatileInt32;
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
  end; { TOtlMREW }

implementation

uses
  System.Diagnostics;

const
  MaxCardinal      : cardinal = Cardinal($FFFFFFFF);
  MaxFiniteCardinal: cardinal = Cardinal($FFFFFFFE);

{ TOtlSynchro }

{$IFDEF MSWINDOWS}
function TOtlSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TOtlSynchro.GetHandle }
{$ENDIF}

procedure TOtlSynchro.Signal;
begin
  // do nothing
end; { TOtlSynchro.Signal }

{ TKernelEvent }

function TKernelEvent.AsMWObject: TObject;
begin
  Result := FEvent;
end; { TKernelEvent.AsMWObject }

constructor TKernelEvent.Create(AManual, AInitialState: boolean);
begin
  FEvent := System.SyncObjs.TEvent.Create( nil, AManual, AInitialState, '', False);
end; { TKernelEvent.Create }

destructor TKernelEvent.Destroy;
begin
  FEvent.Free;
  inherited;
end; { TKernelEvent.Destroy }

{$IFDEF MSWINDOWS}
function TKernelEvent.GetHandle: THandle;
begin
  Result := FEvent.Handle;
end; { TKernelEvent.GetHandle }
{$ENDIF}

function TKernelEvent.IsSignalled: boolean;
begin
  raise TSynchroException.Create( EIsSignalledNotSupported);
end; { TKernelEvent.IsSignalled }

procedure TKernelEvent.ResetEvent;
begin
  FEvent.ResetEvent;
end; { TKernelEvent.ResetEvent }

procedure TKernelEvent.SetEvent;
begin
  FEvent.SetEvent;
end; { TKernelEvent.SetEvent}

procedure TKernelEvent.Signal;
begin
  FEvent.SetEvent;
end; { TKernelEvent.Signal }

function TKernelEvent.WaitFor(timeout: cardinal): TWaitResult;
begin;
  Result := FEvent.WaitFor(timeout);
end; { TKernelEvent.WaitFor }

{ TLightEvent }

class constructor TLightEvent.Create;
begin
  TStopwatch.Create;
end; { TLightEvent.Create }

function TLightEvent.AsMWObject: TObject;
begin
  Result := nil;
end; { TLightEvent.AsMWObject }

constructor TLightEvent.Create(
  AManual, AInitialState: boolean; ASpinMax: cardinal);
begin
  FLock.Initialize;
  FIsSignalled := AInitialState;
  FPulsar      := System.SyncObjs.TEvent.Create(nil, False, FIsSignalled, '', False);
  FKernelUsers := 0;
  FManual      := AManual;
  FSpinMax     := ASpinMax;
end; { TLightEvent.Create }

destructor TLightEvent.Destroy;
begin
  FLock.Finalize;
  FPulsar.Free;
  inherited;
end; { TLightEvent.Destroy }

procedure TLightEvent.Reconfigure(Manual: boolean; Value: cardinal);
begin
  FSpinMax := Value;
  FManual  := Manual;
end; { TLightEvent.Reconfigure }

function TLightEvent.IsSignalled: boolean;
begin
  FLock.Enter;
  Result := FIsSignalled;
  FLock.Leave;
end; { TLightEvent.IsSignalled }

procedure TLightEvent.ResetEvent;
begin
  FLock.Enter;
  FIsSignalled := False;
  FLock.Leave;
end; { TLightEvent.ResetEvent }

procedure TLightEvent.SetEvent;
begin
  FLock.Enter;
  FIsSignalled := True;
  if FKernelUsers > 0 then
    FPulsar.SetEvent;
  FLock.Leave;
end; { TLightEvent.SetEvent }

procedure TLightEvent.Signal;
begin
  SetEvent;
end; { TLightEvent.Signal }

function TLightEvent.WaitFor(timeout: cardinal): TWaitResult;
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
end; { TLightEvent.WaitFor }

{ TOtlSemaphore }

constructor TOtlSemaphore.Create(AInitialCount: cardinal);
begin
  FSem := System.SyncObjs.TSemaphore.Create(nil, AInitialCount, MaxInt, '', False);
end; { TOtlSemaphore.Create }

destructor TOtlSemaphore.Destroy;
begin
  FSem.Free;
  inherited;
end; { TOtlSemaphore.Destroy }

function TOtlSemaphore.AsMWObject: TObject;
begin
  Result := FSem;
end; { TOtlSemaphore.AsMWObject }

{$IFDEF MSWINDOWS}
function TOtlSemaphore.GetHandle: THandle;
begin
  Result := FSem.Handle;
end; { TOtlSemaphore.GetHandle }
{$ENDIF}

function TOtlSemaphore.IsSignalled: boolean;
begin
  raise TSynchroException.Create(EIsSignalledNotSupported);
end; { TOtlSemaphore.IsSignalled }

procedure TOtlSemaphore.Signal;
begin
  FSem.Release;
end; { TOtlSemaphore.Signal }

function TOtlSemaphore.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FSem.WaitFor(timeout);
end; { TOtlSemaphore.WaitFor }

{ TFunctionalEvent }

constructor TFunctionalEvent.Create(ASignalTest: TEventFunction; APLock: PSBDSpinLock);
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
end; { TFunctionalEvent.Create }

class constructor TFunctionalEvent.Create;
begin
  TStopwatch.Create;
end; { TFunctionalEvent.Create }

destructor TFunctionalEvent.Destroy;
begin
  if FPLock = @FLock then
    FLock.Finalize;
  FPulsar.Free;
  inherited;
end; { TFunctionalEvent.Destroy }

function TFunctionalEvent.AsMWObject: TObject;
begin
  Result := nil;
end; { TFunctionalEvent.AsMWObject }

procedure TFunctionalEvent.Reconfigure(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock);
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
end; { TFunctionalEvent.Reconfigure }

function TFunctionalEvent.IsSignalled: boolean;
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
end; { TFunctionalEvent.IsSignalled }

procedure TFunctionalEvent.Signal;
begin
  Pulse;
end; { TFunctionalEvent.Signal }

procedure TFunctionalEvent.SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);
begin
  FSignalTest(doAcquire, wasSuccessfullyAcquired, isInSignalledState);
end; { TFunctionalEvent.SignalTest }

procedure TFunctionalEvent.Pulse;
begin
  FPLock^.WithinLock(
    procedure
    begin
      if not FPulsarIsSignalled then begin
        FPulsarIsSignalled := True;
        FPulsar.SetEvent
      end
    end);
end; { TFunctionalEvent.Pulse }

function TFunctionalEvent.WaitFor(timeout: cardinal): TWaitResult;
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
end; { TFunctionalEvent.WaitFor }

{ TCountDown.TCountDownFunction }

constructor TCountDown.TCountDownFunction.Create(AOwner: TCountDown);
begin
  FOwner := AOwner;
  inherited Create(nil, @FOwner.FLock);
end; { TCountDown.TCountDownFunction.Create }

procedure TCountDown.TCountDownFunction.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  isInSignalledState := FOwner.FValue.Read = 0;
  wasSuccessfullyAcquired := doAcquire and isInSignalledState;
end; { TCountDown.TCountDownFunction.SignalTest }

{ TCountDown }

constructor TCountDown.Create(AInitial: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitial;
  FValue.Initialize(FInitialValue);
  FCountDownFunc := TCountDownFunction.Create(self);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse;
end; { TCountDown.Create }

destructor TCountDown.Destroy;
begin
  FCountDownFunc.Free;
  FValue.Finalize;
  inherited;
end; { TCountDown.Destroy }

function TCountDown.FullCount: cardinal;
begin
  Result := FInitialValue;
end; { TCountDown.FullCount }

function TCountDown.IsSignalled: boolean;
begin
  Result := FValue.Read = 0;
end; { TCountDown.IsSignalled }

procedure TCountDown.Reconfigure(AInitial: cardinal);
begin
  FInitialValue := AInitial;
  FValue.Write(FInitialValue);
  if FInitialValue = 0 then
    FCountDownFunc.Pulse;
end; { TCountDown.Reconfigure }

procedure TCountDown.Reset;
begin
  FLock.WithinLock(
    procedure
    begin
      FValue.Write(FInitialValue)
    end);
  FCountDownFunc.Pulse;
end; { TCountDown.Reset }

procedure TCountDown.Signal;
begin
  Allocate;
end; { TCountDown.Signal }

function TCountDown.Allocate: cardinal;
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
    raise TSynchroException.Create(ESignalCountUpDownRange)
  else
    FCountDownFunc.Pulse;
end; { TCountDown.Allocate }

function TCountDown.SignalHit: boolean;
begin
  Result := Allocate = 0;
end; { TCountDown.SignalHit }

function TCountDown.AsMWObject: TObject;
begin
  Result := nil;
end; { TCountDown.AsMWObject }

procedure TCountDown.CounterSignal;
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
    raise TSynchroException.Create(ESignalCountUpDownRange)
  else if val = 1 then
    FCountDownFunc.Pulse;
end; { TCountDown.CounterSignal }

function TCountDown.Value: cardinal;
begin
  Result := FValue.Read;
end; { TCountDown.Value }

function TCountDown.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FCountDownFunc.WaitFor(timeout);
end; { TCountDown.WaitFor }

{ TCountUp }

constructor TCountUp.Create(AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  if FMaxValue > FInitialValue then
    FCountDown := TCountDown.Create(FMaxValue - FInitialValue)
  else begin
    FCountDown := nil;
    raise TSynchroException.Create(ESignalCountUpDownRange)
  end;
end; { TCountUp.Create }

destructor TCountUp.Destroy;
begin
  FCountDown.Free;
  inherited;
end; { TCountUp.Destroy }

function TCountUp.AsMWObject: TObject;
begin
  Result := nil;
end; { TCountUp.AsMWObject }

function TCountUp.InitialValue: cardinal;
begin
  Result := FInitialValue;
end; { TCountUp.InitialValue }

function TCountUp.IsSignalled: boolean;
begin
  Result := FCountDown.IsSignalled;
end; { TCountUp.IsSignalled }

function TCountUp.MaxValue: cardinal;
begin
  Result := FMaxValue;
end; { TCountUp.MaxValue }

procedure TCountUp.Reconfigure(AInitial, AMaxValue: cardinal);
begin
  FInitialValue := AInitial;
  FMaxValue     := AMaxValue;
  Reset;
end; { TCountUp.Reconfigure }

procedure TCountUp.Reset;
begin
  FCountDown.Reset;
end; { TCountUp.Reset }

procedure TCountUp.Signal;
begin
  FCountDown.Signal;
end; { TCountUp.Signal }

function TCountUp.SignalHit: boolean;
begin
  Result := FCountDown.SignalHit;
end; { TCountUp.SignalHit }

function TCountUp.Value: cardinal;
begin
  Result := FMaxValue - FCountDown.Value;
end; { TCountUp.Value }

function TCountUp.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FCountDown.WaitFor(timeout);
end; { TCountUp.WaitFor }

{ TOtlMREW }

constructor TOtlMREW.Create;
begin
  FReaders.Initialize(0);
  FZeroOrAbove  := System.SyncObjs.TEvent.Create(nil, False, False, '');
  FZero         := System.SyncObjs.TEvent.Create(nil, False, False, '');
  FWriterThread := 0;
  // This is an auto-reset event.
  FGate := TFixedCriticalSection.Create;
end; { TOtlMREW.Create }

destructor TOtlMREW.Destroy;
begin
  FReaders.Finalize;
  FZeroOrAbove.Free;
  FZero.Free;
  FGate.Free;
  inherited;
end; { TOtlMREW.Destroy }

function TOtlMREW.EnterRead(timeout: cardinal): TWaitResult;
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
end; { TOtlMREW.EnterRead }

procedure TOtlMREW.ExitRead;
var
  readers: integer;
begin
  readers := FReaders.Decrement;
  if readers >= 0 then
    FZeroOrAbove.SetEvent;
  if readers = 0 then
    FZero.SetEvent;
end; { TOtlMREW.ExitRead }

function TOtlMREW.EnterWrite(timeout: cardinal): TWaitResult;
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
end; { TOtlMREW.EnterWrite }

procedure TOtlMREW.ExitWrite;
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
end; { TOtlMREW.ExitWrite }

function TOtlMREW.ReaderCount: integer;
begin
  Result := FReaders.Read;
  if Result < -1 then
    Result := -1;
end; { TOtlMREW.ReaderCount }

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
