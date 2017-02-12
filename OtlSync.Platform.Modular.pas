///<summary>Modular platform independant synchronization primitives.
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
///       - Imported from mobile/Otl.Parallel.SynchroPrimitives.ModularLevel.pas.

unit OtlSync.Platform.Modular;

// IMPORTANT!
//  READ THE COMMENTS IN UNIT OtlSync.Platform.

{$I OtlOptions.inc}

interface

uses
 {$IFDEF MSWINDOWS}
   Winapi.Windows,
 {$ENDIF}
 System.SyncObjs, System.Classes, System.SysUtils, System.Generics.Collections,
 OtlSync.Platform.Atomic,
 OtlSync.Platform.ConditionVariables,
 OtlSync.Platform.Interfaced;

type
  TOmniConditionVariable = OtlSync.Platform.ConditionVariables.TOmniConditionVariable;

  IOmniSimpleConditionEvent = interface
  ['{1EAEEE8F-54E1-44B5-BE3C-AFC14A465F74}']
    procedure Pulse;
    function  WaitFor: TWaitResult;
  end; { IOmniSimpleConditionEvent }

  IOmniCompositeSynchro = interface
  ['{3796D8C1-AA28-44A7-836E-F1045AFBF616}']
    function WaitFor(var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;
    function Factors: TOmniSynchroArray;
    function Datum: TObject;
    function IsSignalled: boolean;
  end; { IOmniCompositeSynchro }

  IOmniSynchroExInternal = interface;

  IOmniSynchroObserver = interface
  ['{C1EBE331-613F-4D0F-B645-CF2925F55B22}']
    procedure PossibleStateChange(const Source: IOmniSynchroExInternal; Token: TObject);
  end; { IOmniSynchroObserver }

  IOmniSynchroExInternal = interface(IOmniSynchro)
  ['{A3D01DFF-CF6A-4913-8A34-E46C4C0FCF5F}']
    function  ConsumeResource: TWaitResult;
    procedure Enrol(const Observer: IOmniSynchroObserver; Token: TObject);
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsResourceCounting: boolean;
    function  LockingMechanism: TOmniLockingMechanism;
    function  NativeMultiwait(const Synchros: array of TObject;
                              Timeout: cardinal; AAll: boolean;
                              var SignallerIndex: integer): TWaitResult;
    function  NativeMultiwaitObject: TObject;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure Unenrol(ObserverToken: TObject);
    function  UnionLock: IOmniLock;
  end; { IOmniSynchroExInternal }

  TOmniWaitPropagation = (NoConsume, ConsumeOneSignalled, ConsumeAllSignalled);

  TOmniConditionTest = reference to function(var SignallerIdx: integer; const Peers:
    TOmniSynchroArray; Datum: TObject): boolean;

  TOmniTestClass = (TestAny, TestAll, TestCustom);

  TOmniSignalState = OtlSync.Platform.Interfaced.TOmniSignalState;

  TOmniCompositeSynchro = class(TInterfacedObject, IOmniCompositeSynchro, IOmniSynchro,
    IOmniSynchroExInternal)
  strict private type
    TImplementation = ({$IFDEF MSWINDOWS}Direct, {$ENDIF} Indirect, Solo);
    TMemberObserver = class(TInterfacedObject, IOmniSynchroObserver)
    private
      {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
      [Volatile] FOwner: TOmniCompositeSynchro;
      procedure PossibleStateChange(const Source: IOmniSynchroExInternal; Token: TObject);
    public
      constructor Create(AOwner: TOmniCompositeSynchro);
    end; { TMemberObserver }
  var
    FAllowSolo                   : boolean;
    FCondVar                     : TOmniConditionVariable;
    FDatum                       : TObject;
    FEventFactory                : TOmniSynchroFactory;
    FFactors                     : TOmniSynchroArray;
    FHasAtLeastOneResourceCounter: boolean;
    FImplementation              : TImplementation;
    [Volatile] FIsSignalled      : boolean;
    FLock                        : IOmniLock;
    FMemberCount                 : integer;
    FMemberObserver              : TMemberObserver;
    FObservers                   : TDictionary<TObject,IOmniSynchroObserver>;
    FPropagation                 : TOmniWaitPropagation;
    FRegistered                  : boolean;
    FTest                        : TOmniConditionTest;
    FTestClass                   : TOmniTestClass;
    {$IFDEF MSWINDOWS}
    FSynchroObjs                 : array of TObject;   // Actually array of THandle.
    {$ENDIF}
  protected
    {$IFDEF MSWINDOWS}
    function GetHandle: THandle;
    {$ENDIF}
  public
    constructor Create(const AFactors: TOmniSynchroArray; AEventFactory: TOmniSynchroFactory;
                       APropagation: TOmniWaitPropagation; ATest: TOmniConditionTest;
                       ATestClass: TOmniTestClass; AAllowSolo: boolean);
    destructor  Destroy; override;
    class function IsAll(var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean;
    class function IsAny(var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean;
    class function AnyTest: TOmniConditionTest;
    class function AllTest: TOmniConditionTest;
    function  AsMWObject: TObject;
    function  AsObject: TObject;
    function  ConsumeResource: TWaitResult;
    function  Datum: TObject;
    function  Factors: TOmniSynchroArray;
    function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsResourceCounting: boolean;
    function  IsSignalled: boolean;
    function  LockingMechanism: TOmniLockingMechanism;
    function  NativeMultiwait(const Synchros: array of TObject;
                              Timeout: cardinal; AAll: boolean;
                              var SignallerIndex: integer): TWaitResult;
    function  NativeMultiwaitObject: TObject;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  SignalState: TOmniSignalState;
    function  UnionLock: IOmniLock;
    function  WaitFor(Timeout: cardinal = INFINITE): TWaitResult; overload;
    function  WaitFor(var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult; overload;
    procedure BubbleUp( var Me: IOmniSynchroExInternal);
    procedure Enrol( const Observer: IOmniSynchroObserver; Token: TObject);
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure Signal;
    procedure Unenrol(ObserverToken: TObject);
  end; { TOmniCompositeSynchro }

  TOmniModularSynchro = class abstract(TInterfacedObject, IOmniSynchro,
    IOmniSynchroExInternal)
  private
    FDedicatedToOneIndirectWaiter: boolean;
    [Volatile] FDirectWaiters    : TOmniVolatileInt32;
    [Volatile] FIndirectWaiters  : TOmniVolatileInt32;
    FObservers                   : TDictionary<TObject,IOmniSynchroObserver>;
  protected
    FUnionLock: IOmniLock;
    FBase  : IOmniSynchro;
    FBaseEx: IOmniSynchroExInternal;
    FEventFactory: TOmniSynchroFactory;
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}
  public
    constructor Create( const ABase: IOmniSynchro; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
    destructor  Destroy; override;
    function  AsMWObject: TObject;
    function  AsObject: TObject;
    function  ConsumeResource: TWaitResult; virtual;
    function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsModular: boolean;
    function  IsPoolManaged: boolean;
    function  IsResourceCounting: boolean; virtual;
    function  IsSignalled: boolean;
    function  LockingMechanism: TOmniLockingMechanism;
    function  NativeMultiwait(const Synchros: array of TObject;
                              Timeout: cardinal; AAll: boolean;
                              var SignallerIndex: integer): TWaitResult;
    function  NativeMultiwaitObject: TObject;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  SignalState: TOmniSignalState;
    function  UnionLock: IOmniLock;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; virtual;
    procedure Enrol(const Observer: IOmniSynchroObserver; Token: TObject);
    procedure ObserverProc(Proc: TProc);
    procedure RegisterDedicatedSoleIndirectClient(Delta: integer);
    procedure RegisterDirectClient(Delta: integer);
    procedure RegisterIndirectClient(Delta: integer);
    procedure Signal; virtual;
    procedure Unenrol(ObserverToken: TObject);
  end; { TOmniModularSynchro }

  TOmniModularEvent = class(TOmniModularSynchro, IOmniEvent)
  protected
    FBaseEvent: IOmniEvent;
  public
    constructor Create(const ABase: IOmniEvent; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
    function  ConsumeResource: TWaitResult; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  IsResourceCounting: boolean; override;
    procedure ResetEvent;
    procedure SetEvent;
    procedure Signal; override;
  end; { TOmniModularEvent }

  TOmniModularSemaphore = class(TOmniModularSynchro, IOmniSemaphore)
  protected
    FBaseSem: IOmniSemaphore;
  public
    constructor Create(const ABase: IOmniSemaphore; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
    function  ConsumeResource: TWaitResult; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsResourceCounting: boolean; override;
    procedure Reset;
    procedure Signal; override;
    function  Value: cardinal;
  end; { TOmniModularSemaphore }

  TOmniModularCountDown = class(TOmniModularSynchro, IOmniCountDown)
  protected
    FBaseCountDown: IOmniCountDown;
  public
    constructor Create( const ABase: IOmniCountDown; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
    function  Allocate: cardinal;
    function  ConsumeResource: TWaitResult; override;
    procedure CounterSignal;
    function  InitialValue: cardinal;
    function  IsResourceCounting: boolean; override;
    function  SignalHit: boolean;
    procedure Signal; override;
    function  Value: cardinal;
  end; { TOmniModularCountDown }

implementation

uses
  System.Diagnostics,
  OtlSync.Platform;

{ TOmniCompositeSynchro.TMemberObserver }

constructor TOmniCompositeSynchro.TMemberObserver.Create(AOwner: TOmniCompositeSynchro);
begin
  FOwner := AOwner;
end; { TOmniCompositeSynchro.TMemberObserver.Create }

procedure TOmniCompositeSynchro.TMemberObserver.PossibleStateChange(
  const Source: IOmniSynchroExInternal; Token: TObject);
begin
  // Index of signaller is integer(Token)
  FOwner.Signal;
end; { TOmniCompositeSynchro.TMemberObserver.PossibleStateChange }

{ TOmniCompositeSynchro }

constructor TOmniCompositeSynchro.Create(
  const AFactors: TOmniSynchroArray; AEventFactory: TOmniSynchroFactory;
  APropagation: TOmniWaitPropagation; ATest: TOmniConditionTest;
  ATestClass: TOmniTestClass; AAllowSolo: boolean);
var
  dummy: integer;
  i    : integer;
  memb : IOmniSynchroExInternal;
  mwObj: TObject;
  ok   : boolean;
begin
  FRegistered := False;
  // Test for the direct solution
  FMemberCount := Length(AFactors);
  if FMemberCount = 0 then
    raise TOmniSynchroException.Create(ECompositeNeedsOneFactor);
  FPropagation := APropagation;
  FDatum       := Datum;
  FTest        := ATest;
  FTestClass   := ATestClass;
  FObservers := TDictionary<TObject,IOmniSynchroObserver>.Create;
  SetLength(FFactors, FMemberCount);
  FEventFactory := AEventFactory;
  FAllowSolo := AAllowSolo;

  case FTestClass of
    TestAny   : FTest := IsAny;
    TestAll   : FTest := IsAll;
  end;

  for i := 1 to FMemberCount - 1 do begin
    FFactors[ i] := AFactors[i];
    if not (scModular in FFactors[ i].Capabilities) then
      raise TOmniSynchroException.Create(EOnlyModularCombinable);
  end;

  if Supports(FFactors[0], IOmniSynchroExInternal, memb) then begin
    mwObj := memb.NativeMultiwaitObject;
    FLock := memb.UnionLock
  end
  else begin
    mwObj := nil;
    FLock := nil
  end;
  ok := True;
  for i := 1 to FMemberCount - 1 do begin
    if Supports(FFactors[i], IOmniSynchroExInternal, memb) then begin
      ok := ok and assigned(FLock) and (FLock = memb.UnionLock);
      if assigned(mwObj) and memb.IsCompatibleNativeMWObject(mwObj, memb.NativeMultiwaitObject) then
        mwObj := nil
    end
  end;
  if (not ok) or (not assigned(FLock.AsCriticalSection)) then
    FLock := nil;
  if assigned(FLock)
    {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
      and assigned(FEventFactory)
    {$ENDIF} {$ENDIF}  then
      FCondVar := TOmniConditionVariable.Create(
          {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
            AEventFactory, True,
          {$ENDIF} {$ENDIF}
            FLock)
    else
      FCondVar := nil;

  ok := FMemberCount = 1;
  if ok then
    FImplementation := Solo
  else begin
    {$IFDEF MSWINDOWS}
    if assigned(mwObj) then
      for i := 0 to FMemberCount - 1 do begin
        ok := Supports(FFactors[i], IOmniSynchroExInternal, memb)
              and memb.PermitsDirectClients
              and (memb.NativeMultiwaitObject <> nil)
              and (((FPropagation = ConsumeOneSignalled) and (FTestClass = TestAny))
                  or ((FPropagation = ConsumeAllSignalled) and (FTestClass = TestAll)));
        if not ok then
          break
      end;
    if ok then begin
      FImplementation := Direct;
      FreeAndNil(FCondVar)
    end;
    {$ENDIF}
  end;

  FHasAtLeastOneResourceCounter := False;
  if (not ok) and assigned(FCondVar) then begin
    for i := 0 to FMemberCount - 1 do begin
      FHasAtLeastOneResourceCounter := Supports(FFactors[i], IOmniSynchroExInternal, memb) and
                                       memb.IsResourceCounting;
      if FHasAtLeastOneResourceCounter then
        break
    end;
    for i := 0 to FMemberCount - 1 do begin
      ok := Supports(FFactors[i], IOmniSynchroExInternal, memb) and
            memb.PermitsIndirectClients;
      if ok and (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
        ok := memb.PermitsDedicatedSoleIndirectClient;
      if not ok then
        break
    end;
    FImplementation := Indirect;
  end;

  if ok and (FImplementation = Solo) and (not AAllowSolo) then
    ok := False;

  if not ok then
    raise TOmniSynchroException.Create(ECompositeSynchroMixedBag);

  case FTestClass of
    TestAny   : FIsSignalled := IsAny(dummy, FFactors, nil);
    TestAll   : FIsSignalled := IsAll(dummy, FFactors, nil);
    TestCustom: FIsSignalled := FTest(dummy, FFactors, FDatum);
  end;

  case FImplementation of
    {$IFDEF MSWINDOWS}
    Direct:
      begin
        SetLength(FSynchroObjs, FMemberCount);
        for i := 0 to FMemberCount - 1 do begin
          if Supports(FFactors[i], IOmniSynchroExInternal, memb) then
            FSynchroObjs[i] := memb.NativeMultiwaitObject;
          memb.RegisterDirectClient(+1);
        end;
        if FTestClass <> TestCustom then
          FTest := nil;
        FRegistered := True;
      end;
    {$ENDIF}

    Indirect, Solo:
      begin
        FMemberObserver := TMemberObserver.Create(self);
        FMemberObserver._AddRef;
        for i := 0 to FMemberCount - 1 do begin
          if not Supports(FFactors[i], IOmniSynchroExInternal, memb) then
            continue;
          if (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
            memb.RegisterDedicatedSoleIndirectClient(+1)
          else
            memb.RegisterIndirectClient(+1);
          memb.Enrol(FMemberObserver, TObject(i));
        end;
        FRegistered := True;
        if not assigned(FLock) then begin
          if assigned(FEventFactory) then
            FLock := FEventFactory.AcquireCriticalSection(True)
          else
            FLock := TOmniSynchroFactory.AcquireCriticalSection;
        end;
      end;
  end;
end; { TOmniCompositeSynchro.Create }

destructor TOmniCompositeSynchro.Destroy;
var
  i   : integer;
  memb: IOmniSynchroExInternal;
begin
  if assigned(FLock) then
    FLock.Enter;
  FCondVar.Free;
  if FRegistered then
    case FImplementation of
      {$IFDEF MSWINDOWS}
      Direct:
        begin
          for i := 0 to FMemberCount - 1 do
            if Supports(FFactors[i], IOmniSynchroExInternal, memb) then
              memb.RegisterDirectClient(-1);
        end;
      {$ENDIF}

      Indirect, Solo:
        begin
          for i := 0 to FMemberCount - 1 do begin
            if not Supports(FFactors[i], IOmniSynchroExInternal, memb) then
              continue;
            if (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
              memb.RegisterDedicatedSoleIndirectClient(-1)
            else
              memb.RegisterIndirectClient(-1);
            memb.Unenrol(TObject(i));
          end;
        end;
    end;
  SetLength(FFactors, 0);
  FTest := nil;
  if assigned(FMemberObserver) then begin
    FMemberObserver.FOwner := nil;
    FMemberObserver._Release;
    FMemberObserver := nil
  end;
  FObservers.Free;
  {$IFDEF MSWINDOWS}
    SetLength(FSynchroObjs, 0);
  {$ENDIF}
  inherited;
  if assigned(FLock) then
    FLock.Leave;
  FLock := nil;
end; { TOmniCompositeSynchro.Destroy }

class function TOmniCompositeSynchro.AllTest: TOmniConditionTest;
begin
  Result := function(var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean
    begin
      Result := IsAll(SignallerIdx, Peers, Datum);
    end;
end; { TOmniCompositeSynchro.AllTest }

class function TOmniCompositeSynchro.AnyTest: TOmniConditionTest;
begin
  Result := function(var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean
    begin
      Result := IsAny(SignallerIdx, Peers, Datum);
    end;
end; { TOmniCompositeSynchro.AnyTest }

function TOmniCompositeSynchro.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniCompositeSynchro.AsMWObject }

function TOmniCompositeSynchro.AsObject: TObject;
begin
  Result := Self;
end; { TOmniCompositeSynchro.AsObject }

function TOmniCompositeSynchro.Datum: TObject;
begin
  Result := FDatum;
end; { TOmniCompositeSynchro.Datum }

function TOmniCompositeSynchro.ConsumeResource: TWaitResult;
begin
  Result := WaitFor(0);
end; { TOmniCompositeSynchro.ConsumeResource }

procedure TOmniCompositeSynchro.Enrol(
  const Observer: IOmniSynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue(Token, Observer);
end; { TOmniCompositeSynchro.Enrol }

function TOmniCompositeSynchro.Factors: TOmniSynchroArray;
begin
  Result := FFactors;
end; { TOmniCompositeSynchro.Factors }

{$IFDEF MSWINDOWS}
function TOmniCompositeSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TOmniCompositeSynchro.GetHandle }
{$ENDIF}

class function TOmniCompositeSynchro.IsAll(
  var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean;
var
  member: IOmniSynchro;
begin
  Result := True;
  for member in Peers do begin
    Result := member.SignalState = esSignalled;
    if not Result then
      break;
  end;
end; { TOmniCompositeSynchro.IsAll }

class function TOmniCompositeSynchro.IsAny(
  var SignallerIdx: integer; const Peers: TOmniSynchroArray; Datum: TObject): boolean;
var
  idx: integer;
begin
  Result       := False;
  SignallerIdx := -1;
  for idx := 0 to Length(Peers) - 1 do begin
    Result := Peers[ idx].SignalState = esSignalled;
    if not Result then
      continue;
    SignallerIdx := idx;
    break;
  end;
end; { TOmniCompositeSynchro.IsAny }

function TOmniCompositeSynchro.IsCompatibleNativeMWObject(
  Reference, Peer: TObject): boolean;
begin
  Result := False;
end; { TOmniCompositeSynchro.IsCompatibleNativeMWObject }

function TOmniCompositeSynchro.IsResourceCounting: boolean;
begin
  Result := False;
end; { TOmniCompositeSynchro.IsResourceCounting }

function TOmniCompositeSynchro.IsSignalled: boolean;
begin
  Result := FIsSignalled;
end; { TOmniCompositeSynchro.IsSignalled }

function TOmniCompositeSynchro.LockingMechanism: TOmniLockingMechanism;
var
  isKernel: boolean;
  {$IFDEF MSWINDOW}
  memb: IOmniSynchroExInternal;
  {$ENDIF}
begin
  isKernel := False;
  case Self.FImplementation of
    {$IFDEF MSWINDOW}
    Direct: isKernel := (Length(FFactors) > 0) and
                         Supports(FFactors[0], IOmniSynchroExInternal, memb) and
                         (memb.LockingMechanism = KernelLocking);
    {$ENDIF}
    Indirect, Solo: isKernel := assigned(FLock) and (scKernelMode in FLock.Capabilities);
  end;
  if isKernel then
    Result := KernelLocking
  else
    Result := BusLocking;
end; { TOmniCompositeSynchro.LockingMechanism }

function TOmniCompositeSynchro.NativeMultiwait(
  const Synchros: array of TObject; Timeout: cardinal; AAll: boolean; var SignallerIndex: integer): TWaitResult;
begin
  Result := wrError;
end; { TOmniCompositeSynchro.NativeMultiwait }

function TOmniCompositeSynchro.NativeMultiwaitObject: TObject;
begin
  Result := nil;
end; { TOmniCompositeSynchro.NativeMultiwaitObject }

function TOmniCompositeSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  Result := False;
end; { TOmniCompositeSynchro.PermitsDedicatedSoleIndirectClient }

function TOmniCompositeSynchro.PermitsDirectClients: boolean;
begin
  Result := False;
end; { TOmniCompositeSynchro.PermitsDirectClients }

function TOmniCompositeSynchro.PermitsIndirectClients: boolean;
begin
  Result := True;
end; { TOmniCompositeSynchro.PermitsIndirectClients }

procedure TOmniCompositeSynchro.RegisterDedicatedSoleIndirectClient(Delta: integer);
begin
  // do nothing
end; { TOmniCompositeSynchro.RegisterDedicatedSoleIndirectClient }

procedure TOmniCompositeSynchro.RegisterDirectClient(Delta: integer);
begin
  // do nothing
end; { TOmniCompositeSynchro.RegisterDirectClient }

procedure TOmniCompositeSynchro.RegisterIndirectClient(Delta: integer);
begin
  // do nothing
end; { TOmniCompositeSynchro.RegisterIndirectClient }

procedure TOmniCompositeSynchro.Signal;
begin
  if (FImplementation in [Indirect, Solo]) and assigned(FCondVar) then
    FCondVar.Pulse;
end; { TOmniCompositeSynchro.Signal }

function TOmniCompositeSynchro.SignalState: TOmniSignalState;
begin
  if FIsSignalled then
      Result := esSignalled
    else
      Result := esNotSignalled;
end; { TOmniCompositeSynchro.SignalState }

procedure TOmniCompositeSynchro.Unenrol(ObserverToken: TObject);
begin
  FObservers.Remove(ObserverToken);
end; { TOmniCompositeSynchro.Unenrol }

function TOmniCompositeSynchro.UnionLock: IOmniLock;
begin
  Result := FLock;
end; { TOmniCompositeSynchro.UnionLock }

procedure TOmniCompositeSynchro.BubbleUp(var Me: IOmniSynchroExInternal);
var
  fummy   : integer;
  observer: TPair<TObject,IOmniSynchroObserver>;
begin
  if FTest(fummy, FFactors, FDatum) <> FIsSignalled then begin
    FIsSignalled := not FIsSignalled;
    if assigned(FObservers) then begin
      if not assigned(Me) then
        Me := Self;
      for observer in FObservers do
        observer.Value.PossibleStateChange(Me, observer.Key);
    end;
  end;
end; { TOmniCompositeSynchro.BubbleUp }

function TOmniCompositeSynchro.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [scSupportsSignalled, scModular];
end; { TOmniCompositeSynchro.GetCapabilities }

function TOmniCompositeSynchro.WaitFor(
  var SignallerIdx: integer; TimeOut: cardinal): TWaitResult;
var
  idx                 : integer;
  improperConstruction: boolean;
  me                  : IOmniSynchroExInternal;
begin
  idx := -1;
  Result := wrError;
  improperConstruction := False;
  case FImplementation of
    Indirect, Solo:
      begin
      Result := FCondVar.WaitFor(TimeOut,

        function(doAquireResource: boolean): boolean
        var
          confirmed      : boolean;
          consumedMembers: integer;
          i              : integer;
          synchro        : IOmniSynchroExInternal;
        begin
          Result := FTest(idx, FFactors, FDatum);
          if Result 
             and doAquireResource 
             and (FPropagation <> NoConsume) 
             and FHasAtLeastOneResourceCounter then
          begin
            ConsumedMembers := 0;
            for i := 0 to FMemberCount - 1 do begin
              if (((FPropagation = ConsumeOneSignalled) and (i = idx)) 
                   or (FPropagation = ConsumeAllSignalled)) 
                 and Supports(FFactors[i], IOmniSynchroExInternal, Synchro)
                 and Synchro.IsSignalled
                 and Synchro.IsResourceCounting then
              begin
                Confirmed := Synchro.ConsumeResource = wrSignaled;
                Inc(ConsumedMembers);
                if Confirmed then 
                  continue;
                Result := False;
                if (FPropagation = ConsumeOneSignalled) 
                   or ((FPropagation = ConsumeAllSignalled) and (ConsumedMembers = 1)) then
                begin
                  // We have a competitor waiter on one of the members and the competitor
                  //  slipped in ahead of us, so loop back and try again.
                end
                else begin
                  // This is an error. We should never get here if TConditionEvent is properly constructed.
                  // To prevent the possibility of this condition, we disallow members which have
                  //  multiple waiters (including TConditionEvent) and where the propagation is ConsumeAllSignalled
                  //  and at least one of the members are resource-counting synchros.
                  improperConstruction := True;
                  Result := True
                end;
                break;
              end;
            end;
          end;
        end,

        procedure
        begin
          BubbleUp(me);
        end);

      if improperConstruction then
        Result := wrError;

      if Result = wrSignaled then
        SignallerIdx := idx;
      end;

    {$IFDEF MSWINDOWS}
    Direct:
      begin
        Result := (FFactors[0] as IOmniSynchroExInternal).NativeMultiwait(FSynchroObjs, TimeOut, FTestClass = TestAll, SignallerIdx);
        if Result in [wrSignaled, wrTimeOut] then begin
          FLock.Enter;
          BubbleUp(me);
          FLock.Leave;
        end;
      end;
    {$ENDIF}
    end;
end; { TOmniCompositeSynchro.WaitFor }

function TOmniCompositeSynchro.WaitFor(timeout: cardinal): TWaitResult;
var
  dummy: integer;
begin
  Result := WaitFor(dummy, timeout);
end; { TOmniCompositeSynchro.WaitFor }

{ TOmniModularSynchro }

constructor TOmniModularSynchro.Create(
  const ABase: IOmniSynchro; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
begin
  FUnionLock := AUnionLock;
  FBase      := ABase;
  Supports(FBase, IOmniSynchroExInternal, FBaseEx);
  FDirectWaiters.Initialize(0);
  FIndirectWaiters.Initialize(0);
  FObservers := TDictionary<TObject,IOmniSynchroObserver>.Create;
  FDedicatedToOneIndirectWaiter := False;
  FEventFactory := AEventFactory;
end; { TOmniModularSynchro.Create }

destructor TOmniModularSynchro.Destroy;
begin
  FDirectWaiters.Finalize;
  FIndirectWaiters.Finalize;
  FObservers.Free;
  inherited;
end; { TOmniModularSynchro.Destroy }

function TOmniModularSynchro.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TOmniModularSynchro.AsMWObject }

function TOmniModularSynchro.AsObject: TObject;
begin
  Result := Self;
end; { TOmniModularSynchro.AsObject }

function TOmniModularSynchro.ConsumeResource: TWaitResult;
begin
  Result := FBase.WaitFor(0);
end; { TOmniModularSynchro.ConsumeResource }

procedure TOmniModularSynchro.Enrol(
  const Observer: IOmniSynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue(Token, Observer);
end; { TOmniModularSynchro.Enrol }

function TOmniModularSynchro.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [];
  if scSupportsSignalled in FBase.Capabilities then
    Include(Result, scSupportsSignalled);
end; { TOmniModularSynchro.GetCapabilities }

{$IFDEF MSWINDOWS}
function TOmniModularSynchro.GetHandle: THandle;
begin
  if assigned(FBase) then
    Result := FBase.Handle
  else
    Result := 0;
end; { TOmniModularSynchro.GetHandle }
{$ENDIF}

function TOmniModularSynchro.IsCompatibleNativeMWObject(
  Reference, Peer: TObject): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := assigned(Reference) and assigned(Peer) and
            (Reference.ClassType = Peer.ClassType) and
            (Reference <> Peer) and
            (Reference is THandleObject) and (Peer is THandleObject);
  {$ELSE}
  Result := False;
  {$ENDIF}
end; { TOmniModularSynchro.IsCompatibleNativeMWObject }

function TOmniModularSynchro.IsModular: boolean;
begin
  Result := True;
end; { TOmniModularSynchro.IsModular }

function TOmniModularSynchro.IsPoolManaged: boolean;
begin
  Result := False;
end; { TOmniModularSynchro.IsPoolManaged }

function TOmniModularSynchro.IsResourceCounting: boolean;
begin
  Result := False;
end; { TOmniModularSynchro.IsResourceCounting }

function TOmniModularSynchro.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TOmniModularSynchro.IsSignalled }

function TOmniModularSynchro.LockingMechanism: TOmniLockingMechanism;
begin
  Result := KernelLocking;
end; { TOmniModularSynchro.LockingMechanism }

function TOmniModularSynchro.NativeMultiwait(
  const Synchros: array of TObject; Timeout: cardinal; AAll: boolean; var SignallerIndex: integer): TWaitResult;
{$IFDEF MSWINDOWS}
var
  handleObjs : THandleObjectArray;
  i          : integer;
  l          : integer;
  obj        : TObject;
  ok         : boolean;
  signaledObj: THandleObject;
{$ENDIF}
begin
  SignallerIndex := -1;
  {$IFDEF MSWINDOWS}
  l  := Length(Synchros);
  ok := False;
  SetLength(handleObjs, l);
  for i := 0 to l - 1 do begin
    obj := Synchros[i];
    ok  := assigned(obj) and (obj is THandleObject);
    if not ok then 
      break;
    handleObjs[i] := THandleObject(obj)
  end;
  if ok and (l >= 2) then begin
    Result := THandleObject.WaitForMultiple(handleObjs, Timeout, AAll, signaledObj, False, 0);
    if (Result = wrSignaled) and (not AAll) then
      for i := 0 to l - 1 do begin
        if handleObjs[i] <> signaledObj then 
          continue;
        SignallerIndex := i;
        break;
      end
    end
  else
  {$ENDIF}
    Result := wrError;
end; { TOmniModularSynchro.NativeMultiwait }

function TOmniModularSynchro.NativeMultiwaitObject: TObject;
begin
  Result := nil;
end; { TOmniModularSynchro.NativeMultiwaitObject }

procedure TOmniModularSynchro.ObserverProc(Proc: TProc);
// This method assumes that RefCount > 0, so don't call from destructor.
var
  me      : IOmniSynchroExInternal;
  observer: TPair<TObject,IOmniSynchroObserver>;
begin
  me := Self as IOmniSynchroExInternal;
  FUnionLock.Enter;
  try
    Proc;
    for observer in FObservers do
      observer.Value.PossibleStateChange(me, observer.Key);
  finally FUnionLock.Leave; end;
end; { TOmniModularSynchro.ObserverProc }

function TOmniModularSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  Result := (FIndirectWaiters.Read = 0) and (FDirectWaiters.Read = 0);
end; { TOmniModularSynchro.PermitsDedicatedSoleIndirectClient }

function TOmniModularSynchro.PermitsDirectClients: boolean;
{$IFDEF MSWINDOWS}
var
  obj: TObject;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := (FIndirectWaiters = 0) and (not FDedicatedToOneIndirectWaiter);
  if Result then begin
    obj := FBase.AsMWObject;
    Result := assigned(obj) and (obj is THandleObject);
  end
  {$ELSE}
    Result := False;
  {$ENDIF}
end; { TOmniModularSynchro.PermitsDirectClients }

function TOmniModularSynchro.PermitsIndirectClients: boolean;
begin
  Result := FDirectWaiters.Read = 0;
end; { TOmniModularSynchro.PermitsIndirectClients }

procedure TOmniModularSynchro.RegisterDedicatedSoleIndirectClient(Delta: integer);
begin
  if Delta > 0 then
      FDedicatedToOneIndirectWaiter := True
    else if Delta < 0 then
      FDedicatedToOneIndirectWaiter := False;
  RegisterIndirectClient(Delta);
end; { TOmniModularSynchro.RegisterDedicatedSoleIndirectClient }

procedure TOmniModularSynchro.RegisterDirectClient(Delta: integer);
begin
  FDirectWaiters.Add(Delta);
end; { TOmniModularSynchro.RegisterDirectClient }

procedure TOmniModularSynchro.RegisterIndirectClient(Delta: integer);
begin
  FInDirectWaiters.Add(Delta);
end; { TOmniModularSynchro.RegisterIndirectClient }

procedure TOmniModularSynchro.Signal;
begin
  // do nothing
end; { TOmniModularSynchro.Signal }

function TOmniModularSynchro.SignalState: TOmniSignalState;
begin
  Result := FBase.SignalState;
end; { TOmniModularSynchro.SignalState }

procedure TOmniModularSynchro.Unenrol(ObserverToken: TObject);
begin
  FObservers.Remove(ObserverToken);
end; { TOmniModularSynchro.Unenrol }

function TOmniModularSynchro.UnionLock: IOmniLock;
begin
  Result := FUnionLock;
end; { TOmniModularSynchro.UnionLock }

function TOmniModularSynchro.WaitFor(timeout: cardinal): TWaitResult;
var
  anyTest    : TOmniConditionTest;
  meAsFactors: TOmniSynchroArray;
  waitRes    : TWaitResult;
  wrap       : IOmniSynchro;
begin
  waitRes := wrIOCompletion;
  anyTest := TOmniCompositeSynchro.anyTest();
  ObserverProc(
    procedure 
    begin
      if FIndirectWaiters.Read > 0 then begin
        if FDedicatedToOneIndirectWaiter then
          waitRes := wrError
        else begin
          SetLength(meAsFactors, 1);
          meAsFactors[0] := Self as IOmniSynchro;
          wrap := TOmniCompositeSynchro.Create(meAsFactors, FEventFactory, ConsumeOneSignalled,
                    anyTest, TestAny, True);
          waitRes := wrSignaled;
        end;
      end
      else begin
        FDirectWaiters.Increment;
        waitRes := wrSignaled;
      end;
    end);
  case waitRes of
    wrSignaled:
      begin
        if assigned(wrap) then
          Result := wrap.WaitFor(timeout)
        else
          Result := FBase.WaitFor(timeout);
      end;
    else // wrTimeout, wrAbandoned, wrError, wrIOCompletion:
      Result := wrError;
  end;
end; { TOmniModularSynchro.WaitFor }

constructor TOmniModularEvent.Create(const ABase: IOmniEvent; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
begin
  FBaseEvent := ABase;
  inherited Create(FBaseEvent, AUnionLock, AEventFactory);
end; { TOmniModularEvent.Create }

function TOmniModularEvent.ConsumeResource: TWaitResult;
begin
  if not (scManualEvent in FBaseEvent.Capabilities) then
    Result := FBaseEvent.WaitFor(0) // Equivalent to ResetEvent()
  else
    Result := wrSignaled;
end; { TOmniModularEvent.ConsumeResource }

function TOmniModularEvent.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  if scManualEvent in FBaseEvent.Capabilities then
    Include(Result, scManualEvent);
  Exclude(Result, scLight);
end; { TOmniModularEvent.GetCapabilities }

function TOmniModularEvent.IsResourceCounting: boolean;
begin
  Result := not (scManualEvent in FBaseEvent.Capabilities);
end; { TOmniModularEvent.IsResourceCounting }

procedure TOmniModularEvent.ResetEvent;
begin
  ObserverProc(
    procedure 
    begin
      FBaseEvent.ResetEvent;
    end);
end; { TOmniModularEvent.ResetEvent }

procedure TOmniModularEvent.SetEvent;
begin
  ObserverProc(
    procedure 
    begin
      FBaseEvent.SetEvent;
    end);
end; { TOmniModularEvent.SetEvent }

procedure TOmniModularEvent.Signal;
begin
  SetEvent;
end; { TOmniModularEvent.Signal }

{ TOmniModularSemaphore }

constructor TOmniModularSemaphore.Create(
  const ABase: IOmniSemaphore; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
begin
  FBaseSem := ABase;
  inherited Create(FBaseSem, AUnionLock, AEventFactory);
end; { TOmniModularSemaphore.Create }

function TOmniModularSemaphore.ConsumeResource: TWaitResult;
begin
  Result := FBaseSem.WaitFor(0);
end; { TOmniModularSemaphore.ConsumeResource }

function TOmniModularSemaphore.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TOmniModularSemaphore.GetCapabilities }

function TOmniModularSemaphore.InitialValue: cardinal;
begin
  Result := FBaseSem.InitialValue;
end; { TOmniModularSemaphore.InitialValue }

function TOmniModularSemaphore.IsResourceCounting: boolean;
begin
  Result := True;
end; { TOmniModularSemaphore.IsResourceCounting }

procedure TOmniModularSemaphore.Reset;
begin
  ObserverProc(
    procedure 
    begin
      FBaseSem.Reset;
    end);
end; { TOmniModularSemaphore.Reset }

procedure TOmniModularSemaphore.Signal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseSem.Signal;
    end);
end;  { TOmniModularSemaphore.Signal }

function TOmniModularSemaphore.Value: cardinal;
begin
  Result := FBaseSem.Value;
end; { TOmniModularSemaphore.Value }

{ TOmniModularCountDown }

constructor TOmniModularCountDown.Create(
  const ABase: IOmniCountDown; const AUnionLock: IOmniLock; AEventFactory: TOmniSynchroFactory);
begin
  FBaseCountDown := ABase;
  inherited Create(FBaseCountDown, AUnionLock, AEventFactory);
end; { TOmniModularCountDown.Create }

function TOmniModularCountDown.ConsumeResource: TWaitResult;
begin
  Result := wrSignaled;
end; { TOmniModularCountDown.ConsumeResource }

procedure TOmniModularCountDown.CounterSignal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseCountDown.CounterSignal;
    end);
end; { TOmniModularCountDown.CounterSignal }

function TOmniModularCountDown.InitialValue: cardinal;
begin
  Result := FBaseCountDown.InitialValue;
end; { TOmniModularCountDown.InitialValue }

function TOmniModularCountDown.IsResourceCounting: boolean;
begin
  Result := False;
end; { TOmniModularCountDown.IsResourceCounting }

procedure TOmniModularCountDown.Signal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseCountDown.Signal;
    end);
end;  { TOmniModularCountDown.Signal }

function TOmniModularCountDown.SignalHit: boolean;
var
  res: boolean;
begin
  ObserverProc(
    procedure 
    begin
      res := FBaseCountDown.SignalHit;
    end);
  Result := res;
end; { TOmniModularCountDown.SignalHit }

function TOmniModularCountDown.Allocate: cardinal;
var
  res: cardinal;
begin
  ObserverProc(
    procedure 
    begin
      res := FBaseCountDown.Allocate;
    end);
  Result := res;
end; { TOmniModularCountDown.Allocate }

function TOmniModularCountDown.Value: cardinal;
begin
  Result := FBaseCountDown.Value;
end; { TOmniModularCountDown.Value }

end.
