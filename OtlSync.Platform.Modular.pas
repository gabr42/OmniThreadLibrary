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
//  READ THE COMMENTS IN UNIT OtlPlatform.Sync.

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
  ISimpleConditionEvent = interface ['{1EAEEE8F-54E1-44B5-BE3C-AFC14A465F74}']
    procedure Pulse;
    function  WaitFor: TWaitResult;
  end; { ISimpleConditionEvent }

  ICompositeSynchro = interface ['{3796D8C1-AA28-44A7-836E-F1045AFBF616}']
    function WaitFor(var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;
    function Factors: TSynchroArray;
    function Datum: TObject;
    function IsSignalled: boolean;
  end; { ICompositeSynchro }

  ISynchroExInternal = interface;

  ISynchroObserver = interface ['{C1EBE331-613F-4D0F-B645-CF2925F55B22}']
    procedure PossibleStateChange(const Source: ISynchroExInternal; Token: TObject);
  end; { ISynchroObserver }

  ISynchroExInternal = interface(ISynchro) ['{A3D01DFF-CF6A-4913-8A34-E46C4C0FCF5F}']
    function  ConsumeResource: TWaitResult;
    procedure Enrol(const Observer: ISynchroObserver; Token: TObject);
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsResourceCounting: boolean;
    function  LockingMechanism: TLockingMechanism;
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
    function  UnionLock: ILock;
  end; { ISynchroExInternal }

  TWaitPropagation = (NoConsume, ConsumeOneSignalled, ConsumeAllSignalled);

  TConditionTest = reference to function(
    var   SignallerIdx: integer;
    const Peers: TSynchroArray; Datum: TObject): boolean;

  TTestClass = (TestAny, TestAll, TestCustom);

  TSignalState = OtlSync.Platform.Interfaced.TSignalState;

  TCompositeSynchro = class(TInterfacedObject, ICompositeSynchro, ISynchro, ISynchroExInternal)
  strict private type
    TImplementation = ({$IFDEF MSWINDOWS}Direct, {$ENDIF} Indirect, Solo);
    TMemberObserver = class(TInterfacedObject, ISynchroObserver)
    private
      {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
      [Volatile] FOwner: TCompositeSynchro;
      procedure PossibleStateChange(const Source: ISynchroExInternal; Token: TObject);
    public
      constructor Create(AOwner: TCompositeSynchro);
    end; { TMemberObserver }
  var
    FAllowSolo                   : boolean;
    FCondVar                     : OtlSync.Platform.ConditionVariables.TSBDConditionVariable;
    FDatum                       : TObject;
    FEventFactory                : TSynchroFactory;
    FFactors                     : TSynchroArray;
    FHasAtLeastOneResourceCounter: boolean;
    FImplementation              : TImplementation;
    [Volatile] FIsSignalled      : boolean;
    FLock                        : ILock;
    FMemberCount                 : integer;
    FMemberObserver              : TMemberObserver;
    FObservers                   : TDictionary<TObject,ISynchroObserver>;
    FPropagation                 : TWaitPropagation;
    FRegistered                  : boolean;
    FTest                        : TConditionTest;
    FTestClass                   : TTestClass;
    {$IFDEF MSWINDOWS}
    FSynchroObjs                 : array of TObject;   // Actually array of THandle.
    {$ENDIF}
  protected
    {$IFDEF MSWINDOWS}
    function GetHandle: THandle;
    {$ENDIF}
  public
    constructor Create(const AFactors: TSynchroArray; AEventFactory: TSynchroFactory;
                       APropagation: TWaitPropagation; ATest: TConditionTest;
                       ATestClass: TTestClass; AAllowSolo: boolean);
    destructor  Destroy; override;
    class function IsAll(var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
    class function IsAny(var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
    class function AnyTest: TConditionTest;
    class function AllTest: TConditionTest;
    function  AsMWObject: TObject;
    function  AsObject: TObject;
    function  ConsumeResource: TWaitResult;
    function  Datum: TObject;
    function  Factors: TSynchroArray;
    function  GetCapabilities: TSynchroCapabilities; virtual;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsResourceCounting: boolean;
    function  IsSignalled: boolean;
    function  LockingMechanism: TLockingMechanism;
    function  NativeMultiwait(const Synchros: array of TObject;
                              Timeout: cardinal; AAll: boolean;
                              var SignallerIndex: integer): TWaitResult;
    function  NativeMultiwaitObject: TObject;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  SignalState: TSignalState;
    function  UnionLock: ILock;
    function  WaitFor(Timeout: cardinal = INFINITE): TWaitResult; overload;
    function  WaitFor(var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult; overload;
    procedure BubbleUp( var Me: ISynchroExInternal);
    procedure Enrol( const Observer: ISynchroObserver; Token: TObject);
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure Signal;
    procedure Unenrol(ObserverToken: TObject);
  end; { TCompositeSynchro }

  TModularSynchro = class abstract(TInterfacedObject, ISynchro, ISynchroExInternal)
  private
    FDedicatedToOneIndirectWaiter: boolean;
    [Volatile] FDirectWaiters    : TVolatileInt32;
    [Volatile] FIndirectWaiters  : TVolatileInt32;
    FObservers                   : TDictionary<TObject,ISynchroObserver>;
  protected
    FUnionLock: ILock;
    FBase  : ISynchro;
    FBaseEx: ISynchroExInternal;
    FEventFactory: TSynchroFactory;
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}
  public
    constructor Create( const ABase: ISynchro; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
    destructor  Destroy; override;
    function  AsMWObject: TObject;
    function  AsObject: TObject;
    function  ConsumeResource: TWaitResult; virtual;
    function  GetCapabilities: TSynchroCapabilities; virtual;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  IsModular: boolean;
    function  IsPoolManaged: boolean;
    function  IsResourceCounting: boolean; virtual;
    function  IsSignalled: boolean;
    function  LockingMechanism: TLockingMechanism;
    function  NativeMultiwait(const Synchros: array of TObject;
                              Timeout: cardinal; AAll: boolean;
                              var SignallerIndex: integer): TWaitResult;
    function  NativeMultiwaitObject: TObject;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  SignalState: TSignalState;
    function  UnionLock: ILock;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; virtual;
    procedure Enrol(const Observer: ISynchroObserver; Token: TObject);
    procedure ObserverProc(Proc: TProc);
    procedure RegisterDedicatedSoleIndirectClient(Delta: integer);
    procedure RegisterDirectClient(Delta: integer);
    procedure RegisterIndirectClient(Delta: integer);
    procedure Signal; virtual;
    procedure Unenrol(ObserverToken: TObject);
  end; { TModularSynchro }

  TModularEvent = class(TModularSynchro, IEvent)
  protected
    FBaseEvent: IEvent;
  public
    constructor Create(const ABase: IEvent; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
    function  ConsumeResource: TWaitResult; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  IsResourceCounting: boolean; override;
    procedure ResetEvent;
    procedure SetEvent;
    procedure Signal; override;
  end; { TModularEvent }

  TModularSemaphore = class(TModularSynchro, ISemaphore)
  protected
    FBaseSem: ISemaphore;
  public
    constructor Create(const ABase: ISemaphore; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
    function  ConsumeResource: TWaitResult; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsResourceCounting: boolean; override;
    procedure Reset;
    procedure Signal; override;
    function  Value: cardinal;
  end; { TModularSemaphore }

  TModularCountDown = class(TModularSynchro, ICountDown)
  protected
    FBaseCountDown: ICountDown;
  public
    constructor Create( const ABase: ICountDown; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
    function  Allocate: cardinal;
    function  ConsumeResource: TWaitResult; override;
    procedure CounterSignal;
    function  InitialValue: cardinal;
    function  IsResourceCounting: boolean; override;
    function  SignalHit: boolean;
    procedure Signal; override;
    function  Value: cardinal;
  end; { TModularCountDown }

implementation

uses
  System.Diagnostics,
  OtlSync.Platform.Errors;

{ TCompositeSynchro.TMemberObserver }

constructor TCompositeSynchro.TMemberObserver.Create(AOwner: TCompositeSynchro);
begin
  FOwner := AOwner;
end; { TCompositeSynchro.TMemberObserver.Create }

procedure TCompositeSynchro.TMemberObserver.PossibleStateChange(
  const Source: ISynchroExInternal; Token: TObject);
begin
  // Index of signaller is integer(Token)
  FOwner.Signal;
end; { TCompositeSynchro.TMemberObserver.PossibleStateChange }

{ TCompositeSynchro }

constructor TCompositeSynchro.Create(
  const AFactors: TSynchroArray; AEventFactory: TSynchroFactory;
  APropagation: TWaitPropagation; ATest: TConditionTest;
  ATestClass: TTestClass; AAllowSolo: boolean);
var
  dummy: integer;
  i    : integer;
  memb : ISynchroExInternal;
  mwObj: TObject;
  ok   : boolean;
begin
  FRegistered := False;
  // Test for the direct solution
  FMemberCount := Length(AFactors);
  if FMemberCount = 0 then
    raise TParallelException.Create(ECompositeNeedsOneFactor);
  FPropagation := APropagation;
  FDatum       := Datum;
  FTest        := ATest;
  FTestClass   := ATestClass;
  FObservers := TDictionary<TObject,ISynchroObserver>.Create;
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
      raise TParallelException.Create(EOnlyModularCombinable);
  end;

  if Supports(FFactors[0], ISynchroExInternal, memb) then begin
    mwObj := memb.NativeMultiwaitObject;
    FLock := memb.UnionLock
  end
  else begin
    mwObj := nil;
    FLock := nil
  end;
  ok := True;
  for i := 1 to FMemberCount - 1 do begin
    if Supports(FFactors[i], ISynchroExInternal, memb) then begin
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
      FCondVar := TSBDConditionVariable.Create(
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
        ok := Supports(FFactors[i], ISynchroExInternal, memb)
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
      FHasAtLeastOneResourceCounter := Supports(FFactors[i], ISynchroExInternal, memb) and
                                       memb.IsResourceCounting;
      if FHasAtLeastOneResourceCounter then
        break
    end;
    for i := 0 to FMemberCount - 1 do begin
      ok := Supports(FFactors[i], ISynchroExInternal, memb) and
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
    raise TParallelException.Create(ECompositeSynchroMixedBag);

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
          if Supports(FFactors[i], ISynchroExInternal, memb) then
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
          if not Supports(FFactors[i], ISynchroExInternal, memb) then
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
            FLock := TSynchroFactory.AcquireCriticalSection;
        end;
      end;
  end;
end; { TCompositeSynchro.Create }

destructor TCompositeSynchro.Destroy;
var
  i   : integer;
  memb: ISynchroExInternal;
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
            if Supports(FFactors[i], ISynchroExInternal, memb) then
              memb.RegisterDirectClient(-1);
        end;
      {$ENDIF}

      Indirect, Solo:
        begin
          for i := 0 to FMemberCount - 1 do begin
            if not Supports(FFactors[i], ISynchroExInternal, memb) then
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
end; { TCompositeSynchro.Destroy }

class function TCompositeSynchro.AllTest: TConditionTest;
begin
  Result := function(var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean
    begin
      Result := IsAll(SignallerIdx, Peers, Datum);
    end;
end; { TCompositeSynchro.AllTest }

class function TCompositeSynchro.AnyTest: TConditionTest;
begin
  Result := function(var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean
    begin
      Result := IsAny(SignallerIdx, Peers, Datum);
    end;
end; { TCompositeSynchro.AnyTest }

function TCompositeSynchro.AsMWObject: TObject;
begin
  Result := nil;
end; { TCompositeSynchro.AsMWObject }

function TCompositeSynchro.AsObject: TObject;
begin
  Result := Self;
end; { TCompositeSynchro.AsObject }

function TCompositeSynchro.Datum: TObject;
begin
  Result := FDatum;
end; { TCompositeSynchro.Datum }

function TCompositeSynchro.ConsumeResource: TWaitResult;
begin
  Result := WaitFor(0);
end; { TCompositeSynchro.ConsumeResource }

procedure TCompositeSynchro.Enrol(
  const Observer: ISynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue(Token, Observer);
end; { TCompositeSynchro.Enrol }

function TCompositeSynchro.Factors: TSynchroArray;
begin
  Result := FFactors;
end; { TCompositeSynchro.Factors }

{$IFDEF MSWINDOWS}
function TCompositeSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TCompositeSynchro.GetHandle }
{$ENDIF}

class function TCompositeSynchro.IsAll(
  var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
var
  member: ISynchro;
begin
  Result := True;
  for member in Peers do begin
    Result := member.SignalState = esSignalled;
    if not Result then
      break;
  end;
end; { TCompositeSynchro.IsAll }

class function TCompositeSynchro.IsAny(
  var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
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
end; { TCompositeSynchro.IsAny }

function TCompositeSynchro.IsCompatibleNativeMWObject(
  Reference, Peer: TObject): boolean;
begin
  Result := False;
end; { TCompositeSynchro.IsCompatibleNativeMWObject }

function TCompositeSynchro.IsResourceCounting: boolean;
begin
  Result := False;
end; { TCompositeSynchro.IsResourceCounting }

function TCompositeSynchro.IsSignalled: boolean;
begin
  Result := FIsSignalled;
end; { TCompositeSynchro.IsSignalled }

function TCompositeSynchro.LockingMechanism: TLockingMechanism;
var
  isKernel: boolean;
  {$IFDEF MSWINDOW}
  memb: ISynchroExInternal;
  {$ENDIF}
begin
  isKernel := False;
  case Self.FImplementation of
    {$IFDEF MSWINDOW}
    Direct: isKernel := (Length(FFactors) > 0) and
                         Supports(FFactors[0], ISynchroExInternal, memb) and
                         (memb.LockingMechanism = KernelLocking);
    {$ENDIF}
    Indirect, Solo: isKernel := assigned(FLock) and (scKernelMode in FLock.Capabilities);
  end;
  if isKernel then
    Result := KernelLocking
  else
    Result := BusLocking;
end; { TCompositeSynchro.LockingMechanism }

function TCompositeSynchro.NativeMultiwait(
  const Synchros: array of TObject; Timeout: cardinal; AAll: boolean; var SignallerIndex: integer): TWaitResult;
begin
  Result := wrError;
end; { TCompositeSynchro.NativeMultiwait }

function TCompositeSynchro.NativeMultiwaitObject: TObject;
begin
  Result := nil;
end; { TCompositeSynchro.NativeMultiwaitObject }

function TCompositeSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  Result := False;
end; { TCompositeSynchro.PermitsDedicatedSoleIndirectClient }

function TCompositeSynchro.PermitsDirectClients: boolean;
begin
  Result := False;
end; { TCompositeSynchro.PermitsDirectClients }

function TCompositeSynchro.PermitsIndirectClients: boolean;
begin
  Result := True;
end; { TCompositeSynchro.PermitsIndirectClients }

procedure TCompositeSynchro.RegisterDedicatedSoleIndirectClient(Delta: integer);
begin
  // do nothing
end; { TCompositeSynchro.RegisterDedicatedSoleIndirectClient }

procedure TCompositeSynchro.RegisterDirectClient(Delta: integer);
begin
  // do nothing
end; { TCompositeSynchro.RegisterDirectClient }

procedure TCompositeSynchro.RegisterIndirectClient(Delta: integer);
begin
  // do nothing
end; { TCompositeSynchro.RegisterIndirectClient }

procedure TCompositeSynchro.Signal;
begin
  if (FImplementation in [Indirect, Solo]) and assigned(FCondVar) then
    FCondVar.Pulse;
end; { TCompositeSynchro.Signal }

function TCompositeSynchro.SignalState: TSignalState;
begin
  if FIsSignalled then
      Result := esSignalled
    else
      Result := esNotSignalled;
end; { TCompositeSynchro.SignalState }

procedure TCompositeSynchro.Unenrol(ObserverToken: TObject);
begin
  FObservers.Remove(ObserverToken);
end; { TCompositeSynchro.Unenrol }

function TCompositeSynchro.UnionLock: ILock;
begin
  Result := FLock;
end; { TCompositeSynchro.UnionLock }

procedure TCompositeSynchro.BubbleUp(var Me: ISynchroExInternal);
var
  fummy   : integer;
  observer: TPair<TObject,ISynchroObserver>;
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
end; { TCompositeSynchro.BubbleUp }

function TCompositeSynchro.GetCapabilities: TSynchroCapabilities;
begin
  Result := [scSupportsSignalled, scModular];
end; { TCompositeSynchro.GetCapabilities }

function TCompositeSynchro.WaitFor(
  var SignallerIdx: integer; TimeOut: cardinal): TWaitResult;
var
  idx                 : integer;
  improperConstruction: boolean;
  me                  : ISynchroExInternal;
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
          synchro        : ISynchroExInternal;
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
                 and Supports(FFactors[i], ISynchroExInternal, Synchro) 
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
        Result := (FFactors[0] as ISynchroExInternal).NativeMultiwait(FSynchroObjs, TimeOut, FTestClass = TestAll, SignallerIdx);
        if Result in [wrSignaled, wrTimeOut] then begin
          FLock.Enter;
          BubbleUp(me);
          FLock.Leave;
        end;
      end;
    {$ENDIF}
    end;
end; { TCompositeSynchro.WaitFor }

function TCompositeSynchro.WaitFor(timeout: cardinal): TWaitResult;
var
  dummy: integer;
begin
  Result := WaitFor(dummy, timeout);
end; { TCompositeSynchro.WaitFor }

{ TModularSynchro }

constructor TModularSynchro.Create(
  const ABase: ISynchro; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FUnionLock := AUnionLock;
  FBase      := ABase;
  Supports(FBase, ISynchroExInternal, FBaseEx);
  FDirectWaiters.Initialize(0);
  FIndirectWaiters.Initialize(0);
  FObservers := TDictionary<TObject,ISynchroObserver>.Create;
  FDedicatedToOneIndirectWaiter := False;
  FEventFactory := AEventFactory;
end; { TModularSynchro.Create }

destructor TModularSynchro.Destroy;
begin
  FDirectWaiters.Finalize;
  FIndirectWaiters.Finalize;
  FObservers.Free;
  inherited;
end; { TModularSynchro.Destroy }

function TModularSynchro.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TModularSynchro.AsMWObject }

function TModularSynchro.AsObject: TObject;
begin
  Result := Self;
end; { TModularSynchro.AsObject }

function TModularSynchro.ConsumeResource: TWaitResult;
begin
  Result := FBase.WaitFor(0);
end; { TModularSynchro.ConsumeResource }

procedure TModularSynchro.Enrol(
  const Observer: ISynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue(Token, Observer);
end; { TModularSynchro.Enrol }

function TModularSynchro.GetCapabilities: TSynchroCapabilities;
begin
  Result := [];
  if scSupportsSignalled in FBase.Capabilities then
    Include(Result, scSupportsSignalled);
end; { TModularSynchro.GetCapabilities }

{$IFDEF MSWINDOWS}
function TModularSynchro.GetHandle: THandle;
begin
  if assigned(FBase) then
    Result := FBase.Handle
  else
    Result := 0;
end; { TModularSynchro.GetHandle }
{$ENDIF}

function TModularSynchro.IsCompatibleNativeMWObject(
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
end; { TModularSynchro.IsCompatibleNativeMWObject }

function TModularSynchro.IsModular: boolean;
begin
  Result := True;
end; { TModularSynchro.IsModular }

function TModularSynchro.IsPoolManaged: boolean;
begin
  Result := False;
end; { TModularSynchro.IsPoolManaged }

function TModularSynchro.IsResourceCounting: boolean;
begin
  Result := False;
end; { TModularSynchro.IsResourceCounting }

function TModularSynchro.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TModularSynchro.IsSignalled }

function TModularSynchro.LockingMechanism: TLockingMechanism;
begin
  Result := KernelLocking;
end; { TModularSynchro.LockingMechanism }

function TModularSynchro.NativeMultiwait(
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
end; { TModularSynchro.NativeMultiwait }

function TModularSynchro.NativeMultiwaitObject: TObject;
begin
  Result := nil;
end; { TModularSynchro.NativeMultiwaitObject }

procedure TModularSynchro.ObserverProc(Proc: TProc);
// This method assumes that RefCount > 0, so don't call from destructor.
var
  me      : ISynchroExInternal;
  observer: TPair<TObject,ISynchroObserver>;
begin
  me := Self as ISynchroExInternal;
  FUnionLock.Enter;
  try
    Proc;
    for observer in FObservers do
      observer.Value.PossibleStateChange(me, observer.Key);
  finally FUnionLock.Leave; end;
end; { TModularSynchro.ObserverProc }

function TModularSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  Result := (FIndirectWaiters.Read = 0) and (FDirectWaiters.Read = 0);
end; { TModularSynchro.PermitsDedicatedSoleIndirectClient }

function TModularSynchro.PermitsDirectClients: boolean;
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
end; { TModularSynchro.PermitsDirectClients }

function TModularSynchro.PermitsIndirectClients: boolean;
begin
  Result := FDirectWaiters.Read = 0;
end; { TModularSynchro.PermitsIndirectClients }

procedure TModularSynchro.RegisterDedicatedSoleIndirectClient(Delta: integer);
begin
  if Delta > 0 then
      FDedicatedToOneIndirectWaiter := True
    else if Delta < 0 then
      FDedicatedToOneIndirectWaiter := False;
  RegisterIndirectClient(Delta);
end; { TModularSynchro.RegisterDedicatedSoleIndirectClient }

procedure TModularSynchro.RegisterDirectClient(Delta: integer);
begin
  FDirectWaiters.Add(Delta);
end; { TModularSynchro.RegisterDirectClient }

procedure TModularSynchro.RegisterIndirectClient(Delta: integer);
begin
  FInDirectWaiters.Add(Delta);
end; { TModularSynchro.RegisterIndirectClient }

procedure TModularSynchro.Signal;
begin
  // do nothing
end; { TModularSynchro.Signal }

function TModularSynchro.SignalState: TSignalState;
begin
  Result := FBase.SignalState;
end; { TModularSynchro.SignalState }

procedure TModularSynchro.Unenrol(ObserverToken: TObject);
begin
  FObservers.Remove(ObserverToken);
end; { TModularSynchro.Unenrol }

function TModularSynchro.UnionLock: ILock;
begin
  Result := FUnionLock;
end; { TModularSynchro.UnionLock }

function TModularSynchro.WaitFor(timeout: cardinal): TWaitResult;
var
  anyTest    : TConditionTest;
  meAsFactors: TSynchroArray;
  waitRes    : TWaitResult;
  wrap       : ISynchro;
begin
  waitRes := wrIOCompletion;
  anyTest := TCompositeSynchro.anyTest();
  ObserverProc(
    procedure 
    begin
      if FIndirectWaiters.Read > 0 then begin
        if FDedicatedToOneIndirectWaiter then
          waitRes := wrError
        else begin
          SetLength(meAsFactors, 1);
          meAsFactors[0] := Self as ISynchro;
          wrap := TCompositeSynchro.Create(meAsFactors, FEventFactory, ConsumeOneSignalled,
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
end; { TModularSynchro.WaitFor }

constructor TModularEvent.Create(const ABase: IEvent; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseEvent := ABase;
  inherited Create(FBaseEvent, AUnionLock, AEventFactory);
end; { TModularEvent.Create }

function TModularEvent.ConsumeResource: TWaitResult;
begin
  if not (scManualEvent in FBaseEvent.Capabilities) then
    Result := FBaseEvent.WaitFor(0) // Equivalent to ResetEvent()
  else
    Result := wrSignaled;
end; { TModularEvent.ConsumeResource }

function TModularEvent.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  if scManualEvent in FBaseEvent.Capabilities then
    Include(Result, scManualEvent);
  Exclude(Result, scLight);
end; { TModularEvent.GetCapabilities }

function TModularEvent.IsResourceCounting: boolean;
begin
  Result := not (scManualEvent in FBaseEvent.Capabilities);
end; { TModularEvent.IsResourceCounting }

procedure TModularEvent.ResetEvent;
begin
  ObserverProc(
    procedure 
    begin
      FBaseEvent.ResetEvent;
    end);
end; { TModularEvent.ResetEvent }

procedure TModularEvent.SetEvent;
begin
  ObserverProc(
    procedure 
    begin
      FBaseEvent.SetEvent;
    end);
end; { TModularEvent.SetEvent }

procedure TModularEvent.Signal;
begin
  SetEvent;
end; { TModularEvent.Signal }

{ TModularSemaphore }

constructor TModularSemaphore.Create(
  const ABase: ISemaphore; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseSem := ABase;
  inherited Create(FBaseSem, AUnionLock, AEventFactory);
end; { TModularSemaphore.Create }

function TModularSemaphore.ConsumeResource: TWaitResult;
begin
  Result := FBaseSem.WaitFor(0);
end; { TModularSemaphore.ConsumeResource }

function TModularSemaphore.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TModularSemaphore.GetCapabilities }

function TModularSemaphore.InitialValue: cardinal;
begin
  Result := FBaseSem.InitialValue;
end; { TModularSemaphore.InitialValue }

function TModularSemaphore.IsResourceCounting: boolean;
begin
  Result := True;
end; { TModularSemaphore.IsResourceCounting }

procedure TModularSemaphore.Reset;
begin
  ObserverProc(
    procedure 
    begin
      FBaseSem.Reset;
    end);
end; { TModularSemaphore.Reset }

procedure TModularSemaphore.Signal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseSem.Signal;
    end);
end;  { TModularSemaphore.Signal }

function TModularSemaphore.Value: cardinal;
begin
  Result := FBaseSem.Value;
end; { TModularSemaphore.Value }

{ TModularCountDown }

constructor TModularCountDown.Create(
  const ABase: ICountDown; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseCountDown := ABase;
  inherited Create(FBaseCountDown, AUnionLock, AEventFactory);
end; { TModularCountDown.Create }

function TModularCountDown.ConsumeResource: TWaitResult;
begin
  Result := wrSignaled;
end; { TModularCountDown.ConsumeResource }

procedure TModularCountDown.CounterSignal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseCountDown.CounterSignal;
    end);
end; { TModularCountDown.CounterSignal }

function TModularCountDown.InitialValue: cardinal;
begin
  Result := FBaseCountDown.InitialValue;
end; { TModularCountDown.InitialValue }

function TModularCountDown.IsResourceCounting: boolean;
begin
  Result := False;
end; { TModularCountDown.IsResourceCounting }

procedure TModularCountDown.Signal;
begin
  ObserverProc(
    procedure 
    begin
      FBaseCountDown.Signal;
    end);
end;  { TModularCountDown.Signal }

function TModularCountDown.SignalHit: boolean;
var
  res: boolean;
begin
  ObserverProc(
    procedure 
    begin
      res := FBaseCountDown.SignalHit;
    end);
  Result := res;
end; { TModularCountDown.SignalHit }

function TModularCountDown.Allocate: cardinal;
var
  res: cardinal;
begin
  ObserverProc(
    procedure 
    begin
      res := FBaseCountDown.Allocate;
    end);
  Result := res;
end; { TModularCountDown.Allocate }

function TModularCountDown.Value: cardinal;
begin
  Result := FBaseCountDown.Value;
end; { TModularCountDown.Value }

end.
