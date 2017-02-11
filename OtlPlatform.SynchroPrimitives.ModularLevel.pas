unit OtlPlatform.SynchroPrimitives.ModularLevel;
// IMPORTANT!
//  READ THE COMMENTS IN UNIT Otl.Parallel.SynchroPrimitives .

{$I OtlOptions.inc}

interface
uses OtlPlatform.SynchroPrimitives.InterfaceLevel, System.SyncObjs
   , System.Classes, System.SysUtils, System.Generics.Collections
   , OtlPlatform.SynchroPrimitives.ConditionVariables
 {$IFDEF MSWINDOWS}
   , Winapi.Windows
 {$ENDIF}
   , OtlPlatform.Atomic;

const
  FOREVER = INFINITE;
  MaxCardinal: cardinal = cardinal( -1);

type
  ISimpleConditionEvent = interface ['{1EAEEE8F-54E1-44B5-BE3C-AFC14A465F74}']
    procedure Pulse;
    function  WaitFor: TWaitResult;
  end;

  ICompositeSynchro = interface
    function WaitFor(
      var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;

    function Factors: TSynchroArray;
    function Datum: TObject;
    function isSignalled: boolean;
  end;

type
  ISynchroExInternal = interface;
  ISynchroObserver = interface ['{C1EBE331-613F-4D0F-B645-CF2925F55B22}']
    procedure PossibleStateChange( const Source: ISynchroExInternal; Token: TObject);
  end;

  ISynchroExInternal = interface( ISynchro) ['{A3D01DFF-CF6A-4913-8A34-E46C4C0FCF5F}']
    function  NativeMultiwaitObject: TObject;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  NativeMultiwait( const Synchros: array of TObject;
                               Timeout: cardinal; AAll: boolean;
                               var SignallerIndex: integer): TWaitResult;
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  LockingMechanism: TLockingMechanism;
    function  IsResourceCounting: boolean;
    function  ConsumeResource: TWaitResult;
    procedure Enrol( const Observer: ISynchroObserver; Token: TObject);
    procedure Unenrol( ObserverToken: TObject);
    function  UnionLock: ILock;
  end;

  TWaitPropagation = (NoConsume, ConsumeOneSignalled, ConsumeAllSignalled);
  TConditionTest = reference to function(
    var   SignallerIdx: integer;
    const Peers: TSynchroArray; Datum: TObject): boolean;
  TTestClass = (TestAny, TestAll, TestCustom);

  TCompositeSynchro = class( TInterfacedObject, ICompositeSynchro, ISynchro, ISynchroExInternal)
  private type
    TImplementation = ({$IFDEF MSWINDOWS}Direct, {$ENDIF} Indirect, Solo);

  private
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}

    procedure Signal;
    function  isSignalled: boolean;
    function  isSignalled_IsSupported: boolean;
    function  SignalState: OtlPlatform.SynchroPrimitives.InterfaceLevel.TSignalState;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;                              overload;
    function  WaitFor( var SignallerIdx: integer; TimeOut: cardinal = FOREVER): TWaitResult;    overload;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  IsModular: boolean;
    function  LockingMechanism: TLockingMechanism;

    function  Factors: TSynchroArray;
    function  Datum: TObject;
    function  NativeMultiwaitObject: TObject;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  NativeMultiwait( const Synchros: array of TObject;
                               Timeout: cardinal; AAll: boolean;
                               var SignallerIndex: integer): TWaitResult;
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  IsResourceCounting: boolean;
    function  ConsumeResource: TWaitResult;
    procedure Enrol( const Observer: ISynchroObserver; Token: TObject);
    procedure Unenrol( ObserverToken: TObject);
    function  UnionLock: ILock;
    procedure BubbleUp( var Me: ISynchroExInternal);

  public
    constructor Create( const AFactors: TSynchroArray; AEventFactory: TSynchroFactory;
                        APropagation: TWaitPropagation; ATest: TConditionTest;
                        ATestClass: TTestClass; AAllowSolo: boolean);
    destructor Destroy; override;
    function   AsMWObject: TObject;

    class function IsAll( var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
    class function IsAny( var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
    class function AnyTest: TConditionTest;
    class function AllTest: TConditionTest;

  private
    [Volatile] FisSignalled: boolean;
    FFactors: TSynchroArray;
    FMemberCount: integer;
    FEventFactory: TSynchroFactory;
    FLock: ILock;
    FDatum: TObject;
    FCondVar: OtlPlatform.SynchroPrimitives.ConditionVariables.TSBDConditionVariable;
    FImplementation: TImplementation;
    FPropagation: TWaitPropagation;
    FTest: TConditionTest;
    FTestClass: TTestClass;
    FAllowSolo: boolean;
    FHasAtLeastOneResourceCounter: boolean;
    FObservers: TDictionary<TObject,ISynchroObserver>;
    FRegistered: boolean;

  private type
    TMemberObserver = class( TInterfacedObject, ISynchroObserver)
    private
      {$IFDEF AUTOREFCOUNT} [weak] {$ENDIF}
      [Volatile] FOwner: TCompositeSynchro;
      procedure PossibleStateChange( const Source: ISynchroExInternal; Token: TObject);
    public
      constructor Create( AOwner: TCompositeSynchro);
    end;

  private
    FMemberObserver: TMemberObserver;

    {$IFDEF MSWINDOWS}
      FSynchroObjs: array of TObject;   // Actually array of THandle.
    {$ENDIF}
  end;



  TModularSynchro = class abstract( TInterfacedObject, ISynchro, ISynchroExInternal)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}

    procedure Signal;                                    virtual;
    function  isSignalled: boolean;
    function  isSignalled_IsSupported: boolean;
    function  SignalState: OtlPlatform.SynchroPrimitives.InterfaceLevel.TSignalState;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult;     virtual;
    function  AsObject: TObject;
    function  IsPoolManaged: boolean;
    function  IsModular: boolean;
    function  NativeMultiwaitObject: TObject;
    function  IsCompatibleNativeMWObject( Reference, Peer: TObject): boolean;
    function  NativeMultiwait( const Synchros: array of TObject;
                               Timeout: cardinal; AAll: boolean;
                               var SignallerIndex: integer): TWaitResult;
    procedure RegisterDirectClient( Delta: integer);
    procedure RegisterIndirectClient( Delta: integer);
    procedure RegisterDedicatedSoleIndirectClient( Delta: integer);
    function  PermitsDirectClients: boolean;
    function  PermitsIndirectClients: boolean;
    function  PermitsDedicatedSoleIndirectClient: boolean;
    function  LockingMechanism: TLockingMechanism;
    function  IsResourceCounting: boolean;              virtual;
    function  ConsumeResource: TWaitResult;             virtual;
    procedure Enrol( const Observer: ISynchroObserver; Token: TObject);
    procedure Unenrol( ObserverToken: TObject);
    function  UnionLock: ILock;
    procedure ObserverProc( Proc: TProc);

  public
    FUnionLock: ILock;
    FBase  : ISynchro;
    FBaseEx: ISynchroExInternal;
    FEventFactory: TSynchroFactory;

    constructor Create( const ABase: ISynchro; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
    destructor Destroy; override;
    function AsMWObject: TObject;

  private
    [Volatile] FDirectWaiters  : TVolatileInt32;
    [Volatile] FIndirectWaiters: TVolatileInt32;
    FObservers: TDictionary<TObject,ISynchroObserver>;
    FDedicatedToOneIndirectWaiter: boolean;
  end;

  TModularEvent = class( TModularSynchro, IEvent)
  protected
    procedure SetEvent;
    procedure ResetEvent;
    function  isManualEvent: boolean;
    function  isLight: boolean;
    procedure Signal;                                                  override;
    function  IsResourceCounting: boolean;                             override;
    function  ConsumeResource: TWaitResult;                            override;

  public
    FBaseEvent: IEvent;
    constructor Create( const ABase: IEvent; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
  end;


 TModularSemaphore = class( TModularSynchro, ISemaphore)
  protected
    function  InitialValue: cardinal;
    function  Value: cardinal;
    procedure Reset;
    function  isValueTesting_IsSupported: boolean;
    procedure Signal;                                                  override;
    function  IsResourceCounting: boolean;                             override;
    function  ConsumeResource: TWaitResult;                            override;

  public
    FBaseSem: ISemaphore;
    constructor Create( const ABase: ISemaphore; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
  end;


 TModularCountDown = class( TModularSynchro, ICountDown)
  protected
    function  InitialValue: cardinal;
    function  Value: cardinal;
    function  SignalHit: boolean;
    procedure CounterSignal;
    function  Allocate: cardinal;
    procedure Signal;                                                  override;
    function  IsResourceCounting: boolean;                             override;
    function  ConsumeResource: TWaitResult;                            override;

  public
    FBaseCountDown: ICountDown;
    constructor Create( const ABase: ICountDown; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
  end;



implementation






uses System.Diagnostics, OtlPlatform.Errors;

constructor TCompositeSynchro.Create(
  const AFactors: TSynchroArray; AEventFactory: TSynchroFactory;
  APropagation: TWaitPropagation; ATest: TConditionTest;
  ATestClass: TTestClass; AAllowSolo: boolean);
var
  i: integer;
  Ok: boolean;
  Memb: ISynchroExInternal;
  Dummy: integer;
  MWObj: TObject;
begin
  FRegistered := False;
  // Test for the direct solution
  FMemberCount := Length( AFactors);
  if FMemberCount = 0 then
    raise TParallelException.Create( ECompositeNeedsOneFactor);
  FPropagation := APropagation;
  FDatum       := Datum;
  FTest        := ATest;
  FTestClass   := ATestClass;
  FObservers := TDictionary<TObject,ISynchroObserver>.Create;
  SetLength( FFactors, FMemberCount);
  FEventFactory := AEventFactory;
  FAllowSolo := AAllowSolo;

  case FTestClass of
    TestAny   : FTest := IsAny;
    TestAll   : FTest := IsAll;
  end;

  for i := 1 to FMemberCount - 1 do
    begin
    FFactors[ i] := AFactors[i];
    if not FFactors[ i].IsModular then
      raise TParallelException.Create( EOnlyModularCombinable);
    end;

  if Supports( FFactors[0], ISynchroExInternal, Memb) then
      begin
      MWObj := Memb.NativeMultiwaitObject;
      FLock := Memb.UnionLock
      end
    else
      begin
      MWObj := nil;
      FLock := nil
      end;
  Ok := True;
  for i := 1 to FMemberCount - 1 do
    begin
    if Supports( FFactors[i], ISynchroExInternal, Memb) then
        begin
        Ok := Ok and assigned( FLock) and (FLock = Memb.UnionLock);
        if assigned( MWObj) and Memb.IsCompatibleNativeMWObject( MWObj, Memb.NativeMultiwaitObject) then
          MWObj := nil
        end
    end;
  if (not Ok) or (not assigned( FLock.AsCriticalSection)) then
    FLock := nil;
  if assigned( FLock)
    {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
      and assigned( FEventFactory)
    {$ENDIF} {$ENDIF}  then
      FCondVar := TSBDConditionVariable.Create(
          {$IFNDEF USE_EMBARCADERO_TConditionVariableCS} {$IFNDEF MSWINDOWS}
            AEventFactory, True,
          {$ENDIF} {$ENDIF}
            FLock)
    else
      FCondVar := nil;

  Ok := FMemberCount = 1;
  if Ok then
      FImplementation := Solo
    else
      begin
      {$IFDEF MSWINDOWS}
      if assigned( MWObj) then
        for i := 0 to FMemberCount - 1 do
          begin
          Ok := Supports( FFactors[i], ISynchroExInternal, Memb) and
                Memb.PermitsDirectClients and
                (Memb.NativeMultiwaitObject <> nil) and
                (((FPropagation = ConsumeOneSignalled) and (FTestClass = TestAny)) or
                 ((FPropagation = ConsumeAllSignalled) and (FTestClass = TestAll)));
          if not Ok then break
          end;
      if Ok then
        begin
        FImplementation := Direct;
        FreeAndNil( FCondVar)
        end;
      {$ENDIF}
      end;

  FHasAtLeastOneResourceCounter := False;
  if (not Ok) and assigned( FCondVar) then
    begin
    for i := 0 to FMemberCount - 1 do
      begin
      FHasAtLeastOneResourceCounter := Supports( FFactors[i], ISynchroExInternal, Memb) and
                                       Memb.IsResourceCounting;
      if FHasAtLeastOneResourceCounter then break
      end;
    for i := 0 to FMemberCount - 1 do
      begin
      Ok := Supports( FFactors[i], ISynchroExInternal, Memb) and
            Memb.PermitsIndirectClients;
      if Ok and (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
        Ok := Memb.PermitsDedicatedSoleIndirectClient;
      if not Ok then break
      end;
    FImplementation := Indirect
    end;

  if Ok and (FImplementation = Solo) and (not AAllowSolo) then
    Ok := False;

  if not Ok then
    raise TParallelException.Create( ECompositeSynchroMixedBag);

  case FTestClass of
    TestAny   : FisSignalled := IsAny( Dummy, FFactors, nil);
    TestAll   : FisSignalled := IsAll( Dummy, FFactors, nil);
    TestCustom: FisSignalled := FTest( Dummy, FFactors, FDatum);
  end;

  case FImplementation of
    {$IFDEF MSWINDOWS}
    Direct:
      begin
      SetLength( FSynchroObjs, FMemberCount);
      for i := 0 to FMemberCount - 1 do
        begin
        if Supports( FFactors[i], ISynchroExInternal, Memb) then
          FSynchroObjs[i] := Memb.NativeMultiwaitObject;
        Memb.RegisterDirectClient( +1);
        end;
      if FTestClass <> TestCustom then
        FTest := nil;
      FRegistered := True
      end;
    {$ENDIF}

    Indirect, Solo:
      begin
      FMemberObserver := TMemberObserver.Create( self);
      FMemberObserver._AddRef;
      for i := 0 to FMemberCount - 1 do
        begin
        if not Supports( FFactors[i], ISynchroExInternal, Memb) then continue;
        if (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
            Memb.RegisterDedicatedSoleIndirectClient( +1)
          else
            Memb.RegisterIndirectClient( +1);
        Memb.Enrol( FMemberObserver, TObject( i))
        end;
      FRegistered := True;
      if not assigned( FLock) then
        begin
        if assigned( FEventFactory) then
            FLock := FEventFactory.AcquireCriticalSection( True)
          else
            FLock := TSynchroFactory.AcquireCriticalSection
        end
      end
  end
end;

destructor TCompositeSynchro.Destroy;
var
  i: integer;
  Memb: ISynchroExInternal;
begin
  if assigned( FLock) then
    FLock.Enter;
  FCondVar.Free;
  if FRegistered then
    case FImplementation of
      {$IFDEF MSWINDOWS}
      Direct:
        begin
        for i := 0 to FMemberCount - 1 do
          if Supports( FFactors[i], ISynchroExInternal, Memb) then
            Memb.RegisterDirectClient( -1);
        end;
      {$ENDIF}

      Indirect, Solo:
        begin
        for i := 0 to FMemberCount - 1 do
          begin
          if not Supports( FFactors[i], ISynchroExInternal, Memb) then continue;
          if (FPropagation = ConsumeAllSignalled) and FHasAtLeastOneResourceCounter then
              Memb.RegisterDedicatedSoleIndirectClient( -1)
            else
              Memb.RegisterIndirectClient( -1);
          Memb.Unenrol( TObject( i))
          end
        end
    end;
  SetLength( FFactors, 0);
  FTest := nil;
  if assigned( FMemberObserver) then
    begin
    FMemberObserver.FOwner := nil;
    FMemberObserver._Release;
    FMemberObserver := nil
    end;
  FObservers.Free;
  {$IFDEF MSWINDOWS}
    SetLength( FSynchroObjs, 0);
  {$ENDIF}
  inherited;
  if assigned( FLock) then
    FLock.Leave;
  FLock := nil
end;


class function TCompositeSynchro.AllTest: TConditionTest;
begin
  result := function( var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean
    begin
      result := IsAll( SignallerIdx, Peers, Datum)
    end
end;

class function TCompositeSynchro.AnyTest: TConditionTest;
begin
  result := function( var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean
    begin
      result := IsAny( SignallerIdx, Peers, Datum)
    end
end;

function TCompositeSynchro.AsMWObject: TObject;
begin
  result := nil
end;

function TCompositeSynchro.AsObject: TObject;
begin
  result := self
end;

function TCompositeSynchro.Datum: TObject;
begin
  result := FDatum
end;

function TCompositeSynchro.ConsumeResource: TWaitResult;
begin
  result := WaitFor( 0)
end;

procedure TCompositeSynchro.Enrol(
  const Observer: ISynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue( Token, Observer)
end;

function TCompositeSynchro.Factors: TSynchroArray;
begin
  result := FFactors
end;

{$IFDEF MSWINDOWS}
function TCompositeSynchro.GetHandle: THandle;
begin
  result := 0
end;
{$ENDIF}

class function TCompositeSynchro.IsAll(
  var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
var
  Member: ISynchro;
begin
  result := True;
  for Member in Peers do
  begin
    result := Member.SignalState = esSignalled;
    if not result then break
  end
end;

class function TCompositeSynchro.IsAny(
  var SignallerIdx: integer; const Peers: TSynchroArray; Datum: TObject): boolean;
var
  Idx: integer;
begin
  result       := False;
  SignallerIdx := -1;
  for Idx := 0 to Length( Peers) - 1 do
  begin
    result := Peers[ Idx].SignalState = esSignalled;
    if not result then continue;
    SignallerIdx := Idx;
    break
  end
end;

function TCompositeSynchro.IsCompatibleNativeMWObject(
  Reference, Peer: TObject): boolean;
begin
  result := False
end;

function TCompositeSynchro.IsModular: boolean;
begin
  result := True
end;

function TCompositeSynchro.IsPoolManaged: boolean;
begin
  result := False
end;

function TCompositeSynchro.IsResourceCounting: boolean;
begin
  result := False
end;

function TCompositeSynchro.isSignalled: boolean;
begin
  result := FIsSignalled
end;

function TCompositeSynchro.isSignalled_IsSupported: boolean;
begin
  result := True
end;

function TCompositeSynchro.LockingMechanism: TLockingMechanism;
var
  isKernal: boolean;
  {$IFDEF MSWINDOW}
  Memb: ISynchroExInternal;
  {$ENDIF}
begin
  isKernal := False;
  case self.FImplementation of
    {$IFDEF MSWINDOW}
    Direct: isKernal := (Length( FFactors) > 0) and
                         Supports( FFactors[0], ISynchroExInternal, Memb) and
                         (Memb.LockingMechanism = KernalLocking);
    {$ENDIF}
    Indirect, Solo: isKernal := assigned( FLock) and FLock.IsKernalMode
  end;
  if isKernal then
      result := KernalLocking
    else
      result := BusLocking
end;

function TCompositeSynchro.NativeMultiwait(
  const Synchros: array of TObject; Timeout: cardinal; AAll: boolean; var SignallerIndex: integer): TWaitResult;
begin
  result := wrError
end;

function TCompositeSynchro.NativeMultiwaitObject: TObject;
begin
  result := nil
end;

function TCompositeSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  result := False
end;

function TCompositeSynchro.PermitsDirectClients: boolean;
begin
  result := False
end;

function TCompositeSynchro.PermitsIndirectClients: boolean;
begin
  result := True
end;

procedure TCompositeSynchro.RegisterDedicatedSoleIndirectClient( Delta: integer);
begin
end;

procedure TCompositeSynchro.RegisterDirectClient( Delta: integer);
begin
end;

procedure TCompositeSynchro.RegisterIndirectClient( Delta: integer);
begin
end;

procedure TCompositeSynchro.Signal;
begin
  if (FImplementation in [Indirect, Solo]) and assigned( FCondVar) then
    FCondVar.Pulse
end;

function TCompositeSynchro.SignalState: OtlPlatform.SynchroPrimitives.InterfaceLevel.TSignalState;
begin
  if FisSignalled then
      result := esSignalled
    else
      result := esNotSignalled
end;

procedure TCompositeSynchro.Unenrol( ObserverToken: TObject);
begin
  FObservers.Remove( ObserverToken)
end;

function TCompositeSynchro.UnionLock: ILock;
begin
  result := FLock
end;


procedure TCompositeSynchro.BubbleUp( var Me: ISynchroExInternal);
var
  Obs: TPair<TObject,ISynchroObserver>;
  Dummy: integer;
begin
  if FTest( Dummy, FFactors, FDatum) <> FisSignalled  then
    begin
    FisSignalled := not FisSignalled;
    if assigned( FObservers) then
      begin
      if not assigned( Me) then
        Me := self;
      for Obs in FObservers do
        Obs.Value.PossibleStateChange( Me, Obs.Key)
      end
    end
end;


function TCompositeSynchro.WaitFor(
  var SignallerIdx: integer; TimeOut: cardinal): TWaitResult;
var
  Me: ISynchroExInternal;
  Idx: integer;
  ImproperConstructionDetected: boolean;
begin
  Idx := -1;
  result := wrError;
  ImproperConstructionDetected := False;
  case FImplementation of
    Indirect, Solo:
      begin
      result := FCondVar.WaitFor( TimeOut,

        function( doAquireResource: boolean): boolean
          var
            ConsumedMembers, i: integer;
            Synchro: ISynchroExInternal;
            Confirmed: boolean;
          begin
            result := FTest( Idx, FFactors, FDatum);
            if result and doAquireResource and
               (FPropagation <> NoConsume) and FHasAtLeastOneResourceCounter then
              begin
              ConsumedMembers := 0;
              for i := 0 to FMemberCount - 1 do
                begin
                if (((FPropagation = ConsumeOneSignalled) and (i = Idx)) or
                     (FPropagation = ConsumeAllSignalled)) and
                  Supports( FFactors[i], ISynchroExInternal, Synchro) and
                  Synchro.isSignalled and Synchro.IsResourceCounting then
                    begin
                    Confirmed := Synchro.ConsumeResource = wrSignaled;
                    Inc( ConsumedMembers);
                    if Confirmed then continue;
                    result := False;
                    if (FPropagation = ConsumeOneSignalled) or
                      ((FPropagation = ConsumeAllSignalled) and (ConsumedMembers = 1)) then
                        begin
                        // We have a competitor waiter on one of the members and the competitor
                        //  slipped in ahead of us, so loop back and try again.
                        end
                      else
                        begin
                        // This is an error. We should never get here if TConditionEvent is properly constructed.
                        // To prevent the possibility of this condition, we disallow members which have
                        //  multiple waiters (including TConditionEvent) and where the propagation is ConsumeAllSignalled
                        //  and at least one of the members are resource-counting synchros.
                        ImproperConstructionDetected := True;
                        result := True
                        end;
                    break
                    end
                end
             end
          end,

        procedure
          begin
            BubbleUp( Me)
          end);

      if ImproperConstructionDetected then
        result := wrError;

      if result = wrSignaled then
        SignallerIdx := Idx
      end;

    {$IFDEF MSWINDOWS}
    Direct:
      begin
      result := (FFactors[0] as ISynchroExInternal).NativeMultiwait( FSynchroObjs, TimeOut, FTestClass = TestAll, SignallerIdx);
      if result in [wrSignaled, wrTimeOut] then
        begin
        FLock.Enter;
        BubbleUp( Me);
        FLock.Leave
        end
      end
    {$ENDIF}
    end;
end;



function TCompositeSynchro.WaitFor( Timeout: cardinal): TWaitResult;
var
  Dummy: integer;
begin
  result := WaitFor( Dummy, TimeOut)
end;


constructor TCompositeSynchro.TMemberObserver.Create( AOwner: TCompositeSynchro);
begin
  FOwner := AOwner
end;

procedure TCompositeSynchro.TMemberObserver.PossibleStateChange(
  const Source: ISynchroExInternal; Token: TObject);
begin
  // Index of signaller is integer( Token)
  FOwner.Signal
end;




constructor TModularSynchro.Create(
  const ABase: ISynchro; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FUnionLock := AUnionLock;
  FBase      := ABase;
  Supports( FBase, ISynchroExInternal, FBaseEx);
  FDirectWaiters.Initialize( 0);
  FIndirectWaiters.Initialize( 0);
  FObservers := TDictionary<TObject,ISynchroObserver>.Create;
  FDedicatedToOneIndirectWaiter := False;
  FEventFactory := AEventFactory
end;

destructor TModularSynchro.Destroy;
begin
  FDirectWaiters.Finalize;
  FIndirectWaiters.Finalize;
  FObservers.Free;
  inherited
end;


function TModularSynchro.AsMWObject: TObject;
begin
  result := FBase.AsMWObject
end;

function TModularSynchro.AsObject: TObject;
begin
  result := self
end;

function TModularSynchro.ConsumeResource: TWaitResult;
begin
  result := FBase.WaitFor( 0)
end;

procedure TModularSynchro.Enrol(
  const Observer: ISynchroObserver; Token: TObject);
begin
  FObservers.AddOrSetValue( Token, Observer)
end;

{$IFDEF MSWINDOWS}
function TModularSynchro.GetHandle: THandle;
begin
  if assigned( FBase) then
      result := FBase.Handle
    else
      result := 0
end;
{$ENDIF}

function TModularSynchro.IsCompatibleNativeMWObject(
  Reference, Peer: TObject): boolean;
begin
  {$IFDEF MSWINDOWS}
  result := assigned( Reference) and assigned( Peer) and
            (Reference.ClassType = Peer.ClassType) and
            (Reference <> Peer) and
            (Reference is THandleObject) and (Peer is THandleObject)
  {$ELSE}
  result := False
  {$ENDIF}
end;

function TModularSynchro.IsModular: boolean;
begin
  result := True
end;

function TModularSynchro.IsPoolManaged: boolean;
begin
  result := False
end;

function TModularSynchro.IsResourceCounting: boolean;
begin
  result := False
end;

function TModularSynchro.isSignalled: boolean;
begin
  result := FBase.isSignalled
end;

function TModularSynchro.isSignalled_IsSupported: boolean;
begin
  result := FBase.isSignalled_IsSupported
end;

function TModularSynchro.LockingMechanism: TLockingMechanism;
begin
  result := KernalLocking
end;

function TModularSynchro.NativeMultiwait(
  const Synchros: array of TObject; Timeout: cardinal; AAll: boolean; var SignallerIndex: integer): TWaitResult;
{$IFDEF MSWINDOWS}
var
  HandleObjs: THandleObjectArray;
  L, i: integer;
  Obj: TObject;
  Ok: boolean;
  SignaledObj: THandleObject;
{$ENDIF}
begin
  SignallerIndex := -1;
  {$IFDEF MSWINDOWS}
  L  := Length( Synchros);
  Ok := False;
  SetLength( HandleObjs, L);
  for i := 0 to L - 1 do
    begin
    Obj := Synchros[i];
    Ok  := assigned( Obj) and (Obj is THandleObject);
    if not Ok then break;
    HandleObjs[i] := THandleObject( Obj)
    end;
  if Ok and (L >= 2) then
      begin
      result := THandleObject.WaitForMultiple( HandleObjs, Timeout, AAll, SignaledObj, False, 0);
      if (result = wrSignaled) and (not AAll) then
        for i := 0 to L - 1 do
          begin
          if HandleObjs[i] <> SignaledObj then continue;
          SignallerIndex := i;
          break
          end
      end
    else
  {$ENDIF}
      result := wrError
end;

function TModularSynchro.NativeMultiwaitObject: TObject;
begin
  result := nil
end;

procedure TModularSynchro.ObserverProc( Proc: TProc);
// This method assumes that RefCount > 0, so don't call from destructor.
var
  Obs: TPair<TObject,ISynchroObserver>;
  Me : ISynchroExInternal;
begin
  Me := self as ISynchroExInternal;
  FUnionLock.Enter;
  try
    Proc;
    for Obs in FObservers do
      Obs.Value.PossibleStateChange( Me, Obs.Key)
  finally
    FUnionLock.Leave
    end
end;

function TModularSynchro.PermitsDedicatedSoleIndirectClient: boolean;
begin
  result := (FIndirectWaiters.Read = 0) and (FDirectWaiters.Read = 0)
end;

function TModularSynchro.PermitsDirectClients: boolean;
{$IFDEF MSWINDOWS}
var
  Obj: TObject;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  result := (FIndirectWaiters = 0) and (not FDedicatedToOneIndirectWaiter);
  if result then
    begin
    Obj := FBase.AsMWObject;
    result := assigned( Obj) and (Obj is THandleObject)
    end
  {$ELSE}
  result := False
  {$ENDIF}
end;

function TModularSynchro.PermitsIndirectClients: boolean;
begin
  result := FDirectWaiters.Read = 0
end;

procedure TModularSynchro.RegisterDedicatedSoleIndirectClient( Delta: integer);
begin
  if Delta > 0 then
      FDedicatedToOneIndirectWaiter := True
    else if Delta < 0 then
      FDedicatedToOneIndirectWaiter := False;
  RegisterIndirectClient( Delta)
end;

procedure TModularSynchro.RegisterDirectClient( Delta: integer);
begin
  FDirectWaiters.Add( Delta)
end;

procedure TModularSynchro.RegisterIndirectClient( Delta: integer);
begin
  FInDirectWaiters.Add( Delta)
end;

procedure TModularSynchro.Signal;
begin
end;

function TModularSynchro.SignalState: OtlPlatform.SynchroPrimitives.InterfaceLevel.TSignalState;
begin
  result := FBase.SignalState
end;

procedure TModularSynchro.Unenrol( ObserverToken: TObject);
begin
  FObservers.Remove( ObserverToken)
end;

function TModularSynchro.UnionLock: ILock;
begin
  result := FUnionLock
end;

function TModularSynchro.WaitFor( Timeout: cardinal): TWaitResult;
var
  Wrap: ISynchro;
  MeAsFactors: TSynchroArray;
  WR: TWaitResult;
  AnyTest: TConditionTest;
begin
  WR := wrIOCompletion;
  AnyTest := TCompositeSynchro.AnyTest();
  ObserverProc( procedure begin
    if FIndirectWaiters.Read > 0 then
        begin
        if FDedicatedToOneIndirectWaiter then
            WR := wrError
          else
            begin
            SetLength( MeAsFactors, 1);
            MeAsFactors[0] := self as ISynchro;
            Wrap := TCompositeSynchro.Create( MeAsFactors, FEventFactory, ConsumeOneSignalled,
                          AnyTest, TestAny, True);
            WR   := wrSignaled
            end
        end
      else
        begin
        FDirectWaiters.Increment;
        WR := wrSignaled
        end
    end);
  case WR of
    wrSignaled:
      begin
      if assigned( Wrap) then
          result := Wrap.WaitFor( Timeout)
        else
          result := FBase.WaitFor( Timeout)
      end;

    else // wrTimeout, wrAbandoned, wrError, wrIOCompletion:
      result := wrError
    end
end;




constructor TModularEvent.Create( const ABase: IEvent; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseEvent := ABase;
  inherited Create( FBaseEvent, AUnionLock, AEventFactory)
end;


function TModularEvent.ConsumeResource: TWaitResult;
begin
  if not FBaseEvent.isManualEvent then
      result := FBaseEvent.WaitFor( 0) // Equivalent to ResetEvent()
    else
      result := wrSignaled
end;


function TModularEvent.isLight: boolean;
begin
  result := False
end;

function TModularEvent.isManualEvent: boolean;
begin
  result := FBaseEvent.isManualEvent
end;

function TModularEvent.IsResourceCounting: boolean;
begin
  result := not FBaseEvent.isManualEvent
end;

procedure TModularEvent.ResetEvent;
begin
  ObserverProc( procedure begin
    FBaseEvent.ResetEvent
    end)
end;

procedure TModularEvent.SetEvent;
begin
  ObserverProc( procedure begin
    FBaseEvent.SetEvent
    end)
end;

procedure TModularEvent.Signal;
begin
  SetEvent
end;





constructor TModularSemaphore.Create(
  const ABase: ISemaphore; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseSem := ABase;
  inherited Create( FBaseSem, AUnionLock, AEventFactory)
end;

function TModularSemaphore.ConsumeResource: TWaitResult;
begin
  result := FBaseSem.WaitFor( 0)
end;


function TModularSemaphore.InitialValue: cardinal;
begin
  result := FBaseSem.InitialValue
end;

function TModularSemaphore.IsResourceCounting: boolean;
begin
  result := True
end;

function TModularSemaphore.isValueTesting_IsSupported: boolean;
begin
  result := FBaseSem.isValueTesting_IsSupported
end;

procedure TModularSemaphore.Reset;
begin
  ObserverProc( procedure begin
    FBaseSem.Reset
    end)
end;

procedure TModularSemaphore.Signal;
begin
  ObserverProc( procedure begin
    FBaseSem.Signal
    end)
end;

function TModularSemaphore.Value: cardinal;
begin
  result := FBaseSem.Value
end;


constructor TModularCountDown.Create(
  const ABase: ICountDown; const AUnionLock: ILock; AEventFactory: TSynchroFactory);
begin
  FBaseCountDown := ABase;
  inherited Create( FBaseCountDown, AUnionLock, AEventFactory)
end;


function TModularCountDown.ConsumeResource: TWaitResult;
begin
  result := wrSignaled
end;

procedure TModularCountDown.CounterSignal;
begin
  ObserverProc( procedure begin
    FBaseCountDown.CounterSignal
    end)
end;

function TModularCountDown.InitialValue: cardinal;
begin
  result := FBaseCountDown.InitialValue
end;

function TModularCountDown.IsResourceCounting: boolean;
begin
  result := False
end;

procedure TModularCountDown.Signal;
begin
  ObserverProc( procedure begin
    FBaseCountDown.Signal
    end)
end;

function TModularCountDown.SignalHit: boolean;
var
  Res: boolean;
begin
  ObserverProc( procedure begin
    res := FBaseCountDown.SignalHit
    end);
  result := Res
end;


function TModularCountDown.Allocate: cardinal;
var
  Res: cardinal;
begin
  ObserverProc( procedure begin
    res := FBaseCountDown.Allocate
    end);
  result := Res
end;


function TModularCountDown.Value: cardinal;
begin
  result := FBaseCountDown.Value
end;

end.
