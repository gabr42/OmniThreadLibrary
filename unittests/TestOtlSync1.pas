unit TestOtlSync1;

{$I OtlOptions.Inc}

interface

uses
  TestFramework, GpStuff, OtlContainers, SysUtils, SyncObjs,
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync, OtlTask;

type
  ISingleton = IInterface;

  TSingleton = class(TInterfacedObject, ISingleton)
  strict private class var
    FNumSingletons: TOmniAlignedInt32;
  strict protected
    class function GetNumSingletons: integer; static;
  public
    constructor Create;
    destructor Destroy; override;
    class property NumSingletons: integer read GetNumSingletons;
  end;

  // Test methods for basic synchronisation stuff
  TestOtlSync = class(TTestCase)
  strict private
    FUnalignedLock: packed record
      FFiller1   : byte;
      FSharedLock: TOmniCS;
      FFiller2   : word;
      FFiller3   : byte;
    end;
    FResourceCount: IOmniResourceCount;
    FSharedValue: int64;
  {$IFDEF OTL_Generics}
    FSingleton: TSingleton;
    FSingletonIntf: ISingleton;
  {$ENDIF OTL_Generics}
  strict protected
  {$IFDEF OTL_Generics}
    procedure Asy_AtomicInitIntf(const task: IOmniTask);
    procedure Asy_AtomicInit(const task: IOmniTask);
  {$ENDIF OTL_Generics}
    procedure Asy_LockCS(const task: IOmniTask);
    procedure Asy_ResourceCount(const task: IOmniTask);
  published
    procedure TestCSInitialization;
    procedure TestCSParallel;
    procedure TestCSLock;
    procedure TestResourceCountBasic;
  {$IFDEF OTL_Generics}
    procedure TestOptimisticInitialization;
    procedure TestOptimisticInitializationIntf;
  {$ENDIF OTL_Generics}
  end;

implementation

uses
  OtlTaskControl;

procedure TestOtlSync.TestCSInitialization;
var
  cs: TOmniCS;
  i: integer;

  procedure AcquireRelease;
  var
    cs: TOmniCS;
  begin
    cs.Acquire;
    cs.Release;
  end;

begin
  cs.Initialize;
  cs.Acquire;
  cs.Release;
  for i := 1 to 1000 do
    AcquireRelease;

  CheckTrue(true); // no crash = pass
end;

procedure Asy_InitializeCS(const task: IOmniTask);
var
  i: Integer;

  procedure AcquireRelease;
  var
    cs: TOmniCS;
  begin
    cs.Acquire;
    cs.Release;
  end;

begin
  for i := 1 to 1000 do
    AcquireRelease;
end;

procedure TestOtlSync.TestCSParallel;
var
  i: Integer;
  task: array [1..8] of IOmniTaskControl;
begin
  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_InitializeCS, 'Initialize CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckTrue(true); // no crash = pass
end;

procedure TestOtlSync.Asy_LockCS(const task: IOmniTask);
var
  i: Integer;
begin
  for i := 1 to 10000 do begin
    FUnalignedLock.FSharedLock.Acquire;
    Inc(FSharedValue);
    FUnalignedLock.FSharedLock.Release;
    FUnalignedLock.FSharedLock.Acquire;
    Dec(FSharedValue);
    FUnalignedLock.FSharedLock.Release;
  end;
end;

procedure TestOtlSync.TestCSLock;
var
  i: Integer;
  task: array [1..8] of IOmniTaskControl;
begin
  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_LockCS, 'Lock CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckEquals(0, FSharedValue);
end;

{$IFDEF OTL_Generics}
procedure TestOtlSync.Asy_AtomicInit(const task: IOmniTask);
begin
  WaitForSingleObject(Task.CancellationToken.Handle, INFINITE);
  Atomic<TSingleton>.Initialize(FSingleton,
    function: TSingleton begin Result := TSingleton.Create; end);
end;

procedure TestOtlSync.TestOptimisticInitialization;
var
  i      : integer;
  iRepeat: integer;
  task   : array [1..8] of IOmniTaskControl;
  token  : IOmniCancellationToken;
begin
  for iRepeat := 1 to {$IFDEF CONSOLE_TESTRUNNER}100{$ELSE}10{$ENDIF} do begin
    FreeAndNil(FSingleton);

    token := CreateOmniCancellationToken;
    for i := Low(task) to High(task) do
      task[i] := CreateTask(Asy_AtomicInit, 'AtomicInit #' + IntToStr(i)).CancelWith(token).Run;

    token.Signal;

    for i := Low(task) to High(task) do
      task[i].Terminate;

    CheckTrue(assigned(FSingleton), 'There is no singleton');
  end;
  CheckEquals(1, TSingleton.NumSingletons);
  FreeAndNil(FSingleton);
end;

procedure TestOtlSync.Asy_AtomicInitIntf(const task: IOmniTask);
begin
  WaitForSingleObject(Task.CancellationToken.Handle, INFINITE);
  Atomic<ISingleton>.Initialize(FSingletonIntf,
    function: ISingleton begin Result := TSingleton.Create; end);
end;

procedure TestOtlSync.TestOptimisticInitializationIntf;
var
  i      : integer;
  iRepeat: integer;
  task   : array [1..8] of IOmniTaskControl;
  token  : IOmniCancellationToken;
begin
  for iRepeat := 1 to {$IFDEF CONSOLE_TESTRUNNER}100{$ELSE}10{$ENDIF} do begin
    FSingletonIntf := nil;

    token := CreateOmniCancellationToken;
    for i := Low(task) to High(task) do
      task[i] := CreateTask(Asy_AtomicInitIntf, 'AtomicInitIntf #' + IntToStr(i)).CancelWith(token).Run;

    token.Signal;

    for i := Low(task) to High(task) do
      task[i].Terminate;

    CheckTrue(assigned(FSingletonIntf), 'There is no singleton');
  end;
  CheckEquals(1, TSingleton.NumSingletons);
  FSingletonIntf := nil;
end;
{$ENDIF OTL_Generics}

procedure TestOtlSync.Asy_ResourceCount(const task: IOmniTask);
begin
  FResourceCount.Allocate;
  FResourceCount.Release;
end; { TestOtlSync.Asy_ResourceCount }

procedure TestOtlSync.TestResourceCountBasic;
var
  i   : integer;
  task: array [1..8] of IOmniTaskControl;
begin
  FResourceCount := CreateResourceCount(4);

  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_ResourceCount, 'ResourceCount #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckEquals(3, FResourceCount.Allocate);
end;

constructor TSingleton.Create;
begin
  inherited Create;
  FNumSingletons.Increment;
end;

destructor TSingleton.Destroy;
begin
  FNumSingletons.Decrement;
  inherited;
end;

class function TSingleton.GetNumSingletons: integer;
begin
  Result := FNumSingletons;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestOtlSync.Suite);
end.
