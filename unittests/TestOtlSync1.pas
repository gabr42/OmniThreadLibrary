unit TestOtlSync1;

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils,
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync, OtlTask;

type
  // Test methods for basic synchronisation stuff
  TestOtlSync = class(TTestCase)
  strict private
    FUnalignedLock: packed record
      FFiller1   : byte;
      FSharedLock: TOmniCS;
      FFiller2   : word;
      FFiller3   : byte;
    end;
    FSharedValue: int64;
    FResourceCount: IOmniResourceCount;
    FQueuedCount: TOmniAlignedInt32;
  strict protected
    procedure LockCS(const task: IOmniTask);
    procedure ResourceAllocate(const task: IOmniTask);
    procedure ResourceCount(const task: IOmniTask);
    procedure ResourceRelease(const task: IOmniTask);
  published
    procedure TestCSInitialization;
    procedure TestCSParallel;
    procedure TestCSLock;
    procedure TestResourceCountBasic;
    procedure StressTestResourceCount;
  end;

implementation

uses
  OtlTaskControl;

const
  CResourceCountStressTest_sec = 30;

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
end;

procedure InitializeCS(const task: IOmniTask);
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
    task[i] := CreateTask(InitializeCS, 'Initialize CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;
end;

procedure TestOtlSync.LockCS(const task: IOmniTask);
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
    task[i] := CreateTask(LockCS, 'Lock CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  Assert(FSharedValue = 0);
end;

procedure TestOtlSync.ResourceCount(const task: IOmniTask);
begin
  FResourceCount.Allocate;
  FResourceCount.Release;
end; { TestOtlSync.ResourceCount }

procedure TestOtlSync.TestResourceCountBasic;
var
  i: Integer;
  task: array [1..8] of IOmniTaskControl;
begin
  FResourceCount := CreateResourceCount(4);

  for i := Low(task) to High(task) do
    task[i] := CreateTask(ResourceCount, 'ResourceCount s#' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  Assert(FResourceCount.Allocate = 3);
end;

procedure TestOtlSync.ResourceAllocate(const task: IOmniTask);
var
  startTime: int64;
  i: integer;
begin
  startTime := DSiTimeGetTime64;
  while not DSiHasElapsed64(startTime, CResourceCountStressTest_sec * 1000) do
    for i := 1 to 10 do begin
      FResourceCount.Allocate;
      FQueuedCount.Increment;
    end;
end;

procedure TestOtlSync.ResourceRelease(const task: IOmniTask);
var
  startTime: int64;
  i: integer;
begin
  startTime := DSiTimeGetTime64;
  while not DSiHasElapsed64(startTime, CResourceCountStressTest_sec * 1000) do
    if FQueuedCount.Value > 0 then begin
      FQueuedCount.Decrement;
      FResourceCount.Release;
    end;
end;

procedure TestOtlSync.StressTestResourceCount;
var
  alloc    : IOmniTaskControl;
  release  : IOmniTaskControl;
  startTime: int64;

  function WaitTime: integer;
  var
    wait: int64;
  begin
    wait := (CResourceCountStressTest_sec * 1000 + 1000) - DSiElapsedTime(startTime);
    if wait < 0 then
      Result := 0
    else
      Result := wait;
  end;

begin
  FResourceCount := CreateResourceCount(10);
  FQueuedCount.Value := 0;

  alloc := CreateTask(ResourceAllocate, 'ResourceAllocate');
  release := CreateTask(ResourceRelease, 'ResourceRelease');

  startTime := DSiTimeGetTime64;
  alloc.Run;
  release.Run;

  try
    Assert(alloc.WaitFor(WaitTime), 'ResourceAllocate did not terminate correctly');
  finally
    alloc.Terminate(0);
    try
      Assert(release.WaitFor(WaitTime), 'ResourceRelease did not terminate correctly');
    finally
      release.Terminate(0);
    end;
  end;
end; { TestOtlSync.StressTestResourceCount }

initialization
  // Register any test cases with the test runner
  RegisterTest(TestOtlSync.Suite);
end.
