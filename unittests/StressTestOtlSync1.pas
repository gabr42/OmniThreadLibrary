unit StressTestOtlSync1;

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils,
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync, OtlTask;

type
  // Test methods for basic synchronisation stuff
  TestOtlSync = class(TTestCase)
  strict private
    FResourceCount: IOmniResourceCount;
    FQueuedCount: TOmniAlignedInt32;
  strict protected
    procedure ResourceAllocate(const task: IOmniTask);
    procedure ResourceRelease(const task: IOmniTask);
  published
    procedure StressTestResourceCount;
  end;

implementation

uses
  System.SyncObjs,
  OtlTaskControl;

const
  CResourceCountStressTest_sec = 30;

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

procedure TestOtlSync.ResourceAllocate(const task: IOmniTask);
var
  i        : integer;
  startTime: int64;
begin
  startTime := DSiTimeGetTime64;
  // run this thread for 1 sec less than ResourceRelease - if resources are no longer
  // released, the code will hang in Allocate
  while not DSiHasElapsed64(startTime, (CResourceCountStressTest_sec - 2) * 1000) do
    for i := 1 to 10 do begin
      FResourceCount.Allocate;
      FQueuedCount.Increment;
    end;
end;

procedure TestOtlSync.ResourceRelease(const task: IOmniTask);
var
  startTime: int64;
begin
  startTime := DSiTimeGetTime64;
  while not DSiHasElapsed64(startTime, (CResourceCountStressTest_sec - 1) * 1000) do
    if FQueuedCount.Value > 0 then begin
      FQueuedCount.Decrement;
      FResourceCount.Release;
    end;
end;

procedure TestOtlSync.StressTestResourceCount;
var
  alloc    : TArray<IOmniTaskControl>;
  i        : integer;
  iAlloc   : integer;
  iRelease : integer;
  release  : TArray<IOmniTaskControl>;
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

  for iAlloc := 1 to 3 do begin
    SetLength(alloc, iAlloc);
    for iRelease := 1 to 3 do begin
      SetLength(release, iRelease);
      for i := Low(alloc) to High(alloc) do
        alloc[i] := CreateTask(ResourceAllocate, 'ResourceAllocate');
      for i := Low(release) to High(release) do
        release[i] := CreateTask(ResourceRelease, 'ResourceRelease');
      startTime := DSiTimeGetTime64;
      for i := Low(alloc) to High(alloc) do
        alloc[i].Run;
      for i := Low(release) to High(release) do
        release[i].Run;
    end;
    try
      for i := Low(alloc) to High(alloc) do
        Assert(alloc[i].WaitFor(WaitTime), 'ResourceAllocate did not terminate correctly');
    finally
      for i := Low(alloc) to High(alloc) do
        alloc[i].Terminate(0);
      try
        for i := Low(release) to High(release) do
          Assert(release[i].WaitFor(WaitTime), 'ResourceRelease did not terminate correctly');
      finally
        for i := Low(release) to High(release) do
          release[i].Terminate(0);
      end;
    end;
  end;
end; { TestOtlSync.StressTestResourceCount }

initialization
  // Register any test cases with the test runner
  RegisterTest(TestOtlSync.Suite);
end.
