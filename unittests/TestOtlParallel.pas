unit TestOtlParallel;

interface

{$IFDEF Unicode}
uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils;

type
  // Test methods for class IOmniBlockingCollection
  TestParallelFor = class(TTestCase)
  protected
    FTestData: array of integer;
    procedure TestRange(iFrom, iTo, iStep: integer);
    procedure InternalTestStepZero;
  published
    procedure TestIncreasingStep;
    procedure TestIncreasingEndEqStep;
    procedure TestIncreasingLargeDataStep;
    procedure TestDecreasingStep;
    procedure TestDecreasingStartEqStep;
    procedure TestDecreasingLargeDataStep;
    procedure TestIncreasingStartEqStep;
    procedure TestDecreasingEndEqStep;
    procedure TestNoExecution;
    procedure TestStepZero;
    procedure TestRepeatedDefaultTasks;
    procedure TestRepeatedExplicitTasks;
  end;

  TestJoin = class(TTestCase)
  published
    procedure TestTerminationAllStuck;
    procedure TestTerminationPartialStuck;
    procedure TestTerminationAllTerminated;
  end;

  TestThreadPoolCancel = class(TTestCase)
  published
    procedure TestCancelAllAfterSchedule;
    procedure TestCancelAllStuckTask;
    procedure TestCancelNotStartedTask;
    procedure TestCancelEnforcedNotStartedTask;
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  Math,
  OtlParallel,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool,
  OtlCommon;

{ TestParallelFor }

procedure TestParallelFor.TestIncreasingStep;
var
  i: Integer;
begin
  for i := 1 to 11 do
    TestRange(1, 10, i);
end;

procedure TestParallelFor.TestIncreasingEndEqStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1, i, i);
end;

procedure TestParallelFor.TestIncreasingLargeDataStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1, 100000, i);
end;

procedure TestParallelFor.TestDecreasingStep;
var
  i: Integer;
begin
  for i := 1 to 11 do
    TestRange(10, 1, -i);
end;

procedure TestParallelFor.TestDecreasingStartEqStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(i, 1, -i);
end;

procedure TestParallelFor.TestDecreasingLargeDataStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(100000, 1, -i);
end;

procedure TestParallelFor.TestIncreasingStartEqStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(i, 10, i);
end;

procedure TestParallelFor.InternalTestStepZero;
begin
  TestRange(1, 10, 0);
end;

procedure TestParallelFor.TestDecreasingEndEqStep;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(10, i, -i);
end;

procedure TestParallelFor.TestNoExecution;
var
  i,j: Integer;
begin
  for i := 1 to 10 do
    for j := 1 to 3 do
      TestRange(i, 0, j);
  for i := 1 to 10 do
    for j := 1 to 3 do
      TestRange(0, i, -j);
end;

procedure TestParallelFor.TestRange(iFrom, iTo, iStep: integer);
var
  iMax: integer;
  iMin: integer;
  i: Integer;

  procedure CheckAllEmpty;
  var
    i: integer;
  begin
    for i := Low(FTestData) to High(FTestData) do
      CheckEquals(-1, FTestData[i]);
  end;

begin
  WriteLn(Format('Testing range %d .. %d, step %d', [iFrom, iTo, iStep]));
  OutputDebugString(PChar(Format('Testing range %d .. %d, step %d', [iFrom, iTo, iStep])));
  iMin := Min(iFrom, iTo);
  iMax := Max(iFrom, iTo);
  SetLength(FTestData, iMax - iMin + 1);
  FillChar(FTestData[0], (iMax - iMin + 1) * SizeOf(FTestData[0]), $FF);

  Parallel.For(iFrom, iTo, iStep).Execute(
    procedure (idx: integer)
    begin
      FTestData[idx-iMin] := idx;
    end);

  if iStep > 0 then begin
    if iFrom > iTo then
      CheckAllEmpty
    else for i := iFrom to iTo do begin
      if ((i-iFrom) mod iStep) = 0 then
        CheckEquals(i, FTestData[i-iMin], Format('at index %d', [i]))
      else
        CheckEquals(-1, FTestData[i-iMin], Format('at index %d', [i]));
    end;
  end
  else begin
    if iFrom < iTo then
      CheckAllEmpty
    else for i := iFrom downto iTo do begin
      if ((i-iFrom) mod iStep) = 0 then
        CheckEquals(i, FTestData[i-iMin], Format('at index %d', [i]))
      else
        CheckEquals(-1, FTestData[i-iMin], Format('at index %d', [i]));
    end;
  end;
end;

procedure TestParallelFor.TestStepZero;
begin
  try
    InternalTestStepZero;
    Fail('Expected exception not raised');
  except
    on E: Exception do
      ; // expected
  end;
end;

procedure TestParallelFor.TestRepeatedDefaultTasks;
var
  counter: integer;
  n      : integer;
begin
  // Stress test: repeated Parallel.For with default task count (all cores).
  for n := 1 to 50 do begin
    counter := 0;
    Parallel.For(1, 10, 1)
      .Execute(
        procedure (idx: integer)
        begin
          InterlockedIncrement(counter);
        end);
    CheckEquals(10, counter, Format('iteration %d', [n]));
  end;
end;

procedure TestParallelFor.TestRepeatedExplicitTasks;
var
  counter: integer;
  n      : integer;
begin
  // Stress test with explicit NumTasks(2) and NoThreadPool.
  for n := 1 to 50 do begin
    counter := 0;
    Parallel.For(1, 10, 1).NumTasks(2)
      .TaskConfig(Parallel.TaskConfig.NoThreadPool)
      .Execute(
        procedure (idx: integer)
        begin
          InterlockedIncrement(counter);
        end);
    CheckEquals(10, counter, Format('iteration %d', [n]));
  end;
end;

{ TestJoin }

procedure TestJoin.TestTerminationAllStuck;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: array [0..1] of boolean;
  stopped: array [0..1] of boolean;
  time   : int64;

  function MakeTask(idx: integer; hangForever: boolean): TProc;
  begin
    Result :=
      procedure
      begin
        started[idx] := true;
        Sleep(100);
        if hangForever then
          Sleep(2000);
        stopped[idx] := true;
      end;
  end;

begin
  // Tests IOmniParallelJoin.Terminate when all tasks are stuck and don't terminate. Such tasks
  // are detached (since 3.10; before that, they were killed).

  FillChar(started[0], Length(started), false);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, true), MakeTask(1, true)).NoWait.Execute;
  time := DSiTimeGetTime64;
  CheckFalse(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  CheckTrue(time < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + IntToStr(i));
    CheckFalse(stopped[i], 'stopped ' + IntToStr(i));
  end;
  Sleep(2500); // stuck tasks are detached, not killed
  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + IntToStr(i));
    CheckTrue(stopped[i], 'stopped ' + IntToStr(i));
  end;
end;

procedure TestJoin.TestTerminationAllTerminated;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: array [0..1] of boolean;
  stopped: array [0..1] of boolean;
  time   : int64;

  function MakeTask(idx: integer; hangForever: boolean): TProc;
  begin
    Result :=
      procedure
      begin
        started[idx] := true;
        Sleep(100);
        if hangForever then
          Sleep(2000);
        stopped[idx] := true;
      end;
  end;

begin
  // Tests IOmniParallelJoin.Terminate when some tasks are stuck and don't terminate.

  FillChar(started[0], Length(started), false);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, true), MakeTask(1, false)).NoWait.Execute;
  time := DSiTimeGetTime64;
  CheckFalse(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  CheckTrue(time < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + IntToStr(i));
    CheckEquals(i = 1, stopped[i], 'stopped ' + IntToStr(i));
  end
end;

procedure TestJoin.TestTerminationPartialStuck;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: array [0..1] of boolean;
  stopped: array [0..1] of boolean;
  time   : int64;

  function MakeTask(idx: integer; hangForever: boolean): TProc;
  begin
    Result :=
      procedure
      begin
        started[idx] := true;
        Sleep(100);
        if hangForever then
          Sleep(2000);
        stopped[idx] := true;
      end;
  end;

begin
  // Tests IOmniParallelJoin.Terminate when all tasks terminate correctly.

  FillChar(started[0], Length(started), false);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, false), MakeTask(1, false)).NoWait.Execute;
  time := DSiTimeGetTime64;
  CheckTrue(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  CheckTrue(time < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + IntToStr(i));
    CheckTrue(stopped[i], 'stopped ' + IntToStr(i));
  end;
end;

{ TestThreadPoolCancel }

procedure TestThreadPoolCancel.TestCancelAllAfterSchedule;
// CancelAll must also cancel tasks that were already sent to a worker thread but were not
// yet picked up by it. Otherwise, such task starts after the cancellation and CancelAll
// waits for it.
const
  CGiveUp_ms = 3000; // must be shorter than the default WaitOnTerminate_sec
  CRounds    = 100;
  CTaskCount = 8;
var
  iRound: integer;
  iTask : integer;
  missed: integer;
  pool  : IOmniThreadPool;
begin
  missed := 0;
  for iRound := 1 to CRounds do begin
    pool := CreateThreadPool('TestCancelAllAfterSchedule');
    pool.MaxExecuting := 4;
    for iTask := 1 to CTaskCount do
      CreateTask(
        procedure (const task: IOmniTask)
        var
          start: int64;
        begin
          start := DSiTimeGetTime64;
          while not task.CancellationToken.IsSignalled do begin
            if DSiElapsedTime64(start) > CGiveUp_ms then begin // don't hang the test
              InterlockedIncrement(missed);
              break; //while
            end;
            Sleep(1);
          end;
        end)
      .Unobserved
      .Schedule(pool);
    pool.CancelAll(true);
    pool := nil;
    CheckEquals(0, missed, Format('Tasks not cancelled in round %d', [iRound]));
  end;
end;

procedure TestThreadPoolCancel.TestCancelAllStuckTask;
// CancelAll must not wait for a task that ignores cancellation longer than
// WaitOnTerminate_sec and the pool must stay functional.
const
  CStuck_ms = 10000;
var
  finished: integer;
  pool    : IOmniThreadPool;
  release : integer;
  started : integer;
  time    : int64;
  worked  : integer;
begin
  finished := 0;
  release := 0;
  started := 0;
  worked := 0;
  pool := CreateThreadPool('TestCancelAllStuckTask');
  pool.WaitOnTerminate_sec := 1;
  try
    CreateTask(
      procedure (const task: IOmniTask)
      var
        start: int64;
      begin
        InterlockedIncrement(started);
        start := DSiTimeGetTime64;
        while (release = 0) and (DSiElapsedTime64(start) < CStuck_ms) do
          Sleep(10);
        InterlockedIncrement(finished);
      end)
    .Unobserved
    .Schedule(pool);
    time := DSiTimeGetTime64;
    while (started = 0) and (DSiElapsedTime64(time) < 5000) do
      Sleep(1);
    CheckEquals(1, started, 'started');
    time := DSiTimeGetTime64;
    pool.CancelAll(true);
    time := DSiElapsedTime64(time);
    CheckTrue(time < (CStuck_ms div 2), Format('CancelAll returned after %d ms', [time]));
    CreateTask(
      procedure (const task: IOmniTask)
      begin
        InterlockedIncrement(worked);
      end)
    .Unobserved
    .Schedule(pool);
    time := DSiTimeGetTime64;
    while (worked = 0) and (DSiElapsedTime64(time) < 5000) do
      Sleep(1);
    CheckEquals(1, worked, 'worked');
  finally
    InterlockedIncrement(release); // don't leave a running thread behind
    time := DSiTimeGetTime64;
    while (finished = 0) and (DSiElapsedTime64(time) < 5000) do
      Sleep(1);
  end;
end;

procedure TestThreadPoolCancel.TestCancelNotStartedTask;
// Cancel(taskID) must cancel a task that was sent to a worker thread but not yet picked up
// by it and a task that is waiting in the queue. Such task must never start executing, or
// at least it must see the cancellation.
const
  CGiveUp_ms = 1000;
  CRounds    = 50;
var
  blocked: integer;
  iRound : integer;
  missed : integer;
  pool   : IOmniThreadPool;
  release: integer;
  time   : int64;
  victim : IOmniTaskControl;
begin
  missed := 0;
  for iRound := 1 to CRounds do begin
    blocked := 0;
    release := 0;
    pool := CreateThreadPool('TestCancelNotStartedTask');
    pool.MaxExecuting := 1;
    if Odd(iRound) then begin // victim will be queued
      CreateTask(
        procedure (const task: IOmniTask)
        var
          start: int64;
        begin
          InterlockedIncrement(blocked);
          start := DSiTimeGetTime64;
          while (release = 0) and (DSiElapsedTime64(start) < 5000) do
            Sleep(1);
        end)
      .Unobserved
      .Schedule(pool);
      time := DSiTimeGetTime64;
      while (blocked = 0) and (DSiElapsedTime64(time) < 5000) do
        Sleep(1);
      CheckEquals(1, blocked, 'blocked');
    end;
    victim := CreateTask(
      procedure (const task: IOmniTask)
      var
        start: int64;
      begin
        start := DSiTimeGetTime64;
        while not task.CancellationToken.IsSignalled do begin
          if DSiElapsedTime64(start) > CGiveUp_ms then begin // don't hang the test
            InterlockedIncrement(missed);
            break; //while
          end;
          Sleep(1);
        end;
      end)
    .Schedule(pool);
    time := DSiTimeGetTime64;
    CheckTrue(pool.Cancel(victim.UniqueID, true, 5000), 'Cancel');
    CheckTrue(DSiElapsedTime64(time) < 4000, 'Cancel waited for the full timeout');
    InterlockedIncrement(release);
    CheckTrue(victim.WaitFor(5000), 'Cancelled task is not terminated');
    CheckEquals(0, missed, Format('Task not cancelled in round %d', [iRound]));
    time := DSiTimeGetTime64;
    while (not pool.IsIdle) and (DSiElapsedTime64(time) < 5000) do
      Sleep(1);
    CheckTrue(pool.IsIdle, Format('Pool is not idle; executing: %d, queued: %d',
      [pool.CountExecuting, pool.CountQueued]));
    victim := nil;
    pool := nil;
  end;
end;

var
  GDelayedWorkerThread: TThreadID;

function DelayedWorkerThreadData: IInterface;
// Keeps a new pool worker away from its message loop for 300 ms, so a work item sent
// to it is assigned but not yet picked up for that long.
begin
  GDelayedWorkerThread := GetCurrentThreadId;
  Sleep(300);
  Result := nil;
end;

procedure TestThreadPoolCancel.TestCancelEnforcedNotStartedTask;
// Cancel must not run the body of an Enforced task that was assigned to a worker
// but not yet started in the thread pool manager.
var
  bodyThread: TThreadID;
  pool      : IOmniThreadPool;
  task      : IOmniTaskControl;
begin
  bodyThread := 0;
  GDelayedWorkerThread := 0;
  pool := CreateThreadPool('TestCancelEnforcedNotStartedTask');
  pool.SetThreadDataFactory(DelayedWorkerThreadData);
  task := CreateTask(
    procedure (const task: IOmniTask)
    begin
      bodyThread := GetCurrentThreadId;
    end)
  .Enforced
  .Schedule(pool);
  CheckTrue(pool.Cancel(task.UniqueID, false, 5000), 'Cancel');
  CheckTrue(task.WaitFor(5000), 'Cancelled task is not terminated');
  CheckTrue((bodyThread = 0) or (bodyThread = GDelayedWorkerThread),
    Format('Enforced task body ran on thread %d, not on the pool worker %d', [bodyThread, GDelayedWorkerThread]));
  task := nil;
  pool := nil;
end;

{$ENDIF}

initialization
{$IFDEF Unicode}
  RegisterTest(TestParallelFor.Suite);
  RegisterTest(TestJoin.Suite);
  RegisterTest(TestThreadPoolCancel.Suite);
{$ENDIF}
end.
