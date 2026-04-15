unit TestOtlParallel;

interface

{$IFDEF Unicode}
uses
  DUnitX.TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils;

type
  // Test methods for class IOmniBlockingCollection
  [TestFixture]
  TestParallelFor = class
  protected
    FTestData: array of integer;
    procedure TestRange(iFrom, iTo, iStep: integer);
    procedure InternalTestStepZero;
  public
    [Test]
    procedure TestIncreasingStep;
    [Test]
    procedure TestIncreasingEndEqStep;
    [Test]
    procedure TestIncreasingLargeDataStep;
    [Test]
    procedure TestDecreasingStep;
    [Test]
    procedure TestDecreasingStartEqStep;
    [Test]
    procedure TestDecreasingLargeDataStep;
    [Test]
    procedure TestIncreasingStartEqStep;
    [Test]
    procedure TestDecreasingEndEqStep;
    [Test]
    procedure TestNoExecution;
    [Test]
    procedure TestStepZero;
    [Test]
    procedure TestRepeatedDefaultTasks;
    [Test]
    procedure TestRepeatedExplicitTasks;
  end;

  [TestFixture]
  TestJoin = class
  public
    [Test]
    procedure TestTerminationAllStuck;
    [Test]
    procedure TestTerminationPartialStuck;
    [Test]
    procedure TestTerminationAllTerminated;
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  Math,
  System.SyncObjs,
  OtlParallel,
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
      Assert.AreEqual(-1, FTestData[i]);
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
        Assert.AreEqual(i, FTestData[i-iMin], Format('at index %d', [i]))
      else
        Assert.AreEqual(-1, FTestData[i-iMin], Format('at index %d', [i]));
    end;
  end
  else begin
    if iFrom < iTo then
      CheckAllEmpty
    else for i := iFrom downto iTo do begin
      if ((i-iFrom) mod iStep) = 0 then
        Assert.AreEqual(i, FTestData[i-iMin], Format('at index %d', [i]))
      else
        Assert.AreEqual(-1, FTestData[i-iMin], Format('at index %d', [i]));
    end;
  end;
end;

procedure TestParallelFor.TestStepZero;
begin
  Assert.WillRaise(InternalTestStepZero, Exception);
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
          TInterlocked.Increment(counter);
        end);
    Assert.AreEqual(10, counter, Format('iteration %d', [n]));
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
          TInterlocked.Increment(counter);
        end);
    Assert.AreEqual(10, counter, Format('iteration %d', [n]));
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
  // Tests IOmniParallelJoin.Terminate when all tasks are stuck and don't terminate.

  FillChar(started[0], Length(started), false);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, true), MakeTask(1, true)).NoWait.Execute;
  time := DSiTimeGetTime64;
  Assert.IsFalse(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  Assert.IsTrue(time < 1900, 'Elapsed time');

  Sleep(2000); // in case tasks are not really dead
  for i := 0 to 1 do begin
    Assert.IsTrue(started[i], 'started ' + IntToStr(i));
    Assert.IsFalse(stopped[i], 'stopped ' + IntToStr(i));
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
  Assert.IsFalse(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  Assert.IsTrue(time < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    Assert.IsTrue(started[i], 'started ' + IntToStr(i));
    Assert.AreEqual<boolean>(i = 1, stopped[i], 'stopped ' + IntToStr(i));
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
  Assert.IsTrue(join.Terminate(500), 'Terminate');
  time := DSiTimeGetTime64 - time;
  Assert.IsTrue(time < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    Assert.IsTrue(started[i], 'started ' + IntToStr(i));
    Assert.IsTrue(stopped[i], 'stopped ' + IntToStr(i));
  end;
end;

{$ENDIF}
end.
