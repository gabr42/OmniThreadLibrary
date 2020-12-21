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
  end;

  TestJoin = class(TTestCase)
  published
    procedure TestTerminationAllStuck;
    procedure TestTerminationPartialStuck;
    procedure TestTerminationAllTerminated;
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  Math,
  System.Diagnostics,
  OtlParallel;

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
  Status(Format('Testing range %d .. %d, step %d', [iFrom, iTo, iStep]));
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
  CheckException(InternalTestStepZero, Exception);
end;

{ TestJoin }

procedure TestJoin.TestTerminationAllStuck;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: TArray<boolean>;
  stopped: TArray<boolean>;
  sw     : TStopwatch;

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

  SetLength(started, 2);
  FillChar(started[0], Length(started), false);
  SetLength(stopped, 2);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, true), MakeTask(1, true)).NoWait.Execute;
  sw := TStopwatch.StartNew;
  CheckFalse(join.Terminate(500), 'Terminate');
  CheckTrue(sw.ElapsedMilliseconds < 1900, 'Elapsed time');

  Sleep(2000); // in case tasks are not really dead
  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + i.ToString);
    CheckFalse(stopped[i], 'stopped ' + i.ToString);
  end;
end;

procedure TestJoin.TestTerminationAllTerminated;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: TArray<boolean>;
  stopped: TArray<boolean>;
  sw     : TStopwatch;

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

  SetLength(started, 2);
  FillChar(started[0], Length(started), false);
  SetLength(stopped, 2);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, true), MakeTask(1, false)).NoWait.Execute;
  sw := TStopwatch.StartNew;
  CheckFalse(join.Terminate(500), 'Terminate');
  CheckTrue(sw.ElapsedMilliseconds < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + i.ToString);
    CheckEquals(i = 1, stopped[i], 'stopped ' + i.ToString);
  end
end;

procedure TestJoin.TestTerminationPartialStuck;
var
  i      : integer;
  join   : IOmniParallelJoin;
  started: TArray<boolean>;
  stopped: TArray<boolean>;
  sw     : TStopwatch;

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

  SetLength(started, 2);
  FillChar(started[0], Length(started), false);
  SetLength(stopped, 2);
  FillChar(stopped[0], Length(stopped), false);

  join := Parallel.Join(MakeTask(0, false), MakeTask(1, false)).NoWait.Execute;
  sw := TStopwatch.StartNew;
  CheckTrue(join.Terminate(500), 'Terminate');
  CheckTrue(sw.ElapsedMilliseconds < 1900, 'Elapsed time');

  for i := 0 to 1 do begin
    CheckTrue(started[i], 'started ' + i.ToString);
    CheckTrue(stopped[i], 'stopped ' + i.ToString);
  end;
end;

initialization
  RegisterTest(TestParallelFor.Suite);
  RegisterTest(TestJoin.Suite);
{$ENDIF}
end.

