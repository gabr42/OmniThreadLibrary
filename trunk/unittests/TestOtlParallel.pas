unit TestOtlParallel;

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils;

type
  // Test methods for class IOmniBlockingCollection
  TestParallelFor = class(TTestCase)
  protected
  testData: array of integer;
    procedure TestRange(iFrom, iTo, iStep: integer);
  published
    procedure TestFor1;
    procedure TestFor2;
    procedure TestFor3;
    procedure TestFor4;
    procedure TestFor5;
    procedure TestFor6;
  end;

implementation

uses
  Math,
  OtlParallel;

{ TestParallelFor }

procedure TestParallelFor.TestFor1;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1, 10, i);
end;

procedure TestParallelFor.TestFor2;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1, i, i);
end;

procedure TestParallelFor.TestFor3;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1, 1000000, i);
end;

procedure TestParallelFor.TestFor4;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(10, 1, -i);
end;

procedure TestParallelFor.TestFor5;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(i, 1, -i);
end;

procedure TestParallelFor.TestFor6;
var
  i: Integer;
begin
  for i := 1 to 10 do
    TestRange(1000000, 1, -i);
end;

procedure TestParallelFor.TestRange(iFrom, iTo, iStep: integer);
var
  iMax: integer;
  iMin: integer;
  i: Integer;
begin
  Status(Format('Testing range %d .. %d, step %d', [iFrom, iTo, iStep]));
  iMin := Min(iFrom, iTo);
  iMax := Max(iFrom, iTo);
  SetLength(testData, iMax - iMin + 1);
  FillChar(testData[0], (iMax - iMin + 1) * SizeOf(testData[0]), $FF);

  Parallel.For(iFrom, iTo, iStep).Execute(
    procedure (idx: integer)
    begin
      testData[idx-iMin] := idx;
    end);

  if iFrom < iTo then begin
    for i := iFrom to iTo do begin
      if ((i-iFrom) mod iStep) = 0 then
        CheckEquals(i, testData[i-iMin], Format('at index %d', [i]))
      else
        CheckEquals(-1, testData[i-iMin], Format('at index %d', [i]));
    end;
  end
  else begin
    for i := iFrom downto iTo do begin
      if ((i-iFrom) mod iStep) = 0 then
        CheckEquals(i, testData[i-iMin], Format('at index %d', [i]))
      else
        CheckEquals(-1, testData[i-iMin], Format('at index %d', [i]));
    end;
  end;
end;

initialization
  RegisterTest(TestParallelFor.Suite);
end.
