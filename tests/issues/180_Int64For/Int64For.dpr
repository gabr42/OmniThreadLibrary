program Int64For;

{ Parallel.For must accept Int64 loop bodies and Int64 ranges (issue #180).
  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.SyncObjs,
  OtlParallel,
  OtlTask,
  OtlTaskControl;

var
  failed: boolean = false;

procedure Check(condition: boolean; const msg: string);
begin
  if condition then
    Writeln('PASS: ', msg)
  else begin
    Writeln('FAIL: ', msg);
    failed := true;
  end;
end;

const
  CBase: int64 = Int64($100000000) * 4; // 2^34, does not fit into an integer

var
  count  : integer;
  sum    : int64;
  minV   : int64;
  maxV   : int64;
  lock   : TCriticalSection;
  s      : integer;
  initOK : integer;
  finOK  : integer;
  cnt    : integer;
  n      : int64;

begin
  lock := TCriticalSection.Create;
  try
    // the example from the issue (the range is inclusive: 0..10)
    count := 10;
    Parallel.For(0, count).Execute(
      procedure(I: Int64)
      begin
        TInterlocked.Add(sum, I);
      end);
    Check(sum = 55, 'Int64 body over an integer range (sum=' + IntToStr(sum) + ')');

    // Int64 range, small count
    sum := 0; minV := High(int64); maxV := 0; cnt := 0;
    Parallel.For(CBase, CBase + 999).Execute(
      procedure(I: Int64)
      begin
        lock.Acquire;
        try
          Inc(sum, I - CBase);
          if I < minV then minV := I;
          if I > maxV then maxV := I;
          Inc(cnt);
        finally lock.Release; end;
      end);
    Check((cnt = 1000) and (sum = 499500) and (minV = CBase) and (maxV = CBase + 999),
      'Int64 range above 2^32: ' + IntToStr(cnt) + ' iterations');

    // negative step
    cnt := 0; sum := 0;
    Parallel.For(CBase + 100, CBase, -2).Execute(
      procedure(I: Int64)
      begin
        lock.Acquire;
        try
          Inc(cnt);
          Inc(sum, I - CBase);
        finally lock.Release; end;
      end);
    Check((cnt = 51) and (sum = 2550), 'negative step: ' + IntToStr(cnt) + ' iterations');

    // taskIndex and task variants + Int64 initializer/finalizer
    cnt := 0; initOK := 0; finOK := 0;
    Parallel.For(CBase, CBase + 99)
      .Initialize(procedure(taskIndex: integer; fromIndex, toIndex: int64)
        begin
          if (fromIndex >= CBase) and (toIndex <= CBase + 99) then TInterlocked.Increment(initOK);
        end)
      .Finalize(procedure(const task: IOmniTask; taskIndex: integer; fromIndex, toIndex: int64)
        begin
          if (fromIndex >= CBase) and (toIndex <= CBase + 99) then TInterlocked.Increment(finOK);
        end)
      .Execute(procedure(const task: IOmniTask; taskIndex: integer; value: Int64)
        begin
          TInterlocked.Increment(cnt);
        end);
    Check((cnt = 100) and (initOK > 0) and (initOK = finOK),
      'Int64 initializer/finalizer and task variant: ' + IntToStr(cnt));

    cnt := 0;
    Parallel.For(CBase, CBase + 49).Execute(
      procedure(taskIndex: integer; value: Int64)
      begin
        TInterlocked.Increment(cnt);
      end);
    Check(cnt = 50, 'Int64 taskIndex variant');

    // integer overloads unchanged
    cnt := 0;
    Parallel.For(1, 100).Execute(
      procedure(value: integer)
      begin
        TInterlocked.Increment(cnt);
      end);
    Check(cnt = 100, 'integer body still works');

    // an integer initializer with an out-of-range loop must report an error
    s := 0;
    try
      Parallel.For(CBase, CBase + 9)
        .Initialize(procedure(taskIndex, fromIndex, toIndex: integer) begin end)
        .Execute(procedure(value: Int64) begin end);
    except
      on E: Exception do s := 1;
    end;
    Check(s = 1, 'integer initializer on Int64 range raises');

    // the same ranges beyond the integer limit, stepping
    n := 0;
    Parallel.For(Int64(High(integer)) - 5, Int64(High(integer)) + 5).Execute(
      procedure(I: Int64)
      begin
        TInterlocked.Increment(n);
      end);
    Check(n = 11, 'range crossing High(integer): ' + IntToStr(n));
  finally FreeAndNil(lock); end;
  if failed then Halt(1);
end.
