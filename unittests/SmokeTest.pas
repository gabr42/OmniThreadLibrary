unit SmokeTest;

{$I OtlOptions.inc}

interface

uses
  SysUtils,
  {$IFDEF MSWindows}
  Windows,
  DSiWin32,
  {$ENDIF}
  TestFramework;

type
  // Regression tests
  TSmokeTest = class(TTestCase)
  published
    {$IFDEF MSWindows}
    procedure TestDSiClassWndProcParamSize;
    {$ENDIF}
    procedure TestTOmniValueArrayInt64Cast;
    procedure TestCancelledFuture;
  end;

implementation

uses
  OtlParallel,
  OtlCommon,
  OtlSync;

type
  TDSiWParam = WPARAM;
  TDSiLParam = LPARAM;

{ TSmokeTest }

{$IFDEF MSWindows}
procedure TSmokeTest.TestDSiClassWndProcParamSize;
begin
  {$IFDEF CPUX64}
  CheckEquals(8, SizeOf(TDSiWParam));
  CheckEquals(8, SizeOf(TDSiLParam));
  {$ELSE}
  CheckEquals(4, SizeOf(TDSiWParam));
  CheckEquals(4, SizeOf(TDSiLParam));
  {$ENDIF}
end;
{$ENDIF}

procedure TSmokeTest.TestTOmniValueArrayInt64Cast;
var
  arrIn : TArray<int64>;
  arrOut: TArray<int64>;
  i     : Integer;
  ov    : TOmniValue;
begin
  // Issue #89

  SetLength(arrIn, 5);
  arrIn[0] := 1;
  arrIn[1] := 2;
  arrIn[2] := $FFFFFFFF;
  arrIn[3] := $100000000;
  arrIn[4] := $FFFFFFFFFFFFFF;

  ov := TOmniValue.CastFrom<TArray<Int64>>(arrIn);

  arrOut := ov.CastTo<TArray<Int64>>;

  CheckEquals(Length(arrIn), Length(arrOut));

  for i := Low(arrIn) to High(arrIn) do
    CheckEquals(arrIn[i], arrOut[i]);
end;

procedure TSmokeTest.TestCancelledFuture;
var
  executed: boolean;
  future  : IOmniFuture<Integer>;
  token   : IOmniCancellationToken;
begin
  token := CreateOmniCancellationToken;
  token.Signal;

  executed := false;

  future := Parallel.Future<Integer>(
    function: Integer
    begin
      executed := true;
      Result := 100;
    end,
    Parallel.TaskConfig.CancelWith(token)
  );

  CheckTrue(future.IsCancelled);
  CheckFalse(executed);
end;

initialization
  RegisterTest(TSmokeTest.Suite);
end.
