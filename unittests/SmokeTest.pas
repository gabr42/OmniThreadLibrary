unit SmokeTest;

interface

uses
  TestFramework, SysUtils, Windows, DSiWin32;

type
  // Regression tests for the DSiWin32 unit
  TSmokeTest = class(TTestCase)
  published
    procedure TestDSiClassWndProcParamSize;
  {$IFDEF Unicode}
    procedure TestTOmniValueArrayInt64Cast;
  {$ENDIF}
  end;

implementation

uses
  OtlCommon;

type
  TDSiWParam = {$IFDEF Unicode}WPARAM{$ELSE}longint{$ENDIF};
  TDSiLParam = {$IFDEF Unicode}LPARAM{$ELSE}longint{$ENDIF};

{ TestIOmniBlockingCollection }

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

{$IFDEF Unicode}
procedure TSmokeTest.TestTOmniValueArrayInt64Cast;
var
  arrIn : TArray<int64>;
  arrOut: TArray<int64>;
  i     : Integer;
  ov    : TOmniValue;
begin
  // Issue #89

  arrIn := [1,2, $FFFFFFFF, $100000000, $FFFFFFFFFFFFFF];

  ov := TOmniValue.CastFrom<TArray<Int64>>(arrIn);

  arrOut := ov.CastTo<TArray<Int64>>;

  CheckEquals(Length(arrIn), Length(arrOut));

  for i := Low(arrIn) to High(arrIn) do
    CheckEquals(arrIn[i], arrOut[i]);
end;
{$ENDIF Unicode}

initialization
  RegisterTest(TSmokeTest.Suite);
end.
