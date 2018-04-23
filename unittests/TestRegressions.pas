unit TestRegressions;

interface

uses
  TestFramework;

type
  TestBugfixes = class(TTestCase)
  published
    procedure TestTOmniValueArrayInt64Cast;
  end;

implementation

uses
  System.Generics.Collections,
  OtlCommon;

procedure TestBugfixes.TestTOmniValueArrayInt64Cast;
var
  arrIn : TArray<int64>;
  arrOut: TArray<int64>;
  ov    : TOmniValue;
begin
  // Issue #89

  arrIn := [1,2, $FFFFFFFF, $100000000, $FFFFFFFFFFFFFF];

  ov := TOmniValue.CastFrom<TArray<Int64>>(arrIn);

  arrOut := ov.CastTo<TArray<Int64>>;
end;

initialization
  RegisterTest(TestBugfixes.Suite);
end.
