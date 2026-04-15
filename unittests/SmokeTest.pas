unit SmokeTest;

{$I OtlOptions.inc}

interface

uses
  DUnitX.TestFramework, SysUtils, Windows, DSiWin32;

type
  // Regression tests for the DSiWin32 unit
  [TestFixture]
  TSmokeTest = class
  public
    [Test]
    procedure TestDSiClassWndProcParamSize;
  {$IFDEF Unicode}
  {$IFDEF OTL_HasArrayOfT}
    [Test]
    procedure TestTOmniValueArrayInt64Cast;
  {$ENDIF}
    [Test]
    procedure TestCancelledFuture;
  {$ENDIF}
  end;

implementation

uses
{$IFDEF Unicode}
  OtlParallel,
{$ENDIF}
  OtlCommon,
  OtlSync;

type
  TDSiWParam = {$IFDEF Unicode}WPARAM{$ELSE}longint{$ENDIF};
  TDSiLParam = {$IFDEF Unicode}LPARAM{$ELSE}longint{$ENDIF};

{ TSmokeTest }

procedure TSmokeTest.TestDSiClassWndProcParamSize;
begin
  {$IFDEF CPUX64}
  Assert.AreEqual(8, SizeOf(TDSiWParam));
  Assert.AreEqual(8, SizeOf(TDSiLParam));
  {$ELSE}
  Assert.AreEqual(4, SizeOf(TDSiWParam));
  Assert.AreEqual(4, SizeOf(TDSiLParam));
  {$ENDIF}
end;

{$IFDEF Unicode}
{$IFDEF OTL_HasArrayOfT}
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

  Assert.AreEqual(Length(arrIn), Length(arrOut));

  for i := Low(arrIn) to High(arrIn) do
    Assert.AreEqual<int64>(arrIn[i], arrOut[i]);
end;
{$ENDIF}

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

  Assert.IsTrue(future.IsCancelled);
  Assert.IsFalse(executed);
end;
{$ENDIF Unicode}

end.
