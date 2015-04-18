unit SmokeTest;

interface

uses
  TestFramework, SysUtils, Windows, DSiWin32;

type
  // Regression tests for the DSiWin32 unit
  TSmokeTest = class(TTestCase)
  published
    procedure TestDSiClassWndProcParamSize;
  end;

implementation

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

initialization
  RegisterTest(TSmokeTest.Suite);
end.
