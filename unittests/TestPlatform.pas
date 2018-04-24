unit TestPlatform;

interface

uses
  TestFramework;

type
  // Tests for the OtlPlatform unit
  TPlatformTest = class(TTestCase)
  published
    procedure TestTimestamp;
  end;

implementation

uses
  SysUtils,
  OtlPlatform;

{ TPlatformTest }

procedure TPlatformTest.TestTimestamp;
var
  time_ms: int64;
begin
  time_ms := GetTimestamp_ms;
  Sleep(1000);
  time_ms := GetTimestamp_ms - time_ms;
  CheckTrue(time_ms >= 1000, Format('Time too small: %d', [time_ms]));
  CheckTrue(time_ms <= 1100, Format('Time too large: %d', [time_ms]));
end;

initialization
  RegisterTest(TPlatformTest.Suite);
end.
