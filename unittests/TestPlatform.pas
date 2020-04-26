unit TestPlatform;

interface

uses
  TestFramework;

type
  // Tests for the OtlPlatform unit and other platform-dependant stuff
  TPlatformTest = class(TTestCase)
  published
    procedure TestTimestamp;
    procedure TestEventWaitFor;
  end;

implementation

{$I OtlOptions.inc}

uses
  System.SysUtils,
  System.Diagnostics,
  {$IFDEF MSWINDOWS}
  DSiWin32,
  {$ENDIF MSWINDOWS}
  OtlPlatform,
  OtlSync;

{ TPlatformTest }

procedure TPlatformTest.TestEventWaitFor;
var
  event  : IOmniEvent;
  time_ms: int64;
begin
  event := CreateOmniEvent(false, false);
  event.SetEvent;
  time_ms := Time.Timestamp_ms;
  event.WaitFor(1000);
  time_ms := Time.Elapsed_ms(time_ms);
  CheckTrue(time_ms < 500 {allowed measurement error}, 'WaitFor(1000) took too long');

  event.Reset;
  time_ms := Time.Timestamp_ms;
  event.WaitFor(1000);
  time_ms := Time.Elapsed_ms(time_ms);
  CheckTrue((time_ms >= 1000) and (time_ms <= 1050) {allowed measurement error},
    Format('WaitFor(1000) did not last around 1 s (actual: %d ms)', [time_ms]));
end;

procedure TPlatformTest.TestTimestamp;
var
  time_ms : int64;
  timeD_ms: int64;
  time1_ms: int64;
  time2_ms: int64;
begin
  time_ms := Time.Timestamp_ms;
  Sleep(1000);
  time1_ms := Time.Elapsed_ms(time_ms);
  timeD_ms := Time.Timestamp_ms - time_ms;
  time2_ms := Time.Elapsed_ms(time_ms);
  CheckTrue(timeD_ms >= 1000, Format('Time too small: %d', [timeD_ms]));
  CheckTrue(timeD_ms <= 1100, Format('Time too large: %d', [timeD_ms]));
  CheckTrue((time1_ms = timeD_ms) or (time2_ms = timeD_ms),
    Format('Elapsed time invalid: %d, %d', [time1_ms, time2_ms]));

  CheckTrue(Time.HasElapsed(time_ms, 100), 'Should have elapsed: 100');
  CheckTrue(Time.HasElapsed(time_ms, 1000), 'Should have elapsed: 1000');
  CheckFalse(Time.HasElapsed(time_ms, 2000), 'Should not have elapsed: 2000');
  CheckTrue(Time.HasElapsed(time_ms + 1000, 0), 'Should have elapsed: 0');
  CheckFalse(Time.HasElapsed(0, INFINITE), 'Should not have elapsed: INFINITE');
end;

initialization
  RegisterTest(TPlatformTest.Suite);
end.
