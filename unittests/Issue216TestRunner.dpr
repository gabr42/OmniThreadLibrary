program Issue216TestRunner;

{
  DUnit console runner for the issue #216 regression test.

  MUST be compiled with the OTL_RaceTest conditional define so that the test-only
  GOtlRaceTestHook is compiled into OtlTaskControl. Example (adjust paths):

    dcc32 -B -DOTL_RaceTest -CC ^
      -U"$(BDS)\Source\DUnit\src";..;..\src ^
      Issue216TestRunner.dpr
}

{$APPTYPE CONSOLE}

uses
  TestFramework,
  TextTestRunner,
  TestOtlIssue216 in 'TestOtlIssue216.pas';

begin
  with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
    Free;
end.
