program TestRunner;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
//  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  SmokeTest in 'SmokeTest.pas',
  TestBlockingCollection1 in 'TestBlockingCollection1.pas',
  TestOtlDataManager1 in 'TestOtlDataManager1.pas',
  TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',
  TestOtlSync1 in 'TestOtlSync1.pas',
  TestOmniValue in 'TestOmniValue.pas',
  TestValue in 'TestValue.pas',
  TestOtlParallel in 'TestOtlParallel.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

