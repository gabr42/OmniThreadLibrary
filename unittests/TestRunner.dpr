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
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ENDIF }
  SmokeTest in 'SmokeTest.pas',
  TestBlockingCollection1 in 'TestBlockingCollection1.pas',
  TestOtlDataManager1 in 'TestOtlDataManager1.pas',
  TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',
  TestOtlSync1 in 'TestOtlSync1.pas',
  OtlCommon in '..\OtlCommon.pas',
  TestOmniValue in 'TestOmniValue.pas',
  TestValue in 'TestValue.pas',
  TestOtlParallel in 'TestOtlParallel.pas';

{$R *.RES}

begin
  Application.Initialize;
  {$IFDEF TESTINSIGHT}
  RunRegisteredTests;
  {$ELSE}
  if IsConsole then
    with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
      Free
  else
    GUITestRunner.RunRegisteredTests;
  {$ENDIF}
end.

