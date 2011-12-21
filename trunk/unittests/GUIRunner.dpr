program GUIRunner;
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
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestBlockingCollection1 in 'TestBlockingCollection1.pas',
  TestOtlDataManager1 in 'TestOtlDataManager1.pas',
  OtlCollections in '..\OtlCollections.pas',
  TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',
  OtlCommon in '..\OtlCommon.pas',
  TestOmniValue in 'TestOmniValue.pas',
  TestValue in 'TestValue.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

