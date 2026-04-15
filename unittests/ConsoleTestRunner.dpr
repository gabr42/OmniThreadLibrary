program ConsoleTestRunner;

{

  DUnitX Console Test Project
  ---------------------------
  This project contains the DUnitX test framework and the console test runner.

}

{$APPTYPE CONSOLE}

{$STRONGLINKTYPES ON}

{$I OtlOptions.Inc}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  SmokeTest in 'SmokeTest.pas',
  TestBlockingCollection1 in 'TestBlockingCollection1.pas',
  TestOtlDataManager1 in 'TestOtlDataManager1.pas',
  TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',
  {$IFDEF OTL_Generics}
  TestOtlSync1 in 'TestOtlSync1.pas',
  {$ENDIF OTL_Generics}
  OtlCommon in '..\OtlCommon.pas',
  TestOmniValue in 'TestOmniValue.pas',
  TestValue in 'TestValue.pas',
  TestOtlParallel in 'TestOtlParallel.pas',
  TestInterlocked in 'TestInterlocked.pas',
  TestContainers in 'TestContainers.pas',
  TestOtlComm in 'TestOtlComm.pas',
  TestOtlCommon1 in 'TestOtlCommon1.pas',
  TestContainerObserver1 in 'TestContainerObserver1.pas',
  TestSyncUtils1 in 'TestSyncUtils1.pas',
  TestHooks1 in 'TestHooks1.pas';

var
  runner     : ITestRunner;
  results    : IRunResults;
  logger     : ITestLogger;
  nunitLogger: ITestLogger;
begin
  try
    TDUnitX.CheckCommandLine;
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    runner.FailsOnNoAsserts := False;

    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;

    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
