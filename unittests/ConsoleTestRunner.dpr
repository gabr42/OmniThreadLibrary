program ConsoleTestRunner;

{

  DUnit Console Test Project
  ---------------------------
  This project contains the DUnit test framework and the console test runner.

}

{$APPTYPE CONSOLE}

//{$STRONGLINKTYPES ON}

{$I OtlOptions.Inc}

uses
  TextTestRunner,
  SmokeTest in 'SmokeTest.pas',
  TestBlockingCollection1 in 'TestBlockingCollection1.pas',
  TestOtlDataManager1 in 'TestOtlDataManager1.pas',
  TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',
  {$IFDEF OTL_MobileSupport}
  TestOtlSync1 in 'TestOtlSync1.pas',
  {$ENDIF OTL_MobileSupport}
  OtlCommon in '..\OtlCommon.pas',
  TestOmniValue in 'TestOmniValue.pas',
  TestValue in 'TestValue.pas',
  TestOtlParallel in 'TestOtlParallel.pas',
  TestParallelChannelSelect1 in 'TestParallelChannelSelect1.pas',
  TestInterlocked in 'TestInterlocked.pas',
  TestContainers in 'TestContainers.pas',
  TestOtlComm in 'TestOtlComm.pas',
  TestOtlCommon1 in 'TestOtlCommon1.pas',
  TestStressBlockingCollection1 in 'TestStressBlockingCollection1.pas',
  {$IFDEF OTL_MobileSupport}
  TestContainerObserver1 in 'TestContainerObserver1.pas',
  {$ENDIF}
  {$IFDEF OTL_GoodGenerics}
  TestSyncUtils1 in 'TestSyncUtils1.pas',
  {$ENDIF}
  TestHooks1 in 'TestHooks1.pas';

begin
  RunRegisteredTests;
end.
