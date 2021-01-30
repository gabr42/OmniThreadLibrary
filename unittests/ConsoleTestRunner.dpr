program ConsoleTestRunner;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnit,
  {$ELSE}
  TestFramework,
  TextTestRunner,
  {$ENDIF }
  DUnitX.TestFramework
//  {$IFDEF MSWindows}, SmokeTest in 'SmokeTest.pas'{$ENDIF }
  , TestBlockingCollection1 in 'TestBlockingCollection1.pas'
  , TestOtlDataManager1 in 'TestOtlDataManager1.pas'
  , TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas'
  , TestOmniValue in 'TestOmniValue.pas'
  , TestValue in 'TestValue.pas'
  , TestPlatform in 'TestPlatform.pas'
  , TestInterlocked in 'TestInterlocked.pas'
  , TestContainers in 'TestContainers.pas'
//  {$IFNDEF MSWindows},
  , TestOtlComm in 'TestOtlComm.pas'
//  {$IFDEF MSWindows}, TestTask in 'TestTask.pas'{$ENDIF }
  , TestOtlSync1 in 'TestOtlSync1.pas'
//  {$IFDEF MSWindows}, TestOtlParallel in 'TestOtlParallel.pas'{$ENDIF }
  ;

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnit.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
  if DebugHook <> 0 then begin
    Write('> ');
    Readln;
  end;
{$ENDIF}
end.
