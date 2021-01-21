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
  {$IFDEF MSWindows}, SmokeTest in 'SmokeTest.pas'{$ENDIF }
  {$IFDEF MSWindows}, TestTask in 'TestTask.pas'{$ENDIF }
  , TestBlockingCollection1 in 'TestBlockingCollection1.pas'
  , TestOtlDataManager1 in 'TestOtlDataManager1.pas'
  , TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas'
  {$IFDEF MSWindows}, TestOtlSync1 in 'TestOtlSync1.pas'{$ENDIF }
  , TestOmniValue in 'TestOmniValue.pas'
  , TestValue in 'TestValue.pas'
  {$IFDEF MSWindows}, TestOtlParallel in 'TestOtlParallel.pas'{$ENDIF }
  , TestPlatform in 'TestPlatform.pas'
  , TestInterlocked in 'TestInterlocked.pas'
  , TestContainers in 'TestContainers.pas'
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
