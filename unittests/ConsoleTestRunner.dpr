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
  DUnitX.TestFramework,
  {$IFDEF MSWindows}SmokeTest in 'SmokeTest.pas',{$ENDIF}
  {$IFDEF MSWindows}TestTask in 'TestTask.pas',{$ENDIF}
  {$IFDEF MSWindows}TestBlockingCollection1 in 'TestBlockingCollection1.pas',{$ENDIF}
  {$IFDEF MSWindows}TestOtlDataManager1 in 'TestOtlDataManager1.pas',{$ENDIF}
  {$IFDEF MSWindows}TestOmniInterfaceDictionary in 'TestOmniInterfaceDictionary.pas',{$ENDIF}
  {$IFDEF MSWindows}TestOtlSync1 in 'TestOtlSync1.pas',{$ENDIF}
  {$IFDEF MSWindows}TestOmniValue in 'TestOmniValue.pas',{$ENDIF}
  {$IFDEF MSWindows}TestValue in 'TestValue.pas',{$ENDIF}
  {$IFDEF MSWindows}TestOtlParallel in 'TestOtlParallel.pas',{$ENDIF}
  TestPlatform in 'TestPlatform.pas',
  TestInterlocked in 'TestInterlocked.pas';

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
