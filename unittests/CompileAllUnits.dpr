program CompileAllUnits;

{$APPTYPE CONSOLE}

{$R *.res}

{$I OtlOptions.inc}

uses
  SysUtils,
  Classes,
  OtlContainers,
  OtlCommon,
  OtlCommon.Utils,
  OtlSync,
  OtlCollections,
  OtlComm,
  OtlCommBufferTest,
  OtlContainerObserver,
  OtlDataManager,
  OtlParallel,
  OtlSync.Platform,
  OtlSync.Platform.Interfaced,
  OtlSync.Platform.Basic,
  OtlSync.Platform.Modular,
  OtlSync.Platform.ConditionVariables,
  OtlPlatform.HeavyPool,
  OtlSync.Platform.Errors,
  OtlSync.Platform.Atomic,
  OtlParallel.Platform.Interfaces,
  OtlParallel.Platform.Tasks,
  OtlParallel.Platform.Pipe,
  OtlEventMonitor,
  OtlHooks,
  OtlLogger,
  OtlRegister,
//  OtlSuperObject,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
