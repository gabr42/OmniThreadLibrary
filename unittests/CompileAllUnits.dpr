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
  OtlPlatform.Sync,
  OtlPlatform.Sync.Interfaced,
  OtlPlatform.Sync.Basic,
  OtlPlatform.Sync.Modular,
  OtlPlatform.Sync.ConditionVariables,
  OtlPlatform.Tasks,
  OtlPlatform.Pipe,
  OtlPlatform.HeavyPool,
  OtlPlatform.Extras,
  OtlPlatform.Errors,
  OtlPlatform.Atomic,
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
