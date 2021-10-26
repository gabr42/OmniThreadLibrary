program CompileAllUnits;

{$APPTYPE CONSOLE}

{$R *.res}

{$I OtlOptions.inc}

uses
  SysUtils,
  Classes,
  OtlPlatform,
  OtlContainers,
  OtlCommon,
  OtlCommon.Utils,
  OtlSync,
  OtlSync.Utils,
  OtlCollections,
  OtlComm,
  OtlCommBufferTest,
  OtlContainerObserver,
  OtlDataManager,
  OtlParallel,
  OtlEventMonitor,
  OtlEventMonitor.Notify,
  OtlHooks,
  OtlLogger,
  OtlRegister,
//  OtlSuperobject,
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
