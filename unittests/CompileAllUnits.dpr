program CompileAllUnits;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  OtlCollections,
  OtlComm,
  OtlCommBufferTest,
  OtlCommon,
  OtlCommon.Utils,
  OtlContainerObserver,
  OtlContainers,
  OtlDataManager,
  OtlEventMonitor,
  OtlHooks,
  OtlLogger,
  OtlParallel,
  OtlRegister,
//  OtlSuperObject,
  OtlSync,
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
