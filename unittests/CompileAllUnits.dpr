program CompileAllUnits;

{$APPTYPE CONSOLE}

{$R *.res}

{$I OtlOptions.inc}

uses
  SysUtils,
  Classes,
  OtlCommon,
  OtlCommon.Utils,
  OtlSync,
  OtlCollections,
  OtlComm,
  OtlCommBufferTest,
  OtlContainerObserver,
  OtlContainers,
  {$IFDEF OTL_Generics}
  OtlDataManager,
  OtlParallel,
  {$ENDIF OTL_Generics}
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
