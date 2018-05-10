program app_1_HelloWorld_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  test_1_HelloWorld_FMX in 'test_1_HelloWorld_FMX.pas' {Form77},
  OtlEventMonitor.Notify in '..\..\OtlEventMonitor.Notify.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm77, Form77);
  Application.Run;
end.
