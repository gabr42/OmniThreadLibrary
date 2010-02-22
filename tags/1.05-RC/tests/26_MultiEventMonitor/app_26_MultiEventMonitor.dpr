program app_26_MultiEventMonitor;

uses
  FastMM4,
  Forms,
  test_26_MultiEventMonitor in 'test_26_MultiEventMonitor.pas' {frmMultiMonitorDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMultiMonitorDemo, frmMultiMonitorDemo);
  Application.Run;
end.
