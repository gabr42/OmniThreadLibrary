program app_26_MultiEventMonitor.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_26_MultiEventMonitor in 'test_26_MultiEventMonitor.pas' {frmMultiMonitorDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMultiMonitorDemo, frmMultiMonitorDemo);
  Application.Run;
end.
