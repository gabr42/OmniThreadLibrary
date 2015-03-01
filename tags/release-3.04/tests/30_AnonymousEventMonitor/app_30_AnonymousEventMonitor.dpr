program app_30_AnonymousEventMonitor.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_30_AnonymousEventMonitor in 'test_30_AnonymousEventMonitor.pas' {frmAnonymousEventMonitorDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnonymousEventMonitorDemo, frmAnonymousEventMonitorDemo);
  Application.Run;
end.
