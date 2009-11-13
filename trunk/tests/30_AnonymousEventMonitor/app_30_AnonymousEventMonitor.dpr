program app_30_AnonymousEventMonitor;

uses
  Forms,
  test_30_AnonymousEventMonitor in 'test_30_AnonymousEventMonitor.pas' {frmAnonymousEventMonitorDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnonymousEventMonitorDemo, frmAnonymousEventMonitorDemo);
  Application.Run;
end.
