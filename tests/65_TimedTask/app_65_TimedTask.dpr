program app_65_TimedTask;

uses
  Vcl.Forms,
  test_65_TimedTask in 'test_65_TimedTask.pas' {frmTimedTask};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTimedTask, frmTimedTask);
  Application.Run;
end.
