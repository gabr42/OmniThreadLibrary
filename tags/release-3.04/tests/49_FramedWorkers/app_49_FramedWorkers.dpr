program app_49_FramedWorkers.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_49_FramedWorkers in 'test_49_FramedWorkers.pas' {frmFramedWorkers},
  test_49_FrameWithWorker in 'test_49_FrameWithWorker.pas' {frmFrameWithWorker: TFrame},
  test_49_Worker in 'test_49_Worker.pas',
  test_49_Common in 'test_49_Common.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFramedWorkers, frmFramedWorkers);
  Application.Run;
end.
