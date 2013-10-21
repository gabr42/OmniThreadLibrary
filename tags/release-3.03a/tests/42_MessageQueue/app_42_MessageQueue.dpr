program app_42_MessageQueue.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_42_MessageQueue in 'test_42_MessageQueue.pas' {frmTestMessageQueue},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestMessageQueue, frmTestMessageQueue);
  Application.Run;
end.
