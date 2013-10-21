program app_14_TerminateWhen.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_14_TerminateWhen in 'test_14_TerminateWhen.pas' {frmTestTerminateWhen},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTerminateWhen, frmTestTerminateWhen);
  Application.Run;
end.
