program app_8_RegisterComm.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_8_RegisterComm in 'test_8_RegisterComm.pas' {frmTestRegisterComm},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlSync in '..\..\OtlSync.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestRegisterComm, frmTestRegisterComm);
  Application.Run;
end.
