program app_11_ThreadPool.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_11_ThreadPool in 'test_11_ThreadPool.pas' {frmTestOtlThreadPool},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlThreadPool, frmTestOtlThreadPool);
  Application.Run;
end.
