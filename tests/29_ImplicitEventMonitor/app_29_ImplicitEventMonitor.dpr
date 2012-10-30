program app_29_ImplicitEventMonitor.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_29_ImplicitEventMonitor in 'test_29_ImplicitEventMonitor.pas' {frmImplicitEventMonitor},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas',
  OtlEventMonitor in '..\..\OtlEventMonitor.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmImplicitEventMonitor, frmImplicitEventMonitor);
  Application.Run;
end.
