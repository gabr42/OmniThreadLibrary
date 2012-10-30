program app_2_TwoWayHello.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_2_TwoWayHello in 'test_2_TwoWayHello.pas' {frmTestTwoWayHello},
  DSiWin32 in '..\..\src\DSiWin32.pas',
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas',
  OtlEventMonitor in '..\..\OtlEventMonitor.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTwoWayHello, frmTestTwoWayHello);
  Application.Run;
end.
