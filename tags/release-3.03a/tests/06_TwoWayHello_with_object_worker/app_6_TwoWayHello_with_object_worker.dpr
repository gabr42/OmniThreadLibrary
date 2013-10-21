program app_6_TwoWayHello_with_object_worker.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_6_TwoWayHello_with_object_worker in 'test_6_TwoWayHello_with_object_worker.pas' {frmTestTwoWayHello},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTwoWayHello, frmTestTwoWayHello);
  Application.Run;
end.
