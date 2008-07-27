program app_6_TwoWayHello_with_object_worker;

uses
  Forms,
  test_6_TwoWayHello_with_object_worker in 'test_6_TwoWayHello_with_object_worker.pas' {frmTestOTL},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOTL, frmTestOTL);
  Application.Run;
end.
