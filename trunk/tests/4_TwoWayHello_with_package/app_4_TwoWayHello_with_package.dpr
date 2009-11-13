program app_4_TwoWayHello_with_package;

uses
  FastMM4,
  Forms,
  test_4_TwoWayHello_with_package in 'test_4_TwoWayHello_with_package.pas' {frmTestTwoWayHello},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas' {;

{$R *.res};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTwoWayHello, frmTestTwoWayHello);
  Application.Run;
end.
