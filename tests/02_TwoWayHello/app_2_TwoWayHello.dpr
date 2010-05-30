program app_2_TwoWayHello;

uses
  FastMM4,
  Forms,
  test_2_TwoWayHello in 'test_2_TwoWayHello.pas' {frmTestTwoWayHello},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas',
  OtlEventMonitor in '..\..\OtlEventMonitor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTwoWayHello, frmTestTwoWayHello);
  Application.Run;
end.
