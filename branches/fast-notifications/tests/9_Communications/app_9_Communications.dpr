program app_9_Communications;

uses
  FastMM4,
  Forms,
  test_9_Communications in 'test_9_Communications.pas' {frmTestCommunications},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestCommunications, frmTestCommunications);
  Application.Run;
end.
