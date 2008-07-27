program app_9_Communications;

uses
  Forms,
  test_9_Communications in 'test_9_Communications.pas' {frmTestOtlComm},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlComm, frmTestOtlComm);
  Application.Run;
end.
