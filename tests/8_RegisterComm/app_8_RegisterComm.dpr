program app_8_RegisterComm;

uses
  Forms,
  test_8_RegisterComm in 'test_8_RegisterComm.pas' {frmTestOtlComm},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlComm, frmTestOtlComm);
  Application.Run;
end.
