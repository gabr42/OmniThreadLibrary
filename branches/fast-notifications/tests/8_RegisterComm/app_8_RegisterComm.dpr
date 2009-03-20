program app_8_RegisterComm;

uses
  FastMM4,
  Forms,
  test_8_RegisterComm in 'test_8_RegisterComm.pas' {frmTestRegisterComm},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestRegisterComm, frmTestRegisterComm);
  Application.Run;
end.
