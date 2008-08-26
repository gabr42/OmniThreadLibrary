program app_14_TerminateWhen;

uses
  Forms,
  test_14_TerminateWhen in 'test_14_TerminateWhen.pas' {frmTestTerminateWhen},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTerminateWhen, frmTestTerminateWhen);
  Application.Run;
end.
