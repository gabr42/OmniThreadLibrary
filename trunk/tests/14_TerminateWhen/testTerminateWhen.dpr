program testTerminateWhen;

uses
  FastMM4,
  Forms,
  testTerminateWhen1 in 'testTerminateWhen1.pas' {frmTestTerminateWhen},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTerminateWhen, frmTestTerminateWhen);
  Application.Run;
end.
