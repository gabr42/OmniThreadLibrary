program testOtlContainers;

uses
  Forms,
  testOtlContainers1 in 'testOtlContainers1.pas' {frmTestOtlComm},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlComm, frmTestOtlComm);
  Application.Run;
end.
