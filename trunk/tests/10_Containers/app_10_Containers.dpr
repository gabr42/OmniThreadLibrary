program app_10_Containers;

uses
  FastMM4,
  Forms,
  test_10_Containers in 'test_10_Containers.pas' {frmTestOtlContainers},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlContainers, frmTestOtlContainers);
  Application.Run;
end.
