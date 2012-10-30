program app_10_Containers.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  OtlCommBufferTest,
  test_10_Containers in 'test_10_Containers.pas' {frmTestOtlContainers},
  OtlComm in '..\..\OtlComm.pas',
  OtlTask in '..\..\OtlTask.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlContainers, frmTestOtlContainers);
  Application.Run;
end.
