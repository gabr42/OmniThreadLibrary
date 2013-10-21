program app_15_TaskGroup.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_15_TaskGroup in 'test_15_TaskGroup.pas' {frmTestTaskGroup},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTaskGroup, frmTestTaskGroup);
  Application.Run;
end.
