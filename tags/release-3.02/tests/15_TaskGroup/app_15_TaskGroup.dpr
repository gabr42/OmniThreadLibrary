program app_15_TaskGroup.XE3;

uses
  Forms,
  test_15_TaskGroup in 'test_15_TaskGroup.pas' {frmTestTaskGroup},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTaskGroup, frmTestTaskGroup);
  Application.Run;
end.
