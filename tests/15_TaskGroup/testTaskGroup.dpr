program testTaskGroup;

uses
  Forms,
  testTaskGroup1 in 'testTaskGroup1.pas' {frmTestTaskGroup},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTaskGroup, frmTestTaskGroup);
  Application.Run;
end.
