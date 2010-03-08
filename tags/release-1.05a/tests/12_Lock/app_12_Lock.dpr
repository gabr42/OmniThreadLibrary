program app_12_Lock;

uses
  Forms,
  test_12_Lock in 'test_12_Lock.pas' {frmTestLock},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestLock, frmTestLock);
  Application.Run;
end.
