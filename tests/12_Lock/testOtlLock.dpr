program testOtlLock;

uses
  Forms,
  testOtlLock1 in 'testOtlLock1.pas' {frmTestLock},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestLock, frmTestLock);
  Application.Run;
end.
