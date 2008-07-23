program testExceptions;

uses
  Forms,
  testExceptions1 in 'testExceptions1.pas' {frmTestLock},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestLock, frmTestLock);
  Application.Run;
end.
