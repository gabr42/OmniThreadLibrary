program app_13_Exceptions;

uses
  Forms,
  test_13_Exceptions in 'test_13_Exceptions.pas' {frmTestExceptions},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestExceptions, frmTestExceptions);
  Application.Run;
end.
