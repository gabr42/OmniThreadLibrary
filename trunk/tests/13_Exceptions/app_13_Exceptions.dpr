program app_13_Exceptions.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_13_Exceptions in 'test_13_Exceptions.pas' {frmTestExceptions},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestExceptions, frmTestExceptions);
  Application.Run;
end.
