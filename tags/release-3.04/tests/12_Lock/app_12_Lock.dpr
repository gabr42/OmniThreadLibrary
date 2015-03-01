program app_12_Lock.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_12_Lock in 'test_12_Lock.pas' {frmTestLock},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestLock, frmTestLock);
  Application.Run;
end.
