program app_25_WaitableComm.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_25_WaitableComm in 'test_25_WaitableComm.pas' {frmWaitableCommDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWaitableCommDemo, frmWaitableCommDemo);
  Application.Run;
end.
