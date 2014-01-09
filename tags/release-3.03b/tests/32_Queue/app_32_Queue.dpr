program app_32_Queue.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_32_Queue in 'test_32_Queue.pas' {frmTestOmniQueue};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.Run;
end.
