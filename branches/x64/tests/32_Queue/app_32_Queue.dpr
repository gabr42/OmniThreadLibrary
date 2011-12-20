program app_32_Queue;

uses
  FastMM4,
  Forms,
  test_32_Queue in 'test_32_Queue.pas' {frmTestOmniQueue};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.CreateForm(TfrmTestOmniQueue, frmTestOmniQueue);
  Application.Run;
end.
