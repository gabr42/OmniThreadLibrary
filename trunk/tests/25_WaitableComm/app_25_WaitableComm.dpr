program app_25_WaitableComm;

uses
  FastMM4,
  Forms,
  test_25_WaitableComm in 'test_25_WaitableComm.pas' {frmWaitableCommDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWaitableCommDemo, frmWaitableCommDemo);
  Application.Run;
end.
