program app_17_MsgWait.XE3;

uses
  Forms,
  test_17_MsgWait in 'test_17_MsgWait.pas' {frmTestMsgWait};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestMsgWait, frmTestMsgWait);
  Application.Run;
end.
