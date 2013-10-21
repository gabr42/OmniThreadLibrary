program app_17_MsgWait.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_17_MsgWait in 'test_17_MsgWait.pas' {frmTestMsgWait};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestMsgWait, frmTestMsgWait);
  Application.Run;
end.
