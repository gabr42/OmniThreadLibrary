program app_18_StringMsgDispatch.XE3;

uses
  FastMM4,
  Forms,
  test_18_StringMsgDispatch in 'test_18_StringMsgDispatch.pas' {frmTestStringMsgDispatch},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestStringMsgDispatch, frmTestStringMsgDispatch);
  Application.Run;
end.
