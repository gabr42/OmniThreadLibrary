program app_18_StringMsgDispatch;

uses
  Forms,
  test_18_StringMsgDispatch in 'test_18_StringMsgDispatch.pas' {frmTestStringMsgDispatch},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas' {;

{$R *.res};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestStringMsgDispatch, frmTestStringMsgDispatch);
  Application.Run;
end.
