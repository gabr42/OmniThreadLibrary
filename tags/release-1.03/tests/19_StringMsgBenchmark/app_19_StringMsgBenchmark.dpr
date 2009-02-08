program app_19_StringMsgBenchmark;

uses
  FastMM4,
  Forms,
  test_19_StringMsgBenchmark in 'test_19_StringMsgBenchmark.pas' {frmTestStringMsgBenchmark},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestStringMsgBenchmark, frmTestStringMsgBenchmark);
  Application.Run;
end.
