program testOmniThreadLibrary;

uses
  Forms,
  testOmniThreadLibrary1 in 'testOmniThreadLibrary1.pas' {frmTestOTL},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas',
  OtlEventMonitor in '..\..\OtlEventMonitor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOTL, frmTestOTL);
  Application.Run;
end.
