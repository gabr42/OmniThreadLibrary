program testOtlThreadPool;

uses
  Forms,
  testOtlThreadPool1 in 'testOtlThreadPool1.pas' {frmTestOtlThreadPool},
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlThreadPool, frmTestOtlThreadPool);
  Application.Run;
end.
