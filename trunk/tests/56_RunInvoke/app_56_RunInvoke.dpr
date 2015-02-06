program app_56_RunInvoke;

uses
  Forms,
  test_56_RunInvoke in 'test_56_RunInvoke.pas' {frmRunInvokeTester};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRunInvokeTester, frmRunInvokeTester);
  Application.Run;
end.
