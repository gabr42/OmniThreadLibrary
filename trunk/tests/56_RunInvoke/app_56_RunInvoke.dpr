program app_56_RunInvoke;

uses
  Vcl.Forms,
  test_55_RunInvoke in 'test_55_RunInvoke.pas' {frmRunInvokeTester};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRunInvokeTester, frmRunInvokeTester);
  Application.Run;
end.
