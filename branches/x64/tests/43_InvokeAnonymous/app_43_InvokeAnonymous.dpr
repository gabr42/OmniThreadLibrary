program app_43_InvokeAnonymous;

uses
  FastMM4,
  Forms,
  test_43_InvokeAnonymous in 'test_43_InvokeAnonymous.pas' {frmInvokeAnonymousDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInvokeAnonymousDemo, frmInvokeAnonymousDemo);
  Application.Run;
end.
