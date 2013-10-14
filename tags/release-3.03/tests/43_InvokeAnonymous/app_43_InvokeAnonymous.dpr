program app_43_InvokeAnonymous.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_43_InvokeAnonymous in 'test_43_InvokeAnonymous.pas' {frmInvokeAnonymousDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInvokeAnonymousDemo, frmInvokeAnonymousDemo);
  Application.Run;
end.
