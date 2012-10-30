program app_21_Anonymous_methods.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_21_Anonymous_methods in 'test_21_Anonymous_methods.pas' {frmAnonymousMethodsDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnonymousMethodsDemo, frmAnonymousMethodsDemo);
  Application.Run;
end.
