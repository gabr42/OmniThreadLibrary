program app_21_Anonymous_methods.XE3;

uses
  Forms,
  test_21_Anonymous_methods in 'test_21_Anonymous_methods.pas' {frmAnonymousMethodsDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnonymousMethodsDemo, frmAnonymousMethodsDemo);
  Application.Run;
end.
