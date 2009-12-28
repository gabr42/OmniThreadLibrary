program app_32_Collections;

uses
  Forms,
  test_32_Collections in 'test_32_Collections.pas' {frmTestOmniCollection};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOmniCollection, frmTestOmniCollection);
  Application.Run;
end.
