program app_50_OmniValueArray.XE3;

uses
  FastMM4,
  Forms,
  test_50_OmniValueArray in 'test_50_OmniValueArray.pas' {frmOmniValueArray};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOmniValueArray, frmOmniValueArray);
  Application.Run;
end.
