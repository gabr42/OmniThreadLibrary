program app_50_OmniValueArray.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_50_OmniValueArray in 'test_50_OmniValueArray.pas' {frmOmniValueArray};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOmniValueArray, frmOmniValueArray);
  Application.Run;
end.
