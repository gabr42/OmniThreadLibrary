program app_23_BackgroundFileSearch.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_23_BackgroundFileSearch in 'test_23_BackgroundFileSearch.pas' {frmBackgroundFileSearchDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBackgroundFileSearchDemo, frmBackgroundFileSearchDemo);
  Application.Run;
end.
