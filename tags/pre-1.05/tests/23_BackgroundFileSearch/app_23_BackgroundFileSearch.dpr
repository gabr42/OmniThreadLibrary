program app_23_BackgroundFileSearch;

uses
  FastMM4,
  Forms,
  test_23_BackgroundFileSearch in 'test_23_BackgroundFileSearch.pas' {frmBackgroundFileSearchDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBackgroundFileSearchDemo, frmBackgroundFileSearchDemo);
  Application.Run;
end.
