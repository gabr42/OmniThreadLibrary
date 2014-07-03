program app_55_ForEachProgress;

uses
  Vcl.Forms,
  test_55_ForEachProgress in 'test_55_ForEachProgress.pas' {frmForEachWithProgressBar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmForEachWithProgressBar, frmForEachWithProgressBar);
  Application.Run;
end.
