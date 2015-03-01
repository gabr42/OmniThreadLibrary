program app_57_For;

uses
  Forms,
  test_57_For in 'test_57_For.pas' {frmForEachSlice};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelForDemo, frmParallelForDemo);
  Application.Run;
end.
