program app_57_ForEachSlice;

uses
  Vcl.Forms,
  test_57_ForEachSlice in 'test_57_ForEachSlice.pas' {frmForEachSlice};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmForEachSlice, frmForEachSlice);
  Application.Run;
end.
