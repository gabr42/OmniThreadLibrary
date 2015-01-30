program app_60_Map;

uses
  Vcl.Forms,
  test_60_Map in 'test_60_Map.pas' {frmTestParallelMap};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestParallelMap, frmTestParallelMap);
  Application.Run;
end.
