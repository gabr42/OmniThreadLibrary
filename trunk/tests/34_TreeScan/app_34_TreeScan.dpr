program app_34_TreeScan.XE3;

uses
  FastMM4,
  Forms,
  test_34_TreeScan in 'test_34_TreeScan.pas' {frmTreeScanDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTreeScanDemo, frmTreeScanDemo);
  Application.Run;
end.
