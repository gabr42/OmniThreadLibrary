program app_34_TreeScan.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_34_TreeScan in 'test_34_TreeScan.pas' {frmTreeScanDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTreeScanDemo, frmTreeScanDemo);
  Application.Run;
end.
