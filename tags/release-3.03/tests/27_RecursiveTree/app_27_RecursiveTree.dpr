program app_27_RecursiveTree.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_27_RecursiveTree in 'test_27_RecursiveTree.pas' {frmRecursiveTreeDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRecursiveTreeDemo, frmRecursiveTreeDemo);
  Application.Run;
end.
