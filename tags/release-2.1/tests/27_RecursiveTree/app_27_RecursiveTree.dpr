program app_27_RecursiveTree;

uses
  FastMM4,
  Forms,
  test_27_RecursiveTree in 'test_27_RecursiveTree.pas' {frmRecursiveTreeDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRecursiveTreeDemo, frmRecursiveTreeDemo);
  Application.Run;
end.
