program app_67_ArrayToCollection;

uses
  Forms,
  test_67_ArrayToCollections in 'test_67_ArrayToCollections.pas' {frmArrayToCollection};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmArrayToCollection, frmArrayToCollection);
  Application.Run;
end.
