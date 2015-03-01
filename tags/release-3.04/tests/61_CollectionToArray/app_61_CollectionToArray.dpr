program app_61_CollectionToArray;

uses
  Forms,
  test_61_CollectionToArray in 'test_61_CollectionToArray.pas' {frmTestCollectionToArray};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestCollectionToArray, frmTestCollectionToArray);
  Application.Run;
end.
