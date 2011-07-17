program app_33_BlockingCollection;

uses
  Forms,
  test_33_BlockingCollection in 'test_33_BlockingCollection.pas' {frmTestOtlCollections};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOmniBlockingCollection, frmTestOmniBlockingCollection);
  Application.Run;
end.
