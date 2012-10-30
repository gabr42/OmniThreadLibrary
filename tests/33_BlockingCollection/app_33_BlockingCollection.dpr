program app_33_BlockingCollection.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
//  FastMM4,
  Forms,
  test_33_BlockingCollection in 'test_33_BlockingCollection.pas' {frmTestOtlCollections};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOmniBlockingCollection, frmTestOmniBlockingCollection);
  Application.Run;
end.
