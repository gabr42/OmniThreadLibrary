program app_32_Collections;

uses
//  FastMM4,
  Forms,
  test_32_Collections in 'test_32_Collections.pas' {frmTestOtlCollections};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestOtlCollections, frmTestOtlCollections);
  Application.Run;
end.
