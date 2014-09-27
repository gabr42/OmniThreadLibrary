program app_58_ForVsForEach;

uses
  Vcl.Forms,
  test_58_ForVsForEach in 'test_58_ForVsForEach.pas' {frmForVsForEach};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmForVsForEach, frmForVsForEach);
  Application.Run;
end.
