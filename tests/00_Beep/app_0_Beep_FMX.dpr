program app_0_Beep_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  test_0_Beep_FMX in 'test_0_Beep_FMX.pas' {frmTestSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTestSimple, frmTestSimple);
  Application.Run;
end.
