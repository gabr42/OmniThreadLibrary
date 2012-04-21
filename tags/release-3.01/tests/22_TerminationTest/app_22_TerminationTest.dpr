program app_22_TerminationTest;

uses
  FastMM4,
  Forms,
  test_22_TerminationTest in 'test_22_TerminationTest.pas' {frmTerminationDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTerminationDemo, frmTerminationDemo);
  Application.Run;
end.
