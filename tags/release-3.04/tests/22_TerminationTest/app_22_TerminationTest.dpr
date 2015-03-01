program app_22_TerminationTest.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_22_TerminationTest in 'test_22_TerminationTest.pas' {frmTerminationDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTerminationDemo, frmTerminationDemo);
  Application.Run;
end.
