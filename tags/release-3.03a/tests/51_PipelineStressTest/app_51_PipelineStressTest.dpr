program app_51_PipelineStressTest.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_51_PipelineStressTest in 'test_51_PipelineStressTest.pas' {frmPipelineStressTest};

begin
  // ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipelineStressTest, frmPipelineStressTest);
  Application.Run;
end.
