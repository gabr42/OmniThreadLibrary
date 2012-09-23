program app_51_PipelineStressTest.XE3;

uses
  FastMM4,
  Forms,
  test_51_PipelineStressTest in 'test_51_PipelineStressTest.pas' {frmPipelineStressTest};

{$R *.res}

begin
  // ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipelineStressTest, frmPipelineStressTest);
  Application.Run;
end.
