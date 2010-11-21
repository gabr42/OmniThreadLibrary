program app_41_Pipeline;

uses
  FastMM4,
  Forms,
  test_41_Pipeline in 'test_41_Pipeline.pas' {frmPipelineDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipelineDemo, frmPipelineDemo);
  Application.Run;
end.
