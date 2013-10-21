program app_41_Pipeline;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_41_Pipeline in 'test_41_Pipeline.pas' {frmPipelineDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipelineDemo, frmPipelineDemo);
  Application.Run;
end.
