program app_41_Pipeline;

uses
  Forms,
  test_41_Pipeline in 'test_41_Pipeline.pas' {Form17};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipelineDemo, frmPipelineDemo);
  Application.Run;
end.
