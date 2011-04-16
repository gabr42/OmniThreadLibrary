program app_47_TaskConfig;

uses
  Forms,
  test_47_TaskConfig in 'test_47_TaskConfig.pas' {frmDemoParallelTaskConfig};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoParallelTaskConfig, frmDemoParallelTaskConfig);
  Application.Run;
end.
