program app_47_TaskConfig.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_47_TaskConfig in 'test_47_TaskConfig.pas' {frmDemoParallelTaskConfig};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoParallelTaskConfig, frmDemoParallelTaskConfig);
  Application.Run;
end.
