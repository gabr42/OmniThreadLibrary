program app_47_TaskConfig;

uses
  Forms,
  test_47_TaskConfig in 'test_47_TaskConfig.pas' {frmDemoParallelAsync};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoParallelAsync, frmDemoParallelAsync);
  Application.Run;
end.
