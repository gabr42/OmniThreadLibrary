program app_46_Async;

uses
  Forms,
  test_46_Async in 'test_46_Async.pas' {frmDemoParallelAsync};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoParallelAsync, frmDemoParallelAsync);
  Application.Run;
end.
