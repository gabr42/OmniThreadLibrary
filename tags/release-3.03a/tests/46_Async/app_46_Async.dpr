program app_46_Async.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_46_Async in 'test_46_Async.pas' {frmDemoParallelAsync};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoParallelAsync, frmDemoParallelAsync);
  Application.Run;
end.
