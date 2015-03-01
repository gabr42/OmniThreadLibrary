program app_36_ParallelAggregate.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_36_ParallelAggregate in 'test_36_ParallelAggregate.pas' {frmParallelAggregateDemo},
  OtlDataManager in '..\..\OtlDataManager.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelAggregateDemo, frmParallelAggregateDemo);
  Application.Run;
end.
