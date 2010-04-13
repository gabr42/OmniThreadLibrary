program app_36_ParallelAggregate;

uses
  Forms,
  test_36_ParallelAggregate in 'test_36_ParallelAggregate.pas' {frmParallelAggregateDemo},
  OtlDataManager in '..\..\OtlDataManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelAggregateDemo, frmParallelAggregateDemo);
  Application.Run;
end.
