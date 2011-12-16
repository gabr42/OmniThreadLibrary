program app_35_ParallelFor;

uses
  FastMM4,
  Forms,
  test_35_ParallelFor in 'test_35_ParallelFor.pas' {frmParallelForDemo},
  OtlParallel in '..\..\OtlParallel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelForDemo, frmParallelForDemo);
  Application.Run;
end.
