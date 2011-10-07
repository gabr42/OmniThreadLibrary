program app_37_ParallelJoin;

uses
  FastMM4,
  Forms,
  test_37_ParallelJoin in 'test_37_ParallelJoin.pas' {frmTestParallelJoin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestParallelJoin, frmTestParallelJoin);
  Application.Run;
end.
