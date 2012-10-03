program app_45_ForkJoinMax;

uses
  FastMM4,
  Forms,
  test_45_ForkJoinMax in 'test_45_ForkJoinMax.pas' {frmQuickSortDemo},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmQuickSortDemo, frmQuickSortDemo);
  Application.Run;
end.
