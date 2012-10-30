program app_44_ForkJoinQuickSort.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_44_ForkJoinQuickSort in 'test_44_ForkJoinQuickSort.pas' {frmQuickSortDemo},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmQuickSortDemo, frmQuickSortDemo);
  Application.Run;
end.
