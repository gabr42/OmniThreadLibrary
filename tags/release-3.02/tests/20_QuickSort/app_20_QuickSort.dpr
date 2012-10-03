program app_20_QuickSort.XE3;

uses
  FastMM4,
  Forms,
  test_20_QuickSort in 'test_20_QuickSort.pas' {frmQuickSortDemo},
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
