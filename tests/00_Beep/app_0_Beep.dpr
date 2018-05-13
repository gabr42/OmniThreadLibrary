program app_0_Beep.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_0_Beep in 'test_0_Beep.pas' {frmTestSimple},
  OtlCommon in '..\..\OtlCommon.pas',
  OtlTask in '..\..\OtlTask.pas',
  OtlThreadPool in '..\..\OtlThreadPool.pas',
  OtlComm in '..\..\OtlComm.pas',
  DSiWin32 in '..\..\GpDelphiUnits\src\DSiWin32.pas',
  GpLists in '..\..\GpDelphiUnits\src\GpLists.pas',
  GpStuff in '..\..\GpDelphiUnits\src\GpStuff.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestSimple, frmTestSimple);
  Application.Run;
end.
