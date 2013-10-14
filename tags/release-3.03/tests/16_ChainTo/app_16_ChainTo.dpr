program app_16_ChainTo.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas',
  test_16_ChainTo in 'test_16_ChainTo.pas' {frmTestChainTo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestChainTo, frmTestChainTo);
  Application.Run;
end.
