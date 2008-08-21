program app_16_ChainTo;

uses
  Forms,
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas',
  test_16_ChainTo in 'test_16_ChainTo.pas' {frmTestChainTo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestChainTo, frmTestChainTo);
  Application.Run;
end.
