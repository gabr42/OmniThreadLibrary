program testChainTo;

uses
  Forms,
  OtlComm in '..\..\OtlComm.pas',
  OtlTaskControl in '..\..\OtlTaskControl.pas',
  testChainTo1 in 'testChainTo1.pas' {frmTestTaskGroup};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTaskGroup, frmTestTaskGroup);
  Application.Run;
end.
