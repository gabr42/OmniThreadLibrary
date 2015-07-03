program TThreadComm;

uses
  FastMM4,
  Vcl.Forms,
  tthreadCommMain in 'tthreadCommMain.pas' {frmTThreadComm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTThreadComm, frmTThreadComm);
  Application.Run;
end.
