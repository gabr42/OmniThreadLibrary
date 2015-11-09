program ppb;

uses
  Vcl.Forms,
  ppb1 in 'ppb1.pas' {frmUpdateProgressBar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmUpdateProgressBar, frmUpdateProgressBar);
  Application.Run;
end.
