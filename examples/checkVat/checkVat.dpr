program checkVat;

uses
  FastMM4,
  Vcl.Forms,
  checkVat1 in 'checkVat1.pas' {frmCheckVat};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCheckVat, frmCheckVat);
  Application.Run;
end.
