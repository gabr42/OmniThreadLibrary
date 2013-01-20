program ForEachOutput;

uses
  FastMM4,
  Vcl.Forms,
  ForEachOutput1 in 'ForEachOutput1.pas' {frmForEachOutput};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmForEachOutput, frmForEachOutput);
  Application.Run;
end.
