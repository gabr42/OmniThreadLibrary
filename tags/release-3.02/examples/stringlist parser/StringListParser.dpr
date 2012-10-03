program StringListParser;

uses
  FastMM4,
  Forms,
  StringListParser1 in 'StringListParser1.pas' {frmStringListParser};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmStringListParser, frmStringListParser);
  Application.Run;
end.
