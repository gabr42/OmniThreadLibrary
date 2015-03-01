program ReportGenerator;

uses
  FastMM4,
  Vcl.Forms,
  reportGenerator1 in 'reportGenerator1.pas' {frmReportGenerator},
  ReportGeneratorEngine in 'ReportGeneratorEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmReportGenerator, frmReportGenerator);
  Application.Run;
end.
