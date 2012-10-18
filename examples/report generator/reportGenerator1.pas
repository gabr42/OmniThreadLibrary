unit reportGenerator1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ReportGeneratorEngine;

const
  WM_REPORT_DONE = WM_USER;

type
  TfrmReportGenerator = class(TForm)
    btnStart: TButton;
    lbLog: TListBox;
    procedure btnStartClick(Sender: TObject);
  private
    FReportGenerator: IReportGenerator;
    procedure WMReportDone(var msg: TMessage); message WM_REPORT_DONE;
  public
  end;

var
  frmReportGenerator: TfrmReportGenerator;

implementation

uses
  OtlCommon;

{$R *.dfm}

procedure TfrmReportGenerator.btnStartClick(Sender: TObject);
begin
  if not assigned(FReportGenerator) then begin
    FReportGenerator := CreateReportGenerator;
    FReportGenerator.OnRequestDone_Asy :=
      procedure(Sender: IReportGenerator; const reportInfo: IReportInfo)
      begin
        reportInfo._AddRef;
        PostMessage(frmReportGenerator.Handle, WM_REPORT_DONE, WParam(reportInfo), 0);
      end;
  end;
end;

procedure TfrmReportGenerator.WMReportDone(var msg: TMessage);
var
  reportInfo: IReportInfo;
begin
  reportInfo := IReportInfo(msg.WParam);
  reportInfo._Release;
  lbLog.Items.Add(Format('Completed report %s for client %s; worker thread %d',
    [reportInfo.ReportName, reportInfo.ClientName, reportInfo.WorkerThread]));
end;

end.
