unit reportGenerator1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ReportGeneratorEngine;

const
  WM_REPORT_DONE = WM_USER;

type
  TfrmReportGenerator = class(TForm)
    btnStart: TButton;
    lbLog: TListBox;
    SimulatorTimer: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure SimulatorTimerTimer(Sender: TObject);
  private
    FScriptLine: integer;
    FReportGenerator: IReportGenerator;
    procedure WMReportDone(var msg: TMessage); message WM_REPORT_DONE;
    procedure StartSimulator;
  public
  end;

var
  frmReportGenerator: TfrmReportGenerator;

implementation

uses
  OtlCommon;

{$R *.dfm}

const
  CScript: array [1..9*5] of string = (
    'R', 'Client1', 'Report1', '5', 'Generate Report1 for Client1; should create new background worker',
    'R', 'Client1', 'Report2', '3', 'Generate Report2 for Client1; should reuse the background worker',
    'S', '',        '',        '3', 'Sleep for 3 seconds',
    'R', 'Client2', 'Report3', '1', 'Generate Report3 for Client2; should create new background worker',
    'S', '',        '',        '4', 'Sleep for 4 seconds; Report1 and Report3 should complete',
    'R', 'Client1', 'Report4', '1', 'Generate Report4 for Client1; should reuse the background worker; should destroy Client2 background worker',
    'R', 'Client2', 'Report5', '3', 'Generate Report5 for Client2; should create new background worker',
    'S', ''       , ''       , '4', 'Sleep for 4 seconds; Report2, Report4 and Report5 should complete',
    'R', 'Client1', 'Report6', '0', 'Generate Report6 for Client1; should reuse the background worker; should destroy Client2 background worker');

procedure TfrmReportGenerator.btnStartClick(Sender: TObject);
begin
  if not assigned(FReportGenerator) then begin
    FReportGenerator := CreateReportGenerator;
    FReportGenerator.OnCreateWorker :=
      procedure(Sender: IReportGenerator; const clientName: string)
      begin
        lbLog.Items.Add('Created worker for ' + clientName);
      end;
    FReportGenerator.OnDestroyWorker :=
      procedure(Sender: IReportGenerator; const clientName: string)
      begin
        lbLog.Items.Add('Destroyed worker for ' + clientName);
      end;
    FReportGenerator.OnRequestDone_Asy :=
      procedure(Sender: IReportGenerator; const reportInfo: IReportInfo)
      begin
        reportInfo._AddRef;
        PostMessage(frmReportGenerator.Handle, WM_REPORT_DONE, WParam(reportInfo), 0);
      end;
  end;
  StartSimulator;
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

procedure TfrmReportGenerator.SimulatorTimerTimer(Sender: TObject);
var
  client: string;
  cmd   : string;
  delay : integer;
  desc  : string;
  report: string;
begin
  SimulatorTimer.Enabled := false;
  cmd    := CScript[FScriptLine];           Inc(FScriptLine);
  client := CScript[FScriptLine];           Inc(FScriptLine);
  report := CScript[FScriptLine];           Inc(FScriptLine);
  delay  := StrToInt(CScript[FScriptLine]); Inc(FScriptLine);
  desc   := CScript[FScriptLine];           Inc(FScriptLine);
  lbLog.Items.Add('*** ' + desc);
  if cmd = 'S' then
    SimulatorTimer.Interval := delay * 1000
  else if cmd = 'R' then begin
    SimulatorTimer.Interval := 1;
    FReportGenerator.Schedule(client, report,delay*1000);
  end;
  SimulatorTimer.Enabled := (FScriptLine < High(CScript));
  if not SimulatorTimer.Enabled then
    lbLog.Items.Add('*** Simulation completed');
end;

procedure TfrmReportGenerator.StartSimulator;
begin
  FScriptLine := 1;
  SimulatorTimer.Interval := 1;
  SimulatorTimer.Enabled := true;
end;

end.
