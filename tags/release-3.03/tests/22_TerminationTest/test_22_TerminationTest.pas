unit test_22_TerminationTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTerminationDemo = class(TForm)
    lbLog: TListBox;
    btnStop: TButton;
    btnTerminate: TButton;
    OTLMonitor: TOmniEventMonitor;
    procedure btnStopClick(Sender: TObject);
    procedure btnTerminateClick(Sender: TObject);
    procedure OTLMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OTLMonitorTaskTerminated(const task: IOmniTaskControl);
  private
    function StartWorker: IOmniTaskControl;
  public
  end;

var
  frmTerminationDemo: TfrmTerminationDemo;

implementation

uses
  OtlCommon;

{$R *.dfm}

procedure WaitAndExit(const task: IOmniTask);
var
  delay: integer;
begin
  task.Comm.Send(0, 'Worker started');
  delay := task.Param['Delay1'];
  task.Comm.Send(0, Format('Sleeping for %d ms', [delay]));
  Sleep(delay);
  if task.Terminated then
    Exit;
  delay := task.Param['Delay2'];
  task.Comm.Send(0, Format('Sleeping for %d ms', [delay]));
  Sleep(delay);
  task.Comm.Send(0, 'Worker stopped');
end;

procedure TfrmTerminationDemo.btnStopClick(Sender: TObject);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Terminate returned %d',
    [Ord(StartWorker.Terminate(2000))]));
end;

procedure TfrmTerminationDemo.btnTerminateClick(Sender: TObject);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Terminate returned %d',
    [Ord(StartWorker.Terminate(100))]));
end;

procedure TfrmTerminationDemo.OTLMonitorTaskMessage(
  const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg.MsgData);
end;

procedure TfrmTerminationDemo.OTLMonitorTaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Task %d terminated', [task.UniqueID]));
end;

function TfrmTerminationDemo.StartWorker: IOmniTaskControl;
begin
  Result := CreateTask(WaitAndExit, 'Wait and exit')
    .MonitorWith(OTLMonitor)
    .SetParameter('Delay1', 1000)
    .SetParameter('Delay2', 1000)
    .Run;
  Application.ProcessMessages; // allow OTLMonitor to log arrived messages
end;

end.
