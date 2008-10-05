unit test_19_StringMsgBenchmark;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

const
  WM_RECEIVE = WM_USER;
  WM_STOP    = WM_USER;

type
  TBenchmarkClient = class(TOmniWorker)
  strict private
    bcCounter: integer;
  published
    procedure Receive(const msg: TOmniValue);
    procedure WMReceive(var msg: TOmniMessage); message WM_RECEIVE;
  end;

  TfrmTestStringMsgBenchmark = class(TForm)
    btnStartBenchmark: TButton;
    lbLog: TListBox;
    procedure btnStartBenchmarkClick(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
    procedure OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
  end;

var
  frmTestStringMsgBenchmark: TfrmTestStringMsgBenchmark;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  CNumMessages = 100000;

{ TfrmTestOTL }

procedure TfrmTestStringMsgBenchmark.btnStartBenchmarkClick(Sender: TObject);
var
  benchmarkTask: IOmniTaskControl;
  iMsg         : integer;
  msg          : TOmniMessage;
  timeElapsed  : int64;
  timeStart    : int64;
begin
  lbLog.ItemIndex := lbLog.Items.Add(
    Format('Benchmarking message ID dispatch; %d messages', [CNumMessages]));
  benchmarkTask := CreateTask(TBenchmarkClient.Create(), 'StringMsgBenchmark')
    .SetQueueSize(CNumMessages)
    .Run;
  timeStart := DSiTimeGetTime64;
  for iMsg := 1 to CNumMessages do
    benchmarkTask.Comm.Send(WM_RECEIVE, iMsg-1);
  benchmarkTask.Comm.ReceiveWait(msg, INFINITE);
  timeElapsed := DSiTimeGetTime64 - timeStart;
  lbLog.ItemIndex := lbLog.Items.Add(Format('Elapsed time: %d ms', [timeElapsed]));
  if msg.MsgID <> WM_STOP then
    lbLog.ItemIndex := lbLog.Items.Add(Format(
      'Invalid message received. Expected %d, received %d', [WM_STOP, msg.MsgID]));
  benchmarkTask.Terminate;

  lbLog.ItemIndex := lbLog.Items.Add(
    Format('Benchmarking method name dispatch; %d invocations', [CNumMessages]));
  benchmarkTask := CreateTask(TBenchmarkClient.Create(), 'StringMsgBenchmark')
    .SetQueueSize(CNumMessages)
    .Run;
  timeStart := DSiTimeGetTime64;
  for iMsg := 1 to CNumMessages do
    benchmarkTask.Invoke('Receive', iMsg-1);
  benchmarkTask.Comm.ReceiveWait(msg, INFINITE);
  timeElapsed := DSiTimeGetTime64 - timeStart;
  lbLog.ItemIndex := lbLog.Items.Add(Format('Elapsed time: %d ms', [timeElapsed]));
  if msg.MsgID <> WM_STOP then
    lbLog.ItemIndex := lbLog.Items.Add(Format(
      'Invalid message received. Expected %d, received %d', [WM_STOP, msg.MsgID]));
  benchmarkTask.Terminate;

  lbLog.ItemIndex := lbLog.Items.Add(
    Format('Benchmarking method pointer dispatch; %d invocations', [CNumMessages]));
  benchmarkTask := CreateTask(TBenchmarkClient.Create(), 'StringMsgBenchmark')
    .SetQueueSize(CNumMessages)
    .Run;
  timeStart := DSiTimeGetTime64;
  for iMsg := 1 to CNumMessages do
    benchmarkTask.Invoke(@TBenchmarkClient.Receive, iMsg-1);
  benchmarkTask.Comm.ReceiveWait(msg, INFINITE);
  timeElapsed := DSiTimeGetTime64 - timeStart;
  lbLog.ItemIndex := lbLog.Items.Add(Format('Elapsed time: %d ms', [timeElapsed]));
  if msg.MsgID <> WM_STOP then
    lbLog.ItemIndex := lbLog.Items.Add(Format(
      'Invalid message received. Expected %d, received %d', [WM_STOP, msg.MsgID]));
  benchmarkTask.Terminate;
end;

procedure TfrmTestStringMsgBenchmark.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TfrmTestStringMsgBenchmark.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated %s',
    [task.UniqueID, task.Name, task.ExitMessage]));
end;

{ TBenchmarkClient }

procedure TBenchmarkClient.Receive(const msg: TOmniValue);
begin
  if bcCounter <> msg.AsInteger then
    raise Exception.CreateFmt('Invalid counter. Expected %d, received %d',
      [bcCounter, msg.AsInteger]);
  Inc(bcCounter);
  if bcCounter = CNumMessages then
    Task.Comm.Send(WM_STOP);
end;

procedure TBenchmarkClient.WMReceive(var msg: TOmniMessage);
begin
  if bcCounter <> msg.MsgData.AsInteger then
    raise Exception.CreateFmt('Invalid counter. Expected %d, received %d',
      [bcCounter, msg.MsgData.AsInteger]);
  Inc(bcCounter);
  if bcCounter = CNumMessages then
    Task.Comm.Send(WM_STOP);
end;

initialization
  Randomize;
end.
