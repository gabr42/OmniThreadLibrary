unit test_25_WaitableComm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl;

type
  TfrmWaitableCommDemo = class(TForm)
    lbLog: TListBox;
    btnReceive: TButton;
    btnSendWait: TButton;
    procedure btnReceiveClick(Sender: TObject);
    procedure btnSendWaitClick(Sender: TObject);
  private
    procedure Log(const msg: string);
  public
  end;

var
  frmWaitableCommDemo: TfrmWaitableCommDemo;

implementation

uses
  DSiWin32,
  OtlCommon,
  OtlComm;

type
  TSendMsg = class
  strict private
    FMsgCount   : integer;
    FPerfCounter: int64;
  public
    constructor Create(msgCount: integer);
    property MsgCount: integer read FMsgCount;
    property PerfCounter: int64 read FPerfCounter;
 end;

{$R *.dfm}

procedure ReceiveTestWorker(const task: IOmniTask);
var
  sl: TStringList;
begin
  Sleep(2000);
  sl := TStringList.Create;
  sl.Add('message 1');
  task.Comm.Send(0, sl);
  Sleep(7000);
end;

procedure SendTestWorker(const task: IOmniTask);
var
  i  : integer;
  msg: TOmniMessage;
  sm : TSendMsg;
begin
  Sleep(1000); //wait for messages to queue up
  for i := 1 to 256 do begin
    if not task.Comm.ReceiveWait(msg, 1000) then
      raise Exception.Create('SendTestWorker failed');
    sm := TSendMsg(msg.MsgData.AsObject);
    if sm.MsgCount <> i then
      raise Exception.Create('SendTestWorker failed');
    FreeAndNil(sm);
  end;
  task.Comm.Send(0);
  Sleep(5000);
  for i := 1 to 256 do begin
    if not task.Comm.ReceiveWait(msg, 1000) then
      raise Exception.Create('SendTestWorker failed');
    sm := TSendMsg(msg.MsgData.AsObject);
    if sm.MsgCount <> i then
      raise Exception.Create('SendTestWorker failed');
    FreeAndNil(sm);
  end;
  task.Comm.Send(0);
  Sleep(5000);
  for i := 1 to 256 do begin
    if not task.Comm.ReceiveWait(msg, 1000) then
      raise Exception.Create('SendTestWorker failed');
  end;
  task.Comm.Send(0);
end;

{ TfrmWaitableCommDemo }

procedure TfrmWaitableCommDemo.btnReceiveClick(Sender: TObject);
var
  msg            : TOmniMessage;
  receiveTestTask: IOmniTaskControl;
begin
  receiveTestTask := CreateTask(ReceiveTestWorker, 'Receive test worker').Run;
  Log('Task started; running ReceiveWait that should succeed in 2 seconds');
  if receiveTestTask.Comm.ReceiveWait(msg, 5000) then begin
    Log('Message received: ' + TStringList(msg.MsgData.AsObject).Text);
    msg.MsgData.AsObject.Free;
  end
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail in 5 seconds because of timeout');
  if receiveTestTask.Comm.ReceiveWait(msg, 5000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail in 2 seconds because worker will terminate');
  if receiveTestTask.Comm.ReceiveWait(msg, 60000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail immediately because worker is terminated');
  if receiveTestTask.Comm.ReceiveWait(msg, 60000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  receiveTestTask.Terminate;
  receiveTestTask := nil;
end;

procedure TfrmWaitableCommDemo.btnSendWaitClick(Sender: TObject);
var
  i           : integer;
  msg         : TOmniMessage;
  sendTestTask: IOmniTaskControl;
  sm          : TSendMsg;
begin
  sendTestTask := CreateTask(SendTestWorker, 'Send test worker').SetQueueSize(256).Run;
  Log('Task started, sending 256 messages');
  for i := 1 to 256 do 
    if not sendTestTask.Comm.SendWait(1, TSendMsg.Create(i), 10000 {10 sec}) then
      Log('SendWait failed where it shouldn''t');
  if not sendTestTask.Comm.ReceiveWait(msg, 10000) then
    Log('Timeout waiting for EOT')
  else
    Log('All received');
  Log('Sending 257 messages, last should timeout');
  for i := 1 to 257 do begin
    sm := TSendMsg.Create(i);
    if not sendTestTask.Comm.SendWait(i, sm) then begin
      if i < 257 then
        Log('SendWait failed where it shouldn''t')
      else begin
        Log('Timeout');
        FreeAndNil(sm);
      end;
    end
    else if i = 257 then
      Log('SendWait succeeded where it shouldn''t');
  end;
  if not sendTestTask.Comm.ReceiveWait(msg, 10000) then
    Log('Timeout waiting for EOT')
  else
    Log('Test complete');
  Log('Sending 257 messages, last should timeout');
  for i := 1 to 257 do begin
    if not sendTestTask.Comm.SendWait(i, IntToStr(i)) then begin
      if i < 257 then
        Log('SendWait failed where it shouldn''t')
      else 
        Log('Timeout');
    end
    else if i = 257 then
      Log('SendWait succeeded where it shouldn''t')
  end;
  if not sendTestTask.Comm.ReceiveWait(msg, 10000) then
    Log('Timeout waiting for EOT')
  else
    Log('Test complete');
  sendTestTask.Terminate;
  sendTestTask := nil;
end;

procedure TfrmWaitableCommDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss ', Now) + msg);
end;

{ TSendMsg }

constructor TSendMsg.Create(msgCount: integer);
begin
  FMsgCount := msgCount;
  FPerfCounter := DSiQueryPerfCounterAsUS;
end;

end.
