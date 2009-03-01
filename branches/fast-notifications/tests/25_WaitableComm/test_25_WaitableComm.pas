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
    procedure btnReceiveClick(Sender: TObject);
  private
    FReceiveTask: IOmniTaskControl;
    procedure Log(const msg: string);
  public
  end;

var
  frmWaitableCommDemo: TfrmWaitableCommDemo;

implementation

uses
  OtlComm;

{$R *.dfm}

procedure ReceiveTestWorker(const task: IOmniTask);
begin
  Sleep(2000);
  task.Comm.Send(0, 'message 1');
  Sleep(10000);
end;

{ TfrmWaitableCommDemo }

procedure TfrmWaitableCommDemo.btnReceiveClick(Sender: TObject);
var
  msg: TOmniMessage;
begin
  FReceiveTask := CreateTask(ReceiveTestWorker, 'Receive test worker').Run;
  Log('Task started; running ReceiveWait that should succeed');
  if FReceiveTask.Comm.ReceiveWait(msg, 5000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail in 5 seconds because of timeout');
  if FReceiveTask.Comm.ReceiveWait(msg, 5000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail in 2 seconds because worker will terminate');
  if FReceiveTask.Comm.ReceiveWait(msg, 60000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
  Log('Running ReceiveWait that should fail immediately because worker is terminated');
  if FReceiveTask.Comm.ReceiveWait(msg, 60000) then
    Log('Message received: ' + msg.MsgData)
  else
    Log('Receive failed');
end;

procedure TfrmWaitableCommDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss ', Now) + msg);
end;

end.
