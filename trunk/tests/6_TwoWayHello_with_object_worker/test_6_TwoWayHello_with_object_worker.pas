unit test_6_TwoWayHello_with_object_worker;

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
  MSG_CHANGE_MESSAGE = 1;
  MSG_SEND_MESSAGE   = 2;

type
  TAsyncHello = class(TOmniWorker)
  strict private
    aiMessage: string;
  public
    constructor Create(const initialMessage: string);
    procedure OMChangeMessage(var msg: TOmniMessage); message MSG_CHANGE_MESSAGE;
    procedure OMSendMessage(var msg: TOmniMessage); message MSG_SEND_MESSAGE;
  end;

  TfrmTestTwoWayHello = class(TForm)
    actChangeMessage      : TAction;
    ActionList            : TActionList;
    actStartHello         : TAction;
    actStopHello          : TAction;
    btnChangeMessage      : TButton;
    btnStartHello         : TButton;
    btnStopHello          : TButton;
    lbLog                 : TListBox;
    OmniEventMonitor1: TOmniEventMonitor;
    procedure actChangeMessageExecute(Sender: TObject);
    procedure actChangeMessageUpdate(Sender: TObject);
    procedure actStartHelloExecute(Sender: TObject);
    procedure actStartHelloUpdate(Sender: TObject);
    procedure actStopHelloExecute(Sender: TObject);
    procedure actStopHelloUpdate(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
    procedure OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
  strict private
    FHelloTask: IOmniTaskControl;
  private
  end;

var
  frmTestTwoWayHello: TfrmTestTwoWayHello;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestTwoWayHello.actChangeMessageExecute(Sender: TObject);
begin
  FHelloTask.Comm.Send(MSG_CHANGE_MESSAGE, 'Random ' + IntToStr(Random(1234)));
end;

procedure TfrmTestTwoWayHello.actChangeMessageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestTwoWayHello.actStartHelloExecute(Sender: TObject);
begin
  FHelloTask :=
    OmniEventMonitor1.Monitor(CreateTask(TAsyncHello.Create('Hello'), 'Hello'))
    .SetTimer(1000, MSG_SEND_MESSAGE)
    .SetParameter('Delay', 1000)
    .Run;
end;

procedure TfrmTestTwoWayHello.actStartHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not assigned(FHelloTask);
end;

procedure TfrmTestTwoWayHello.actStopHelloExecute(Sender: TObject);
begin
  FHelloTask.Terminate;
  FHelloTask := nil;
end;

procedure TfrmTestTwoWayHello.actStopHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestTwoWayHello.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData]));
end;

procedure TfrmTestTwoWayHello.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

{ TAsyncHello }

constructor TAsyncHello.Create(const initialMessage: string);
begin
  aiMessage := initialMessage;
end;

procedure TAsyncHello.OMChangeMessage(var msg: TOmniMessage);
begin
  aiMessage := msg.MsgData;
end;

procedure TAsyncHello.OMSendMessage(var msg: TOmniMessage);
begin
  Task.Comm.Send(0, aiMessage);
end;

initialization
  Randomize;
end.
