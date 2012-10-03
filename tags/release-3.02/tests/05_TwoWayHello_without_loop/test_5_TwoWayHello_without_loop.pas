unit test_5_TwoWayHello_without_loop;

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
  MSG_TIMER_1        = 3;

type
  TAsyncHello = class(TOmniWorker)
  strict private
    aiMessage: string;
  public
    function  Initialize: boolean; override;
    procedure OMChangeMessage(var msg: TOmniMessage); message MSG_CHANGE_MESSAGE;
    procedure OMSendMessage(var msg: TOmniMessage); message MSG_SEND_MESSAGE;
    procedure OMTimer2(var msg: TOmniMessage); message MSG_TIMER_1;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg:
      TOmniMessage);
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
var
  worker: IOmniWorker;
begin
  worker := TAsyncHello.Create;
  FHelloTask := OmniEventMonitor1.Monitor(CreateTask(worker, 'Hello'))
    .SetTimer(1000, MSG_SEND_MESSAGE)
    .SetTimer(1, 1200, MSG_TIMER_1)
    .SetParameter('Delay', 1000)
    .SetParameter('Message', 'Hello')
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

procedure TfrmTestTwoWayHello.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  actStopHello.Execute;
end;

procedure TfrmTestTwoWayHello.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TfrmTestTwoWayHello.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

{ TAsyncHello }

function TAsyncHello.Initialize: boolean;
begin
  aiMessage := Task.Param['Message'];
  Result := true;
end;

procedure TAsyncHello.OMChangeMessage(var msg: TOmniMessage);
begin
  aiMessage := msg.MsgData;
end;

procedure TAsyncHello.OMSendMessage(var msg: TOmniMessage);
begin
  Task.Comm.Send(0, aiMessage);
end;

procedure TAsyncHello.OMTimer2(var msg: TOmniMessage);
begin
  Task.ClearTimer(1);
  Task.Comm.Send(1, 'One-shot timer');
end;

initialization
  Randomize;
end.
