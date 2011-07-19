unit test_4_TwoWayHello_with_package;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor,
  OtlThreadPool;

type
  TfrmTestTwoWayHello = class(TForm)
    actChangeMessage : TAction;
    ActionList       : TActionList;
    actStartHello    : TAction;
    actStopHello     : TAction;
    btnChangeMessage : TButton;
    btnStartHello    : TButton;
    btnStopHello     : TButton;
    lbLog            : TListBox;
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

const
  MSG_CHANGE_MESSAGE = 1;

procedure RunHello(const task: IOmniTask);
var
  msg    : string;
  msgData: TOmniValue;
  msgID  : word;
begin
  msg := task.Param['Message'];
  repeat
    case DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent, false, task.Param['Delay']) of
      WAIT_OBJECT_1:
        begin
          task.Comm.Receive(msgID, msgData);
          if msgID = MSG_CHANGE_MESSAGE then
            msg := msgData;
        end;
      WAIT_TIMEOUT:
        task.Comm.Send(0, msg);
      else
        break; //repeat
    end;
  until false;
end;

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
    OmniEventMonitor1.Monitor(CreateTask(RunHello, 'Hello')).
    SetParameter('Delay', 1000).
    SetParameter('Message', 'Hello').
    Run;
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
    [task.UniqueID, task.Name, msg.MsgID, msg.MsgData.AsString]))
end;

procedure TfrmTestTwoWayHello.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

initialization
  Randomize;
end.
