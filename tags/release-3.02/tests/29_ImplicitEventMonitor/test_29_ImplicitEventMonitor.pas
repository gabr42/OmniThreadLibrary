unit test_29_ImplicitEventMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor,
  OtlComm;

type
  TfrmImplicitEventMonitor = class(TForm)
    actChangeMessage: TAction;
    ActionList      : TActionList;
    actStartHello   : TAction;
    actStopHello    : TAction;
    btnChangeMessage: TButton;
    btnStartHello   : TButton;
    btnStopHello    : TButton;
    lbLog           : TListBox;
    procedure actChangeMessageExecute(Sender: TObject);
    procedure actChangeMessageUpdate(Sender: TObject);
    procedure actStartHelloExecute(Sender: TObject);
    procedure actStartHelloUpdate(Sender: TObject);
    procedure actStopHelloExecute(Sender: TObject);
    procedure actStopHelloUpdate(Sender: TObject);
  strict private
    FHelloTask: IOmniTaskControl;
  private
    procedure HandleTaskTerminated(const task: IOmniTaskControl);
    procedure HandleTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  end;

var
  frmImplicitEventMonitor: TfrmImplicitEventMonitor;

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
          while task.Comm.Receive(msgID, msgData) do begin
            if msgID = MSG_CHANGE_MESSAGE then
              msg := msgData;
          end;
        end;
      WAIT_TIMEOUT:
        task.Comm.Send(0, msg);
      else
        break; //repeat
    end;
  until false;
end; 

{ TfrmTestOTL }

procedure TfrmImplicitEventMonitor.actChangeMessageExecute(Sender: TObject);
begin
  FHelloTask.Comm.Send(MSG_CHANGE_MESSAGE, WideString('Random ' + IntToStr(Random(1234))));
end;

procedure TfrmImplicitEventMonitor.actChangeMessageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmImplicitEventMonitor.actStartHelloExecute(Sender: TObject);
begin
  FHelloTask :=
    CreateTask(RunHello, 'Hello')
    .SetParameter('Delay', 1000)
    .SetParameter('Message', 'Hello')
    .OnMessage(HandleTaskMessage)
    .OnTerminated(HandleTaskTerminated)
    .Run;
end;

procedure TfrmImplicitEventMonitor.actStartHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not assigned(FHelloTask);
end;

procedure TfrmImplicitEventMonitor.actStopHelloExecute(Sender: TObject);
begin
  FHelloTask.Terminate;
  FHelloTask := nil;
end;

procedure TfrmImplicitEventMonitor.actStopHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmImplicitEventMonitor.HandleTaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

procedure TfrmImplicitEventMonitor.HandleTaskMessage(const task: IOmniTaskControl; const msg:
  TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.MsgID, msg.MsgData.AsString]));
end;

initialization
  Randomize;
end.
