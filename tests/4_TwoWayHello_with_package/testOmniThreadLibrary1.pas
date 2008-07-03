unit testOmniThreadLibrary1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskEvents;

type
  TfrmTestOTL = class(TForm)
    actChangeMessage: TAction;
    ActionList      : TActionList;
    actStartHello   : TAction;
    actStopHello    : TAction;
    btnChangeMessage: TButton;
    btnStartHello   : TButton;
    btnStopHello    : TButton;
    lbLog           : TListBox;
    OmniTaskEventDispatch1: TOmniTaskEventDispatch;
    procedure actChangeMessageExecute(Sender: TObject);
    procedure actChangeMessageUpdate(Sender: TObject);
    procedure actStartHelloExecute(Sender: TObject);
    procedure actStartHelloUpdate(Sender: TObject);
    procedure actStopHelloExecute(Sender: TObject);
    procedure actStopHelloUpdate(Sender: TObject);
    procedure OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
    procedure OmniTaskEventDispatch1TaskTerminated(task: IOmniTaskControl);
  strict private
    FHelloTask: IOmniTaskControl;
  private
  end;

var
  frmTestOTL: TfrmTestOTL;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  MSG_CHANGE_MESSAGE = 1;

procedure RunHello(task: IOmniTask);
var
  msg    : string;
  msgData: TOmniValue;
  msgID  : word;
begin
  msg := task.ParamByName['Message'];
  repeat
    case DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent, false, task.ParamByName['Delay']) of
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

procedure TfrmTestOTL.actChangeMessageExecute(Sender: TObject);
begin
  FHelloTask.Comm.Send(MSG_CHANGE_MESSAGE, 'Random ' + IntToStr(Random(1234)));
end;

procedure TfrmTestOTL.actChangeMessageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestOTL.actStartHelloExecute(Sender: TObject);
begin
  FHelloTask :=
    OmniTaskEventDispatch1.Monitor(CreateTask(RunHello, 'Hello')).
    SetParameter('Delay', 1000).
    SetParameter('Message', 'Hello').
    Run;
end;

procedure TfrmTestOTL.actStartHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not assigned(FHelloTask);
end;

procedure TfrmTestOTL.actStopHelloExecute(Sender: TObject);
begin
  FHelloTask.Terminate;
  FHelloTask := nil;
end;

procedure TfrmTestOTL.actStopHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestOTL.OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
var
  msgID  : word;
  msgData: TOmniValue;
begin
  task.Comm.Receive(msgID, msgData);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s', [task.UniqueID, task.Name, msgID, msgData]));
end;

procedure TfrmTestOTL.OmniTaskEventDispatch1TaskTerminated(task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

initialization
  Randomize;
end.
