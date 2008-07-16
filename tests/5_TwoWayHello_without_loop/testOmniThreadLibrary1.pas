unit testOmniThreadLibrary1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlTaskEvents;

const
  MSG_CHANGE_MESSAGE = 1;
  MSG_SEND_MESSAGE   = 2;

type
  TAsyncHello = class(TOmniWorker)
  strict private
    aiMessage: string;
  public
    function  Initialize: boolean; override;
    procedure OMChangeMessage(var msg: TOmniMessage); message MSG_CHANGE_MESSAGE;
    procedure OMSendMessage(var msg: TOmniMessage); message MSG_SEND_MESSAGE;
  end;

  TfrmTestOTL = class(TForm)
    actChangeMessage      : TAction;
    ActionList            : TActionList;
    actStartHello         : TAction;
    actStopHello          : TAction;
    btnChangeMessage      : TButton;
    btnStartHello         : TButton;
    btnStopHello          : TButton;
    lbLog                 : TListBox;
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
var
  worker: IOmniWorker;
begin
  worker := TAsyncHello.Create;
  FHelloTask :=
    OmniTaskEventDispatch1.Monitor(CreateTask(worker, 'Hello')).
    SetTimer(1000, MSG_SEND_MESSAGE).
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
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData]));
end;

procedure TfrmTestOTL.OmniTaskEventDispatch1TaskTerminated(task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
end;

{ TAsyncHello }

function TAsyncHello.Initialize: boolean;
begin
  aiMessage := Task.ParamByName['Message'];
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

initialization
  Randomize;
end.
