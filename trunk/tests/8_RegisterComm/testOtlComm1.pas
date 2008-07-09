unit testOtlComm1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlTask,
  OtlComm,
  OtlTaskEvents;

type
  TfrmTestOtlComm = class(TForm)
    lbLog: TListBox;
    btnSendTo1: TButton;
    OmniTaskEventDispatch1: TOmniTaskEventDispatch;
    btnSendTo2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSendTo1Click(Sender: TObject);
    procedure btnSendTo2Click(Sender: TObject);
    procedure OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
  private
    FClient1: IOmniTaskControl;
    FClient2: IOmniTaskControl;
    FCommChannel: IOmniTwoWayChannel;
    procedure Log(const msg: string);
  public
  end;

var
  frmTestOtlComm: TfrmTestOtlComm;

implementation

{$R *.dfm}

const
  // GUI -> thread messages
  MSG_FORWARD = 1;

  // thread -> thread messages
  MSG_FORWARDING = 2;

  // thread -> GUI messages
  MSG_NOTIFY_FORWARD = 3;
  MSG_NOTIFY_RECEPTION = 4;

type
  TCommTester = class(TOmniWorker)
  strict private
    ctComm    : IOmniCommunicationEndpoint;
    ctCommSize: integer;
  public
    constructor Create(commEndpoint: IOmniCommunicationEndpoint; commBufferSize: integer);
    function  Initialize: boolean; override;
    procedure OMForward(var msg: TOmniMessage); message MSG_FORWARD;
    procedure OMForwarding(var msg: TOmniMessage); message MSG_FORWARDING;
  end; { TCommTester }

{ TCommTester }

constructor TCommTester.Create(commEndpoint: IOmniCommunicationEndpoint; commBufferSize:
  integer);
begin
  inherited Create;
  ctComm := commEndpoint;
  ctCommSize := commBufferSize;
end;

function TCommTester.Initialize: boolean;
begin
  Task.RegisterComm(ctComm);
  Result := true;
end;

procedure TCommTester.OMForward(var msg: TOmniMessage);
begin
  Task.Comm.Send(MSG_NOTIFY_FORWARD, msg.MsgData);
  ctComm.Send(MSG_FORWARDING, msg.MsgData);
end;

procedure TCommTester.OMForwarding(var msg: TOmniMessage);
begin
  Task.Comm.Send(MSG_NOTIFY_RECEPTION, msg.MsgData);
end;

{ TfrmTestOtlComm }

procedure TfrmTestOtlComm.FormDestroy(Sender: TObject);
begin
  FClient1.Terminate;
  FClient2.Terminate;
end;

procedure TfrmTestOtlComm.FormCreate(Sender: TObject);
begin
  FCommChannel := CreateTwoWayChannel(1024);
  FClient1 := OmniTaskEventDispatch1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint1, 1024))).Run;
  FClient2 := OmniTaskEventDispatch1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint2, 1024))).Run;
end;

procedure TfrmTestOtlComm.btnSendTo1Click(Sender: TObject);
var
  value: integer;
begin
  value := Random(100);
  Log(Format('Sending %d to task 1', [value]));
  FClient1.Comm.Send(MSG_FORWARD, value);
end;

procedure TfrmTestOtlComm.btnSendTo2Click(Sender: TObject);
var
  value: integer;
begin
  value := Random(100);
  Log(Format('Sending %d to task 2', [value]));
  FClient2.Comm.Send(MSG_FORWARD, value);
end;

procedure TfrmTestOtlComm.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestOtlComm.OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
var
  msgID  : word;
  msgData: TOmniValue;
begin
  task.Comm.Receive(msgID, msgData);
  if msgID = MSG_NOTIFY_FORWARD then
    Log(Format('[%d/%s] Notify forward of %s', [task.UniqueID, task.Name, msgData]))
  else if msgID = MSG_NOTIFY_RECEPTION then
    Log(Format('[%d/%s] Notify reception of %s', [task.UniqueID, task.Name, msgData]))
  else
    Log(Format('[%d/%s] Unknown message %d|%s', [task.UniqueID, task.Name, msgID, msgData]));
end;

end.
