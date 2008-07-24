unit testOtlComm1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlComm,
  OtlTaskEvents;

type
  TfrmTestOtlComm = class(TForm)
    btnSendObject: TButton;
    btnSendString: TButton;
    btnSendTo1   : TButton;
    btnSendTo2   : TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniTaskEventDispatch;
    procedure btnSendObjectClick(Sender: TObject);
    procedure btnSendStringClick(Sender: TObject);
    procedure btnSendTo1Click(Sender: TObject);
    procedure btnSendTo2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniTEDTaskMessage(task: IOmniTaskControl);
  private
    FClient1    : IOmniTaskControl;
    FClient2    : IOmniTaskControl;
    FCommChannel: IOmniTwoWayChannel;
    procedure Log(const msg: string);
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

procedure TfrmTestOtlComm.btnSendObjectClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('123');
  sl.Add('abc');
  Log('Sending TStringList to task 1');
  FClient1.Comm.Send(MSG_FORWARD, [sl]);
end;

procedure TfrmTestOtlComm.btnSendStringClick(Sender: TObject);
begin
  Log('Sending ''abc'' to task 1');
  FClient1.Comm.Send(MSG_FORWARD, 'abc');
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

procedure TfrmTestOtlComm.FormCreate(Sender: TObject);
begin
  FCommChannel := CreateTwoWayChannel(1024);
  FClient1 := CreateTask(TCommTester.Create(FCommChannel.Endpoint1, 1024))
    .MonitorWith(OmniTED)
    .FreeOnTerminate
    .Run;
  FClient2 := CreateTask(TCommTester.Create(FCommChannel.Endpoint2, 1024))
    .MonitorWith(OmniTED)
    .FreeOnTerminate
    .Run;
end;

procedure TfrmTestOtlComm.FormDestroy(Sender: TObject);
begin
  FClient1.Terminate;
  FClient2.Terminate;
end;

procedure TfrmTestOtlComm.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestOtlComm.OmniTEDTaskMessage(task: IOmniTaskControl);
var
  msgData: TOmniValue;
  msgID  : word;
  sData  : string;
  sl     : TStringList;
begin
  task.Comm.Receive(msgID, msgData);
  if not VarIsArray(msgData) then
    sData := msgData
  else begin
    sl := TStringList(integer(msgData[0]));
    sData := sl.ClassName + '/' + sl.Text;
    if msgID = MSG_NOTIFY_RECEPTION then
      sl.Free;
  end;
  if msgID = MSG_NOTIFY_FORWARD then
    Log(Format('[%d/%s] Notify forward of %s', [task.UniqueID, task.Name, sData]))
  else if msgID = MSG_NOTIFY_RECEPTION then
    Log(Format('[%d/%s] Notify reception of %s', [task.UniqueID, task.Name, sData]))
  else
    Log(Format('[%d/%s] Unknown message %d|%s', [task.UniqueID, task.Name, msgID, sData]));
end;

end.
