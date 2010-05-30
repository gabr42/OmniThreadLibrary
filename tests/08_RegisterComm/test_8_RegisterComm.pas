unit test_8_RegisterComm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlComm,
  OtlEventMonitor, OtlThreadPool;

type
  TfrmTestRegisterComm = class(TForm)
    btnSendFloat : TButton;
    btnSendObject: TButton;
    btnSendString: TButton;
    btnSendTo1   : TButton;
    btnSendTo2   : TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniEventMonitor;
    btnSendBool: TButton;
    btnSendIntf: TButton;
    btnSendWS: TButton;
    procedure btnSendBoolClick(Sender: TObject);
    procedure btnSendFloatClick(Sender: TObject);
    procedure btnSendIntfClick(Sender: TObject);
    procedure btnSendObjectClick(Sender: TObject);
    procedure btnSendStringClick(Sender: TObject);
    procedure btnSendTo1Click(Sender: TObject);
    procedure btnSendTo2Click(Sender: TObject);
    procedure btnSendWSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  private
    FClient1    : IOmniTaskControl;
    FClient2    : IOmniTaskControl;
    FCommChannel: IOmniTwoWayChannel;
    procedure Log(const msg: string);
  end;

var
  frmTestRegisterComm: TfrmTestRegisterComm;

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
    ctComm: IOmniCommunicationEndpoint;
  public
    constructor Create(commEndpoint: IOmniCommunicationEndpoint);
    function  Initialize: boolean; override;
    procedure OMForward(var msg: TOmniMessage); message MSG_FORWARD;
    procedure OMForwarding(var msg: TOmniMessage); message MSG_FORWARDING;
  end; { TCommTester }

  IDataIntf = interface ['{9A2EA2D3-5615-48D1-B878-D083B1802522}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  end; { IDataIntf }

  TDataIntf = class(TInterfacedObject, IDataIntf)
  strict private
    FValue: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  end; { TDataIntf }

{ TCommTester }

constructor TCommTester.Create(commEndpoint: IOmniCommunicationEndpoint);
begin
  inherited Create;
  ctComm := commEndpoint;
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

procedure TfrmTestRegisterComm.btnSendBoolClick(Sender: TObject);
begin
  Log('Sending ''true'' to task 1');
  FClient1.Comm.Send(MSG_FORWARD, true);
  Log('Sending ''false'' to task 1');
  FClient1.Comm.Send(MSG_FORWARD, false);
end;

procedure TfrmTestRegisterComm.btnSendFloatClick(Sender: TObject);
var
  a: TOmniValue;
  d: Double;
  e: Extended;
begin
  Log('Sending ''3.1415'' to task 1');
  d := 3.1415;
  FClient1.Comm.Send(MSG_FORWARD, d);
  Log('Sending ''2.7182'' to task 1');
  e := 2.7182;
  FClient1.Comm.Send(MSG_FORWARD, e);
  //test - integer->float conversion must be automatic
  a := 1;
  d := a;
  e := a;
end;

procedure TfrmTestRegisterComm.btnSendIntfClick(Sender: TObject);
var
  intf: IDataIntf;
begin
  intf := TDataIntf.Create;
  intf.SetValue(42);
  Log('Sending IDataIntf to task 1');
  FClient1.Comm.Send(MSG_FORWARD, intf);
end;

procedure TfrmTestRegisterComm.btnSendObjectClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('123');
  sl.Add('abc');
  Log('Sending TStringList to task 1');
  FClient1.Comm.Send(MSG_FORWARD, sl);
end;

procedure TfrmTestRegisterComm.btnSendStringClick(Sender: TObject);
begin
  Log('Sending ''abc'' to task 1');
  FClient1.Comm.Send(MSG_FORWARD, 'abc');
end;

procedure TfrmTestRegisterComm.btnSendTo1Click(Sender: TObject);
var
  value: integer;
begin
  value := Random(100);
  Log(Format('Sending %d to task 1', [value]));
  FClient1.Comm.Send(MSG_FORWARD, value);
end;

procedure TfrmTestRegisterComm.btnSendTo2Click(Sender: TObject);
var
  value: integer;
begin
  value := Random(100);
  Log(Format('Sending %d to task 2', [value]));
  FClient2.Comm.Send(MSG_FORWARD, value);
end;

procedure TfrmTestRegisterComm.btnSendWSClick(Sender: TObject);
begin
  Log('Sending WideString ''abc'' to task 1');
  FClient1.Comm.Send(MSG_FORWARD, WideString('abc'));
end;

procedure TfrmTestRegisterComm.FormCreate(Sender: TObject);
begin
  FCommChannel := CreateTwoWayChannel(1024);
  FClient1 := CreateTask(TCommTester.Create(FCommChannel.Endpoint1))
    .MonitorWith(OmniTED)
    .Run;
  FClient2 := CreateTask(TCommTester.Create(FCommChannel.Endpoint2))
    .MonitorWith(OmniTED)
    .Run;
end;

procedure TfrmTestRegisterComm.FormDestroy(Sender: TObject);
begin
  FClient1.Terminate;
  FClient2.Terminate;
end;

procedure TfrmTestRegisterComm.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestRegisterComm.OmniTEDTaskMessage(const task: IOmniTaskControl; const
  msg: TOmniMessage);
var
  sData: string;
  sl   : TStringList;
begin
  if msg.MsgData.IsInterface then 
    sData := IntToStr((msg.MsgData.AsInterface as IDataIntf).GetValue)
  else if msg.MsgData.IsObject then begin
    sl := TStringList(msg.msgData.AsObject);
    sData := sl.ClassName + '/' + sl.Text;
    if msg.msgID = MSG_NOTIFY_RECEPTION then
      sl.Free;
  end
  else if msg.MsgData.IsWideString then
    sData := msg.MsgData.AsWideString
  else
    sData := msg.MsgData;
  if msg.msgID = MSG_NOTIFY_FORWARD then
    Log(Format('[%d/%s] Notify forward of %s', [task.UniqueID, task.Name, sData]))
  else if msg.msgID = MSG_NOTIFY_RECEPTION then
    Log(Format('[%d/%s] Notify reception of %s', [task.UniqueID, task.Name, sData]))
  else
    Log(Format('[%d/%s] Unknown message %d|%s', [task.UniqueID, task.Name, msg.msgID, sData]));
end;

constructor TDataIntf.Create;
begin
  inherited Create;
end; { TDataIntf.Create }

destructor TDataIntf.Destroy;
begin
  inherited;
end; { TDataIntf.Destroy }

function TDataIntf.GetValue: integer;
begin
  Result := FValue;
end; { TDataIntf.GetValue }

procedure TDataIntf.SetValue(const value: integer);
begin
  FValue := value;
end; { TDataIntf.SetValue }

end.
