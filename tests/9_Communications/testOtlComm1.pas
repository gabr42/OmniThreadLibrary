unit testOtlComm1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlComm,
  OtlTaskEvents;

type
  TfrmTestOtlComm = class(TForm)
    lbLog: TListBox;
    btnRunTests: TButton;
    OmniTaskEventDispatch1: TOmniTaskEventDispatch;
    procedure btnRunTestsClick(Sender: TObject);
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

uses
  DSiWin32;

{$R *.dfm}

const
  // GUI -> thread messages
  MSG_START_TEST        = 1;
  MSG_NOTIFY_TEST_START = 2;
  MSG_NOTIFY_TEST_END   = 3;

  // thread -> thread messages
  MSG_START_TIMING  = 100;
  MSG_END_TIMING    = 101;
  MSG_DUMP_TEST     = 102;

type
  TTestSuite = (tsDump);

  TCommTester = class(TOmniWorker)
  strict private
    ctComm          : IOmniCommunicationEndpoint;
    ctCommSize      : integer;
    ctExpectedValue : integer;
    ctTestRepetition: integer;
    ctTestStart_ms  : int64;
    ctTestSuite     : TTestSuite;
  private
  strict protected
    procedure RunDumpTest;
  public
    constructor Create(commEndpoint: IOmniCommunicationEndpoint; commBufferSize: integer);
    function  Initialize: boolean; override;
    procedure OMDumpTest(var msg: TOmniMessage); message MSG_DUMP_TEST;
    procedure OMEndTiming(var msg: TOmniMessage); message MSG_END_TIMING;
    procedure OMNotifyTestEnd(var msg: TOmniMessage); message MSG_NOTIFY_TEST_END;
    procedure OMStartTest(var msg: TOmniMessage); message MSG_START_TEST;
    procedure OMStartTiming(var msg: TOmniMessage); message MSG_START_TIMING;
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

procedure TCommTester.OMDumpTest(var msg: TOmniMessage);
begin
  Assert(ctTestSuite = tsDump);
  if msg.MsgData <> ctExpectedValue then
    raise Exception.CreateFmt('Invalid value received (%d, expected %d)',
      [integer(msg.MsgData), ctExpectedValue]);
  Inc(ctExpectedValue);
end;

procedure TCommTester.OMEndTiming(var msg: TOmniMessage);
var
  testDuration_ms: int64;
begin
  testDuration_ms := DSiTimeGetTime64 - ctTestStart_ms;
  Task.Comm.Send(MSG_NOTIFY_TEST_END, testDuration_ms);
  ctComm.Send(MSG_NOTIFY_TEST_END, Ord(ctTestSuite));
end;

procedure TCommTester.OMNotifyTestEnd(var msg: TOmniMessage);
begin
  Assert(TTestSuite(msg.MsgData) = ctTestSuite);
  if ctTestSuite = tsDump then begin
    Inc(ctTestRepetition);
    if ctTestRepetition <= 10 then
      RunDumpTest;
  end;
end;

procedure TCommTester.OMStartTest(var msg: TOmniMessage);
begin
  Task.Comm.Send(MSG_NOTIFY_TEST_START, 'Dump');
  ctTestSuite := tsDump;
  ctTestRepetition := 1;
  RunDumpTest;
end;

procedure TCommTester.OMStartTiming(var msg: TOmniMessage);
begin
  ctTestStart_ms := DSiTimeGetTime64;
  ctTestSuite := TTestSuite(msg.MsgData);
  ctExpectedValue := 1;
end;

procedure TCommTester.RunDumpTest;
var
  iMsg: integer;
begin
  // dump ctCommSize message into the comm link
  ctComm.Send(MSG_START_TIMING, Ord(tsDump));
  for iMsg := 1 to ctCommSize do
    ctComm.Send(MSG_DUMP_TEST, iMsg);
  ctComm.Send(MSG_END_TIMING, 0);
end;

{ TfrmTestOtlComm }

procedure TfrmTestOtlComm.btnRunTestsClick(Sender: TObject);
begin
  FCommChannel := CreateTwoWayChannel(10240);
  FClient1 := OmniTaskEventDispatch1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint1, 10240))).Run;
  FClient2 := OmniTaskEventDispatch1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint2, 10240))).Run;
  FClient1.Comm.Send(MSG_START_TEST, 0);
end;

procedure TfrmTestOtlComm.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestOtlComm.OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
var
  msg            : TOmniMessage;
  testDuration_ms: int64;
begin
  task.Comm.Receive(msg);
  case msg.MsgID of
    MSG_NOTIFY_TEST_START:
      Log(Format('Running test %s', [msg.MsgData]));
    MSG_NOTIFY_TEST_END:
      begin
        testDuration_ms := msg.MsgData;
        Log(Format('Test completed in %d ms', [testDuration_ms]));
      end
    else
      Log(Format('Unknown message %d', [msg.MsgID]));
  end;
end;

end.
