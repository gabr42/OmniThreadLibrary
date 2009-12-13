unit test_9_Communications;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlComm,
  OtlEventMonitor;

type
  TfrmTestCommunications = class(TForm)
    btnRunTests      : TButton;
    lbLog            : TListBox;
    OmniEventMonitor1: TOmniEventMonitor;
    procedure btnRunTestsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg:
      TOmniMessage);
  private
    FClient1    : IOmniTaskControl;
    FClient2    : IOmniTaskControl;
    FCommChannel: IOmniTwoWayChannel;
    procedure Log(const msg: string);
  end;

var
  frmTestCommunications: TfrmTestCommunications;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  CTestQueueLength = 10000;

  // GUI -> thread messages
  MSG_START_TEST        = 1;
  MSG_NOTIFY_TEST_START = 2;
  MSG_NOTIFY_TEST_END   = 3;

  // thread -> thread messages
  MSG_START_TIMING  = 100;
  MSG_END_TIMING    = 101;
  MSG_DUMP_TEST     = 102;
  MSG_REQ           = 103;
  MSG_ACK           = 104;

type
  TTestSuite = (tsDump, tsMessageExchange);

  TCommTester = class(TOmniWorker)
  strict private
    ctComm          : IOmniCommunicationEndpoint;
    ctCommSize      : integer;
    ctExpectedValue : integer;
    ctTestRepetition: integer;
    ctTestStart_ms  : int64;
    ctTestSuite     : TTestSuite;
  private
    procedure InitiateMessageExchangeTest;
  strict protected
    procedure InitiateDumpTest;
    procedure RunDumpTest;
    procedure RunMessageExchangeTest;
  public
    constructor Create(commEndpoint: IOmniCommunicationEndpoint; commBufferSize: integer);
    function  Initialize: boolean; override;
    procedure OMAck(var msg: TOmniMessage); message MSG_ACK;
    procedure OMDumpTest(var msg: TOmniMessage); message MSG_DUMP_TEST;
    procedure OMEndTiming(var msg: TOmniMessage); message MSG_END_TIMING;
    procedure OMNotifyTestEnd(var msg: TOmniMessage); message MSG_NOTIFY_TEST_END;
    procedure OMReq(var msg: TOmniMessage); message MSG_REQ;
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

procedure TCommTester.InitiateDumpTest;
begin
  Task.Comm.Send(MSG_NOTIFY_TEST_START, 'Dump');
  ctTestSuite := tsDump;
  ctTestRepetition := 1;
  RunDumpTest;
end; 

procedure TCommTester.InitiateMessageExchangeTest;
begin
  Task.Comm.Send(MSG_NOTIFY_TEST_START, 'MessageExchange');
  ctTestSuite := tsMessageExchange;
  ctTestRepetition := 1;
  RunMessageExchangeTest;
end;

procedure TCommTester.OMAck(var msg: TOmniMessage);
begin
  if msg.MsgData.AsInteger < ctCommSize then
    ctComm.Send(MSG_REQ, msg.MsgData.AsInteger + 1)
  else
    ctComm.Send(MSG_END_TIMING, 0);
end;

procedure TCommTester.OMDumpTest(var msg: TOmniMessage);
begin
  Assert(ctTestSuite = tsDump);
  if msg.MsgData.AsInteger <> ctExpectedValue then
    raise Exception.CreateFmt('Invalid value received (%d, expected %d)',
      [msg.MsgData.AsInteger, ctExpectedValue]);
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
  Assert(TTestSuite(msg.MsgData.AsInteger) = ctTestSuite);
  if ctTestSuite = tsMessageExchange then begin
    Inc(ctTestRepetition);
    if ctTestRepetition <= 10 then
      RunMessageExchangeTest;
  end
  else if ctTestSuite = tsDump then begin
    Inc(ctTestRepetition);
    if ctTestRepetition <= 10 then
      RunDumpTest
    else
      InitiateMessageExchangeTest;
  end;
end;

procedure TCommTester.OMReq(var msg: TOmniMessage);
begin
  Assert(ctTestSuite = tsMessageExchange);
  if msg.MsgData.AsInteger <> ctExpectedValue then
    raise Exception.CreateFmt('Invalid value received (%d, expected %d)',
      [msg.MsgData.AsInteger, ctExpectedValue]);
  ctComm.Send(MSG_ACK, msg.MsgData);
  Inc(ctExpectedValue);
end;

procedure TCommTester.OMStartTest(var msg: TOmniMessage);
begin
  InitiateDumpTest;
end;

procedure TCommTester.OMStartTiming(var msg: TOmniMessage);
begin
  ctTestStart_ms := DSiTimeGetTime64;
  ctTestSuite := TTestSuite(msg.MsgData.AsInteger);
  ctExpectedValue := 1;
end;

procedure TCommTester.RunDumpTest;
var
  iMsg: integer;
begin
  // dump ctCommSize message into the comm link
  ctComm.Send(MSG_START_TIMING, Ord(tsDump));
  for iMsg := 1 to ctCommSize-2 do // leave place for MSG_START_TIMING and MSG_END_TIMING in the queue
    ctComm.Send(MSG_DUMP_TEST, iMsg);
  ctComm.Send(MSG_END_TIMING, 0);
end;

procedure TCommTester.RunMessageExchangeTest;
begin
  // run ctCommSize message exchanges
  ctComm.Send(MSG_START_TIMING, Ord(tsMessageExchange));
  ctComm.Send(MSG_REQ, 1);
end;

{ TfrmTestOtlComm }

procedure TfrmTestCommunications.btnRunTestsClick(Sender: TObject);
begin
  FClient1.Comm.Send(MSG_START_TEST, 0);
end;

procedure TfrmTestCommunications.FormCreate(Sender: TObject);
begin
  FCommChannel := CreateTwoWayChannel(CTestQueueLength);
  FClient1 := OmniEventMonitor1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint1, CTestQueueLength))).Run;
  FClient2 := OmniEventMonitor1.Monitor(
    CreateTask(TCommTester.Create(FCommChannel.Endpoint2, CTestQueueLength))).Run;
end;

procedure TfrmTestCommunications.FormDestroy(Sender: TObject);
begin
  FClient1.Terminate;
  FClient2.Terminate;
end;

procedure TfrmTestCommunications.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestCommunications.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
var
  testDuration_ms: int64;
begin
  case msg.MsgID of
    MSG_NOTIFY_TEST_START:
      Log(Format('Running test %s; %d messages', [string(msg.MsgData), CTestQueueLength]));
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
