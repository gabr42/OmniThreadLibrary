unit TestOtlComm;

interface

uses
  TestFramework;

type
  TestOmniMessageQueue = class(TTestCase)
  published
    procedure TestBasics;
  end;

  TestOmniMessageQueueSize1 = class(TTestCase)
  published
    procedure TestSize1Queue;
  end;

  TestIOmniTwoWayChannel = class(TTestCase)
  published
    procedure TestSendReceive;
    procedure TestOtherEndpoint;
    procedure TestFIFOOrder;
  end;

  TestIOmniMessageQueueTee = class(TTestCase)
  published
    procedure TestBasicTee;
  end;

  TestWorkerMessageLoop = class(TTestCase)
  published
    procedure TestRebuildInNestedProcessMessages;
  end;

implementation

uses
  Windows,
  SysUtils, Classes,
  OtlSync,
  OtlCommon, OtlComm, OtlTask, OtlTaskControl;

{ TestOmniMessageQueue }

procedure TestOmniMessageQueue.TestBasics;
var
  mq : TOmniMessageQueue;
  msg: TOmniMessage;

  procedure CheckDequeue(msgId: integer; const msgData: string; success: boolean);
  var
    msg: TOmniMessage;
  begin
    CheckEquals(success, mq.TryDequeue(msg), '#' + msgData + '.TryDequeue');
    if success then begin
      CheckEquals(msgId, integer(msg.MsgID), '#' + msgData + '.MsgID');
      CheckEquals(msgData, msg.MsgData.AsString, '#' + msgData + '.MsgData');
    end;
  end;

begin
  mq := TOmniMessageQueue.Create(3);
  try
    CheckTrue(mq.Enqueue(TOmniMessage.Create(11, '11')));
    CheckTrue(mq.Enqueue(TOmniMessage.Create(12, '12')));
    CheckTrue(mq.Enqueue(TOmniMessage.Create(13, '13')));
    CheckFalse(mq.Enqueue(TOmniMessage.Create(14, '14')));
    mq.Empty;
    CheckTrue(mq.Enqueue(TOmniMessage.Create(1, '1')));
    CheckTrue(mq.Enqueue(TOmniMessage.Create(2, '2')));
    CheckTrue(mq.Enqueue(TOmniMessage.Create(3, '3')));
    CheckFalse(mq.Enqueue(TOmniMessage.Create(4, '4')));
    CheckDequeue(1, '1', true);
    CheckDequeue(2, '2', true);
    CheckDequeue(3, '3', true);
    CheckDequeue(4, '4', false);
    CheckTrue(mq.Enqueue(TOmniMessage.Create(9, '9')));
    msg := mq.Dequeue;
    CheckEquals(9, integer(msg.MsgID), 'MsgID');
    CheckEquals('9', msg.MsgData.AsString, 'MsgData');
  finally FreeAndNil(mq); end;
end;

{ TestOmniMessageQueueSize1 }

procedure TestOmniMessageQueueSize1.TestSize1Queue;
var
  mq : TOmniMessageQueue;
  msg: TOmniMessage;
begin
  mq := TOmniMessageQueue.Create(1);
  try
    // Empty dequeue fails
    CheckFalse(mq.TryDequeue(msg), 'Empty dequeue');

    // Enqueue 1 succeeds
    CheckTrue(mq.Enqueue(TOmniMessage.Create(1, 'first')), 'Enqueue.1');

    // Second enqueue fails (full)
    CheckFalse(mq.Enqueue(TOmniMessage.Create(2, 'second')), 'Enqueue.2');

    // Dequeue succeeds
    CheckTrue(mq.TryDequeue(msg), 'Dequeue.1');
    CheckEquals(1, integer(msg.MsgID));
    CheckEquals('first', msg.MsgData.AsString);

    // Empty again
    CheckFalse(mq.TryDequeue(msg), 'Dequeue.2');
  finally FreeAndNil(mq); end;
end;

{ TestIOmniTwoWayChannel }

procedure TestIOmniTwoWayChannel.TestSendReceive;
var
  chan: IOmniTwoWayChannel;

  procedure CheckReceive(success: boolean; const endpoint: IOmniCommunicationEndpoint;
    msgID: integer; const msgData: string; const tag: string);
  var
    msg: TOmniMessage;
  begin
    CheckEquals(success, endpoint.Receive(msg), tag + '.Receive');
    if success then begin
      CheckEquals(msgID, integer(msg.MsgID), tag + '.MsgID');
      CheckEquals(msgData, msg.MsgData.AsString, tag + '.MsgData');
    end;
  end;

begin
  chan := CreateTwoWayChannel(3);

  chan.Endpoint1.Send(TOmniMessage.Create(1, '1'));
  CheckReceive(false, chan.Endpoint1, 0, '', '1');
  CheckReceive(true, chan.Endpoint2, 1, '1', '2');
  CheckReceive(false, chan.Endpoint2, 0, '', '3');

  chan.Endpoint2.Send(TOmniMessage.Create(2, '2'));
  CheckReceive(false, chan.Endpoint2, 0, '', '4');
  CheckReceive(true, chan.Endpoint1, 2, '2', '5');
  CheckReceive(false, chan.Endpoint1, 0, '', '6');
end;

procedure TestIOmniTwoWayChannel.TestOtherEndpoint;
var
  chan: IOmniTwoWayChannel;

  procedure CheckReceive(success: boolean; const endpoint: IOmniCommunicationEndpoint;
    msgID: integer; const msgData: string; const tag: string);
  var
    msg: TOmniMessage;
  begin
    CheckEquals(success, endpoint.Receive(msg), tag + '.Receive');
    if success then begin
      CheckEquals(msgID, integer(msg.MsgID), tag + '.MsgID');
      CheckEquals(msgData, msg.MsgData.AsString, tag + '.MsgData');
    end;
  end;

begin
  chan := CreateTwoWayChannel(3);

  chan.Endpoint1.Send(TOmniMessage.Create(1, '1'));
  CheckReceive(true, chan.Endpoint1.OtherEndpoint, 1, '1', '2');

  chan.Endpoint2.Send(TOmniMessage.Create(2, '2'));
  CheckReceive(true, chan.Endpoint2.OtherEndpoint, 2, '2', '5');
end;

procedure TestIOmniTwoWayChannel.TestFIFOOrder;
var
  chan: IOmniTwoWayChannel;
  msg : TOmniMessage;
begin
  chan := CreateTwoWayChannel(10);

  // Send multiple messages
  chan.Endpoint1.Send(TOmniMessage.Create(1, 'a'));
  chan.Endpoint1.Send(TOmniMessage.Create(2, 'b'));
  chan.Endpoint1.Send(TOmniMessage.Create(3, 'c'));

  // Receive in FIFO order
  CheckTrue(chan.Endpoint2.Receive(msg), 'Receive.1');
  CheckEquals(1, integer(msg.MsgID));
  CheckTrue(chan.Endpoint2.Receive(msg), 'Receive.2');
  CheckEquals(2, integer(msg.MsgID));
  CheckTrue(chan.Endpoint2.Receive(msg), 'Receive.3');
  CheckEquals(3, integer(msg.MsgID));
  CheckFalse(chan.Endpoint2.Receive(msg), 'Receive.4');
end;

{ TestIOmniMessageQueueTee }

procedure TestIOmniMessageQueueTee.TestBasicTee;
var
  tee       : TOmniMessageQueueTee;
  q1        : TOmniMessageQueue;
  q2        : TOmniMessageQueue;
  msg1, msg2: TOmniMessage;
begin
  tee := TOmniMessageQueueTee.Create;
  q1 := TOmniMessageQueue.Create(3);
  q2 := TOmniMessageQueue.Create(3);
  try
    tee.Attach(q1);
    tee.Attach(q2);

    // Enqueue via tee - both queues should receive copy
    CheckTrue(tee.Enqueue(TOmniMessage.Create(42, 'hello')));

    CheckTrue(q1.TryDequeue(msg1), 'q1.Dequeue');
    CheckEquals(42, integer(msg1.MsgID), 'q1.MsgID');
    CheckEquals('hello', msg1.MsgData.AsString, 'q1.MsgData');

    CheckTrue(q2.TryDequeue(msg2), 'q2.Dequeue');
    CheckEquals(42, integer(msg2.MsgID), 'q2.MsgID');
    CheckEquals('hello', msg2.MsgData.AsString, 'q2.MsgData');

    tee.Detach(q1);
    tee.Detach(q2);
  finally
    FreeAndNil(q2);
    FreeAndNil(q1);
    // tee is ref-counted (TInterfacedObject)
  end;
end;

{ TestWorkerMessageLoop }

const
  MSG_OUTER      = 1;
  MSG_UNREGISTER = 2;
  MSG_INNER      = 3;

type
  TNestedLoopWorker = class(TOmniWorker)
  strict private
    FComm    : IOmniTwoWayChannel;
    FGo      : THandle;
    FInInner : boolean;
    FNumWaits: integer;
  protected
    procedure BeforeWait(var timeout_ms: cardinal); override;
    procedure Cleanup; override;
    function  Initialize: boolean; override;
  public
    constructor Create(go: THandle);
    procedure MsgInner(var msg: TOmniMessage); message MSG_INNER;
    procedure MsgOuter(var msg: TOmniMessage); message MSG_OUTER;
    procedure MsgUnregister(var msg: TOmniMessage); message MSG_UNREGISTER;
  end;

constructor TNestedLoopWorker.Create(go: THandle);
begin
  inherited Create;
  FGo := go;
end;

procedure TNestedLoopWorker.BeforeWait(var timeout_ms: cardinal);
begin
  if FInInner then begin
    Inc(FNumWaits);
    if FNumWaits > 1000 then // don't hang the test
      raise Exception.Create('TNestedLoopWorker: ProcessMessages is looping');
  end;
end;

procedure TNestedLoopWorker.Cleanup;
begin
  FComm := nil;
  inherited;
end;

function TNestedLoopWorker.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then begin
    FComm := CreateTwoWayChannel;
    Task.RegisterComm(FComm.Endpoint1);
  end;
end;

procedure TNestedLoopWorker.MsgInner(var msg: TOmniMessage);
begin
  FInInner := true;
  ProcessMessages;
  FInInner := false;
  Task.Comm.Send(MSG_INNER);
end;

procedure TNestedLoopWorker.MsgOuter(var msg: TOmniMessage);
begin
  WaitForSingleObject(FGo, 5000); // wait until all messages are in the queue
  ProcessMessages;
end;

procedure TNestedLoopWorker.MsgUnregister(var msg: TOmniMessage);
begin
  Task.UnregisterComm(FComm.Endpoint1);
  FComm := nil; // closes the handle
end;

procedure TestWorkerMessageLoop.TestRebuildInNestedProcessMessages;
// MsgOuter -> ProcessMessages -> MsgUnregister, which triggers a rebuild of wait handles.
// Rebuild code then dispatches MsgInner, which calls ProcessMessages. That call must not use
// wait handles from before the rebuild.
var
  go  : THandle;
  msg : TOmniMessage;
  task: IOmniTaskControl;
begin
  go := CreateEvent(nil, true, false, nil);
  try
    task := CreateTask(TNestedLoopWorker.Create(go) as IOmniWorker, 'nested loop').Run;
    task.WaitForInit;
    Sleep(100); // let the worker enter the message loop
    task.Comm.Send(MSG_OUTER);
    task.Comm.Send(MSG_UNREGISTER);
    task.Comm.Send(MSG_INNER);
    SetEvent(go);
    CheckTrue(task.Comm.ReceiveWait(msg, 5000), 'Nested ProcessMessages did not exit');
    CheckEquals(MSG_INNER, msg.MsgID);
    CheckTrue(task.Terminate(5000), 'Terminate');
    task := nil;
  finally CloseHandle(go); end;
end;

initialization
  RegisterTest(TestOmniMessageQueue.Suite);
  RegisterTest(TestOmniMessageQueueSize1.Suite);
  RegisterTest(TestIOmniTwoWayChannel.Suite);
  RegisterTest(TestIOmniMessageQueueTee.Suite);
  RegisterTest(TestWorkerMessageLoop.Suite);
end.
