unit TestOtlComm;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TestOmniMessageQueue = class
  public
    [Test]
    procedure TestBasics;
  end;

  [TestFixture]
  TestOmniMessageQueueSize1 = class
  public
    [Test]
    procedure TestSize1Queue;
  end;

  [TestFixture]
  TestIOmniTwoWayChannel = class
  public
    [Test]
    procedure TestSendReceive;
    [Test]
    procedure TestOtherEndpoint;
    [Test]
    procedure TestFIFOOrder;
  end;

  [TestFixture]
  TestIOmniMessageQueueTee = class
  public
    [Test]
    procedure TestBasicTee;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,
  OtlSync,
  OtlCommon, OtlComm;

{ TestOmniMessageQueue }

procedure TestOmniMessageQueue.TestBasics;
var
  mq : TOmniMessageQueue;
  msg: TOmniMessage;

  procedure CheckDequeue(msgId: integer; const msgData: string; success: boolean);
  var
    msg: TOmniMessage;
  begin
    Assert.AreEqual<boolean>(success, mq.TryDequeue(msg), '#' + msgData + '.TryDequeue');
    if success then begin
      Assert.AreEqual<integer>(msgId, msg.MsgID, '#' + msgData + '.MsgID');
      Assert.AreEqual<string>(msgData, msg.MsgData.AsString, '#' + msgData + '.MsgData');
    end;
  end;

begin
  mq := TOmniMessageQueue.Create(3);
  try
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(11, '11')));
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(12, '12')));
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(13, '13')));
    Assert.IsFalse(mq.Enqueue(TOmniMessage.Create(14, '14')));
    mq.Empty;
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(1, '1')));
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(2, '2')));
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(3, '3')));
    Assert.IsFalse(mq.Enqueue(TOmniMessage.Create(4, '4')));
    CheckDequeue(1, '1', true);
    CheckDequeue(2, '2', true);
    CheckDequeue(3, '3', true);
    CheckDequeue(4, '4', false);
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(9, '9')));
    msg := mq.Dequeue;
    Assert.AreEqual<integer>(9, msg.MsgID, 'MsgID');
    Assert.AreEqual<string>('9', msg.MsgData.AsString, 'MsgData');
  finally FreeAndNil(mq); end;
end;

{ TestOmniMessageQueueSize1 }

procedure TestOmniMessageQueueSize1.TestSize1Queue;
var
  msg: TOmniMessage;
begin
  var mq := TOmniMessageQueue.Create(1);
  try
    // Empty dequeue fails
    Assert.IsFalse(mq.TryDequeue(msg), 'Empty dequeue');

    // Enqueue 1 succeeds
    Assert.IsTrue(mq.Enqueue(TOmniMessage.Create(1, 'first')), 'Enqueue.1');

    // Second enqueue fails (full)
    Assert.IsFalse(mq.Enqueue(TOmniMessage.Create(2, 'second')), 'Enqueue.2');

    // Dequeue succeeds
    Assert.IsTrue(mq.TryDequeue(msg), 'Dequeue.1');
    Assert.AreEqual<integer>(1, msg.MsgID);
    Assert.AreEqual<string>('first', msg.MsgData.AsString);

    // Empty again
    Assert.IsFalse(mq.TryDequeue(msg), 'Dequeue.2');
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
    Assert.AreEqual<boolean>(success, endpoint.Receive(msg), tag + '.Receive');
    if success then begin
      Assert.AreEqual<integer>(msgID, msg.MsgID, tag + '.MsgID');
      Assert.AreEqual<string>(msgData, msg.MsgData.AsString, tag + '.MsgData');
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
    Assert.AreEqual<boolean>(success, endpoint.Receive(msg), tag + '.Receive');
    if success then begin
      Assert.AreEqual<integer>(msgID, msg.MsgID, tag + '.MsgID');
      Assert.AreEqual<string>(msgData, msg.MsgData.AsString, tag + '.MsgData');
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
  msg: TOmniMessage;
begin
  var chan := CreateTwoWayChannel(10);

  // Send multiple messages
  chan.Endpoint1.Send(TOmniMessage.Create(1, 'a'));
  chan.Endpoint1.Send(TOmniMessage.Create(2, 'b'));
  chan.Endpoint1.Send(TOmniMessage.Create(3, 'c'));

  // Receive in FIFO order
  Assert.IsTrue(chan.Endpoint2.Receive(msg), 'Receive.1');
  Assert.AreEqual<integer>(1, msg.MsgID);
  Assert.IsTrue(chan.Endpoint2.Receive(msg), 'Receive.2');
  Assert.AreEqual<integer>(2, msg.MsgID);
  Assert.IsTrue(chan.Endpoint2.Receive(msg), 'Receive.3');
  Assert.AreEqual<integer>(3, msg.MsgID);
  Assert.IsFalse(chan.Endpoint2.Receive(msg), 'Receive.4');
end;

{ TestIOmniMessageQueueTee }

procedure TestIOmniMessageQueueTee.TestBasicTee;
var
  msg1, msg2: TOmniMessage;
begin
  var tee := TOmniMessageQueueTee.Create;
  var q1 := TOmniMessageQueue.Create(3);
  var q2 := TOmniMessageQueue.Create(3);
  try
    tee.Attach(q1);
    tee.Attach(q2);

    // Enqueue via tee - both queues should receive copy
    Assert.IsTrue(tee.Enqueue(TOmniMessage.Create(42, 'hello')));

    Assert.IsTrue(q1.TryDequeue(msg1), 'q1.Dequeue');
    Assert.AreEqual<integer>(42, msg1.MsgID, 'q1.MsgID');
    Assert.AreEqual<string>('hello', msg1.MsgData.AsString, 'q1.MsgData');

    Assert.IsTrue(q2.TryDequeue(msg2), 'q2.Dequeue');
    Assert.AreEqual<integer>(42, msg2.MsgID, 'q2.MsgID');
    Assert.AreEqual<string>('hello', msg2.MsgData.AsString, 'q2.MsgData');

    tee.Detach(q1);
    tee.Detach(q2);
  finally
    FreeAndNil(q2);
    FreeAndNil(q1);
    // tee is ref-counted (TInterfacedObject)
  end;
end;

end.
