unit TestOtlComm;

interface

uses
  TestFramework;

type
  TestOmniMessageQueue = class(TTestCase)
  published
    procedure TestBasics;
    procedure TestNewMessageEvent;
  end;

  TestIOmniTwoWayChannel = class(TTestCase)
  published
    procedure TestSendReceive;
    procedure TestOtherEndpoint;
    procedure TestWait;
  end;

  TestIOmniMessageQueueTee = class(TTestCase)
  end;

implementation

uses
  {$IFDEF MSWindows}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.Classes, System.Threading,
  OtlSync, OtlSync.Utils,
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
    CheckEquals(success, mq.TryDequeue(msg), '#' + msgData + '.TryDequeue');
    if success then begin
      CheckEquals(msgId, msg.MsgID, '#' + msgData + '.MsgID');
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
    CheckEquals(9, msg.MsgID, 'MsgID');
    CheckEquals('9', msg.MsgData.AsString, 'MsgData');
  finally FreeAndNil(mq); end;
end;

procedure TestOmniMessageQueue.TestNewMessageEvent;
var
  evt: TOmniTransitionEvent;
  mq: TOmniMessageQueue;
  msg: TOmniMessage;

  procedure CheckEvent(state: boolean; const tag: string);
  begin
    {$IFDEF MSWindows}
    CheckEquals(state, WaitForSingleObject(evt, 0) = WAIT_OBJECT_0, tag);
    {$ELSE}
    CheckEquals(state, evt.WaitFor(0) = wrSignaled);
    {$ENDIF}
  end;

begin
  mq := TOmniMessageQueue.Create(3, true);
  try
    evt := mq.GetNewMessageEvent;
    CheckNotEquals(0, NativeUInt(evt), 'assigned event');
    CheckEvent(false, '#1');
    CheckTrue(mq.Enqueue(TOmniMessage.Create(1, '1')));
    CheckEvent(true, '#2');
    CheckEvent(false, '#3');
    msg := mq.Dequeue;
    CheckEvent(false, '#4');
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
      CheckEquals(msgID, msg.MsgID, tag + '.MsgID');
      CheckEquals(msgData, msg.MsgData.AsString, tag + '.MsgData');
    end;
  end;

begin
  chan := CreateTwoWayChannel(3, nil);

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

  procedure CheckReceive(success: boolean; const endpoint: IOmniCommuniCationEndpoint;
    msgID: integer; const msgData: string; const tag: string);
  var
    msg: TOmniMessage;
  begin
    CheckEquals(success, endpoint.Receive(msg), tag + '.Receive');
    if success then begin
      CheckEquals(msgID, msg.MsgID, tag + '.MsgID');
      CheckEquals(msgData, msg.MsgData.AsString, tag + '.MsgData');
    end;
  end;

begin
  chan := CreateTwoWayChannel(3, nil);

  chan.Endpoint1.Send(TOmniMessage.Create(1, '1'));
  CheckReceive(true, chan.Endpoint1.OtherEndpoint, 1, '1', '2');

  chan.Endpoint2.Send(TOmniMessage.Create(2, '2'));
  CheckReceive(true, chan.Endpoint2.OtherEndpoint, 2, '2', '5');
end;

procedure TestIOmniTwoWayChannel.TestWait;
var
  chan: IOmniTwoWayChannel;
  reader: IOmniCommunicationEndpoint;
  writer: IOmniCommunicationEndpoint;
  synch: IOmniSynchronizer<string>;
  readerTask: ITask;
  terminate: IOmniEvent;
  writerTask: ITask;
begin
  synch := TOmniSynchronizer<string>.Create;
  terminate := CreateOmniEvent(true, false);
  chan := CreateTwoWayChannel(3, terminate);
  reader:= chan.Endpoint1;
  writer := chan.Endpoint2;

  writerTask := TTask.Run(
    procedure
    begin
      synch.Signal('W');
      synch.WaitFor('start');
      synch.WaitFor('W:1');
      CheckTrue(writer.SendWait(1, '1', 0));
      synch.WaitFor('W:2');
      CheckTrue(writer.SendWait(21, '21', 0));
      CheckTrue(writer.SendWait(22, '22', 0));
      CheckTrue(writer.SendWait(23, '23', 0));
      CheckFalse(writer.SendWait(24, '24', 0));
      synch.Signal('W:3');
      CheckTrue(writer.SendWait(25, '25', 1000));
    end);

  readerTask := TTask.Run(
    procedure
    var
      i: integer;
      msg: TOmniMessage;
    begin
      synch.Signal('R');
      synch.WaitFor('start');
      CheckFalse(reader.ReceiveWait(msg, 0), 'R:Receive.1');
      CheckFalse(reader.ReceiveWait(msg, 100), 'R:Receive.2');
      synch.Signal('R:1');
      CheckTrue(reader.ReceiveWait(msg, 3000), 'R:Receive.3');
      synch.Signal('R:2');
      CheckEquals(1, msg.MsgID, 'R:MsgID.1');
      CheckEquals('1', msg.MsgData.AsString, 'R:MsgData.1');
      synch.WaitFor('W:3');
      for i := 1 to 3 do
        CheckTrue(reader.ReceiveWait(msg, 500), 'R:Receive.4.' + i.ToString);
      CheckTrue(reader.ReceiveWait(msg, 500), 'R:Receive.5');
      CheckEquals(25, msg.MsgID, 'R:MsgID');
      CheckEquals('25', msg.MsgData.AsString, 'R:MsgData');
    end);

  synch.WaitFor('W');
  synch.WaitFor('R');
  synch.Signal('start');
  CheckTrue(synch.WaitFor('R:1', 1000), 'WaitFor R:1');
  Sleep(100);
  synch.Signal('W:1');
  CheckTrue(synch.WaitFor('R:2', 3000), 'WaitFor R:2');
  synch.Signal('W:2');

  CheckTrue(readerTask.Wait(5000), 'R:Wait');
  CheckTrue(writerTask.Wait(5000), 'W:Wait');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestOmniMessageQueue.Suite);
  RegisterTest(TestIOmniTwoWayChannel.Suite);
  RegisterTest(TestIOmniMessageQueueTee.Suite);
end.
