///<summary>Two-way intraprocess communication channel. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Author            : Primoz Gabrijelcic
///   Contributors      : GJ, Lee_Nover
///   Creation date     : 2008-06-12
///   Last modification : 2011-11-09
///   Version           : 1.08a
///</para><para>
///   History:
///     1.08a: 2011-11-09
///       - TOmniMessageQueue.Enqueue leaked if queue was full and value contained
///         reference counted value (found by [meishier]).
///     1.08: 2011-11-05
///       - Adapted to OtlCommon 1.24.
///     1.07: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.06a: 2010-05-06
///       - Fixed memory leak when sending String, WideString, Variant and Extended values
///         over the communication channel.
///     1.06: 2010-03-08
///       - Implemented TOmniMessageQueueTee and IOmniCommDispatchingObserver.
///     1.05: 2009-11-13
///       - Default queue size reduced to 1000 messages.
///     1.04: 2009-04-05
///       - Implemented TOmniMessageQueue.Empty and TryDequeue.
///       - TOmniMessageQueue empties itself before it is destroyed.
///     1.03: 2008-10-05
///       - Added two overloaded versions of IOmniCommunicationEndpoint.ReceivedWait,
///         which are just simple wrappers for WaitForSingleObject(NewMessageEvent) +
///         Receive.
///       - Defined OmniThreadLibrary-reserved message ID $FFFF.
///     1.02: 2008-09-26
///       - Better default queue calculation that takes into account OtlContainers
///         overhead and FastMM4 granulation.
///     1.01: 2008-09-20
///       - Added two TOmniMessage constructors.
///</para></remarks>

unit OtlComm;

{$I OtlOptions.inc}
{$WARN SYMBOL_PLATFORM OFF} // Win32Check

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  SyncObjs,
  GpStuff,
  DSiWin32,
  OtlCommon,
  OtlSync,
  OtlContainerObserver,
  OtlContainers;

const
  //reserved for internal OTL messaging
  COtlReservedMsgID = $FFFF;
  //Max send wait time
  CMaxSendWaitTime_ms = 100;

  CDefaultQueueSize = 1000;

type
  {$A4}
  TOmniMessage = record
    MsgID  : word;
    MsgData: TOmniValue;
    constructor Create(aMsgID: word; aMsgData: TOmniValue); overload;
    constructor Create(aMsgID: word); overload;
  end; { TOmniMessage }

  TOmniMessageQueue = class;

  IOmniCommunicationEndpoint = interface ['{910D329C-D049-48B9-B0C0-9434D2E57870}']
    function  GetNewMessageEvent: THandle;
    function  GetOtherEndpoint: IOmniCommunicationEndpoint;
    function  GetReader: TOmniMessageQueue;
    function  GetWriter: TOmniMessageQueue;
  //
    function  Receive(var msg: TOmniMessage): boolean; overload;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload;
    function  ReceiveWait(var msg: TOmniMessage; timeout_ms: cardinal): boolean; overload;
    function  ReceiveWait(var msgID: word; var msgData: TOmniValue; timeout_ms: cardinal): boolean; overload;
    procedure Send(const msg: TOmniMessage); overload;
    procedure Send(msgID: word); overload;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload;
    function  SendWait(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    function  SendWait(msgID: word; msgData: TOmniValue; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    property NewMessageEvent: THandle read GetNewMessageEvent;
    property OtherEndpoint: IOmniCommunicationEndpoint read GetOtherEndpoint;
    property Reader: TOmniMessageQueue read GetReader;
    property Writer: TOmniMessageQueue read GetWriter;
  end; { IOmniCommunicationEndpoint }

  IOmniTwoWayChannel = interface ['{3ED1AB88-4209-4E01-AA79-A577AD719520}']
    function Endpoint1: IOmniCommunicationEndpoint;
    function Endpoint2: IOmniCommunicationEndpoint;
  end; { IOmniTwoWayChannel }

  TOmniMessageQueueMessageEvent = procedure(Sender: TObject; const msg: TOmniMessage) of object;

  {:Fixed-size ring buffer of TOmniMessage data. Supports multiple simultaneous readers
    and writers.
  }
  TOmniMessageQueue = class(TOmniBoundedQueue)
  strict private
    mqWinEventObserver: TOmniContainerWindowsEventObserver;
    mqWinMsgObserver  : record
      Observer : TOmniContainerWindowsMessageObserver;
      Window   : THandle;
      OnMessage: TOmniMessageQueueMessageEvent;
    end;
  strict protected
    procedure AttachWinEventObserver;
    procedure SetOnMessage(const value: TOmniMessageQueueMessageEvent);
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(numMessages: integer; createEventObserver: boolean = true);
      reintroduce;
    destructor  Destroy; override;
    function  Dequeue: TOmniMessage; reintroduce;
    function  Enqueue(const value: TOmniMessage): boolean; reintroduce;
    procedure Empty;
    function  GetNewMessageEvent: THandle;
    function  TryDequeue(var msg: TOmniMessage): boolean; reintroduce;
    property EventObserver: TOmniContainerWindowsEventObserver read mqWinEventObserver;
    property OnMessage: TOmniMessageQueueMessageEvent read mqWinMsgObserver.OnMessage write SetOnMessage;
  end; { TOmniMessageQueue }

  IOmniMessageQueueTee = interface ['{8A9526BF-71AA-4D78-BAE8-3490C3987327}']
    procedure Attach(const queue: TOmniMessageQueue);
    procedure Detach(const queue: TOmniMessageQueue);
    function Enqueue(const value: TOmniMessage): boolean;
  end;{ IOmniMessageQueueTee }

  TOmniMessageQueueTee = class(TInterfacedObject, IOmniMessageQueueTee)
  strict private
    obqtQueueList: TList;
    obqtQueueLock: TOmniCS;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Attach(const queue: TOmniMessageQueue);
    procedure Detach(const queue: TOmniMessageQueue);
    function Enqueue(const value: TOmniMessage): boolean;
  end; { TOmniMessageQueueTee }

  IOmniCommDispatchingObserver = interface ['{3DCC4745-14E1-4AE2-B2B3-D4B9E36CF483}']
  end; { IOmniCommDispatchingObserver }

  function CreateDispatchingObserver(queue: TOmniMessageQueue; dispatchTo: TObject):
    IOmniCommDispatchingObserver;

  function CreateTwoWayChannel(numElements: integer = CDefaultQueueSize;
    taskTerminatedEvent: THandle = 0): IOmniTwoWayChannel;

implementation

uses
  Variants,
  {$IFDEF OTL_HasSystemTypes}
  System.Types,
  {$ENDIF}
  {$IFDEF DEBUG}OtlCommBufferTest,{$ENDIF}
  OtlEventMonitor;

const
  MSG_CLIENT_MESSAGE = WM_USER;

type
  IOmniCommunicationEndpointInternal = interface ['{4F872DE9-6E9A-4881-B9EC-E2189DAC00F4}']
    procedure DetachFromQueues;
  end; { IOmniCommunicationEndpointInternal }

  TOmniTwoWayChannel = class;

  TOmniCommunicationEndpoint = class(TInterfacedObject, IOmniCommunicationEndpoint,
    IOmniCommunicationEndpointInternal)
  strict private
    ceOwner_ref              : TOmniTwoWayChannel;
    cePartlyEmptyObserver    : TOmniContainerWindowsEventObserver;
    ceReader_ref             : TOmniMessageQueue;
    ceTaskTerminatedEvent_ref: THandle;
    ceWriter_ref             : TOmniMessageQueue;
  strict protected
    procedure RequirePartlyEmptyObserver;
  protected
    procedure DetachFromQueues;
    function  GetNewMessageEvent: THandle;
    function  GetOtherEndpoint: IOmniCommunicationEndpoint;
    function  GetReader: TOmniMessageQueue;
    function  GetWriter: TOmniMessageQueue;
  public
    constructor Create(owner: TOmniTwoWayChannel; readQueue, writeQueue: TOmniMessageQueue;
      taskTerminatedEvent_ref: THandle);
    destructor  Destroy; override;
    function  Receive(var msg: TOmniMessage): boolean; overload; inline;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload; inline;
    function  ReceiveWait(var msg: TOmniMessage; timeout_ms: cardinal): boolean; overload; inline;
    function  ReceiveWait(var msgID: word; var msgData: TOmniValue; timeout_ms: cardinal):
      boolean; overload; inline;
    procedure Send(msgID: word); overload; inline;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload; inline;
    procedure Send(const msg: TOmniMessage); overload; inline;
    function  SendWait(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload; inline;
    function  SendWait(msgID: word; msgData: TOmniValue;
      timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    property NewMessageEvent: THandle read GetNewMessageEvent;
    property OtherEndpoint: IOmniCommunicationEndpoint read GetOtherEndpoint;
    property Reader: TOmniMessageQueue read GetReader;
    property Writer: TOmniMessageQueue read GetWriter;
  end; { TOmniCommunicationEndpoint }

  TOmniTwoWayChannel = class(TInterfacedObject, IOmniTwoWayChannel)
  strict private
    twcEndpoint             : array [1..2] of IOmniCommunicationEndpoint;
    twcLock                 : TOmniCS;
    twcMessageQueueSize     : integer;
    twcTaskTerminatedEvt_ref: THandle;
    twcUnidirQueue          : array [1..2] of TOmniMessageQueue;
  strict protected
    procedure CreateBuffers; inline;
  protected
    function  OtherEndpoint(endpoint: IOmniCommunicationEndpoint): IOmniCommunicationEndpoint;
  public
    constructor Create(messageQueueSize: integer; taskTerminatedEvent: THandle);
    destructor  Destroy; override;
    function Endpoint1: IOmniCommunicationEndpoint; inline;
    function Endpoint2: IOmniCommunicationEndpoint; inline;
  end; { TOmniTwoWayChannel }

  TOmniCommDispatchingObserverImpl = class(TInterfacedObject, IOmniCommDispatchingObserver)
  strict private
    cdoDispatchTo : TObject;
    cdoDispatchWnd: HWND;
    cdoObserver   : TOmniContainerWindowsMessageObserver;
    cdoQueue      : TOmniMessageQueue;
  strict protected
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(queue: TOmniMessageQueue; dispatchTo: TObject);
    destructor  Destroy; override;
  end; { TOmniCommDispatchingObserverImpl }

{ exports }

function CreateDispatchingObserver(queue: TOmniMessageQueue; dispatchTo: TObject):
  IOmniCommDispatchingObserver;
begin
  Result := TOmniCommDispatchingObserverImpl.Create(queue, dispatchTo);
end; { CreateDispatchingObserver }

function CreateTwoWayChannel(numElements: integer = CDefaultQueueSize;
  taskTerminatedEvent: THandle = 0): IOmniTwoWayChannel;
begin
  Result := TOmniTwoWayChannel.Create(numElements, taskTerminatedEvent);
end; { CreateTwoWayChannel }

{ TOmniMessage }

constructor TOmniMessage.Create(aMsgID: word; aMsgData: TOmniValue);
begin
  MsgID := aMsgID;
  MsgData := aMsgData;
end; { TOmniMessage.Create }

constructor TOmniMessage.Create(aMsgID: word);
begin
  MsgID := aMsgID;
  MsgData := TOmniValue.Null;
end; { TOmniMessage.Create }

{ TOmniMessageQueue }

constructor TOmniMessageQueue.Create(numMessages: integer; createEventObserver: boolean);
begin
  inherited Create(numMessages, SizeOf(TOmniMessage));
  if createEventObserver then
    AttachWinEventObserver;
end; { TOmniMessageQueue.Create }

destructor TOmniMessageQueue.Destroy;
begin
  OnMessage := nil;
  ContainerSubject.Detach(mqWinEventObserver, coiNotifyOnAllInserts);
  FreeAndNil(mqWinEventObserver);
  Empty;
  inherited;
end; { TOmniMessageQueue.Destroy }

procedure TOmniMessageQueue.AttachWinEventObserver;
begin
  if not assigned(mqWinEventObserver) then begin
    mqWinEventObserver := CreateContainerWindowsEventObserver;
    ContainerSubject.Attach(mqWinEventObserver, coiNotifyOnAllInserts);
  end;
  mqWinEventObserver.Activate;
end; { TOmniMessageQueue.AttachWinEventObserver }

function TOmniMessageQueue.Dequeue: TOmniMessage;
begin
  if not TryDequeue(Result) then
    raise Exception.Create('TOmniMessageQueue.Dequeue: Message queue is empty');
end; { TOmniMessageQueue.Dequeue }

procedure TOmniMessageQueue.Empty;
var
  msg: TOmniMessage;
begin
  while TryDequeue(msg) do
    ;
end; { TOmniMessageQueue.Empty }

function TOmniMessageQueue.Enqueue(const value: TOmniMessage): boolean;
var
  tmp: TOmniMessage;
begin
  tmp := value;
  tmp.MsgData._AddRef;
  Result := inherited Enqueue(tmp);
  if Result then
    tmp.MsgData.RawZero
  else
    tmp.MsgData._Release;
end; { TOmniMessageQueue.Enqueue }

function TOmniMessageQueue.GetNewMessageEvent: THandle;
begin
  AttachWinEventObserver;
  Result := mqWinEventObserver.GetEvent;
end; { TOmniMessageQueue.GetNewMessageEvent }

procedure TOmniMessageQueue.SetOnMessage(const value: TOmniMessageQueueMessageEvent);
begin
  if (not assigned(mqWinMsgObserver.OnMessage)) and assigned(value) then begin // set up observer
    mqWinMsgObserver.Window := DSiAllocateHWnd(WndProc);
    mqWinMsgObserver.Observer := CreateContainerWindowsMessageObserver(
      mqWinMsgObserver.Window, MSG_CLIENT_MESSAGE, 0, 0);
    ContainerSubject.Attach(mqWinMsgObserver.Observer, coiNotifyOnAllInserts);
    mqWinMsgObserver.Observer.Activate;
  end
  else if assigned(mqWinMsgObserver.OnMessage) and (not assigned(value)) then begin // tear down observer
    mqWinMsgObserver.Observer.Deactivate;
    ContainerSubject.Detach(mqWinMsgObserver.Observer, coiNotifyOnAllInserts);
    FreeAndNil(mqWinMsgObserver.Observer);
    DSiDeallocateHWnd(mqWinMsgObserver.Window);
  end;
  mqWinMsgObserver.OnMessage := value;
end; { TOmniMessageQueue.SetOnMessage }

function TOmniMessageQueue.TryDequeue(var msg: TOmniMessage): boolean;
var
  tmp: TOmniMessage;
begin
  tmp.MsgData.RawZero;
  Result := inherited Dequeue(tmp);
  if not Result then
    Exit;
  msg := tmp;
  tmp.MsgData._Release;
end; { TOmniMessageQueue.TryDequeue }

procedure TOmniMessageQueue.WndProc(var msg: TMessage);
var
  queueMsg: TOmniMessage;
begin
  if (msg.Msg = MSG_CLIENT_MESSAGE) and assigned(mqWinMsgObserver.OnMessage) then
    while TryDequeue(queueMsg) do
      mqWinMsgObserver.OnMessage(Self, queueMsg);
end; { TOmniMessageQueue.WndProc }

{ TOmniCommunicationEndpoint }

constructor TOmniCommunicationEndpoint.Create(owner: TOmniTwoWayChannel; readQueue,
  writeQueue: TOmniMessageQueue; taskTerminatedEvent_ref: THandle);
begin
  inherited Create;
  ceOwner_ref := owner;
  ceReader_ref := readQueue;
  ceWriter_ref := writeQueue;
  ceTaskTerminatedEvent_ref := taskTerminatedEvent_ref;
end; { TOmniCommunicationEndpoint.Create }

destructor TOmniCommunicationEndpoint.Destroy;
begin
  if assigned(ceWriter_ref) and assigned(ceWriter_ref.ContainerSubject) and
     assigned(cePartlyEmptyObserver) 
  then
    ceWriter_ref.ContainerSubject.Detach(cePartlyEmptyObserver, coiNotifyOnPartlyEmpty);
  FreeAndNil(cePartlyEmptyObserver);
end; { TOmniCommunicationEndpoint.Destroy }

procedure TOmniCommunicationEndpoint.DetachFromQueues;
begin
  ceReader_ref := nil;
  ceWriter_ref := nil;
end; { TOmniCommunicationEndpoint.DetachFromQueues }

function TOmniCommunicationEndpoint.GetNewMessageEvent: THandle;
begin
  Result := ceReader_ref.GetNewMessageEvent;
end; { TOmniCommunicationEndpoint.GetNewMessageEvent }

function TOmniCommunicationEndpoint.GetOtherEndpoint: IOmniCommunicationEndpoint;
begin
  Result := ceOwner_ref.OtherEndpoint(Self);
end; { TOmniCommunicationEndpoint.GetOtherEndpoint }

{ TOmniCommunicationEndpoint.GetNewMessageEvent }

function TOmniCommunicationEndpoint.GetReader: TOmniMessageQueue;
begin
  Result := ceReader_ref;
end; { TOmniCommunicationEndpoint.GetReader }

function TOmniCommunicationEndpoint.Receive(var msgID: word; var msgData:
  TOmniValue): boolean;
var
  msg: TOmniMessage;
begin
  Result := Receive(msg);
  if Result then begin
    msgID := msg.msgID;
    msgData := msg.msgData;
  end;
end; { TOmniCommunicationEndpoint.Receive }

function TOmniCommunicationEndpoint.Receive(var msg: TOmniMessage): boolean;
begin
  Result := ceReader_ref.TryDequeue(msg);
end; { TOmniCommunicationEndpoint.Receive }

function TOmniCommunicationEndpoint.ReceiveWait(var msg: TOmniMessage; timeout_ms:
  cardinal): boolean;
begin
  Result := Receive(msg);
  if (not Result) and (timeout_ms > 0) then begin
    if ceTaskTerminatedEvent_ref = 0 then
      raise Exception.Create('TOmniCommunicationEndpoint.ReceiveWait: <task terminated> event is not set');
    ResetEvent(ceReader_ref.GetNewMessageEvent);
    Result := Receive(msg);
    if not Result then begin
      if DSiWaitForTwoObjects(ceReader_ref.GetNewMessageEvent,
           ceTaskTerminatedEvent_ref, false, timeout_ms) = WAIT_OBJECT_0 then
      begin
        msg := ceReader_ref.Dequeue;
        Result := true;
      end
    end; //if not Result
  end;
end; { TOmniCommunicationEndpoint.ReceiveWait }

function TOmniCommunicationEndpoint.ReceiveWait(var msgID: word; var msgData: TOmniValue;
  timeout_ms: cardinal): boolean;
var
  msg: TOmniMessage;
begin
  Result := ReceiveWait(msg, timeout_ms);
  if Result then begin
    msgID := msg.MsgID;
    msgData := msg.MsgData;
  end;
end; { TOmniCommunicationEndpoint.ReceiveWait }

procedure TOmniCommunicationEndpoint.RequirePartlyEmptyObserver;
begin
  if not assigned(cePartlyEmptyObserver) then begin
    cePartlyEmptyObserver := CreateContainerWindowsEventObserver;
    OtherEndpoint.Reader.ContainerSubject.Attach(cePartlyEmptyObserver, coiNotifyOnPartlyEmpty);
  end
  else begin
    cePartlyEmptyObserver.Deactivate;
    ResetEvent(cePartlyEmptyObserver.GetEvent);
  end;
end; { TOmniCommunicationEndpoint.RequirePartlyEmptyObserver }

procedure TOmniCommunicationEndpoint.Send(const msg: TOmniMessage);
begin
  if not ceWriter_ref.Enqueue(msg) then
    raise Exception.Create('TOmniCommunicationEndpoint.Send: Queue is full');
end;  { TOmniCommunicationEndpoint.Send }

function TOmniCommunicationEndpoint.SendWait(msgID: word; msgData: TOmniValue;
  timeout_ms: cardinal): boolean;
label
  retry;
var
  msg      : TOmniMessage;
  startTime: int64;
  waitTime : integer;
begin
  msg.msgID := msgID;
  msg.msgData := msgData;
  Result := ceWriter_ref.Enqueue(msg);
  if (not Result) and (timeout_ms > 0) then begin
    if ceTaskTerminatedEvent_ref = 0 then
      raise Exception.Create('TOmniCommunicationEndpoint.SendWait: <task terminated> event is not set');
    startTime := GetTickCount;
  retry:
    RequirePartlyEmptyObserver;
    Result := ceWriter_ref.Enqueue(msg);
    if not Result then begin
      while not Result do begin
        waitTime := timeout_ms - DSiElapsedSince(GetTickCount, startTime);
        if (waitTime >= 0) and
           (DSiWaitForTwoObjects(cePartlyEmptyObserver.GetEvent, ceTaskTerminatedEvent_ref,
             false, waitTime) = WAIT_OBJECT_0)
        then begin
          Result := ceWriter_ref.Enqueue(msg);
          if not Result then
            goto retry;
        end
        else
          break; //while
      end; //while
    end; //if not Result
  end;
  if not Result then
    msg.msgData._ReleaseAndClear;
end; { TOmniCommunicationEndpoint.SendWait }

function TOmniCommunicationEndpoint.SendWait(msgID: word;
  timeout_ms: cardinal): boolean;
var
  msg: TOmniMessage;
begin
  msg.msgID := msgID;
  msg.msgData := TOmniValue.Null;
  result := SendWait(msgID, msg.msgData, timeout_ms);
end; { TOmniCommunicationEndpoint.SendWait }

procedure TOmniCommunicationEndpoint.Send(msgID: word; msgData: TOmniValue);
var
  msg: TOmniMessage;
begin
  msg.msgID := msgID;
  msg.msgData := msgData;
  Send(msg);
end; { TOmniCommunicationEndpoint.Send }

procedure TOmniCommunicationEndpoint.Send(msgID: word; msgData: array of const);
begin
  Send(msgID, TOmniValue.Create(msgData));
end; { TOmniCommunicationEndpoint.Send }

procedure TOmniCommunicationEndpoint.Send(msgID: word);
begin
  Send(msgID, TOmniValue.Null);
end; { TOmniCommunicationEndpoint.Send }

function TOmniCommunicationEndpoint.GetWriter: TOmniMessageQueue;
begin
  Result := ceWriter_ref;
end; { TOmniCommunicationEndpoint.GetWriter }

{ TOmniTwoWayChannel }

constructor TOmniTwoWayChannel.Create(messageQueueSize: integer; taskTerminatedEvent:
  THandle);
begin
  inherited Create;
  twcMessageQueueSize := messageQueueSize;
  twcTaskTerminatedEvt_ref := taskTerminatedEvent;
end; { TOmniTwoWayChannel.Create }

destructor TOmniTwoWayChannel.Destroy;
var
  i: integer;
begin
  for i := 1 to 2 do
    if assigned(twcEndpoint[i]) then
      (twcEndpoint[i] as IOmniCommunicationEndpointInternal).DetachFromQueues;
  for i := 1 to 2 do begin
    twcUnidirQueue[i].Free;
    twcUnidirQueue[i] := nil;
  end;
  inherited;
end; { TOmniTwoWayChannel.Destroy }

procedure TOmniTwoWayChannel.CreateBuffers;
begin
  if twcUnidirQueue[1] = nil then
    twcUnidirQueue[1] := TOmniMessageQueue.Create(twcMessageQueueSize);
  if twcUnidirQueue[2] = nil then
    twcUnidirQueue[2] := TOmniMessageQueue.Create(twcMessageQueueSize);
end; { TOmniTwoWayChannel.CreateBuffers }

function TOmniTwoWayChannel.Endpoint1: IOmniCommunicationEndpoint;
begin
  if twcEndpoint[1] = nil then begin
    twcLock.Acquire;
    try
      if twcEndpoint[1] = nil then begin
        CreateBuffers;
        twcEndpoint[1] := TOmniCommunicationEndpoint.Create(Self, twcUnidirQueue[1], twcUnidirQueue[2], twcTaskTerminatedEvt_ref);
      end;
    finally twcLock.Release; end;
  end;
  Result := twcEndpoint[1];
end; { TOmniTwoWayChannel.Endpoint1 }

function TOmniTwoWayChannel.Endpoint2: IOmniCommunicationEndpoint;
begin
  if twcEndpoint[2] = nil then begin
    twcLock.Acquire;
    try
      if twcEndpoint[2] = nil then begin
        CreateBuffers;
        twcEndpoint[2] := TOmniCommunicationEndpoint.Create(Self, twcUnidirQueue[2], twcUnidirQueue[1], twcTaskTerminatedEvt_ref);
      end;
    finally twcLock.Release; end;
  end;
  Result := twcEndpoint[2];
end; { TOmniTwoWayChannel.Endpoint2 }

function TOmniTwoWayChannel.OtherEndpoint(endpoint: IOmniCommunicationEndpoint):
  IOmniCommunicationEndpoint;
begin
  if endpoint = Endpoint1 then
    Result := Endpoint2
  else if endpoint = Endpoint2 then
    Result := Endpoint1
  else
    raise Exception.Create('TOmniTwoWayChannel.OtherEndpoint: Invalid endpoint!');
end; { TOmniTwoWayChannel.OtherEndpoint }

{ TOmniMessageQueueTee }

constructor TOmniMessageQueueTee.Create;
begin
  inherited Create;
  obqtQueueList := TList.Create;
end; { TOmniMessageQueueTee.Create }

destructor TOmniMessageQueueTee.Destroy;
begin
  FreeAndNil(obqtQueueList);
  inherited;
end; { TOmniMessageQueueTee.Destroy }

procedure TOmniMessageQueueTee.Attach(const queue: TOmniMessageQueue);
begin
  obqtQueueLock.Acquire;
  try
    obqtQueueList.Add(queue);
  finally obqtQueueLock.Release; end;
end; { TOmniMessageQueueTee.Attach }

procedure TOmniMessageQueueTee.Detach(const queue: TOmniMessageQueue);
begin
  obqtQueueLock.Acquire;
  try
    obqtQueueList.Remove(queue);
  finally obqtQueueLock.Release; end;
end; { TOmniMessageQueueTee.Detach }

function TOmniMessageQueueTee.Enqueue(const value: TOmniMessage): boolean;
var
  pQueue: pointer;
begin
  Result := true;
  obqtQueueLock.Acquire;
  try
    for pQueue in obqtQueueList do
      Result := Result and TOmniMessageQueue(pQueue).Enqueue(value);
  finally obqtQueueLock.Release; end;
end; { TOmniMessageQueueTee.Enqueue }

{ TOmniCommDispatchingObserverImpl }

constructor TOmniCommDispatchingObserverImpl.Create(queue: TOmniMessageQueue;
  dispatchTo: TObject);
begin
  inherited Create;
  cdoDispatchTo := dispatchTo;
  cdoQueue := queue;
  cdoDispatchWnd := DSiAllocateHWnd(WndProc);
  Win32Check(cdoDispatchWnd <> 0);
  cdoObserver := CreateContainerWindowsMessageObserver(cdoDispatchWnd, WM_USER, 0, 0);
  cdoQueue.ContainerSubject.Attach(cdoObserver, coiNotifyOnAllInserts);
end; { TOmniCommDispatchingObserverImpl.Create }

destructor TOmniCommDispatchingObserverImpl.Destroy;
begin
  if assigned(cdoQueue) then
    cdoQueue.ContainerSubject.Detach(cdoObserver, coiNotifyOnAllInserts);
  FreeAndNil(cdoObserver);
  if cdoDispatchWnd <> 0 then begin
    DSiDeallocateHWnd(cdoDispatchWnd);
    cdoDispatchWnd := 0;
  end;
  inherited;
end; { TOmniCommDispatchingObserverImpl.Destroy }

procedure TOmniCommDispatchingObserverImpl.WndProc(var msg: TMessage);
var
  omsg: TOmniMessage;
begin
  if msg.msg = WM_USER then begin
    while cdoQueue.TryDequeue(omsg) do
      cdoDispatchTo.Dispatch(omsg);
    msg.Result := 0;
  end
  else
    msg.Result := DefWindowProc(cdoDispatchWnd, msg.Msg, msg.WParam, msg.LParam);
end; { TOmniCommDispatchingObserverImpl.WndProc }

end.

