///<summary>Two-way interprocess communication channel. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2008, Primoz Gabrijelcic
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
///   Last modification : 2008-10-05
///   Version           : 1.03
///</para><para>
///   History:
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

interface

uses
  SyncObjs,
  SpinLock,
  GpStuff,
  DSiWin32,
  OtlCommon,
  OtlContainerObserver,
  OtlContainers;

const
  //reserved for internal OTL messaging
  COtlReservedMsgID = $FFFF;
  //Max send wait time
  CMaxSendWaitTime_ms = 100;

type
  {$A4}
  TOmniMessage = record
    MsgID  : word;
    MsgData: TOmniValue;
    constructor Create(aMsgID: word; aMsgData: TOmniValue); overload;
    constructor Create(aMsgID: word); overload;
  end; { TOmniMessage }

const
  //default queues are quite small
  CDefaultQueueSize = 128;

type
  TOmniMessageQueue = class;

  IOmniCommunicationEndpoint = interface ['{910D329C-D049-48B9-B0C0-9434D2E57870}']
    function  GetNewMessageEvent: THandle;
  //
    function  Receive(var msg: TOmniMessage): boolean; overload;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload;
    function  ReceiveWait(var msg: TOmniMessage; timeout_ms: cardinal): boolean; overload;
    function  ReceiveWait(var msgID: word; var msgData: TOmniValue; timeout_ms: cardinal): boolean; overload;
    function  Reader: TOmniMessageQueue;
    procedure RemoveMonitor;
    procedure Send(const msg: TOmniMessage); overload;
    procedure Send(msgID: word); overload;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload;
    function  SendWait(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    function  SendWait(msgID: word; msgData: TOmniValue; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    procedure SendWaitE(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms); overload;
    procedure SendWaitE(msgID: word; msgData: TOmniValue;
      timeout_ms: cardinal = CMaxSendWaitTime_ms); overload;
    procedure SetMonitor(hWindow: THandle; msg: cardinal; messageWParam, messageLParam: integer);
    procedure SetTerminateEvent(TerminateEvent: THandle);
    function Writer: TOmniMessageQueue; 
    property NewMessageEvent: THandle read GetNewMessageEvent;
  end; { IOmniCommunicationEndpoint }

  IOmniTwoWayChannel = interface ['{3ED1AB88-4209-4E01-AA79-A577AD719520}']
    function Endpoint1: IOmniCommunicationEndpoint;
    function Endpoint2: IOmniCommunicationEndpoint;
  end; { IOmniTwoWayChannel }

  {:Fixed-size ring buffer of TOmniValues references.
  }
  TOmniMessageQueue = class(TOmniQueue)
  strict private
    mqWinEventObserver: IOmniContainerWindowsEventObserver;
  public
    constructor Create(numMessages: integer); reintroduce;
    destructor  Destroy; override;
    function  Dequeue: TOmniMessage; reintroduce;
    function  Enqueue(const value: TOmniMessage): boolean; reintroduce;
    property EventObserver: IOmniContainerWindowsEventObserver read mqWinEventObserver;
  end; { TOmniMessageQueue }

  function CreateTwoWayChannel(numElements: integer = CDefaultQueueSize):
    IOmniTwoWayChannel;

implementation

uses
  Windows,
  SysUtils,
  Variants,
  {$IFDEF DEBUG}OtlCommBufferTest,{$ENDIF}
  OtlEventMonitor;

type
  TOmniCommunicationEndpoint = class(TInterfacedObject, IOmniCommunicationEndpoint)
  strict private
    ceReader_ref: TOmniMessageQueue;
    ceWriter_ref: TOmniMessageQueue;
  protected
    function  GetNewMessageEvent: THandle;
  public
    constructor Create(readQueue, writeQueue: TOmniMessageQueue);
    function  GetWriteQueueFreeSpaceEvent: THandle; inline;
    function  Reader: TOmniMessageQueue;
    function  Receive(var msg: TOmniMessage): boolean; overload; inline;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload; inline;
    function  ReceiveWait(var msg: TOmniMessage; timeout_ms: cardinal): boolean; overload; inline;
    function  ReceiveWait(var msgID: word; var msgData: TOmniValue; timeout_ms: cardinal):
      boolean; overload; inline;
    procedure RemoveMonitor; inline;
    procedure Send(msgID: word); overload; inline;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload; inline;
    procedure Send(const msg: TOmniMessage); overload; inline;
    function  SendWait(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload; inline;
    function  SendWait(msgID: word; msgData: TOmniValue;
      timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
    procedure  SendWaitE(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms) overload; inline;
    procedure  SendWaitE(msgID: word; msgData: TOmniValue;
      timeout_ms: cardinal = CMaxSendWaitTime_ms); overload; inline;
    procedure SetMonitor(hWindow: THandle; msg: cardinal; messageWParam, messageLParam:
      integer); inline;
    procedure SetTerminateEvent(TerminateEvent: THandle);
    function  Writer: TOmniMessageQueue;
    property NewMessageEvent: THandle read GetNewMessageEvent;
    property  WriteQueueFreeSpaceEvent: THandle read GetWriteQueueFreeSpaceEvent;
  end; { TOmniCommunicationEndpoint }

  TOmniTwoWayChannel = class(TInterfacedObject, IOmniTwoWayChannel)
  strict private
    twcEndpoint        : array [1..2] of IOmniCommunicationEndpoint;
    twcLock            : TSynchroObject;
    twcMessageQueueSize: integer;
    twcUnidirQueue     : array [1..2] of TOmniMessageQueue;
  strict protected
    procedure CreateBuffers; inline; 
  public
    constructor Create(messageQueueSize: integer);
    destructor  Destroy; override;
    function Endpoint1: IOmniCommunicationEndpoint; inline;
    function Endpoint2: IOmniCommunicationEndpoint; inline;
  end; { TOmniTwoWayChannel }

{ exports }

function CreateTwoWayChannel(numElements: integer): IOmniTwoWayChannel;
begin
  Result := TOmniTwoWayChannel.Create(numElements);
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

constructor TOmniMessageQueue.Create(numMessages: integer);
begin
  inherited Create(numMessages, SizeOf(TOmniMessage));
  mqWinEventObserver := CreateContainerWindowsEventObserver;
  ContainerSubject.Attach(mqWinEventObserver, [coiNotifyOnFirstInsert]);
  // TODO 1 -oPrimoz Gabrijelcic : We don't need monitor and notification subsystem in every queue!
end; { TOmniMessageQueue.Create }

destructor TOmniMessageQueue.Destroy;
begin
  ContainerSubject.Detach(mqWinEventObserver);
  inherited;
end; { TOmniMessageQueue.Destroy }

function TOmniMessageQueue.Dequeue: TOmniMessage;
var
  tmp: TOmniMessage;
begin
  tmp.MsgData.RawZero;
  if not inherited Dequeue(tmp) then
    raise Exception.Create('TOmniMessageQueue.Dequeue: Message queue is empty');
  Result := tmp;
end; { TOmniMessageQueue.Dequeue }

function TOmniMessageQueue.Enqueue(const value: TOmniMessage): boolean;
var
  tmp: TOmniMessage;
begin
  tmp := value;
  Result := inherited Enqueue(tmp);
  tmp.MsgData.RawZero;
end; { TOmniMessageQueue.Enqueue }

{ TOmniCommunicationEndpoint }

constructor TOmniCommunicationEndpoint.Create(readQueue, writeQueue: TOmniMessageQueue);
begin
  inherited Create;
  ceReader_ref := readQueue;
  ceWriter_ref := writeQueue;
end; { TOmniCommunicationEndpoint.Create }

function TOmniCommunicationEndpoint.GetNewMessageEvent: THandle;
begin
  Result := ceReader_ref.EventObserver.GetEvent(coiNotifyOnFirstInsert);
end; { TOmniCommunicationEndpoint.GetNewMessageEvent }

function TOmniCommunicationEndpoint.GetWriteQueueFreeSpaceEvent: THandle;
begin
  // TODO 1 -oPrimoz Gabrijelcic : I think ...
  Result := ceWriter_ref.EventObserver.GetEvent(coiNotifyOnPartlyEmpty);
end; { TOmniCommunicationEndpoint.GetWriteQueueFreeSpaceEvent }

{ TOmniCommunicationEndpoint.GetNewMessageEvent }

function TOmniCommunicationEndpoint.Reader: TOmniMessageQueue;
begin
  Result :=  ceReader_ref;
end; { TOmniCommunicationEndpoint.Reader }

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
  Result := not ceReader_ref.IsEmpty;
  if Result then
    msg := ceReader_ref.Dequeue;
end; { TOmniCommunicationEndpoint.Receive }

function TOmniCommunicationEndpoint.ReceiveWait(var msg: TOmniMessage; timeout_ms:
  cardinal): boolean;
begin
  Result := (WaitForSingleObject(NewMessageEvent, timeout_ms) = WAIT_OBJECT_0);
  if Result then
    Result := Receive(msg);
end; { TOmniCommunicationEndpoint.ReceiveWait }

function TOmniCommunicationEndpoint.ReceiveWait(var msgID: word; var msgData: TOmniValue;
  timeout_ms: cardinal): boolean;
begin
  Result := (WaitForSingleObject(NewMessageEvent, timeout_ms) = WAIT_OBJECT_0);
  if Result then
    Result := Receive(msgID, msgData);
end; { TOmniCommunicationEndpoint.ReceiveWait }

procedure TOmniCommunicationEndpoint.RemoveMonitor;
begin
  ceWriter_ref.MonitorSupport.RemoveMonitor;
end; { TOmniCommunicationEndpoint.RemoveMonitor }

procedure TOmniCommunicationEndpoint.Send(const msg: TOmniMessage);
begin
  if not ceWriter_ref.Enqueue(msg) then
    raise Exception.Create('TOmniCommunicationEndpoint.Send: Queue is full');
end;  { TOmniCommunicationEndpoint.Send }

function TOmniCommunicationEndpoint.SendWait(msgID: word; msgData: TOmniValue;
  timeout_ms: cardinal): boolean;
var
  msg: TOmniMessage;
begin
  msg.msgID := msgID;
  msg.msgData := msgData;
  // TODO 1 -oPrimoz Gabrijelcic : fix this
  result := (ceWriter_ref as TOmniQueue).Enqueue(msg);
  if not result then begin
    ResetEvent(ceWriter_ref.EventObserver.GetEvent(coiNotifyOnPartlyEmpty));
// TODO 1 -oPrimoz Gabrijelcic : Wait on 'partly empty' and 'terminate' events    
//    if WaitForMultipleObjects(2, ceWriter_ref.NotifySupport.GetEventPair, false, timeout_ms) = 1 then
      raise Exception.Create('TOmniCommunicationEndpoint.SendWait: Thread terminate event!');
    result := (ceWriter_ref as TOmniQueue).Enqueue(msg);
    if not result then
      exit;
  end;
  msg.MsgData.RawZero;
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
  Send(msgID, OpenArrayToVarArray(msgData));
end; { TOmniCommunicationEndpoint.Send }

procedure TOmniCommunicationEndpoint.Send(msgID: word);
begin
  Send(msgID, TOmniValue.Null);
end; { TOmniCommunicationEndpoint.Send }

procedure TOmniCommunicationEndpoint.SendWaitE(msgID: word;
  timeout_ms: cardinal);
var
  msg: TOmniMessage;
begin
  msg.msgID := msgID;
  msg.msgData := TOmniValue.Null;
  SendWaitE(msgID, msg.msgData, timeout_ms);
end; { TOmniCommunicationEndpoint.SendWaitE }

procedure TOmniCommunicationEndpoint.SendWaitE(msgID: word; msgData: TOmniValue; timeout_ms: cardinal);
var
  msg    : TOmniMessage;
  result : boolean;
begin
  msg.msgID := msgID;
  msg.msgData := msgData;
  // TODO 1 -oPrimoz Gabrijelcic : fix this
  result := (ceWriter_ref as TOmniQueue).Enqueue(msg);
  if not result then begin
    ResetEvent(ceWriter_ref.EventObserver.GetEvent(coiNotifyOnPartlyEmpty));
// TODO 1 -oPrimoz Gabrijelcic : Wait on 'partly empty' and 'terminate' events
//    WaitForMultipleObjects(2, ceWriter_ref.NotifySupport.GetEventPair, false, timeout_ms);
    result := (ceWriter_ref as TOmniQueue).Enqueue(msg);
    if not result then
      raise Exception.Create('TOmniCommunicationEndpoint.SendWaitE: out of time!');
  end;
  msg.MsgData.RawZero;
end; { TOmniCommunicationEndpoint.SendWaitE }

procedure TOmniCommunicationEndpoint.SetMonitor(hWindow: THandle; msg: cardinal;
  messageWParam, messageLParam: integer);
begin
  ceWriter_ref.MonitorSupport.SetMonitor(CreateOmniMonitorParams(
    hWindow, msg, messageWParam, messageLParam));
end; { TOmniCommunicationEndpoint.SetMonitor }

procedure TOmniCommunicationEndpoint.SetTerminateEvent(TerminateEvent: THandle);
begin
  // TODO 1 -oPrimoz Gabrijelcic : Rethink this approach
//  ceWriter_ref.NotifySupport.SetTerminateEvent(TerminateEvent);
//  ceReader_ref.NotifySupport.SetTerminateEvent(TerminateEvent);
end; { TOmniCommunicationEndpoint.SetTerminateEvent }

function TOmniCommunicationEndpoint.Writer: TOmniMessageQueue;
begin
  Result := ceWriter_ref;
end; { TOmniCommunicationEndpoint.Writer }

{ TOmniTwoWayChannel }

constructor TOmniTwoWayChannel.Create(messageQueueSize: integer);
begin
  inherited Create;
  twcMessageQueueSize := messageQueueSize;
  twcLock := TTicketSpinLock.Create;
end; { TOmniTwoWayChannel.Create }

destructor TOmniTwoWayChannel.Destroy;
begin
  twcUnidirQueue[1].Free;
  twcUnidirQueue[1] := nil;
  twcUnidirQueue[2].Free;
  twcUnidirQueue[2] := nil;
  FreeAndNil(twcLock);
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
  Assert((cardinal(@twcEndpoint[1]) AND 3) = 0);
  if twcEndpoint[1] = nil then begin
    twcLock.Acquire;
    try
      if twcEndpoint[1] = nil then begin
        CreateBuffers;
        twcEndpoint[1] := TOmniCommunicationEndpoint.Create(twcUnidirQueue[1], twcUnidirQueue[2]);
      end;
    finally twcLock.Release; end;
  end;
  Result := twcEndpoint[1];
end; { TOmniTwoWayChannel.Endpoint1 }

function TOmniTwoWayChannel.Endpoint2: IOmniCommunicationEndpoint;
begin
  Assert((cardinal(@twcEndpoint[2]) AND 3) = 0);
  if twcEndpoint[2] = nil then begin
    twcLock.Acquire;
    try
      if twcEndpoint[2] = nil then begin
        CreateBuffers;
        twcEndpoint[2] := TOmniCommunicationEndpoint.Create(twcUnidirQueue[2], twcUnidirQueue[1]);
      end;
    finally twcLock.Release; end;
  end;
  Result := twcEndpoint[2];
end; { TOmniTwoWayChannel.Endpoint2 }

end.

