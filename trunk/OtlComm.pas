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
///   Last modification : 2008-07-11
///   Version           : 0.3
///</para><para>
///   History:
///     0.3: 2008-07-11
///       - Lock-free buffer is functional again and is switched on as default.
///         To compile with locking buffer, define OTL_LockingBuffer.        
///     0.2b: 2008-07-10
///       - Disabled lock-free buffer as it turned out to be a stack, not a queue.
///     0.2a: 2008-07-09
///       - Replaced spinlocks with ticket spinlocks. There seems to be a
///         problem with the SpinLock code and ticket spinlocks should be faster
///         in our scenario anyway.
///     0.2: 2008-07-07
///       - Included experimenal lock-free buffer, donated by GJ.
///         To enable this code, compile with /dOTL_LockFreeBuffer.
///</para></remarks>

unit OtlComm;

interface

uses
  SyncObjs,
  SpinLock,
  GpStuff,
  DSiWin32,
  OtlCommon,
  OtlContainers;

type
  {$A4}
  TOmniMessage = record
    MsgID  : word;
    MsgData: TOmniValue;
  end; { TOmniMessage }

const
  CDefaultQueueSize = 65520 div SizeOf(TOmniMessage); {3276 entries}

type
  IOmniCommunicationEndpoint = interface ['{910D329C-D049-48B9-B0C0-9434D2E57870}']
    function  GetNewMessageEvent: THandle;
  //
    function  Receive(var msg: TOmniMessage): boolean; overload;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload;
    procedure RemoveMonitor;
    procedure Send(const msg: TOmniMessage); overload;
    procedure Send(msgID: word); overload;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload;
    procedure SetMonitor(hWindow: THandle; msg: cardinal; messageWParam, messageLParam:
      integer); 
    property NewMessageEvent: THandle read GetNewMessageEvent;
  end; { IOmniCommunicationEndpoint }

  IOmniTwoWayChannel = interface ['{3ED1AB88-4209-4E01-AA79-A577AD719520}']
    function Endpoint1: IOmniCommunicationEndpoint;
    function Endpoint2: IOmniCommunicationEndpoint;
  end; { IOmniTwoWayChannel }

  {:Fixed-size ring buffer of TOmniValues references.
  }
  TOmniMessageQueue = class(TOmniQueue)
  public
    constructor Create(numMessages: integer); reintroduce;
    function  Dequeue: TOmniMessage; reintroduce;
    function  Enqueue(value: TOmniMessage): boolean; reintroduce;
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
    function  Receive(var msg: TOmniMessage): boolean; overload; inline;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload; inline;
    procedure RemoveMonitor; inline;
    procedure Send(msgID: word); overload; inline;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(msgID: word; msgData: TOmniValue); overload; inline;
    procedure Send(const msg: TOmniMessage); overload; inline;
    procedure SetMonitor(hWindow: THandle; msg: cardinal; messageWParam, messageLParam:
      integer); inline;
    property NewMessageEvent: THandle read GetNewMessageEvent;
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

  TVariantRec = record
    v1: int64;
    v2: int64;
  end; { TVariantRec }

{ exports }

function CreateTwoWayChannel(numElements: integer): IOmniTwoWayChannel;
begin
  Result := TOmniTwoWayChannel.Create(numElements);
end; { CreateTwoWayChannel }

{ TOmniMessageQueue }

constructor TOmniMessageQueue.Create(numMessages: integer);
begin
  inherited Create(numMessages, SizeOf(TOmniMessage));
end; { TOmniMessageQueue.Create }

function TOmniMessageQueue.Dequeue: TOmniMessage;
var
  tmp: TOmniMessage;
begin
  with TVariantRec(tmp.MsgData) do begin
    v1 := 0;
    v2 := 0;
  end;
  if not inherited Dequeue(tmp) then
    raise Exception.Create('TOmniMessageQueue.Dequeue: Message queue is empty');
  Result := tmp;
end; { TOmniMessageQueue.Dequeue }

function TOmniMessageQueue.Enqueue(value: TOmniMessage): boolean;
var
  tmp: TOmniMessage;
begin
  tmp := value;
  Result := inherited Enqueue(tmp);
  with TVariantRec(tmp.MsgData) do begin
    v1 := 0;
    v2 := 0;
  end;
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
  Result := ceReader_ref.NotifySupport.NewDataEvent;
end; { TOmniCommunicationEndpoint.GetNewMessageEvent }

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

procedure TOmniCommunicationEndpoint.RemoveMonitor;
begin
  ceWriter_ref.MonitorSupport.RemoveMonitor;
end; { TOmniCommunicationEndpoint.RemoveMonitor }

procedure TOmniCommunicationEndpoint.Send(const msg: TOmniMessage);
begin
  if not ceWriter_ref.Enqueue(msg) then
    raise Exception.Create('TOmniCommunicationEndpoint.Send: Queue is full');
end; { TOmniCommunicationEndpoint.Send }

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
  Send(msgID, Null); 
end; { TOmniCommunicationEndpoint.Send }

procedure TOmniCommunicationEndpoint.SetMonitor(hWindow: THandle; msg: cardinal;
  messageWParam, messageLParam: integer);
begin
  ceWriter_ref.MonitorSupport.SetMonitor(CreateOmniMonitorParams(
    hWindow, msg, messageWParam, messageLParam));
end; { TOmniCommunicationEndpoint.SetMonitor }

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

initialization
  Assert(SizeOf(TVariantRec) = SizeOf(TOmniValue));
end.

