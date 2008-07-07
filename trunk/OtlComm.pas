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
///   Creation date     : 2008-06-12
///   Last modification : 2008-07-07
///   Version           : 0.2
///</para><para>
///   History:
///     0.2: 2008-07-07
///       - Included experimenal lock-free buffer, donated by GJ.
///         To enable this code, compile with /dLockFreeBuffer.
///</para></remarks>

unit OtlComm;

interface

uses
  Variants,
  SpinLock,
  GpLists,
  GpStuff,
  DSiWin32,
  OtlCommon;

const
  CDefaultQueueSize = 65520 div 20 {3276 entries; 20 = SizeOf(TOmniMessage)};
type
  TOmniMessage = record
    MsgID  : word;
    MsgData: TOmniValue;
  end; { TOmniMessage }

  IOmniCommunicationEndpoint = interface ['{910D329C-D049-48B9-B0C0-9434D2E57870}']
    function  GetNewMessageEvent: THandle;
  //
    procedure RemoveMonitor;
    procedure Send(msgID: word; msgData: TOmniValue); overload;
    procedure Send(msgID: word; msgData: array of const); overload;
    procedure Send(const msg: TOmniMessage); overload;
    procedure SetMonitor(hWindow: THandle; messageWParam, messageLParam: integer);
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload;
    function  Receive(var msg: TOmniMessage): boolean; overload;
    property NewMessageEvent: THandle read GetNewMessageEvent;
  end; { IOmniTaskCommunication }

  IOmniTwoWayChannel = interface ['{3ED1AB88-4209-4E01-AA79-A577AD719520}']
    function Endpoint1: IOmniCommunicationEndpoint;
    function Endpoint2: IOmniCommunicationEndpoint;
  end; { IOmniTwoWayChannel }

  function CreateTwoWayChannel(queueSize: integer = CDefaultQueueSize): IOmniTwoWayChannel;

implementation

uses
  Windows,
  SysUtils,
  OtlTaskEvents;

type
  {:Fixed-size ring buffer of TOmniValues references.
  }

{$IFNDEF LockFreeBuffer}
  TOmniRingBuffer = class
  strict private
    orbBuffer              : array of TOmniMessage;
    orbBufferSize          : integer;
    orbCount               : TGp4AlignedInt;
    orbHead                : integer;
    orbLock                : TSpinLock;
    orbMonitorMessageLParam: integer;
    orbMonitorMessageWParam: integer;
    orbMonitorWindow       : TGp4AlignedInt;
    orbNewMessageEvt       : TDSiEventHandle;
    orbTail                : integer;
  strict protected
    function  IncPointer(const ptr: integer; increment: integer = 1): integer; inline;
  public
    constructor Create(bufferSize: integer);
    destructor  Destroy; override;
    procedure Clear; inline;
    function  Count: integer; inline;
    function  Dequeue: TOmniMessage;
    function  Enqueue(value: TOmniMessage): boolean;
    function  IsEmpty: boolean; inline;
    function  IsFull: boolean; inline;
    procedure Lock; inline;
    procedure RemoveMonitor;
    procedure SetMonitor(hWindow: THandle; messageWParam, messageLParam: integer);
    procedure Unlock; inline;
    property NewMessageEvent: TDSiEventHandle read orbNewMessageEvt write orbNewMessageEvt;
  end; { TOmniRingBuffer }
{$ELSE}
  PLinkedOmniMessage = ^TLinkedOmniMessage;
  TLinkedOmniMessage = packed record
    Next: PLinkedOmniMessage;
    OmniMessage: TOmniMessage;
  end;

  TOmniRingBuffer = class
  strict private
    orbBottomInUseLink     : PLinkedOmniMessage;
    orbTopInUseLink        : PLinkedOmniMessage;
    orbBottomFreeLink      : PLinkedOmniMessage;
    orbTopFreeLink         : PLinkedOmniMessage;
    orbBuffer              : array of TLinkedOmniMessage;
    orbCount               : TGp4AlignedInt;
    orbBufferSize          : integer;
//
    orbMonitorMessageLParam: integer;
    orbMonitorMessageWParam: integer;
    orbMonitorWindow       : Cardinal;
    orbNewMessageEvt       : TDSiEventHandle;
    function NewLink: PLinkedOmniMessage;
    procedure RecycleLink(const ALink: PLinkedOmniMessage);
    procedure PushLink(const ALink: PLinkedOmniMessage);
    function PopLink: PLinkedOmniMessage;
  public
    constructor Create(bufferSize: integer);
    destructor Destroy; override;
    function  Count: integer; inline;
    function Dequeue: TOmniMessage;
    function Enqueue(value: TOmniMessage): Boolean;
    function IsEmpty: boolean; inline;
    function IsFull: boolean; inline;
    procedure RemoveMonitor;
    procedure SetMonitor(hWindow: THandle; messageWParam, messageLParam: integer);
    property NewMessageEvent: TDSiEventHandle read orbNewMessageEvt write orbNewMessageEvt;
  end; { TOmniRingBuffer }
{$ENDIF}

  TOmniCommunicationEndpoint = class(TInterfacedObject, IOmniCommunicationEndpoint)
  strict private
    ceReader_ref: TOmniRingBuffer;
    ceWriter_ref: TOmniRingBuffer;
  protected
    function  GetNewMessageEvent: THandle;
  public
    constructor Create(readQueue, writeQueue: TOmniRingBuffer);
    function  Receive(var msg: TOmniMessage): boolean; overload; inline;
    function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload; inline;
    procedure RemoveMonitor; inline;
    procedure Send(const msg: TOmniMessage); overload; inline;
    procedure Send(msgID: word; msgData: array of const); overload; 
    procedure Send(msgID: word; msgData: TOmniValue); overload; inline;
    procedure SetMonitor(hWindow: THandle; messageWParam, messageLParam: integer); inline;
    property NewMessageEvent: THandle read GetNewMessageEvent;
  end; { TOmniCommunicationEndpoint }

  TOmniTwoWayChannel = class(TInterfacedObject, IOmniTwoWayChannel)
  strict private
    twcEndpoint        : array [1..2] of IOmniCommunicationEndpoint;
    twcLock            : TSpinLock;
    twcMessageQueueSize: integer;
    twcUnidirQueue     : array [1..2] of TOmniRingBuffer;
  strict protected
    procedure CreateBuffers; {inline;} // TODO 1 -oPrimoz Gabrijelcic : testing, remove! 
  public
    constructor Create(messageQueueSize: integer);
    destructor  Destroy; override;
    function Endpoint1: IOmniCommunicationEndpoint; inline;
    function Endpoint2: IOmniCommunicationEndpoint; inline;
  end; { TOmniTwoWayChannel }

{ exports }

function CreateTwoWayChannel(queueSize: integer = CDefaultQueueSize): IOmniTwoWayChannel;
begin
  Result := TOmniTwoWayChannel.Create(queueSize);
end; { CreateTwoWayChannel }

{ TOmniRingBuffer }

{$IFNDEF LockFreeBuffer}
constructor TOmniRingBuffer.Create(bufferSize: integer);
begin
  orbLock := TSpinLock.Create;
  orbBufferSize := bufferSize;
  SetLength(orbBuffer, orbBufferSize+1);
  orbNewMessageEvt := CreateEvent(nil, false, false, nil);
  Win32Check(orbNewMessageEvt <> 0);
  Assert(SizeOf(THandle) = SizeOf(cardinal));
  orbMonitorWindow.Value := 0;
end; { TOmniRingBuffer.Create }

{:Destroys ring buffer. If OwnsObjects is set, destroys all objects currently
  in the buffer.
}
destructor TOmniRingBuffer.Destroy;
begin
  DSiCloseHandleAndNull(orbNewMessageEvt);
  Clear;
  FreeAndNil(orbLock);
  inherited;
end; { TOmniRingBuffer.Destroy }

procedure TOmniRingBuffer.Clear;
begin
  Lock;
  try
    orbTail := orbHead;
    orbCount.Value := 0;
  finally Unlock; end;
end; { TOmniRingBuffer.Clear }

{:Returns number of objects in the buffer.
}
function TOmniRingBuffer.Count: integer;
begin
  Result := orbCount;
end; { TOmniRingBuffer.Count }

{:Removes tail object from the buffer, without destroying it. Returns nil if
  buffer is empty.
}
function TOmniRingBuffer.Dequeue: TOmniMessage;
begin
  Lock;
  try
    if IsEmpty then
      raise Exception.Create('TOmniRingBuffer.Dequeue: Ring buffer is empty')
    else begin
      Result := orbBuffer[orbTail];
      orbTail := IncPointer(orbTail);
      orbCount.Value := orbCount - 1;
      if orbCount > 0 then
        SetEvent(orbNewMessageEvt);
    end;
  finally Unlock; end;
end; { TOmniRingBuffer.Dequeue }

{:Inserts object into the buffer. Returns false if the buffer is full.
}
function TOmniRingBuffer.Enqueue(value: TOmniMessage): boolean;
begin
  Lock;
  try
    if IsFull then
      Result := false
    else begin
      orbBuffer[orbHead] := value;
      orbHead := IncPointer(orbHead);
      orbCount.Value := orbCount + 1;
      SetEvent(orbNewMessageEvt);
      if orbMonitorWindow <> 0 then
        PostMessage(orbMonitorWindow, COmniTaskMsg_NewMessage, orbMonitorMessageWParam,
          orbMonitorMessageLParam);
      Result := true;
    end;
  finally Unlock; end;
end; { TOmniRingBuffer.Enqueue }

{:Increments internal pointer (head or tail), wraps it to the buffer size and
  returns new value.
}
function TOmniRingBuffer.IncPointer(const ptr: integer;
  increment: integer): integer;
begin
  Result := (ptr + increment) mod (orbBufferSize + 1);
end; { TOmniRingBuffer.IncPointer }

{:Checks whether the buffer is empty.
}
function TOmniRingBuffer.IsEmpty: boolean;
begin
  Result := (orbCount = 0);
end; { TOmniRingBuffer.IsEmpty }

function TOmniRingBuffer.IsFull: boolean;
begin
  Result := (orbCount = orbBufferSize);
end; { TOmniRingBuffer.IsFull }

procedure TOmniRingBuffer.Lock;
begin
  orbLock.Acquire;
end; { TOmniRingBuffer.Lock }

procedure TOmniRingBuffer.RemoveMonitor;
begin
  orbMonitorWindow.Value := 0;
end; { TOmniRingBuffer.RemoveMonitor }

procedure TOmniRingBuffer.SetMonitor(hWindow: THandle; messageWParam, messageLParam:
  integer);
begin
  Lock;
  try
    orbMonitorWindow.Value := cardinal(hWindow);
    orbMonitorMessageWParam := messageWParam;
    orbMonitorMessageLParam := messageLParam;
  finally Unlock; end;
end; { TOmniRingBuffer.SetMonitor }

procedure TOmniRingBuffer.Unlock;
begin
  orbLock.Release;
end; { TOmniRingBuffer.Unlock }
{$ELSE}

function TOmniRingBuffer.Count: integer;
begin
  Result := orbCount;
end;

constructor TOmniRingBuffer.Create(bufferSize: integer);
var
  n: Cardinal;
begin
  orbBufferSize := bufferSize;
  SetLength(orbBuffer, orbBufferSize + 1);
  orbNewMessageEvt := CreateEvent(nil, false, false, nil);
  Win32Check(orbNewMessageEvt <> 0);
  Assert(SizeOf(THandle) = SizeOf(cardinal));
  orbMonitorWindow := 0;
//Format buffer to FreeChain, init orbTopFreeLink and orbBottomFreeLink
  orbBottomFreeLink := @orbBuffer[0];
  for n := 0 to orbBufferSize -1 do
    orbBuffer[n].Next := @orbBuffer[n +1];
  orbBuffer[orbBufferSize].Next := @orbBottomFreeLink;
  orbTopFreeLink := @orbBuffer[orbBufferSize];
//Init orbSubInUseLink and orbTailInUseLink
  orbTopInUseLink := @orbBottomInUseLink;
  orbBottomInUseLink := @orbBottomInUseLink;
end;

function TOmniRingBuffer.NewLink: PLinkedOmniMessage;
//orbTopFreeLink >> <<  Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                                        ^------ < orbBottomFreeLink
asm
//orbBottomFreeLink point on first added link in chain.
//orbTopFreeLink point on last added link in chain. The pointer address of last link is @orbBottomFreeLink
//Remove link from chain

  lea   ecx, eax.orbTopFreeLink           //ecx := @orbTopFreeLink
  mov   eax, [ecx]                        //Result := orbTopFreeLink^
@spin:
  mov   edx, [eax]                        //edx := Result.Next
  lock  cmpxchg [ecx], edx                //orbBottomFreeLink := Result.Next
  jnz   @spin                             //Do spin ???
end;

procedure TOmniRingBuffer.RecycleLink(const ALink: PLinkedOmniMessage);
//orbTopFreeLink >> <<  Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                                      ^------ < orbBottomFreeLink
asm
//orbBottomFreeLink point on first added link in chain.
//orbTopFreeLink point on last added link in chain. The pointer address of last link is @orbBottomFreeLink

  push  ebx                                //edx := @ALink
  lea   ebx, eax.orbBottomFreeLink         //ebx = @orbBottomFreeLink
  lea   ecx, eax.orbTopFreeLink            //ecx := @orbTopFreeLink
  mov   [edx], ebx                         //ALink.Next := @orbBottomFreeLink (mark new chain end)
  mov   eax, [ecx]                         //eax := orbTopFreeLink
@spin:
  lock  cmpxchg [ecx], edx                 //orbTopFreeLink := @ALink
  jnz   @spin                              //Do spin ???
  mov   eax, ecx
  lock  cmpxchg [ebx], edx                 //Set orbBottomFreeLink
  pop   ebx
end;

function TOmniRingBuffer.PopLink: PLinkedOmniMessage;
//orbTopInUseLink >> << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                                        ^------ < orbBottomInUseLink
//Remove link from chain
asm
  lea   ecx, eax.orbTopInUseLink           //ecx := @orbTopInUseLink
  mov   eax, [ecx]                         //eax := Result orbTopInUseLink^
@spin:
  mov   edx, [eax]                         //edx := Result.Next       
  lock  cmpxchg [ecx], edx                 //orbTopInUseLink := Result.Next
  jnz   @spin                              //Do spin ???
end;

procedure TOmniRingBuffer.PushLink(const ALink: PLinkedOmniMessage);
//orbTopInUseLink >> << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                                        ^------ < orbBottomInUseLink
asm
  push  ebx                                //edx := @ALink
  lea   ebx, eax.orbBottomInUseLink        //ebx := @orbBottomInUseLink
  lea   ecx, eax.orbTopInUseLink           //ecx := @orbTopInUseLink
  mov   [edx], ebx                         //ALink.Next := @orbBottomInUseLink
  mov   eax, [ecx]
@spin:
  lock  cmpxchg [ecx], edx                 //orbTopInUseLink := @ALink
  jnz   @spin                              //Do spin ???
  mov   eax, ecx
  lock  cmpxchg [ebx], edx                 //Set orbBottomInUseLink
  pop   ebx
end;

function TOmniRingBuffer.IsEmpty: boolean;
begin
  Result := orbTopInUseLink = @orbBottomInUseLink;
end;

function TOmniRingBuffer.IsFull: boolean;
begin
  Result := orbTopFreeLink = @orbBottomFreeLink;
end;

function TOmniRingBuffer.Enqueue(value: TOmniMessage): Boolean;
var
  LinkedOmniMessage: PLinkedOmniMessage;
begin
  LinkedOmniMessage := NewLink;
  Result := not(LinkedOmniMessage = @orbBottomFreeLink);
  if not Result then
    exit;
  LinkedOmniMessage^.OmniMessage := value;;
  PushLink(LinkedOmniMessage);
//...
  orbCount.Value := orbCount + 1;
  SetEvent(orbNewMessageEvt);
  if orbMonitorWindow <> 0 then
    PostMessage(orbMonitorWindow, COmniTaskMsg_NewMessage, orbMonitorMessageWParam,
      orbMonitorMessageLParam);
end;

function TOmniRingBuffer.Dequeue: TOmniMessage;
var
  LinkedOmniMessage: PLinkedOmniMessage;
begin
  LinkedOmniMessage := PopLink;
  if LinkedOmniMessage = @orbBottomInUseLink then
    raise Exception.Create('TOmniRingBuffer.Dequeue: Ring buffer is empty');
  Result := LinkedOmniMessage^.OmniMessage;
  RecycleLink(LinkedOmniMessage);
//...
  orbCount.Value := orbCount - 1;
  if not IsEmpty then
    SetEvent(orbNewMessageEvt);
end;

destructor TOmniRingBuffer.Destroy;
begin
  DSiCloseHandleAndNull(orbNewMessageEvt);
  inherited;
end;

procedure TOmniRingBuffer.RemoveMonitor;
begin
  orbMonitorWindow := 0;
end; { TOmniRingBuffer.RemoveMonitor }

procedure TOmniRingBuffer.SetMonitor(hWindow: THandle; messageWParam, messageLParam:
  integer);
begin
  orbMonitorWindow := cardinal(hWindow);
  orbMonitorMessageWParam := messageWParam;
  orbMonitorMessageLParam := messageLParam;
end; { TOmniRingBuffer.SetMonitor }

{$ENDIF}
{ TOmniCommunicationEndpoint }

constructor TOmniCommunicationEndpoint.Create(readQueue, writeQueue: TOmniRingBuffer);
begin
  inherited Create;
  ceReader_ref := readQueue;
  ceWriter_ref := writeQueue;
end; { TOmniCommunicationEndpoint.Create }

function TOmniCommunicationEndpoint.GetNewMessageEvent: THandle;
begin
  Result := ceReader_ref.NewMessageEvent;
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
  ceWriter_ref.RemoveMonitor;
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

procedure TOmniCommunicationEndpoint.SetMonitor(hWindow: THandle; messageWParam,
  messageLParam: integer);
begin
  ceWriter_ref.SetMonitor(hWindow, messageWParam, messageLParam);
end; { TOmniCommunicationEndpoint.SetMonitor }

{ TOmniTwoWayChannel }

constructor TOmniTwoWayChannel.Create(messageQueueSize: integer);
begin
  inherited Create;
  twcMessageQueueSize := messageQueueSize;
  twcLock := TSpinLock.Create;
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
    twcUnidirQueue[1] := TOmniRingBuffer.Create(twcMessageQueueSize);
  if twcUnidirQueue[2] = nil then
    twcUnidirQueue[2] := TOmniRingBuffer.Create(twcMessageQueueSize);
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
