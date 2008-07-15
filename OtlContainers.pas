///<summary>Lock-free containers. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic, GJ</author>
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
///   Author            : Primoz Gabrijelcic, GJ
///   Creation date     : 2008-07-13
///   Last modification : 2008-07-15
///   Version           : 0.2
///</para><para>
///   History:
///     0.2: 2008-07-15
///       - Fixed a bug in PopLink.
///       - Implemented Empty method in both containers.
///</para></remarks>

unit OtlContainers;

interface

type
  {:Lock-free, single writer, single reader, size-limited stack.
  }
  IOmniStack = interface ['{F4C57327-18A0-44D6-B95D-2D51A0EF32B4}']
    procedure Empty; 
    procedure Initialize(numElements, elementSize: integer);
    function  Pop(var value): boolean;
    function  Push(const value): boolean;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniStack }

  {:Lock-free, single writer, single reader ring buffer.
  }
  IOmniRingBuffer = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    procedure Empty; 
    procedure Initialize(numElements, elementSize: integer);
    function  Enqueue(const value): boolean;
    function  Dequeue(var value): boolean;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniRingBuffer }

  IOmniMonitorParams = interface
    function  GetLParam: integer;
    function  GetMessage: cardinal;
    function  GetWindow: THandle;
    function  GetWParam: integer;
  //
    property Window: THandle read GetWindow;
    property Msg: cardinal read GetMessage;
    property WParam: integer read GetWParam;
    property LParam: integer read GetLParam;
  end; { IOmniMonitorParams }

  IOmniMonitorSupport = interface ['{6D5F1191-9E4A-4DD5-99D8-694C95B0DE90}']
    function  GetMonitor: IOmniMonitorParams;
  //
    procedure Notify; 
    procedure RemoveMonitor;
    procedure SetMonitor(monitor: IOmniMonitorParams);
    property Monitor: IOmniMonitorParams read GetMonitor;
  end; { IOmniMonitorSupport }

  IOmniNotifySupport = interface ['{E5FFC739-669A-4931-B0DC-C5005A94A08B}']
    function  GetNewDataEvent: THandle;
  //
    procedure Signal;
    property NewDataEvent: THandle read GetNewDataEvent;
  end; { IOmniNotifySupport }

  POmniLinkedData = ^TOmniLinkedData;
  TOmniLinkedData = packed record
    Next: POmniLinkedData;
    Data: byte; //user data, variable size
  end; { TLinkedOmniData }

  TOmniBaseContainer = class(TInterfacedObject)
  strict protected
    obcBuffer      : pointer;
    obcElementSize : integer;
    obcNumElements : integer;
    obcPublicChain : POmniLinkedData;
    obcRecycleChain: POmniLinkedData;
    function  InvertOrder(chainHead: POmniLinkedData): POmniLinkedData;
    function  Pop(var value): boolean; virtual;
    function  PopLink(var chainHead: POmniLinkedData): POmniLinkedData;
    function  Push(const value): boolean; virtual;
    procedure PushLink(const link: POmniLinkedData; var chainHead: POmniLinkedData);
    function  UnlinkAll(var chainHead: POmniLinkedData): POmniLinkedData;
  public
    destructor  Destroy; override;
    procedure Empty; virtual;
    procedure Initialize(numElements, elementSize: integer); virtual;
    function  IsEmpty: boolean; virtual;
    function  IsFull: boolean; virtual;
    property ElementSize: integer read obcElementSize;
    property NumElements: integer read obcNumElements;
  end; { TOmniBaseContainer }

  TOmniContainerOption = (coEnableMonitor, coEnableNotify);
  TOmniContainerOptions = set of TOmniContainerOption;

  TOmniStack = class(TOmniBaseContainer, IOmniStack, IOmniNotifySupport, IOmniMonitorSupport)
  strict private
    osMonitorSupport: IOmniMonitorSupport;
    osNotifySupport : IOmniNotifySupport;
    osOptions       : TOmniContainerOptions;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [coEnableMonitor, coEnableNotify]);
    function Pop(var value): boolean; override;
    function Push(const value): boolean; override;
    property MonitorSupport: IOmniMonitorSupport read osMonitorSupport implements IOmniMonitorSupport;
    property NotifySupport: IOmniNotifySupport read osNotifySupport implements IOmniNotifySupport;
    property Options: TOmniContainerOptions read osOptions write osOptions;
  end; { TOmniStack }

  TOmniRingBuffer = class(TOmniBaseContainer, IOmniRingBuffer, IOmniNotifySupport, IOmniMonitorSupport)
  strict private
    orbDequeuedMessages: POmniLinkedData;
    orbMonitorSupport  : IOmniMonitorSupport;
    orbNotifySupport   : IOmniNotifySupport;
    orbOptions         : TOmniContainerOptions;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [coEnableMonitor, coEnableNotify]);
    procedure Empty; override;
    function  Enqueue(const value): boolean;
    function  Dequeue(var value): boolean;
    function  IsEmpty: boolean; override;
    property MonitorSupport: IOmniMonitorSupport read orbMonitorSupport implements IOmniMonitorSupport;
    property NotifySupport: IOmniNotifySupport read orbNotifySupport implements IOmniNotifySupport;
    property Options: TOmniContainerOptions read orbOptions write orbOptions;
  end; { TOmniRingBuffer }

  function CreateOmniMonitorParams(window: THandle; msg: cardinal;
    wParam, lParam: integer): IOmniMonitorParams;

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

type
  TOmniMonitorParams = class(TInterfacedObject, IOmniMonitorParams)
  strict private
    ompLParam : integer;
    ompMessage: cardinal;
    ompWindow : THandle;
    ompWParam : integer;
  protected
    function  GetLParam: integer;
    function  GetMessage: cardinal;
    function  GetWindow: THandle;
    function  GetWParam: integer;
  public
    constructor Create(window: THandle; msg: cardinal; wParam, lParam: integer);
    destructor Destroy; override;
    property LParam: integer read GetLParam;
    property Msg: cardinal read GetMessage;
    property Window: THandle read GetWindow;
    property WParam: integer read GetWParam;
  end; { TOmniMonitorParams }

  TOmniMonitorSupport = class(TInterfacedObject, IOmniMonitorSupport)
  strict private
    omsMonitor: IOmniMonitorParams;
  protected
    function GetMonitor: IOmniMonitorParams;
  public
    procedure Notify;
    procedure RemoveMonitor;
    procedure SetMonitor(monitor: IOmniMonitorParams);
    property Monitor: IOmniMonitorParams read GetMonitor;
  end; { TOmniMonitorSupport }

  TOmniNotifySupport = class(TInterfacedObject, IOmniNotifySupport)
  strict private
    onsNewDataEvent: TDSiEventHandle;
  protected
    function  GetNewDataEvent: THandle;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Signal;
    property NewData: THandle read GetNewDataEvent;
  end; { TOmniNotifySupport }

{ exports }

function CreateOmniMonitorParams(window: THandle; msg: cardinal;
  wParam, lParam: integer): IOmniMonitorParams;
begin
  Result := TOmniMonitorParams.Create(window, msg, wParam, lParam);
end; { CreateOmniMonitorParams }

{ TOmniMonitorParams }

constructor TOmniMonitorParams.Create(window: THandle; msg: cardinal; wParam, lParam:
  integer);
begin
  ompMessage := msg;
  ompLParam := lParam;
  ompWParam := wParam;
  ompWindow := window;
end; { TOmniMonitorParams.Create }

destructor TOmniMonitorParams.Destroy;
begin
  inherited Destroy;
end;

function TOmniMonitorParams.GetLParam: integer;
begin
  Result := ompLParam;
end; { TOmniMonitorParams.GetLParam }

function TOmniMonitorParams.GetMessage: cardinal;
begin
  Result := ompMessage;
end; { TOmniMonitorParams.GetMessage }

function TOmniMonitorParams.GetWindow: THandle;
begin
  Result := ompWindow;
end; { TOmniMonitorParams.GetWindow }

function TOmniMonitorParams.GetWParam: integer;
begin
  Result := ompWParam;
end; { TOmniMonitorParams.GetWParam }

{ TOmniMonitorSupport }

function TOmniMonitorSupport.GetMonitor: IOmniMonitorParams;
begin
  Result := omsMonitor;
end; { TOmniMonitorSupport.GetMonitor }

procedure TOmniMonitorSupport.Notify;
var
  params: IOmniMonitorParams;
begin
  params := GetMonitor;
  if assigned(params) then
    PostMessage(params.Window, params.Msg, params.WParam, params.LParam);
end; { TOmniMonitorSupport.Notify }

procedure TOmniMonitorSupport.RemoveMonitor;
begin
  omsMonitor := nil;
end; { TOmniMonitorSupport.RemoveMonitor }

procedure TOmniMonitorSupport.SetMonitor(monitor: IOmniMonitorParams);
begin
  omsMonitor := monitor;
end; { TOmniMonitorSupport.SetMonitor }

{ TOmniNotifySupport }

constructor TOmniNotifySupport.Create;
begin
  inherited Create;
  onsNewDataEvent := CreateEvent(nil, false, false, nil);
  Win32Check(onsNewDataEvent <> 0);
end; { TOmniNotifySupport.Create }

destructor TOmniNotifySupport.Destroy;
begin
  DSiCloseHandleAndNull(onsNewDataEvent);
  inherited Destroy;
end; { TOmniNotifySupport.Destroy }

function TOmniNotifySupport.GetNewDataEvent: THandle;
begin
  Result := onsNewDataEvent;
end; { TOmniNotifySupport.GetNewDataEvent }

procedure TOmniNotifySupport.Signal;
begin
  Win32Check(SetEvent(onsNewDataEvent));
end; { TOmniNotifySupport.Signal }

{ TOmniBaseContainer }

destructor TOmniBaseContainer.Destroy;
begin
  FreeMem(obcBuffer);
end; { TOmniBaseContainer.Destroy }

procedure TOmniBaseContainer.Empty;
var
  linkedData: POmniLinkedData;
begin
  repeat
    linkedData := PopLink(obcPublicChain);
    if not assigned(linkedData) then
      break; //repeat
    PushLink(linkedData, obcRecycleChain);
  until false;
end; { TOmniBaseContainer.Empty }

procedure TOmniBaseContainer.Initialize(numElements, elementSize: integer);
var
  bufferElementSize: cardinal;
  currElement      : POmniLinkedData;
  iElement         : integer;
  nextElement      : POmniLinkedData;
begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(cardinal(obcPublicChain) AND 3 = 0);
  Assert(cardinal(obcRecycleChain) AND 3 = 0);
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obcNumElements := numElements;
  obcElementSize := elementSize;
  // calculate element size, round up to next 4-aligned value
  bufferElementSize := ((SizeOf(POmniLinkedData) + elementSize) + 3) AND NOT 3;
  GetMem(obcBuffer, bufferElementSize * cardinal(numElements));
  Assert(cardinal(obcBuffer) AND 3 = 0);
  //Format buffer to recycleChain, init orbRecycleChain and orbPublicChain.
  //At the beginning, all elements are linked into the recycle chain.
  obcRecycleChain := obcBuffer;
  nextElement := nil; // to remove compiler warning in nextElement.Next := nil assignment below
  currElement := obcRecycleChain;
  for iElement := 0 to obcNumElements - 2 do begin
    nextElement := POmniLinkedData(cardinal(currElement) + bufferElementSize);
    currElement.Next := nextElement;
    currElement := nextElement;
  end;
  nextElement.Next := nil; // terminate the chain
  obcPublicChain := nil;
end; { TOmniBaseContainer.Initialize }

///<summary>Invert links in a chain.</summary>
///<returns>New chain head (previous tail) or nil if chain is empty.</returns>
///<since>2008-07-13</since>
function TOmniBaseContainer.InvertOrder(chainHead: POmniLinkedData): POmniLinkedData;
asm
  mov   eax, edx
  test  eax, eax
  jz    @Exit
@Walk:
  xchg  [eax], ecx                        //Turn links
  and   ecx, ecx          
  jz    @Exit
  xchg  [ecx], eax
  and   eax, eax
  jnz   @Walk
  mov   eax, ecx
@Exit:
end; { TOmniBaseContainer.InvertOrder }

function TOmniBaseContainer.IsEmpty: boolean;
begin
  Result := not assigned(obcPublicChain);
end; { TOmniBaseContainer.IsEmpty }

function TOmniBaseContainer.IsFull: boolean;
begin
  Result := not assigned(obcRecycleChain);
end; { TOmniBaseContainer.IsFull }

function TOmniBaseContainer.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obcPublicChain);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obcRecycleChain);
end; { TOmniBaseContainer.Pop }

///<summary>Removes first element from the chain, atomically.</summary>
///<returns>Removed first element. If the chain is empty, returns nil.</returns>
function TOmniBaseContainer.PopLink(var chainHead: POmniLinkedData): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
asm
  mov   eax, [edx]                        //Result := chainHead
@Spin:
  test  eax, eax
  jz    @Exit
  mov   ecx, [eax]                        //ecx := Result.Next
  lock cmpxchg [edx], ecx                 //chainHead := Result.Next
  jnz   @Spin                             //Do spin ???
@Exit:
end; { TOmniBaseContainer.PopLink }

function TOmniBaseContainer.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obcRecycleChain);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  PushLink(linkedData, obcPublicChain);
end; { TOmniBaseContainer.Push }

///<summary>Inserts element at the beginning of the chain, atomically.</summary>
procedure TOmniBaseContainer.PushLink(const link: POmniLinkedData; var chainHead:
  POmniLinkedData);
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
asm
  mov   eax, [ecx]                         //ecx = chainHead
@Spin:
  mov   [edx], eax                         //link := chainHead.Next
  lock cmpxchg [ecx], edx                  //chainHead := link
  jnz   @Spin
end; { TOmniBaseContainer.PushLink }

///<summary>Removes all elements from a chain, atomically.</summary>
///<returns>Head of the chain.</returns>
///<since>2008-07-13</since>
function TOmniBaseContainer.UnlinkAll(var chainHead: POmniLinkedData): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                        ^------ < chainHead
asm
  xor   ecx, ecx
  mov   eax, [edx]
@Spin:
  lock cmpxchg [edx], ecx                 //Cut ChainHead
  jnz   @Spin
end; { TOmniRingBuffer.UnlinkAll }

{ TOmniStack }

constructor TOmniStack.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  osOptions := options;
  if coEnableMonitor in Options then
    osMonitorSupport := TOmniMonitorSupport.Create;
  if coEnableNotify in Options then
    osNotifySupport := TOmniNotifySupport.Create;
end; { TOmniStack.Create }

function TOmniStack.Pop(var value): boolean;
begin
  Result := inherited Pop(value);
  if Result then
    if coEnableNotify in Options then
      osNotifySupport.Signal;
end; { TOmniStack.Pop }

function TOmniStack.Push(const value): boolean;
begin
  Result := inherited Push(value);
  if Result then begin
    if coEnableNotify in Options then
      osNotifySupport.Signal;
    if coEnableMonitor in Options then
      osMonitorSupport.Notify;
  end;
end; { TOmniStack.Push }

{ TOmniRingBuffer }

constructor TOmniRingBuffer.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  orbOptions := options;
  if coEnableMonitor in Options then
    orbMonitorSupport := TOmniMonitorSupport.Create;
  if coEnableNotify in Options then
    orbNotifySupport := TOmniNotifySupport.Create;
end; { TOmniRingBuffer.Create }

function TOmniRingBuffer.Dequeue(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  if orbDequeuedMessages = nil then
    orbDequeuedMessages := InvertOrder(UnlinkAll(obcPublicChain));
  linkedData := PopLink(orbDequeuedMessages);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obcRecycleChain);
  if coEnableNotify in Options then
    orbNotifySupport.Signal;
end; { TOmniRingBuffer.Dequeue }

procedure TOmniRingBuffer.Empty;
var
  linkedData: POmniLinkedData;
begin
  inherited;
  if assigned(orbDequeuedMessages) then repeat
    linkedData := PopLink(orbDequeuedMessages);
    if not assigned(linkedData) then
      break; //repeat
    PushLink(linkedData, obcRecycleChain);
  until false;
end; { TOmniRingBuffer.Empty }

function TOmniRingBuffer.Enqueue(const value): boolean;
begin
  Result := inherited Push(value);
  if Result then begin
    if coEnableNotify in Options then
      orbNotifySupport.Signal;
    if coEnableMonitor in Options then
      orbMonitorSupport.Notify;
  end;
end; { TOmniRingBuffer.Enqueue }

function TOmniRingBuffer.IsEmpty: boolean;
begin
  Result := not (assigned(obcPublicChain) or assigned(orbDequeuedMessages));
end; { TOmniRingBuffer.IsEmpty }

end.
