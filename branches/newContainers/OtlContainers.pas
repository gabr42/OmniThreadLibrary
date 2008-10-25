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
///   Last modification : dt
///   Version           : 0.3....
///</para><para>
///   History:
///     0.3: 2008-07-16
///       - TOmniBaseContainer made abstract.
///       - Added TOmniBaseStack class which encapsulates base stack functionality.
///       - TOmniQueue renamed to TOmniQueue.
///       - Added TOmniBaseQueue class which encapsulates base queue functionality.
///     0.2: 2008-07-15
///       - Fixed a bug in PopLink.
///       - Implemented Empty method in both containers.
///</para></remarks>

unit OtlContainers;

{$R-,O+,A8}

interface

uses
  OtlCommon;

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
  IOmniQueue = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    procedure Empty;
    procedure Initialize(numElements, elementSize: integer);
    function  Enqueue(const value): boolean;
    function  Dequeue(var value): boolean;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniQueue }

  IOmniNotifySupport = interface ['{E5FFC739-669A-4931-B0DC-C5005A94A08B}']
    function  GetNewDataEvent: THandle;
  //
    procedure Signal;
    property NewDataEvent: THandle read GetNewDataEvent;
  end; { IOmniNotifySupport }

  TOmniContainerOption = (coEnableMonitor, coEnableNotify, coAdvancedStack,
    coStackedQueue, coQueue);
  TOmniContainerOptions = set of TOmniContainerOption;

  PReferencedPtr = ^TReferencedPtr;
  TReferencedPtr = record
    PData    : pointer;
    Reference: cardinal;
  end; { TReferencedPtr }

  TReferencedPtrBuffer = array [0..MaxInt shr 4] of TReferencedPtr;

  POmniRingBuffer = ^TOmniRingBuffer;
  TOmniRingBuffer  = packed record
    FirstIn        : TReferencedPtr;
    LastIn         : TReferencedPtr;
    StartBuffer    : pointer;
    EndBuffer      : pointer;
    TaskInsertLoops: cardinal;
    TaskRemoveLoops: cardinal;
    Buffer         : TReferencedPtrBuffer;
  end; { TOmniRingBuffer }

  TOmniChain = packed record
    Head         : TReferencedPtr;
    TaskPopLoops : cardinal;
    TaskPushLoops: cardinal;
  end; { TOmniChain }
  POmniChain = ^TOmniChain;

  POmniLinkedData = ^TOmniLinkedData;
  TOmniLinkedData = packed record
    Next: POmniLinkedData;
    Data: record end;           //user data, variable size
  end; { TOmniLinkedData }

  TOmniBaseContainer = class abstract(TInterfacedObject)
  strict protected // placeholder for list headers
    obcListHeaders      : array [1 .. 3 * SizeOf(TOmniChain) + 4] of byte;
  strict protected
    obcDataBuffer       : pointer;
    obcElementSize      : integer;
    obcNumElements      : integer;
    obcPublicChainP     : POmniChain;
    obcRecycleChainP    : POmniChain;
    obcPublicRingBuffer : POmniRingBuffer;
    obcRecycleRingBuffer: POmniRingBuffer;
    obcDequeuedMessagesP: POmniChain;
    obcOptions          : TOmniContainerOptions;
    RemoveLinkFormStack : function (var chain: TOmniChain): POmniLinkedData;
    InsertLinkToStack   : procedure (const link: POmniLinkedData; var chain: TOmniChain);
    class function  InvertOrder(chainHead: POmniLinkedData): POmniLinkedData; static;
    class function  UnlinkAll(var chain: TOmniChain): POmniLinkedData; static;
    class function  RemoveLinkFromQueue(const ringBuffer: POmniRingBuffer): pointer; static;
    class procedure InsertLinkToQueue(const data: pointer; const ringBuffer: POmniRingBuffer); static;
    function  EnqueueQueue(const value): boolean;
    function  DequeueQueue(var value): boolean;
    function  DequeueStack(var value): boolean;
    function  EnqueueStack(const value): boolean;
    procedure EmptyPureQueue;
    procedure EmptyStackedQueue;
    procedure EmptyStack; virtual;
  public
    constructor Create(Options: TOmniContainerOptions = []);
    destructor  Destroy; override;
    function  IsEmptyPureQueue: boolean;
    function  IsEmptyStackedQueue: boolean;
    function  IsEmptyStack: boolean;
    function  IsFullPureQueue: boolean;
    function  IsFullStack: boolean;
    procedure Initialize(numElements, elementSize: integer); virtual;
    property ElementSize: integer read obcElementSize;
    property NumElements: integer read obcNumElements;
    property Options: TOmniContainerOptions read obcOptions;
  end; { TOmniBaseContainer }

  TOmniBaseStack = class(TOmniBaseContainer, IOmniStack)
  public
    constructor Create(options: TOmniContainerOptions = []);
    function  Pop(var value): boolean; virtual;
    function  Push(const value): boolean; virtual;
    procedure Empty; inline;
    function  IsEmpty: boolean; inline;
    function  IsFull: boolean; inline;
  end; { TOmniBaseStack }

  TOmniStack = class(TOmniBaseStack, IOmniNotifySupport, IOmniMonitorSupport)
  strict private
    osMonitorSupport: IOmniMonitorSupport;
    osNotifySupport : IOmniNotifySupport;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [coEnableMonitor, coEnableNotify]);
    function Pop(var value): boolean; override;
    function Push(const value): boolean; override;
    property MonitorSupport: IOmniMonitorSupport read osMonitorSupport implements IOmniMonitorSupport;
    property NotifySupport: IOmniNotifySupport read osNotifySupport implements IOmniNotifySupport;
  end; { TOmniStack }

  TDequeue = function(var value): boolean of object;
  TEmpty = procedure of object;
  TEnqueue = function(const value): boolean of object;
  TInitialize = procedure(numElements, elementSize: integer) of object;
  TIsEmpty = function: boolean of object;
  TIsFull = function: boolean of object;

  TOmniBaseQueue = class(TOmniBaseContainer, IOmniQueue)
  strict private
    obqDequeue: TDequeue;
    obqEmpty  : TEmpty;
    obqEnqueue: TEnqueue;
    obqIsEmpty: TIsEmpty;
    obqIsFull : TIsFull;
  public
    constructor Create(options: TOmniContainerOptions = []);
    function  Dequeue(var value): boolean; inline;
    procedure Empty; inline;
    function  Enqueue(const value): boolean; inline;
    procedure Initialize(numElements, elementSize: integer); override;
    function  IsEmpty: boolean; inline;
    function  IsFull: boolean; inline;
  end; { TOmniBaseQueue }

  TOmniQueue = class(TOmniBaseQueue, IOmniNotifySupport, IOmniMonitorSupport)
  strict private
    orbMonitorSupport: IOmniMonitorSupport;
    orbNotifySupport : IOmniNotifySupport;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [coEnableMonitor, coEnableNotify]);
    function  Dequeue(var value): boolean;
    function  Enqueue(const value): boolean;
    property MonitorSupport: IOmniMonitorSupport read orbMonitorSupport implements IOmniMonitorSupport;
    property NotifySupport: IOmniNotifySupport read orbNotifySupport implements IOmniNotifySupport;
  end; { TOmniQueue }

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

type
  TOmniNotifySupport = class(TInterfacedObject, IOmniNotifySupport)
  strict private
    onsNewDataEvent     : TDSiEventHandle;
  protected
    function  GetNewDataEvent: THandle;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Signal;
    property NewData: THandle read GetNewDataEvent;
  end; { TOmniNotifySupport }

{ Intel Atomic functions support }

function AtomicCmpXchg4b(const Old4b, New4b: cardinal; var Destination): boolean; overload;
//ATOMIC FUNCTION
//begin
//  result := Old4b = PCardinal(Destination)^;
//  if result then
//    PCardinal(Destination)^ := New4b;
//end;
asm
  lock cmpxchg dword ptr [Destination], New4b
  setz  al
end; { AtomicCmpXchg4b }

function AtomicCmpXchg4b(const Old4b, New4b: pointer; var Destination): boolean; overload;
//ATOMIC FUNCTION
//begin
//  result := Old4b = PPointer(Destination)^;
//  if result then
//    PPointer(Destination)^ := New4b;
//end;
asm
  lock cmpxchg dword ptr [Destination], New4b
  setz  al
end; { AtomicCmpXchg4b }

function AtomicCmpXchg8b(const OldPData: pointer; OldReference: cardinal; NewPData: pointer; NewReference: cardinal;
  var Destination: TReferencedPtr): boolean;
//ATOMIC FUNCTION
//begin
//  result := (Destination.PData = OldPData) and (Destination.Reference = OldReference);
//  if result then
//  begin
//    Destination.PData := NewPData;
//    Destination.Reference := NewReference;
//  end;
//end;
asm
  push  edi
  push  ebx
  mov   ebx, NewPData
  mov   ecx, NewReference
  mov   edi, Destination
  lock cmpxchg8b qword ptr [edi]
  setz  al
  pop   ebx
  pop   edi
end; { AtomicCmpXchg8b }

function AtomicInc4b(var Destination: cardinal; const Count: cardinal = 1): cardinal;
//ATOMIC FUNCTION
//begin
//  inc(Destination, Count);
//  result := Destination;
//end;
asm
  mov   ecx, Count
  lock xadd [Destination], Count
  lea   eax, edx + ecx
end; { AtomicInc4b }

function GetThreadId: cardinal;
//result := GetCurrentThreadId;
asm
  mov   eax, fs:[$18]                                           //eax := thread information block
  mov   eax, [eax + $24]                                        //eax := thread id
end; { GetThreadId }

function GetTimeStamp: int64;
//result := QueryPerformanceCounter;
asm
  rdtsc
end; { GetTimeStamp }

function SimpleStackPopLink(var chain: TOmniChain): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
//Simple stack PopLink model
var
  Reference          : cardinal;
begin
  repeat
    Reference := AtomicInc4b(chain.Head.Reference);
    result := chain.Head.PData;
  until (result = nil) or AtomicCmpXchg8b(result, Reference, result.Next, Reference, chain.Head);
end; { SimpleStackPopLink }


function AdvancedStackPopLink(var chain: TOmniChain): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
//Advanced stack PopLink model with idle/busy status bit
var
  AtStartReference              : cardinal;
  CurrentReference              : cardinal;
  Reference                     : cardinal;
  TaskCounter                   : cardinal;
label
  TryAgain;
begin
  Reference := GetThreadId + 1;                                 //Reference.bit0 := 1
  with chain do
  begin
TryAgain:
    TaskCounter := TaskPopLoops;
    AtStartReference := Head.Reference OR 1;                    //Reference.bit0 := 1
    repeat
      CurrentReference := Head.Reference;
      dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
      not AtomicCmpXchg4b(CurrentReference, Reference, Head.Reference) then
      goto TryAgain;
    //Reference is set...
    result := Head.PData;
    //Empty test
    if result = nil then
      AtomicCmpXchg4b(Reference, 0, Head.Reference)             //Clear Reference if task own reference
    else
      if not AtomicCmpXchg8b(result, Reference, result.Next ,0 ,Head) then
        goto TryAgain;
  end;
end; { AdvancedStackPopLink }

procedure SimpleStackPushLink(const link: POmniLinkedData; var chain: TOmniChain);
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                        ^------ < chainHead
//Simple stack PushLink model
var
  PData                 : pointer;
begin
  repeat
    PData := chain.Head.PData;
    link.Next := PData;
  until AtomicCmpXchg4b(PData, link, chain.Head.PData);
end; { SimpleStackPushLink }

procedure AdvacedStackPushLink(const link: POmniLinkedData; var chain:
  TOmniChain);

//Advanced stack PushLink model with idle/busy status bit
var
  PData: pointer;
  TaskCounter           : cardinal;
begin
  with chain do
  begin
    for TaskCounter := 0 to TaskPushLoops do
      if (Head.Reference AND 1 = 0) then
        break;
    repeat
      PData := Head.PData;
      link.Next := PData;
    until AtomicCmpXchg4b(PData, link, Head.PData);
  end;
end; { AdvacedStackPushLink }

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

constructor TOmniBaseContainer.Create(Options: TOmniContainerOptions);
begin
  obcOptions := options;
end; { TOmniBaseContainer.Create }

destructor TOmniBaseContainer.Destroy;
begin
  FreeMem(obcDataBuffer);
  if not (coStackedQueue in obcOptions) and (coQueue in obcOptions) then begin
    FreeMem(obcPublicRingBuffer);
    FreeMem(obcRecycleRingBuffer);
  end;
  inherited Destroy;
end; { TOmniBaseContainer.Destroy }

function TOmniBaseContainer.DequeueQueue(var value): boolean;
var
  Data                  : pointer;
begin
  Data := RemoveLinkFromQueue(obcPublicRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(Data^, value, ElementSize);
  InsertLinkToQueue(Data, obcRecycleRingBuffer);
end; { TOmniBaseContainer.Dequeue }

function TOmniBaseContainer.DequeueStack(var value): boolean;
var
  linkedData            : POmniLinkedData;
begin
  if obcDequeuedMessagesP^.Head.PData = nil then
    obcDequeuedMessagesP^.Head.PData := InvertOrder(UnlinkAll(obcPublicChainP^));
  linkedData := RemoveLinkFormStack(obcDequeuedMessagesP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  InsertLinkToStack(linkedData, obcRecycleChainP^);
end; { TOmniBaseContainer.DequeueStack }

procedure TOmniBaseContainer.EmptyPureQueue;
var
  Data                  : pointer;
begin
  repeat
    Data := RemoveLinkFromQueue(obcPublicRingBuffer);
    if assigned(Data) then
      InsertLinkToQueue(Data, obcRecycleRingBuffer)
    else
      break;
  until false;
end; { TOmniBaseContainer.EmptyPureQueue }

procedure TOmniBaseContainer.EmptyStack;
var
  linkedData: POmniLinkedData;
begin
  repeat
    linkedData := RemoveLinkFormStack(obcPublicChainP^);
    if not assigned(linkedData) then
      break; //repeat
    InsertLinkToStack(linkedData, obcRecycleChainP^);
  until false;
end; { TOmniBaseContainer.EmptyStack }

procedure TOmniBaseContainer.EmptyStackedQueue;
var
  linkedData: POmniLinkedData;
begin
  EmptyStack;
  if assigned(obcDequeuedMessagesP^.Head.PData) then repeat
    linkedData := RemoveLinkFormStack(obcDequeuedMessagesP^);
    if not assigned(linkedData) then
      break; //repeat
    InsertLinkToStack(linkedData, obcRecycleChainP^);
  until false;
end; { TOmniBaseContainer.EmptyStackedQueue }

function TOmniBaseContainer.EnqueueQueue(const value): boolean;
var
  Data                  : pointer;
begin
  Data := RemoveLinkFromQueue(obcRecycleRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(value, Data^, ElementSize);
  InsertLinkToQueue(Data, obcPublicRingBuffer);
end; { TOmniBaseQueue.EnqueueQueue }

function TOmniBaseContainer.EnqueueStack(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := RemoveLinkFormStack(obcRecycleChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  InsertLinkToStack(linkedData, obcPublicChainP^);
end; { TOmniBaseContainer.EnqueueStack }

procedure TOmniBaseContainer.Initialize(numElements, elementSize: integer);
const
  NumOfSamples = 10;
var
  currElement  : POmniLinkedData;
  nextElement  : POmniLinkedData;
  TimeTestField: array [0..2]of array [1..NumOfSamples] of int64;

  function GetMinAndClear(Rutine, Count: cardinal): int64;
  var
    m                   :cardinal;
    n                   :integer;
    x                   :integer;
  begin
    result := 0;
    for m := 1 to Count do begin
      x:= 1;
      for n:= 2 to NumOfSamples do
        if TimeTestField[Rutine, n] < TimeTestField[Rutine, x] then
          x := n;
      inc(result, TimeTestField[Rutine, x]);
      TimeTestField[Rutine, x] := MaxLongInt;
    end;
  end; { GetMinAndClear }

  procedure InitializeQueue;
  var
    RingBufferSize: cardinal;
    n             : integer;
    currElement   : pointer;
  begin
    // allocate obcDataBuffer
    GetMem(obcDataBuffer, elementSize * numElements + elementSize);
    // allocate RingBuffers
    RingBufferSize := SizeOf(TReferencedPtr) * (numElements + 1) +
      SizeOf(TOmniRingBuffer) - SizeOf(TReferencedPtrBuffer);
    obcPublicRingBuffer := AllocMem(RingBufferSize);
    obcRecycleRingBuffer := AllocMem(RingBufferSize);
    // set obcPublicRingBuffer head
    obcPublicRingBuffer.FirstIn.PData := @obcPublicRingBuffer.Buffer[0];
    obcPublicRingBuffer.LastIn.PData := @obcPublicRingBuffer.Buffer[0];
    obcPublicRingBuffer.StartBuffer := @obcPublicRingBuffer.Buffer[0];
    obcPublicRingBuffer.EndBuffer := @obcPublicRingBuffer.Buffer[numElements];
    // set obcRecycleRingBuffer head
    obcRecycleRingBuffer.FirstIn.PData := @obcRecycleRingBuffer.Buffer[0];
    obcRecycleRingBuffer.LastIn.PData := @obcRecycleRingBuffer.Buffer[numElements];
    obcRecycleRingBuffer.StartBuffer := @obcRecycleRingBuffer.Buffer[0];
    obcRecycleRingBuffer.EndBuffer := @obcRecycleRingBuffer.Buffer[numElements];
    // format obcRecycleRingBuffer
    for n := 0 to numElements do
      obcRecycleRingBuffer.Buffer[n].PData := pointer(cardinal(obcDataBuffer) + cardinal(n * elementSize));
    //Calcolate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
    obcPublicRingBuffer.TaskRemoveLoops := 1;
    obcPublicRingBuffer.TaskInsertLoops := 1;
    obcRecycleRingBuffer.TaskRemoveLoops := 1;
    obcRecycleRingBuffer.TaskInsertLoops := 1;
    for n := 1 to NumOfSamples do  begin
      //Measure RemoveLink rutine delay
      TimeTestField[0, n] := GetTimeStamp;
      currElement := RemoveLinkFromQueue(obcRecycleRingBuffer);
      TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
      //Measure InsertLink rutine delay
      TimeTestField[1, n] := GetTimeStamp;
      InsertLinkToQueue(currElement, obcRecycleRingBuffer);
      TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
      //Measure GetTimeStamp rutine delay
      TimeTestField[2, n] := GetTimeStamp;
      TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
    end;
    //Calculate first 4 minimum average for GetTimeStamp
    n := GetMinAndClear(2, 4);
    //Calculate first 4 minimum average for RemoveLink rutine
    obcRecycleRingBuffer.TaskRemoveLoops := (GetMinAndClear(0, 4) - n) div 4;
    obcPublicRingBuffer.TaskRemoveLoops := obcRecycleRingBuffer.TaskRemoveLoops;
    //Calculate first 4 minimum average for InsertLink rutine
    obcRecycleRingBuffer.TaskInsertLoops := (GetMinAndClear(1, 4) - n) div 4;
    obcPublicRingBuffer.TaskInsertLoops := obcRecycleRingBuffer.TaskInsertLoops;
  end; { InitializeQueue }

  procedure InitializeStack;
  var
    bufferElementSize: integer;
    iElement         : integer;
    n                : integer;
  begin
    if (cardinal(@obcListHeaders) mod 8) = 0 then
      obcPublicChainP := @obcListHeaders
    else if (cardinal(@obcListHeaders) mod 8) = 4 then
      obcPublicChainP := @(obcListHeaders[5])
    else
      raise Exception.Create('TOmniBaseContainer: Object is not 4-aligned');
    obcRecycleChainP := POmniChain(cardinal(obcPublicChainP) + SizeOf(TOmniChain));
    obcDequeuedMessagesP := POmniChain(cardinal(obcRecycleChainP) + SizeOf(TOmniChain));
    //Method model type initiation
    if coAdvancedStack in obcOptions then begin
      InsertLinkToStack := AdvacedStackPushLink;
      RemoveLinkFormStack := AdvancedStackPopLink;
    end
    else begin
      InsertLinkToStack := SimpleStackPushLink;
      RemoveLinkFormStack := SimpleStackPopLink;
    end;
    //calculate bzfferelement size, round up to next 4-aligned value
    bufferElementSize := ((SizeOf(TOmniLinkedData) + obcElementSize) + 3) AND NOT 3;
    GetMem(obcDataBuffer, bufferElementSize * numElements);
    if cardinal(obcDataBuffer) AND 3 <> 0 then
      raise Exception.Create('TOmniBaseContainer: obcBuffer is not 4-aligned');
    //Format buffer to recycleChain, init orbRecycleChain and orbPublicChain.
    //At the beginning, all elements are linked into the recycle chain.
    obcRecycleChainP^.Head.PData := obcDataBuffer;
    currElement := obcRecycleChainP^.Head.PData;
    for iElement := 0 to obcNumElements - 2 do begin
      nextElement := POmniLinkedData(integer(currElement) + bufferElementSize);
      currElement.Next := nextElement;
      currElement := nextElement;
    end;
    currElement.Next := nil; // terminate the chain
    obcPublicChainP^.Head.PData := nil;
    if coAdvancedStack in obcOptions then begin
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obcPublicChainP^.TaskPopLoops := 1;
      obcPublicChainP^.TaskPushLoops := 1;
      obcRecycleChainP^.TaskPopLoops := 1;
      obcRecycleChainP^.TaskPushLoops := 1;
      for n := 1 to NumOfSamples do begin
        //Measure RemoveLink rutine delay
        TimeTestField[0, n] := GetTimeStamp;
        currElement := RemoveLinkFormStack(obcRecycleChainP^);
        TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
        //Measure InsertLink rutine delay
        TimeTestField[1, n] := GetTimeStamp;
        InsertLinkToStack(currElement, obcRecycleChainP^);
        TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
        //Measure GetTimeStamp rutine delay
        TimeTestField[2, n] := GetTimeStamp;
        TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
      end;
      //Calculate first 4 minimum average for GetTimeStamp
      n := GetMinAndClear(2, 4);
      //Calculate first 4 minimum average for RemoveLink rutine
      obcRecycleChainP^.TaskPopLoops := (GetMinAndClear(0, 4) - n) div 2;
      obcPublicChainP^.TaskPopLoops := obcRecycleChainP^.TaskPopLoops;
      //Calculate first 4 minimum average for InsertLink rutine
      obcRecycleChainP^.TaskPushLoops := (GetMinAndClear(1, 4) - n) div 4;
      obcPublicChainP^.TaskPushLoops := obcRecycleChainP^.TaskPushLoops;
    end;
  end;  { InitializeStack }

begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obcNumElements := numElements;
  // calculate element size, round up to next 4-aligned value
  obcElementSize := (elementSize + 3) AND NOT 3;
  if not (coStackedQueue in obcOptions) and (coQueue in obcOptions) then
    InitializeQueue
  else
    InitializeStack;
end; { Initialize }

class procedure TOmniBaseContainer.InsertLinkToQueue(const data: pointer;
  const ringBuffer: POmniRingBuffer);
//FIFO buffer logic
//Insert link to queue model with idle/busy status bit
var
  AtStartReference:cardinal;
  CurrentLastIn   :PReferencedPtr;
  CurrentReference:cardinal;
  NewLastIn       :PReferencedPtr;
  Reference       :cardinal;
  TaskCounter     :cardinal;
label
  TryAgain;
begin
  Reference := GetThreadId + 1;                                 //Reference.bit0 := 1
  with ringBuffer^ do
  begin
TryAgain:
    TaskCounter := TaskInsertLoops;
    AtStartReference := LastIn.Reference OR 1;                  //Reference.bit0 := 1
    repeat
      CurrentReference := LastIn.Reference;
      dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
      not AtomicCmpXchg4b(CurrentReference, Reference, LastIn.Reference) then
      goto TryAgain;
    //Reference is set...
    CurrentLastIn := LastIn.PData;
    AtomicCmpXchg4b(CurrentLastIn.Reference, Reference, CurrentLastIn.Reference);
    if (Reference <> LastIn.Reference) or
      not AtomicCmpXchg8b(CurrentLastIn.PData, Reference, data, Reference, CurrentLastIn^) then
      goto TryAgain;
    //Calculate ringBuffer next LastIn address
    NewLastIn := pointer(cardinal(CurrentLastIn) + SizeOf(TReferencedPtr));
    if cardinal(NewLastIn) > cardinal(EndBuffer) then
      NewLastIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not AtomicCmpXchg8b(CurrentLastIn, Reference, NewLastIn, 0, LastIn) then
      goto TryAgain;
  end;
end; { TOmniBaseContainer.InsertLinkToQueue }

class function TOmniBaseContainer.InvertOrder(chainHead: POmniLinkedData): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
var
  ALink:POmniLinkedData;
begin
  result := nil;
  while chainHead <> nil do
  begin
    ALink := chainHead.Next;
    chainHead.Next := result;
    result := chainHead;
    chainHead := ALink;
  end;
end; { TOmniBaseContainer.InvertOrder }

function TOmniBaseContainer.IsEmptyPureQueue: boolean;
begin
  result := obcPublicRingBuffer.FirstIn.PData = obcPublicRingBuffer.LastIn.PData;
end; { TOmniBaseContainer.IsEmptyPureQueue }

function TOmniBaseContainer.IsEmptyStack: boolean;
begin
  Result := not assigned(obcPublicChainP^.Head.PData);
end; { TOmniBaseContainer.IsEmptyStack }

function TOmniBaseContainer.IsEmptyStackedQueue: boolean;
begin
  Result := not (assigned(obcPublicChainP^.Head.PData) or assigned(obcDequeuedMessagesP^.Head.PData));
end; {TOmniBaseContainer.IsEmptyStackedQueue }

function TOmniBaseContainer.IsFullPureQueue: boolean;
var
  NewLastIn             : pointer;
begin
  NewLastIn := pointer(cardinal(obcPublicRingBuffer.LastIn.PData) + SizeOf(TReferencedPtr));
  if cardinal(NewLastIn) > cardinal(obcPublicRingBuffer.EndBuffer) then
    NewLastIn := obcPublicRingBuffer.StartBuffer;
  result := (cardinal(NewLastIn) > cardinal(obcPublicRingBuffer.LastIn.PData)) or
    (obcRecycleRingBuffer.FirstIn.PData = obcRecycleRingBuffer.LastIn.PData);
end; { TOmniBaseContainer.IsFullPureQueue }

function TOmniBaseContainer.IsFullStack: boolean;
begin
  Result := not assigned(obcRecycleChainP^.Head.PData);
end; { TOmniBaseContainer.IsFullStack }

class function TOmniBaseContainer.RemoveLinkFromQueue(const ringBuffer: POmniRingBuffer): pointer;
//FIFO buffer logic
//Remove link from queue model with idle/busy status bit
var
  AtStartReference      : cardinal;
  CurrentFirstIn        : pointer;
  CurrentReference      : cardinal;
  NewFirstIn            : pointer;
  Reference             : cardinal;
  TaskCounter           : cardinal;
label
  TryAgain;
begin
  Reference := GetThreadId + 1;                                 //Reference.bit0 := 1
  with ringBuffer^ do
  begin
TryAgain:
    TaskCounter := TaskRemoveLoops;
    AtStartReference := FirstIn.Reference OR 1;                 //Reference.bit0 := 1
    repeat
      CurrentReference := FirstIn.Reference;
      dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
      not AtomicCmpXchg4b(CurrentReference, Reference, FirstIn.Reference) then
      goto TryAgain;
    //Reference is set...
    CurrentFirstIn := FirstIn.PData;
    //Empty test
    if CurrentFirstIn = LastIn.PData then
    begin
      //Clear Reference if task own reference
      AtomicCmpXchg4b(Reference, 0, FirstIn.Reference);
      result := nil;
      exit;
    end;
    //Load result
    result := PReferencedPtr(FirstIn.PData).PData;
    //Calculate ringBuffer next FirstIn address
    NewFirstIn := pointer(cardinal(CurrentFirstIn) + SizeOf(TReferencedPtr));
    if cardinal(NewFirstIn) > cardinal(EndBuffer) then
      NewFirstIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not AtomicCmpXchg8b(CurrentFirstIn, Reference, NewFirstIn, 0, FirstIn) then
      goto TryAgain;
  end;
end; { TOmniBaseContainer.RemoveLinkFromQueue }

class function TOmniBaseContainer.UnlinkAll(var chain: TOmniChain): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                        ^------ < chain.Head
begin
  repeat
    result := chain.Head.PData;
  until AtomicCmpXchg4b(result, nil, chain.Head);
end; { TOmniBaseContainer.UnlinkAll }

{ TOmniBaseStack }

constructor TOmniBaseStack.Create(options: TOmniContainerOptions);
begin
//  Include(options, coAdvancedStack);
  Exclude(options, coQueue);
  inherited Create(options);
end; { TOmniBaseStack.Create }

procedure TOmniBaseStack.Empty;
begin
  EmptyStack;
end; {TOmniBaseStack.Empty }

function TOmniBaseStack.IsEmpty: boolean;
begin
  result := IsEmptyStack;
end; { TOmniBaseStack.IsEmpty }

function TOmniBaseStack.IsFull: boolean;
begin
  result := IsFullStack;
end; { TOmniBaseStack.IsFull }

function TOmniBaseStack.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := RemoveLinkFormStack(obcPublicChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  InsertLinkToStack(linkedData, obcRecycleChainP^);
end; { TOmniBaseStack.Pop }

function TOmniBaseStack.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := RemoveLinkFormStack(obcRecycleChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  InsertLinkToStack(linkedData, obcPublicChainP^);
end; { TOmniBaseStack.Push }

{ TOmniStack }

constructor TOmniStack.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create(options);
  Initialize(numElements, elementSize);
  if coEnableMonitor in Options then
    osMonitorSupport := CreateOmniMonitorSupport;
  if coEnableNotify in Options then
    osNotifySupport := TOmniNotifySupport.Create;
end; { TOmniStack.Create }

function TOmniStack.Pop(var value): boolean;
begin
  Result := inherited Pop(value);
  if Result then
    if coEnableNotify in obcOptions then
      osNotifySupport.Signal;
end; { TOmniStack.Pop }

function TOmniStack.Push(const value): boolean;
begin
  Result := inherited Push(value);
  if Result then begin
    if coEnableNotify in obcOptions then
      osNotifySupport.Signal;
    if coEnableMonitor in obcOptions then
      osMonitorSupport.Notify;
  end;
end; { TOmniStack.Push }

{ TOmniBaseQueue }

constructor TOmniBaseQueue.Create(options: TOmniContainerOptions);
begin
//  Include(options, coStackedQueue);
  Include(options, coQueue);
  inherited Create(options);
end; { TOmniBaseQueue.Create }

function TOmniBaseQueue.Dequeue(var value): boolean;
begin
  result := obqDequeue(value);
end; { TOmniBaseQueue.Dequeue }

procedure TOmniBaseQueue.Empty;
begin
  obqEmpty;
end; { TOmniBaseQueue.Empty }

function TOmniBaseQueue.Enqueue(const value): boolean;
begin
  result := obqEnqueue(value);
end; { TOmniBaseQueue.Enqueue }

procedure TOmniBaseQueue.Initialize(numElements, elementSize: integer);
begin
//Method model type initiation
  if not (coStackedQueue in obcOptions) and (coQueue in obcOptions) then begin
    obqDequeue:= DequeueQueue;
    obqEmpty := EmptyPureQueue;
    obqEnqueue:= EnqueueQueue;
    obqIsEmpty:= IsEmptyPureQueue;
    obqIsFull:= IsFullPureQueue;
  end
  else begin
    obqDequeue:= DequeueStack;
    obqEmpty := EmptyStackedQueue;
    obqEnqueue:= EnqueueStack;
    obqIsEmpty:= IsEmptyStackedQueue;
    obqIsFull:= IsFullStack;
  end;
  inherited Initialize(numElements, elementSize);
end; { TOmniBaseQueue.Initialize }

function TOmniBaseQueue.IsEmpty: boolean;
begin
  result := obqIsEmpty;
end; { TOmniBaseQueue.IsEmpty }

function TOmniBaseQueue.IsFull: boolean;
begin
  result := obqIsFull;
end; { TOmniBaseQueue.IsFull }

{ TOmniQueue }

constructor TOmniQueue.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create(options);
  Initialize(numElements, elementSize);
  if coEnableMonitor in Options then
    orbMonitorSupport := CreateOmniMonitorSupport;
  if coEnableNotify in Options then
    orbNotifySupport := TOmniNotifySupport.Create;
end; { TOmniQueue.Create }            

function TOmniQueue.Dequeue(var value): boolean;
begin
  Result := inherited Dequeue(value);
  if Result then
    if coEnableNotify in obcOptions then
      orbNotifySupport.Signal;
end; { TOmniQueue.Dequeue }

function TOmniQueue.Enqueue(const value): boolean;
begin
  Result := inherited Enqueue(value);
  if Result then begin
    if coEnableNotify in obcOptions then
      orbNotifySupport.Signal;
    if coEnableMonitor in obcOptions then
      orbMonitorSupport.Notify;
  end;
end; { TOmniQueue.Enqueue }

end.
