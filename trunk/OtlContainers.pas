///<summary>Microlocking containers. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic, GJ</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2009 Primoz Gabrijelcic
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
///   Author            : GJ, Primoz Gabrijelcic
///   Creation date     : 2008-07-13
///   Last modification : 2009-12-25
///   Version           : 2.0
///</para><para>
///   History:
///     2.0: 2009-12-25
///       - Implemented dynamically allocated, O(1) enqueue and dequeue, threadsafe,
///         microlocking queue. Class TOmniBaseQueue contains base implementation
///         while TOmniQueue adds notification support.
///       - Big class rename: TOmniBaseStack -> TOmniBaseBoundedStack,
///         TOmniStack -> TOmniBoundedStack, TOmniBaseQueue -> TOmniBaseBoundedQueue,
///         TOmniQueue -> TOmniBoundedQueue.
///     1.02: 2009-12-22
///       - TOmniContainerSubject moved into OtlContainerObserver because it will also be
///         used in OtlCollections.
///     1.01b: 2009-11-11
///       - [GJ] better fix for the initialization crash.
///     1.01a: 2009-11-10
///       - Bug fixed: Initialization code could crash with range check error.
///     1.01: 2008-10-26
///       - [GJ] Redesigned stack with better lock contention.
///       - [GJ] Totally redesigned queue, which is no longer based on stack and allows
///         multiple readers.
///</para></remarks>

{$WARN SYMBOL_PLATFORM OFF}
//DEFINE DEBUG_OMNI_QUEUE to enable assertions in TOmniBaseQueue

unit OtlContainers;

{$R-,O+,A8}

interface

uses
  Classes,
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlSync,
  OtlContainerObserver;

const
  CPartlyEmptyLoadFactor = 0.8; // When an element count drops below 80%, the container is considered 'partly empty'.
  CAlmostFullLoadFactor  = 0.9; // When an element count raises above 90%, the container is considered 'almost full'.

type
  {:Lock-free, single writer, single reader, bounded stack.
  }
  IOmniStack = interface ['{F4C57327-18A0-44D6-B95D-2D51A0EF32B4}']
    procedure Empty;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    function  Pop(var value): boolean;
    function  Push(const value): boolean;
  end; { IOmniStack }

  {:Lock-free, single writer, single reader ring buffer (bounded queue).
  }
  IOmniQueue = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    function  Dequeue(var value): boolean;
    procedure Empty;
    function  Enqueue(const value): boolean;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniQueue }

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
    Buffer         : TReferencedPtrBuffer;
  end; { TOmniRingBuffer }

  POmniLinkedData = ^TOmniLinkedData;
  TOmniLinkedData = packed record
    Next: POmniLinkedData;
    Data: record end;           //user data, variable size
  end; { TOmniLinkedData }

  TOmniBaseBoundedStack = class abstract(TInterfacedObject, IOmniStack)
  strict private
    obsDataBuffer   : pointer;
    obsElementSize  : integer;
    obsNumElements  : integer;
    obsPublicChainP : PReferencedPtr;
    obsRecycleChainP: PReferencedPtr;
    class var obsIsInitialized: boolean;                //default is false
    class var obsTaskPopLoops : cardinal;
    class var obsTaskPushLoops: cardinal;
  strict protected
    procedure MeasureExecutionTimes;
    class function  PopLink(var chain: TReferencedPtr): POmniLinkedData; static;
    class procedure PushLink(const link: POmniLinkedData; var chain: TReferencedPtr); static;
  public
    destructor Destroy; override;
    procedure Empty;
    procedure Initialize(numElements, elementSize: integer); virtual;
    function  IsEmpty: boolean; inline;
    function  IsFull: boolean; inline;
    function  Pop(var value): boolean;
    function  Push(const value): boolean;
    property  ElementSize: integer read obsElementSize;
    property  NumElements: integer read obsNumElements;
  end; { TOmniBaseBoundedStack }

  TOmniBoundedStack = class(TOmniBaseBoundedStack)
  strict private
    osAlmostFullCount : integer;
    osContainerSubject: TOmniContainerSubject;
    osInStackCount    : TGp4AlignedInt;
    osPartlyEmptyCount: integer;
  public
    constructor Create(numElements, elementSize: integer;
      partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
      almostFullLoadFactor: real = CAlmostFullLoadFactor);
    destructor  Destroy; override;
    function Pop(var value): boolean;
    function Push(const value): boolean; 
    property ContainerSubject: TOmniContainerSubject read osContainerSubject;
  end; { TOmniBoundedStack }

  TOmniBaseBoundedQueue = class abstract(TInterfacedObject, IOmniQueue)
  strict private
    obqDataBuffer               : pointer;
    obqElementSize              : integer;
    obqNumElements              : integer;
    obqPublicRingBuffer         : POmniRingBuffer;
    obqRecycleRingBuffer        : POmniRingBuffer;
    class var obqTaskInsertLoops: cardinal;             //default is false
    class var obqTaskRemoveLoops: cardinal;
    class var obqIsInitialized  : boolean;
  strict protected
    class procedure InsertLink(const data: pointer; const ringBuffer: POmniRingBuffer);
      static;
    class function  RemoveLink(const ringBuffer: POmniRingBuffer): pointer; static;
    procedure MeasureExecutionTimes;
  public
    destructor Destroy; override;
    function  Dequeue(var value): boolean;
    procedure Empty;
    function  Enqueue(const value): boolean;
    procedure Initialize(numElements, elementSize: integer); virtual;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    property  ElementSize: integer read obqElementSize;
    property  NumElements: integer read obqNumElements;
  end; { TOmniBaseBoundedQueue }

  TOmniBoundedQueue = class(TOmniBaseBoundedQueue)
  strict private
    oqAlmostFullCount : integer;
    oqContainerSubject: TOmniContainerSubject;
    oqInQueueCount    : TGp4AlignedInt;
    oqPartlyEmptyCount: integer;
  public
    constructor Create(numElements, elementSize: integer;
      partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
      almostFullLoadFactor: real = CAlmostFullLoadFactor);
    destructor  Destroy; override;
    function  Dequeue(var value): boolean;
    function  Enqueue(const value): boolean;
    property  ContainerSubject: TOmniContainerSubject read oqContainerSubject;
  end; { TOmniBoundedQueue }

  TOmniQueueTag = (tagFree, tagAllocating, tagAllocated, tagRemoving,
    tagEndOfList, tagExtending, tagBlockPointer, tagDestroying
    {$IFDEF DEBUG_OMNI_QUEUE}, tagStartOfList, tagSentinel{$ENDIF});

  TOmniTaggedValue = packed record
    Tag     : TOmniQueueTag;
    Stuffing: word;
    Value   : TOmniValue;
    function CASTag(oldTag, newTag: TOmniQueueTag): boolean;
  end; { TOmniTaggedValue }
  POmniTaggedValue = ^TOmniTaggedValue;

  ///<summary>Dynamically allocated, O(1) enqueue and dequeue, threadsafe, microlocking queue.</summary>
  TOmniBaseQueue = class
  strict private // keep 4-aligned
    obcCachedBlock: POmniTaggedValue;
    obcHeadPointer: POmniTaggedValue;
    obcTailPointer: POmniTaggedValue;
  strict private
    obcRemoveCount: TGp4AlignedInt;
  strict protected
    function  AllocateBlock: POmniTaggedValue;
    procedure EnterReader; 
    procedure LeaveReader; inline;
    procedure LeaveWriter; inline;
    procedure ReleaseBlock(lastSlot: POmniTaggedValue; forceFree: boolean = false);
    procedure EnterWriter; 
  public
    constructor Create;
    destructor  Destroy; override;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
  end; { TOmniBaseQueue }

  TOmniQueue = class(TOmniBaseQueue)
  strict private
    ocContainerSubject: TOmniContainerSubject;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
    property ContainerSubject: TOmniContainerSubject read ocContainerSubject;
  end; { TOmniQueue }

implementation

uses
  Windows,
  SysUtils;

const
  CCollNumSlots = 4*1024 {$IFDEF DEBUG_OMNI_QUEUE} - 3 {$ENDIF};
  CCollBlockSize = SizeOf(TOmniTaggedValue) * CCollNumSlots; //64 KB

{ TOmniBaseBoundedStack }

destructor TOmniBaseBoundedStack.Destroy;
begin
  FreeMem(obsPublicChainP);
  inherited;
end; { TOmniBaseBoundedStack.Destroy }

procedure TOmniBaseBoundedStack.Empty;
var
  linkedData: POmniLinkedData;
begin
  repeat
    linkedData := PopLink(obsPublicChainP^);
    if not assigned(linkedData) then
      break; //repeat
    PushLink(linkedData, obsRecycleChainP^);
  until false;
end; { TOmniBaseBoundedStack.Empty }

procedure TOmniBaseBoundedStack.Initialize(numElements, elementSize: integer);
var
  bufferElementSize: integer;
  currElement      : POmniLinkedData;
  iElement         : integer;
  nextElement      : POmniLinkedData;
begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obsNumElements := numElements;
  //calculate element size, round up to next 4-aligned value
  obsElementSize := (elementSize + 3) AND NOT 3;
  //calculate buffer element size, round up to next 4-aligned value
  bufferElementSize := ((SizeOf(TOmniLinkedData) + obsElementSize) + 3) AND NOT 3;
  //calculate DataBuffer
  GetMem(obsDataBuffer, bufferElementSize * numElements + 2 * SizeOf(TReferencedPtr));
  if cardinal(obsDataBuffer) AND 7 <> 0 then
    raise Exception.Create('TOmniBaseContainer: obcBuffer is not 8-aligned');
  obsPublicChainP := obsDataBuffer;
  inc(cardinal(obsDataBuffer), SizeOf(TReferencedPtr));
  obsRecycleChainP := obsDataBuffer;
  inc(cardinal(obsDataBuffer), SizeOf(TReferencedPtr));
  //Format buffer to recycleChain, init obsRecycleChain and obsPublicChain.
  //At the beginning, all elements are linked into the recycle chain.
  obsRecycleChainP^.PData := obsDataBuffer;
  currElement := obsRecycleChainP^.PData;
  for iElement := 0 to obsNumElements - 2 do begin
    nextElement := POmniLinkedData(integer(currElement) + bufferElementSize);
    currElement.Next := nextElement;
    currElement := nextElement;
  end;
  currElement.Next := nil; // terminate the chain
  obsPublicChainP^.PData := nil;
  MeasureExecutionTimes;
end; { TOmniBaseBoundedStack.Initialize }

function TOmniBaseBoundedStack.IsEmpty: boolean;
begin
  Result := not assigned(obsPublicChainP^.PData);
end; { TOmniBaseBoundedStack.IsEmpty }

function TOmniBaseBoundedStack.IsFull: boolean;
begin
  Result := not assigned(obsRecycleChainP^.PData);
end; { TOmniBaseBoundedStack.IsFull }

procedure TOmniBaseBoundedStack.MeasureExecutionTimes;
const
  NumOfSamples = 10;
var
  TimeTestField: array [0..1] of array [1..NumOfSamples] of int64;

  function GetMinAndClear(routine, count: cardinal): int64;
  var
    m: cardinal;
    n: integer;
    x: integer;
  begin
    Result := 0;
    for m := 1 to count do begin
      x:= 1;
      for n:= 2 to NumOfSamples do
        if TimeTestField[routine, n] < TimeTestField[routine, x] then
          x := n;
      Inc(Result, TimeTestField[routine, x]);
      TimeTestField[routine, x] := MaxLongInt;
    end;
  end; { GetMinAndClear }

var
  affinity   : string;
  currElement: POmniLinkedData;
  n          : integer;

begin { TOmniBaseBoundedStack.MeasureExecutionTimes }
  if not obsIsInitialized then begin
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obsTaskPopLoops := 1;
      obsTaskPushLoops := 1;
      for n := 1 to NumOfSamples do begin
        DSiYield;
        //Measure RemoveLink rutine delay
        TimeTestField[0, n] := GetCPUTimeStamp;
        currElement := PopLink(obsRecycleChainP^);
        TimeTestField[0, n] := GetCPUTimeStamp - TimeTestField[0, n];
        //Measure InsertLink rutine delay
        TimeTestField[1, n] := GetCPUTimeStamp;
        PushLink(currElement, obsRecycleChainP^);
        TimeTestField[1, n] := GetCPUTimeStamp - TimeTestField[1, n];
      end;
      //Calculate first 4 minimum average for RemoveLink rutine
      obsTaskPopLoops := GetMinAndClear(0, 4) div 4;
      //Calculate first 4 minimum average for InsertLink rutine
      obsTaskPushLoops := GetMinAndClear(1, 4) div 4;
      obsIsInitialized := true;
    finally DSiSetThreadAffinity(affinity); end;
  end;
end;  { TOmniBaseBoundedStack.MeasureExecutionTimes }

function TOmniBaseBoundedStack.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obsPublicChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obsRecycleChainP^);
end; { TOmniBaseBoundedStack.Pop }

class function TOmniBaseBoundedStack.PopLink(var chain: TReferencedPtr): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead
//Advanced stack PopLink model with idle/busy status bit
var
  AtStartReference: cardinal;
  CurrentReference: cardinal;
  TaskCounter     : cardinal;
  ThreadReference : cardinal;
label
  TryAgain;
begin
  ThreadReference := GetThreadId + 1;                           //Reference.bit0 := 1
  with chain do begin
TryAgain:
    TaskCounter := obsTaskPopLoops;
    AtStartReference := Reference OR 1;                         //Reference.bit0 := 1
    repeat
      CurrentReference := Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
       not CAS32(CurrentReference, ThreadReference, Reference)
    then
      goto TryAgain;
    //Reference is set...
    result := PData;
    //Empty test
    if result = nil then
      CAS32(ThreadReference, 0, Reference)            //Clear Reference if task own reference
    else
      if not CAS64(result, ThreadReference, result.Next, 0, chain) then
        goto TryAgain;
  end;
end; { TOmniBaseBoundedStack.PopLink }

function TOmniBaseBoundedStack.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obsRecycleChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  PushLink(linkedData, obsPublicChainP^);
end; { TOmniBaseBoundedStack.Push }

class procedure TOmniBaseBoundedStack.PushLink(const link: POmniLinkedData; var chain: TReferencedPtr);
//Advanced stack PushLink model with idle/busy status bit
var
  PMemData   : pointer;
  TaskCounter: cardinal;
begin
  with chain do begin
    for TaskCounter := 0 to obsTaskPushLoops do
      if (Reference AND 1 = 0) then
        break;
    repeat
      PMemData := PData;
      link.Next := PMemData;
    until CAS32(PMemData, link, PData);
  end;
end; { TOmniBaseBoundedStack.PushLink }

{ TOmniBoundedStack }

constructor TOmniBoundedStack.Create(numElements, elementSize: integer; partlyEmptyLoadFactor,
  almostFullLoadFactor: real);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  osContainerSubject := TOmniContainerSubject.Create;
  osInStackCount.Value := 0;
  osPartlyEmptyCount := Round(numElements * partlyEmptyLoadFactor);
  if osPartlyEmptyCount >= numElements then
    osPartlyEmptyCount := numElements - 1;
  osAlmostFullCount := Round(numElements * almostFullLoadFactor);
  if osAlmostFullCount >= numElements then
    osAlmostFullCount := numElements - 1;
end; { TOmniBoundedStack.Create }

destructor TOmniBoundedStack.Destroy;
begin
  FreeAndNil(osContainerSubject);
  inherited;
end; { TOmniBoundedStack.Destroy }

function TOmniBoundedStack.Pop(var value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Pop(value);
  if Result then begin
    countAfter := osInStackCount.Decrement;  //' range check error??
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
    if countAfter <= osPartlyEmptyCount then
      ContainerSubject.NotifyOnce(coiNotifyOnPartlyEmpty);
  end;
end; { TOmniBoundedStack.Pop }

function TOmniBoundedStack.Push(const value): boolean;
var
  countAfter : integer;
begin
  Result := inherited Push(value);
  if Result then begin
    countAfter := osInStackCount.Increment;
    ContainerSubject.Notify(coiNotifyOnAllInserts);
    if countAfter >= osAlmostFullCount then
      ContainerSubject.NotifyOnce(coiNotifyOnAlmostFull);
  end;
end; { TOmniBoundedStack.Push }

{ TOmniBaseBoundedQueue }

function TOmniBaseBoundedQueue.Dequeue(var value): boolean;
var
  Data: pointer;
begin
  Data := RemoveLink(obqPublicRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(Data^, value, ElementSize);
  InsertLink(Data, obqRecycleRingBuffer);
end; { TOmniBaseBoundedQueue.Dequeue }

destructor TOmniBaseBoundedQueue.Destroy;
begin
  FreeMem(obqDataBuffer);
  FreeMem(obqPublicRingBuffer);
  FreeMem(obqRecycleRingBuffer);
  inherited;
end; { TOmniBaseBoundedQueue.Destroy }

procedure TOmniBaseBoundedQueue.Empty;
var
  Data: pointer;
begin
  repeat
    Data := RemoveLink(obqPublicRingBuffer);
    if assigned(Data) then
      InsertLink(Data, obqRecycleRingBuffer)
    else
      break;
  until false;
end; { TOmniBaseBoundedQueue.Empty }

function TOmniBaseBoundedQueue.Enqueue(const value): boolean;
var
  Data: pointer;
begin
  Data := RemoveLink(obqRecycleRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(value, Data^, ElementSize);
  InsertLink(Data, obqPublicRingBuffer);
end; { TOmniBaseBoundedQueue.Enqueue }

procedure TOmniBaseBoundedQueue.Initialize(numElements, elementSize: integer);
var
  n             : integer;
  ringBufferSize: cardinal;
begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obqNumElements := numElements;
  // calculate element size, round up to next 4-aligned value
  obqElementSize := (elementSize + 3) AND NOT 3;
  // allocate obqDataBuffer
  GetMem(obqDataBuffer, elementSize * numElements + elementSize);
  // allocate RingBuffers
  ringBufferSize := SizeOf(TReferencedPtr) * (numElements + 1) +
    SizeOf(TOmniRingBuffer) - SizeOf(TReferencedPtrBuffer);
  obqPublicRingBuffer := AllocMem(ringBufferSize);
  Assert(cardinal(obqPublicRingBuffer) mod 8 = 0,
    'TOmniBaseContainer: obcPublicRingBuffer is not 8-aligned');
  obqRecycleRingBuffer := AllocMem(ringBufferSize);
  Assert(cardinal(obqRecycleRingBuffer) mod 8 = 0,
    'TOmniBaseContainer: obcRecycleRingBuffer is not 8-aligned');
  // set obqPublicRingBuffer head
  obqPublicRingBuffer.FirstIn.PData := @obqPublicRingBuffer.Buffer[0];
  obqPublicRingBuffer.LastIn.PData := @obqPublicRingBuffer.Buffer[0];
  obqPublicRingBuffer.StartBuffer := @obqPublicRingBuffer.Buffer[0];
  obqPublicRingBuffer.EndBuffer := @obqPublicRingBuffer.Buffer[numElements];
  // set obqRecycleRingBuffer head
  obqRecycleRingBuffer.FirstIn.PData := @obqRecycleRingBuffer.Buffer[0];
  obqRecycleRingBuffer.LastIn.PData := @obqRecycleRingBuffer.Buffer[numElements];
  obqRecycleRingBuffer.StartBuffer := @obqRecycleRingBuffer.Buffer[0];
  obqRecycleRingBuffer.EndBuffer := @obqRecycleRingBuffer.Buffer[numElements];
  // format obqRecycleRingBuffer
  for n := 0 to numElements do
    obqRecycleRingBuffer.Buffer[n].PData := pointer(cardinal(obqDataBuffer) + cardinal(n * elementSize));
  MeasureExecutionTimes;
end; { TOmniBaseBoundedQueue.Initialize }

class procedure TOmniBaseBoundedQueue.InsertLink(const data: pointer; const ringBuffer:
  POmniRingBuffer);
//FIFO buffer logic
//Insert link to queue model with idle/busy status bit
var
  AtStartReference: cardinal;
  CurrentLastIn   : PReferencedPtr;
  CurrentReference: cardinal;
  NewLastIn       : PReferencedPtr;
  TaskCounter     : cardinal;
  ThreadReference : cardinal;
label
  TryAgain;
begin
  ThreadReference := GetThreadId + 1;                           //Reference.bit0 := 1
  with ringBuffer^ do begin
TryAgain:
    TaskCounter := obqTaskInsertLoops;
    AtStartReference := LastIn.Reference OR 1;                  //Reference.bit0 := 1
    repeat
      CurrentReference := LastIn.Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
       not CAS32(CurrentReference, ThreadReference, LastIn.Reference)
    then
      goto TryAgain;
    //Reference is set...
    CurrentLastIn := LastIn.PData;
    CAS32(CurrentLastIn.Reference, ThreadReference, CurrentLastIn.Reference);
    if (ThreadReference <> LastIn.Reference) or
      not CAS64(CurrentLastIn.PData, ThreadReference, data, ThreadReference, CurrentLastIn^)
    then
      goto TryAgain;
    //Calculate ringBuffer next LastIn address
    NewLastIn := pointer(cardinal(CurrentLastIn) + SizeOf(TReferencedPtr));
    if cardinal(NewLastIn) > cardinal(EndBuffer) then
      NewLastIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not CAS64(CurrentLastIn, ThreadReference, NewLastIn, 0, LastIn) then
      goto TryAgain;
  end;
end; { TOmniBaseBoundedQueue.InsertLink }

function TOmniBaseBoundedQueue.IsEmpty: boolean;
begin
  Result := (obqPublicRingBuffer.FirstIn.PData = obqPublicRingBuffer.LastIn.PData);
end; { TOmniBaseBoundedQueue.IsEmpty }

function TOmniBaseBoundedQueue.IsFull: boolean;
var
  NewLastIn: pointer;
begin
  NewLastIn := pointer(cardinal(obqPublicRingBuffer.LastIn.PData) + SizeOf(TReferencedPtr));
  if cardinal(NewLastIn) > cardinal(obqPublicRingBuffer.EndBuffer) then
    NewLastIn := obqPublicRingBuffer.StartBuffer;
  result := (cardinal(NewLastIn) = cardinal(obqPublicRingBuffer.LastIn.PData)) or
    (obqRecycleRingBuffer.FirstIn.PData = obqRecycleRingBuffer.LastIn.PData);
end; { TOmniBaseBoundedQueue.IsFull }

procedure TOmniBaseBoundedQueue.MeasureExecutionTimes;
const
  NumOfSamples = 10;
var
  TimeTestField: array [0..1] of array [1..NumOfSamples] of int64;

  function GetMinAndClear(routine, count: cardinal): int64;
  var
    m: cardinal;
    n: integer;
    x: integer;
  begin
    Result  := 0;
    for m := 1 to count do begin
      x:= 1;
      for n:= 2 to NumOfSamples do
        if TimeTestField[routine, n] < TimeTestField[routine, x] then
          x := n;
      Inc(Result, TimeTestField[routine, x]);
      TimeTestField[routine, x] := MaxLongInt;
    end;
  end; { GetMinAndClear }

var
  affinity   : string;
  currElement: pointer;
  n          : integer;

begin { TOmniBaseBoundedQueue.MeasureExecutionTimes }
  if not obqIsInitialized then begin
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obqTaskRemoveLoops := 1;
      obqTaskInsertLoops := 1;
      for n := 1 to NumOfSamples do  begin
        DSiYield;
        //Measure RemoveLink rutine delay
        TimeTestField[0, n] := GetCPUTimeStamp;
        currElement := RemoveLink(obqRecycleRingBuffer);
        TimeTestField[0, n] := GetCPUTimeStamp - TimeTestField[0, n];
        //Measure InsertLink rutine delay
        TimeTestField[1, n] := GetCPUTimeStamp;
        InsertLink(currElement, obqRecycleRingBuffer);
        TimeTestField[1, n] := GetCPUTimeStamp - TimeTestField[1, n];
      end;
      obqTaskRemoveLoops := GetMinAndClear(0, 4) div 4;
      obqTaskInsertLoops := GetMinAndClear(1, 4) div 4;
      obqIsInitialized := true;
    finally DSiSetThreadAffinity(affinity); end;
  end;
end; { TOmniBaseBoundedQueue.MeasureExecutionTimes }

class function TOmniBaseBoundedQueue.RemoveLink(const ringBuffer: POmniRingBuffer): pointer;
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
  with ringBuffer^ do begin
TryAgain:
    TaskCounter := obqTaskRemoveLoops;
    AtStartReference := FirstIn.Reference OR 1;                 //Reference.bit0 := 1
    repeat
      CurrentReference := FirstIn.Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
      not CAS32(CurrentReference, Reference, FirstIn.Reference)
    then
      goto TryAgain;
    //Reference is set...
    CurrentFirstIn := FirstIn.PData;
    //Empty test
    if CurrentFirstIn = LastIn.PData then begin
      //Clear Reference if task own reference
      CAS32(Reference, 0, FirstIn.Reference);
      Result := nil;
      Exit;
    end;
    //Load Result
    Result := PReferencedPtr(FirstIn.PData).PData;
    //Calculate ringBuffer next FirstIn address
    NewFirstIn := pointer(cardinal(CurrentFirstIn) + SizeOf(TReferencedPtr));
    if cardinal(NewFirstIn) > cardinal(EndBuffer) then
      NewFirstIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not CAS64(CurrentFirstIn, Reference, NewFirstIn, 0, FirstIn) then
      goto TryAgain;
  end;
end; { TOmniBaseBoundedQueue.RemoveLink }

{ TOmniBoundedQueue }

constructor TOmniBoundedQueue.Create(numElements, elementSize: integer; partlyEmptyLoadFactor,
  almostFullLoadFactor: real);
begin
  inherited Create;
  oqContainerSubject := TOmniContainerSubject.Create;
  oqInQueueCount.Value := 0;
  oqPartlyEmptyCount := Round(numElements * partlyEmptyLoadFactor);
  if oqPartlyEmptyCount >= numElements then
    oqPartlyEmptyCount := numElements - 1;
  oqAlmostFullCount := Round(numElements * almostFullLoadFactor);
  if oqAlmostFullCount >= numElements then
    oqAlmostFullCount := numElements - 1;
  Initialize(numElements, elementSize);
end; { TOmniBoundedQueue.Create }

destructor TOmniBoundedQueue.Destroy;
begin
  FreeAndNil(oqContainerSubject);
  inherited;
end; { TOmniBoundedQueue.Destroy }

function TOmniBoundedQueue.Dequeue(var value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Dequeue(value);
  if Result then begin
    countAfter := oqInQueueCount.Decrement;
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
    if countAfter <= oqPartlyEmptyCount then
      ContainerSubject.NotifyOnce(coiNotifyOnPartlyEmpty);
  end;
end; { TOmniBoundedQueue.Dequeue }

function TOmniBoundedQueue.Enqueue(const value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Enqueue(value);
  if Result then begin
    countAfter := oqInQueueCount.Increment;
    ContainerSubject.Notify(coiNotifyOnAllInserts);
    if countAfter >= oqAlmostFullCount then
      ContainerSubject.NotifyOnce(coiNotifyOnAlmostFull);
  end;
end; { TOmniBoundedQueue.Enqueue }

(*
TOmniQueue
===============

slot contains:
  tag = 1 byte
  2 bytes left empty
  TOmniValue = 13 bytes
tags are 4-aligned

tags:
  tagFree
  tagAllocating
  tagAllocated
  tagRemoving
  tagEndOfList
  tagExtending
  tagBlockPointer
  tagDestroying

Enqueue:
  readlock GC
  repeat
    fetch tag from current tail
    if tag = tagFree and CAS(tag, tagAllocating) then
      break
    if tag = tagEndOfList and CAS(tag, tagExtending) then
      break
    yield
  forever
  if tag = tagFree then
    increment tail
    store (tagAllocated, value) into locked slot
  else
    allocate and initialize new block
      last entry has tagEndOfList tag, others have tagFree
    set tail to new block's slot 1
    store (tagAllocated, value) into new block's slot 0
    store (tagBlockPointer, pointer to new block) into locked slot
  leave GC

Dequeue:
  readlock GC
  repeat
    fetch tag from current head
    if tag = tagFree then
      return Empty
    if tag = tagAllocated and CAS(tag, tagRemoving) then
      break
    if tag = tagBlockPointer and CAS(tag, tagDestroying) then
    yield
  forever
  if tag = tagAllocated then
    increment head
    get value
  else
    set head to new block's slot 1
    get value
    leave GC
    writelock GC
    release original block
    leave GC
    exit

  leave GC
*)

{ TOmniTaggedValue }

function TOmniTaggedValue.CASTag(oldTag, newTag: TOmniQueueTag): boolean;
var
  newValue: DWORD;
  oldValue: DWORD;
begin
  oldValue := PDWORD(@Tag)^ AND $FFFFFF00 OR DWORD(ORD(oldTag));
  newValue := oldValue AND $FFFFFF00 OR DWORD(Ord(newTag));
  {$IFDEF DEBUG_OMNI_QUEUE} Assert(cardinal(@Tag) mod 4 = 0); {$ENDIF}
  Result := CAS32(oldValue, newValue, Tag);
end; { TOmniTaggedValue.CASTag }

{ TOmniBaseQueue }

constructor TOmniBaseQueue.Create;
begin
  inherited;
  Assert(cardinal(obcHeadPointer) mod 4 = 0);
  Assert(cardinal(obcTailPointer) mod 4 = 0);
  Assert(cardinal(obcCachedBlock) mod 4 = 0);
  obcHeadPointer := AllocateBlock;
  obcTailPointer := obcHeadPointer;
end; { TOmniBaseQueue.Create }

function TOmniBaseQueue.Dequeue: TOmniValue;
begin
  if not TryDequeue(Result) then
    raise Exception.Create('TOmniBaseQueue.Dequeue: Message queue is empty');
end; { TOmniBaseQueue.Dequeue }

destructor TOmniBaseQueue.Destroy;
var
  pBlock: POmniTaggedValue;
begin
  while assigned(obcHeadPointer) do begin
    if obcHeadPointer.Tag in [tagBlockPointer, tagEndOfList] then begin
      pBlock := obcHeadPointer;
      obcHeadPointer := POmniTaggedValue(obcHeadPointer.Value.RawData^); //retrieveing Value.AsPointer raises exception
      ReleaseBlock(pBlock, true);
    end
    else
      Inc(obcHeadPointer);
  end;
  if assigned(obcCachedBlock) then
    FreeMem(obcCachedBlock);
  inherited;
end; { TOmniBaseQueue.Destroy }

function TOmniBaseQueue.AllocateBlock: POmniTaggedValue;
var
  cached: POmniTaggedValue;
  pEOL  : POmniTaggedValue;
begin
  cached := obcCachedBlock;
  if assigned(cached) and CAS32(cached, nil, obcCachedBlock) then begin
    Result := cached;
    ZeroMemory(Result, CCollBlockSize {$IFDEF DEBUG_OMNI_QUEUE} + 3*SizeOf(TOmniTaggedValue){$ENDIF});
  end
  else
    Result := AllocMem(CCollBlockSize {$IFDEF DEBUG_OMNI_QUEUE} + 3*SizeOf(TOmniTaggedValue){$ENDIF});
  Assert(Ord(tagFree) = 0);
  {$IFDEF DEBUG_OMNI_QUEUE}
  Assert(Result^.Tag = tagFree);
  Result^.Tag := tagSentinel;
  Inc(Result);
  Assert(Result^.Tag = tagFree);
  Result^.Tag := tagStartOfList;
  Inc(Result, CCollNumSlots + 1);
  Assert(Result^.Tag = tagFree);    
  Result^.Tag := tagSentinel;
  Dec(Result, CCollNumSlots);
  {$ENDIF}
  pEOL := Result;
  Inc(pEOL, CCollNumSlots - 1);
  {$IFDEF DEBUG_OMNI_QUEUE} Assert(Result^.Tag = tagFree); {$ENDIF}
  pEOL^.tag := tagEndOfList;
end; { TOmniBaseQueue.AllocateBlock }

procedure TOmniBaseQueue.Enqueue(const value: TOmniValue);
var
  extension: POmniTaggedValue;
  tag      : TOmniQueueTag;
  tail     : POmniTaggedValue;
begin
  EnterReader;
  repeat
    tail := obcTailPointer;
    tag := tail^.tag;
    if tag = tagFree then begin
      if tail^.CASTag(tag, tagAllocating) then
        break //repeat
    end
    else if tag = tagEndOfList then begin
      if tail^.CASTag(tag, tagExtending) then
        break //repeat
    end
    else if tag = tagExtending then
      DSIYield
    else // very temporary condition, retry quickly
      asm pause; end;
  until false;
  {$IFDEF DEBUG_OMNI_QUEUE} Assert(tail = obcTailPointer); {$ENDIF}
  if tag = tagFree then begin // enqueueing
    Inc(obcTailPointer); // release the lock
    tail^.Value := value; // this works because the slot was initialized to zero when allocating
    {$IFDEF DEBUG_OMNI_QUEUE} tail^.Stuffing := GetCurrentThreadID AND $FFFF; {$ENDIF}
    {$IFNDEF DEBUG_OMNI_QUEUE} tail^.Tag := tagAllocated; {$ELSE} Assert(tail^.CASTag(tagAllocating, tagAllocated)); {$ENDIF}
  end
  else begin // allocating memory
    {$IFDEF DEBUG_OMNI_QUEUE} Assert(tag = tagEndOfList); {$ENDIF}
    extension := AllocateBlock;
    Inc(extension);             // skip allocated slot
    obcTailPointer := extension; // release the lock
    Dec(extension);
    {$IFDEF DEBUG_OMNI_QUEUE} // create backlink
    Dec(extension);
    extension^.Value.AsPointer := tail;
    Inc(extension);
    {$ENDIF}
    {$IFNDEF DEBUG_OMNI_QUEUE} extension^.Tag := tagAllocated; {$ELSE} Assert(extension^.CASTag(tagFree, tagAllocated)); {$ENDIF}
    extension^.Value := value;  // this works because the slot was initialized to zero when allocating
    tail^.Value := extension;
    {$IFNDEF DEBUG_OMNI_QUEUE} tail^.Tag := tagBlockPointer; {$ELSE} Assert(tail^.CASTag(tagExtending, tagBlockPointer)); {$ENDIF DEBUG}
  end;
  LeaveReader;
end; { TOmniBaseQueue.Enqueue }

procedure TOmniBaseQueue.EnterReader;
var
  value: integer;
begin
  repeat
    value := obcRemoveCount.Value;
    if value >= 0 then
      if obcRemoveCount.CAS(value, value + 1) then
        break
    else
      DSiYield; // let the GC do its work
  until false;
end; { TOmniBaseQueue.EnterReader }

procedure TOmniBaseQueue.EnterWriter;
begin
  while not ((obcRemoveCount.Value = 0) and (obcRemoveCount.CAS(0, -1))) do
   DSiYield;
end; { TOmniBaseQueue.EnterWriter }

procedure TOmniBaseQueue.LeaveReader;
begin
  obcRemoveCount.Decrement;
end; { TOmniBaseQueue.LeaveReader }

procedure TOmniBaseQueue.LeaveWriter;
begin
  obcRemoveCount.Value := 0;
end; { TOmniBaseQueue.LeaveWriter }

procedure TOmniBaseQueue.ReleaseBlock(lastSlot: POmniTaggedValue; forceFree: boolean);
begin
  {$IFDEF DEBUG_OMNI_QUEUE}
  Inc(lastSlot);
  Assert(lastSlot^.Tag = tagSentinel);
  Dec(lastSlot);
  {$ENDIF}
  Dec(lastSlot, CCollNumSlots - 1);
  {$IFDEF DEBUG_OMNI_QUEUE}
  Dec(lastSlot);
  Assert(lastSlot^.Tag = tagStartOfList);
  Dec(lastSlot);
  Assert(lastSlot^.Tag = tagSentinel);
  {$ENDIF};
  if forceFree or assigned(obcCachedBlock) or (not CAS32(nil, lastSlot, obcCachedBlock)) then
    FreeMem(lastSlot);
end; { TOmniBaseQueue.ReleaseBlock }

function TOmniBaseQueue.TryDequeue(var value: TOmniValue): boolean;
var
  head: POmniTaggedValue;
  next: POmniTaggedValue;
  tag : TOmniQueueTag;
begin
  Result := true;
  EnterReader;
  repeat
    head := obcHeadPointer;
    tag := head^.Tag;
    if tag = tagFree then begin
      Result := false;
      break; //repeat
    end
    else if tag = tagAllocated then begin
      if head^.CASTag(tag, tagRemoving) then
        break //repeat
    end
    else if tag = tagBlockPointer then begin
      if head^.CASTag(tag, tagDestroying) then
        break //repeat
    end
    else
      DSiYield;
  until false;
  if Result then begin // dequeueing
    {$IFDEF DEBUG_OMNI_QUEUE} Assert(head = obcHeadPointer); {$ENDIF}
    if tag = tagAllocated then begin
      Inc(obcHeadPointer); // release the lock
      value := head^.Value;
      if value.IsInterface then begin
        head^.Value.AsInterface._Release;
        head^.Value.RawZero;
      end;
    end
    else begin // releasing memory
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(tag = tagBlockPointer); {$ENDIF}
      next := POmniTaggedValue(head^.Value.AsPointer);
      Assert(next^.Tag = tagAllocated);
      Inc(next);
      obcHeadPointer := next; // release the lock
      Dec(next);
      value := next^.Value;
      if value.IsInterface then begin
        next^.Value.AsInterface._Release;
        next^.Value.RawZero;
      end;
      // At this moment, another thread may still be dequeueing from one of the previous
      // slots and memory should not yet be released! Even worse - another thread may be
      // trying to *enqueue* somewhere inside the current memory block and we definitely
      // don't want to recycle this memory block and allow that thread to succeed!
      LeaveReader;
      EnterWriter;
      ReleaseBlock(head);
      LeaveWriter;
      Exit;
    end;
  end;
  LeaveReader;
end; { TOmniBaseQueue.TryDequeue }

{ initialization }

procedure InitializeTimingInfo;
var
  queue: TOmniBaseBoundedQueue;
  stack: TOmniBaseBoundedStack;
begin
  stack := TOmniBaseBoundedStack.Create;
  stack.Initialize(10, 4); // enough for initialization
  FreeAndNil(stack);
  queue := TOmniBaseBoundedQueue.Create;
  queue.Initialize(10, 4); // enough for initialization
  FreeAndNil(queue);
end; { InitializeTimingInfo }

{ TOmniQueue }

constructor TOmniQueue.Create;
begin
  inherited Create;
  ocContainerSubject := TOmniContainerSubject.Create;
end; { TOmniQueue.Create }

destructor TOmniQueue.Destroy;
begin
  FreeAndNil(ocContainerSubject);
  inherited;
end; { TOmniQueue.Destroy }

function TOmniQueue.Dequeue: TOmniValue;
begin
  Result := inherited Dequeue;
  ContainerSubject.Notify(coiNotifyOnAllRemoves);
end; { TOmniQueue.Dequeue }

procedure TOmniQueue.Enqueue(const value: TOmniValue);
begin
  inherited Enqueue(value);
  ContainerSubject.Notify(coiNotifyOnAllInserts);
end; { TOmniQueue.Enqueue }

function TOmniQueue.TryDequeue(var value: TOmniValue): boolean;
begin
  Result := inherited TryDequeue(value);
  if Result then
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
end; { TOmniQueue.TryDequeue }

initialization
  Assert(SizeOf(TOmniValue) = 13);
  Assert(SizeOf(TOmniTaggedValue) = 16);
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(CCollBlockSize = (65536 {$IFDEF DEBUG_OMNI_QUEUE} - 3*SizeOf(TOmniTaggedValue){$ENDIF}));
  InitializeTimingInfo;
end.

