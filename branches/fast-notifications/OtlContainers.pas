///<summary>Lock-free containers. Part of the OmniThreadLibrary project.</summary>
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
///   Last modification : 2008-10-26
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2008-10-26
///       - [GJ] Redesigned stack with better lock contention.
///       - [GJ] Totally redesigned queue, which is no longer based on stack and allows
///         multiple readers.
///</para></remarks>

{$WARN SYMBOL_PLATFORM OFF}

unit OtlContainers;

{$R-,O+,A8}

interface

uses
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlContainerObserver;

const
  CPartlyEmptyLoadFactor = 0.8; // When an element count drops below 90%, the container is considered 'partly empty'.
  CAlmostFullLoadFactor  = 0.9; // When an element count raises above 90%, the container is considered 'almost full'.

type
  {:Lock-free, single writer, single reader, size-limited stack.
  }
  IOmniStack = interface ['{F4C57327-18A0-44D6-B95D-2D51A0EF32B4}']
    procedure Empty;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    function  Pop(var value): boolean;
    function  Push(const value): boolean;
  end; { IOmniStack }

  {:Lock-free, single writer, single reader ring buffer.
  }
  IOmniQueue = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    function  Dequeue(var value): boolean;
    procedure Empty;
    function  Enqueue(const value): boolean;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniQueue }

  TOmniContainerOption = ({coEnableMonitor, }coMonitorOnDequeue);
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
    Buffer         : TReferencedPtrBuffer;
  end; { TOmniRingBuffer }

  POmniLinkedData = ^TOmniLinkedData;
  TOmniLinkedData = packed record
    Next: POmniLinkedData;
    Data: record end;           //user data, variable size
  end; { TOmniLinkedData }

  TOmniBaseStack = class abstract(TInterfacedObject, IOmniStack)
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
    function  Pop(var value): boolean; virtual;
    function  Push(const value): boolean; virtual;
    property  ElementSize: integer read obsElementSize;
    property  NumElements: integer read obsNumElements;
  end; { TOmniBaseStack }

  TOmniStack = class(TOmniBaseStack, IOmniMonitorSupport, IOmniContainerSubject)
  strict private
    osAlmostFullCount : integer;
    osContainerSubject: IOmniContainerSubject;
    osInStackCount    : TGp4AlignedInt;
    osMonitorSupport  : IOmniMonitorSupport;
    osOptions         : TOmniContainerOptions;
    osPartlyEmptyCount: integer;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [{coEnableMonitor}];
      partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
      almostFullLoadFactor: real = CAlmostFullLoadFactor);
    function Pop(var value): boolean; override;
    function Push(const value): boolean; override;
    property MonitorSupport: IOmniMonitorSupport read osMonitorSupport implements IOmniMonitorSupport;
    property Options: TOmniContainerOptions read osOptions;
    property ContainerSubject: IOmniContainerSubject read osContainerSubject
      implements IOmniContainerSubject;
  end; { TOmniStack }

  TOmniBaseQueue = class abstract(TInterfacedObject, IOmniQueue)
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
  end; { TOmniBaseQueue }

  TOmniQueue = class(TOmniBaseQueue, IOmniContainerSubject)
  strict private
    oqAlmostFullCount    : integer;
    oqContainerSubject   : IOmniContainerSubject;
    oqInQueueCount       : TGp4AlignedInt;
    oqOptions            : TOmniContainerOptions;
    oqPartlyEmptyCount   : integer;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [{coEnableMonitor}];
      partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
      almostFullLoadFactor: real = CAlmostFullLoadFactor);
    function  Dequeue(var value): boolean;
    function  Enqueue(const value): boolean;
    property  ContainerSubject: IOmniContainerSubject read oqContainerSubject
      implements IOmniContainerSubject;
    property  Options: TOmniContainerOptions read oqOptions;
  end; { TOmniQueue }

implementation

uses
  Windows,
  SysUtils, 
  Classes;

type
  TOmniContainerSubject = class(TInterfacedObject, IOmniContainerSubject)
  strict private
    csObserverList: IOmniContainerObserverList;
  public
    constructor Create;
    procedure Attach(const observer: IOmniContainerObserver; interest:
      TOmniContainerObserverInterest);
    procedure Detach(const observer: IOmniContainerObserver);
    procedure Notify(interest: TOmniContainerObserverInterest);
    procedure NotifyOnce(interest: TOmniContainerObserverInterest);
  end; { TOmniContainerSubject }

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

{ TOmniContainerSubject }

procedure TOmniContainerSubject.Attach(const observer: IOmniContainerObserver; interest:
  TOmniContainerObserverInterest);
begin
  csObserverList.Add(observer, interest);
end; { TOmniContainerSubject.Attach }

constructor TOmniContainerSubject.Create;
begin
  inherited Create;
  csObserverList := CreateContainerObserverList;
end; { TOmniContainerSubject.Create }

procedure TOmniContainerSubject.Detach(const observer: IOmniContainerObserver);
begin
  csObserverList.Remove(observer);
end; { TOmniContainerSubject.Detach }

procedure TOmniContainerSubject.Notify(interest: TOmniContainerObserverInterest);
var
  observer: IOmniContainerObserver;
begin
  for observer in csObserverList.Enumerate(interest) do
    observer.Notify;
end; { TOmniContainerSubject.Notify }

procedure TOmniContainerSubject.NotifyOnce( interest: TOmniContainerObserverInterest);
var
  iIntf     : IInterface;
  observer  : IOmniContainerObserver;
  removeList: TInterfaceList;
begin
  removeList := TInterfaceList.Create;
  try
    for observer in csObserverList.Enumerate(interest) do begin
      observer.Notify;
      removeList.Add(observer);
    end;
    for iIntf in removeList do begin
      Supports(iIntf, IOmniContainerObserver, observer);
      Detach(observer);
    end;
  finally FreeAndNil(removeList); end;
end; { TOmniContainerSubject.NotifyAndRemove }

{ TOmniBaseStack }

destructor TOmniBaseStack.Destroy;
begin
  FreeMem(obsPublicChainP);
  inherited;
end; { TOmniBaseStack.Destroy }

procedure TOmniBaseStack.Empty;
var
  linkedData: POmniLinkedData;
begin
  repeat
    linkedData := PopLink(obsPublicChainP^);
    if not assigned(linkedData) then
      break; //repeat
    PushLink(linkedData, obsRecycleChainP^);
  until false;
end; { TOmniBaseStack.Empty }

procedure TOmniBaseStack.Initialize(numElements, elementSize: integer);
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
end; { TOmniBaseStack.Initialize }

function TOmniBaseStack.IsEmpty: boolean;
begin
  Result := not assigned(obsPublicChainP^.PData);
end; { TOmniBaseStack.IsEmpty }

function TOmniBaseStack.IsFull: boolean;
begin
  Result := not assigned(obsRecycleChainP^.PData);
end; { TOmniBaseStack.IsFull }

procedure TOmniBaseStack.MeasureExecutionTimes;
const
  NumOfSamples = 10;
var
  TimeTestField: array [0..2] of array [1..NumOfSamples] of int64;

  function GetMinAndClear(Rutine, Count: cardinal): int64;
  var
    m: cardinal;
    n: integer;
    x: integer;
  begin
    result := 0;
    for m := 1 to Count do begin
      x:= 1;
      for n:= 2 to NumOfSamples do
        if TimeTestField[Rutine, n] < TimeTestField[Rutine, x] then
          x := n;
      Inc(result, TimeTestField[Rutine, x]);
      TimeTestField[Rutine, x] := MaxLongInt;
    end;
  end; { GetMinAndClear }

var
  affinity   : string;
  currElement: POmniLinkedData;
  n          : integer;

begin { TOmniBaseStack.MeasureExecutionTimes }
  if not obsIsInitialized then begin
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obsTaskPopLoops := 1;
      obsTaskPushLoops := 1;
      for n := 1 to NumOfSamples do begin
        //Measure RemoveLink rutine delay
        TimeTestField[0, n] := GetTimeStamp;
        currElement := PopLink(obsRecycleChainP^);
        TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
        //Measure InsertLink rutine delay
        TimeTestField[1, n] := GetTimeStamp;
        PushLink(currElement, obsRecycleChainP^);
        TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
        //Measure GetTimeStamp rutine delay
        TimeTestField[2, n] := GetTimeStamp;
        TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
      end;
      //Calculate first 4 minimum average for GetTimeStamp
      n := GetMinAndClear(2, 4);
      //Calculate first 4 minimum average for RemoveLink rutine
      obsTaskPopLoops := (GetMinAndClear(0, 4) - n) div 2;
      //Calculate first 4 minimum average for InsertLink rutine
      obsTaskPushLoops := (GetMinAndClear(1, 4) - n) div 4;
      obsIsInitialized := true;
    finally DSiSetThreadAffinity(affinity); end;
  end;
end;  { TOmniBaseStack.MeasureExecutionTimes }

function TOmniBaseStack.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obsPublicChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obsRecycleChainP^);
end; { TOmniBaseStack.Pop }

class function TOmniBaseStack.PopLink(var chain: TReferencedPtr): POmniLinkedData;
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
       not AtomicCmpXchg4b(CurrentReference, ThreadReference, Reference)
    then
      goto TryAgain;
    //Reference is set...
    result := PData;
    //Empty test
    if result = nil then
      AtomicCmpXchg4b(ThreadReference, 0, Reference)            //Clear Reference if task own reference
    else
      if not AtomicCmpXchg8b(result, ThreadReference, result.Next, 0, chain) then
        goto TryAgain;
  end;
end; { TOmniBaseStack.PopLink }

function TOmniBaseStack.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obsRecycleChainP^);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  PushLink(linkedData, obsPublicChainP^);
end; { TOmniBaseStack.Push }

class procedure TOmniBaseStack.PushLink(const link: POmniLinkedData; var chain: TReferencedPtr);
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
    until AtomicCmpXchg4b(PMemData, link, PData);
  end;
end; { TOmniBaseStack.PushLink }

{ TOmniStack }

constructor TOmniStack.Create(numElements, elementSize: integer; options:
  TOmniContainerOptions; partlyEmptyLoadFactor, almostFullLoadFactor: real);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  osContainerSubject := TOmniContainerSubject.Create;
  osOptions := options;
  osInStackCount.Value := 0;
  osPartlyEmptyCount := Round(numElements * partlyEmptyLoadFactor);
  if osPartlyEmptyCount >= numElements then
    osPartlyEmptyCount := numElements - 1;
  osAlmostFullCount := Round(numElements * almostFullLoadFactor);
  if osAlmostFullCount >= numElements then
    osAlmostFullCount := numElements - 1;
end; { TOmniStack.Create }

function TOmniStack.Pop(var value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Pop(value);
  if Result then begin
    countAfter := osInStackCount.Decrement;
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
    if countAfter = 0 then
      ContainerSubject.Notify(coiNotifyOnLastRemove);
    if countAfter <= osPartlyEmptyCount then
      ContainerSubject.NotifyOnce(coiNotifyOnPartlyEmpty);
  end;
end; { TOmniStack.Pop }

function TOmniStack.Push(const value): boolean;
var
  countAfter : integer;
begin
  Result := inherited Push(value);
  if Result then begin
    countAfter := osInStackCount.Increment;
    ContainerSubject.Notify(coiNotifyOnAllInserts);
    ContainerSubject.NotifyOnce(coiNotifyOnFirstInsert);
    if countAfter >= osAlmostFullCount then
      ContainerSubject.NotifyOnce(coiNotifyOnAlmostFull);
  end;
end; { TOmniStack.Push }

{ TOmniBaseQueue }

function TOmniBaseQueue.Dequeue(var value): boolean;
var
  Data: pointer;
begin
  Data := RemoveLink(obqPublicRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(Data^, value, ElementSize);
  InsertLink(Data, obqRecycleRingBuffer);
end; { TOmniBaseQueue.Dequeue }

destructor TOmniBaseQueue.Destroy;
begin
  FreeMem(obqDataBuffer);
  FreeMem(obqPublicRingBuffer);
  FreeMem(obqRecycleRingBuffer);
  inherited;
end; { TOmniBaseQueue.Destroy }

procedure TOmniBaseQueue.Empty;
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
end; { TOmniBaseQueue.Empty }

function TOmniBaseQueue.Enqueue(const value): boolean;
var
  Data: pointer;
begin
  Data := RemoveLink(obqRecycleRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(value, Data^, ElementSize);
  InsertLink(Data, obqPublicRingBuffer);
end; { TOmniBaseQueue.Enqueue }

procedure TOmniBaseQueue.Initialize(numElements, elementSize: integer);
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
end; { TOmniBaseQueue.Initialize }

class procedure TOmniBaseQueue.InsertLink(const data: pointer; const ringBuffer:
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
       not AtomicCmpXchg4b(CurrentReference, ThreadReference, LastIn.Reference)
    then
      goto TryAgain;
    //Reference is set...
    CurrentLastIn := LastIn.PData;
    AtomicCmpXchg4b(CurrentLastIn.Reference, ThreadReference, CurrentLastIn.Reference);
    if (ThreadReference <> LastIn.Reference) or
      not AtomicCmpXchg8b(CurrentLastIn.PData, ThreadReference, data, ThreadReference, CurrentLastIn^)
    then
      goto TryAgain;
    //Calculate ringBuffer next LastIn address
    NewLastIn := pointer(cardinal(CurrentLastIn) + SizeOf(TReferencedPtr));
    if cardinal(NewLastIn) > cardinal(EndBuffer) then
      NewLastIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not AtomicCmpXchg8b(CurrentLastIn, ThreadReference, NewLastIn, 0, LastIn) then
      goto TryAgain;
  end;
end; { TOmniBaseQueue.InsertLink }

function TOmniBaseQueue.IsEmpty: boolean;
begin
  Result := (obqPublicRingBuffer.FirstIn.PData = obqPublicRingBuffer.LastIn.PData);
end; { TOmniBaseQueue.IsEmpty }

function TOmniBaseQueue.IsFull: boolean;
var
  NewLastIn: pointer;
begin
  NewLastIn := pointer(cardinal(obqPublicRingBuffer.LastIn.PData) + SizeOf(TReferencedPtr));
  if cardinal(NewLastIn) > cardinal(obqPublicRingBuffer.EndBuffer) then
    NewLastIn := obqPublicRingBuffer.StartBuffer;
  result := (cardinal(NewLastIn) = cardinal(obqPublicRingBuffer.LastIn.PData)) or
    (obqRecycleRingBuffer.FirstIn.PData = obqRecycleRingBuffer.LastIn.PData);
end; { TOmniBaseQueue.IsFull }

procedure TOmniBaseQueue.MeasureExecutionTimes;
const
  NumOfSamples = 10;
var
  TimeTestField: array [0..2]of array [1..NumOfSamples] of int64;

  function GetMinAndClear(Rutine, Count: cardinal): int64;
  var
    m: cardinal;
    n: integer;
    x: integer;
  begin
    result := 0;
    for m := 1 to Count do begin
      x:= 1;
      for n:= 2 to NumOfSamples do
        if TimeTestField[Rutine, n] < TimeTestField[Rutine, x] then
          x := n;
      Inc(result, TimeTestField[Rutine, x]);
      TimeTestField[Rutine, x] := MaxLongInt;
    end;
  end; { GetMinAndClear }

var
  affinity   : string;
  currElement: pointer;
  n          : integer;

begin { TOmniBaseQueue.MeasureExecutionTimes }
  if not obqIsInitialized then begin
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obqTaskRemoveLoops := 1;
      obqTaskInsertLoops := 1;
      for n := 1 to NumOfSamples do  begin
        //Measure RemoveLink rutine delay
        TimeTestField[0, n] := GetTimeStamp;
        currElement := RemoveLink(obqRecycleRingBuffer);
        TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
        //Measure InsertLink rutine delay
        TimeTestField[1, n] := GetTimeStamp;
        InsertLink(currElement, obqRecycleRingBuffer);
        TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
        //Measure GetTimeStamp rutine delay
        TimeTestField[2, n] := GetTimeStamp;
        TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
      end;
      //Calculate first 4 minimum average for GetTimeStamp
      n := GetMinAndClear(2, 4);
      //Calculate first 4 minimum average for RemoveLink rutine
      obqTaskRemoveLoops := (GetMinAndClear(0, 4) - n) div 4;
      //Calculate first 4 minimum average for InsertLink rutine
      obqTaskInsertLoops := (GetMinAndClear(1, 4) - n) div 4;
      obqIsInitialized := true;
    finally DSiSetThreadAffinity(affinity); end;
  end;
end; { TOmniBaseQueue.MeasureExecutionTimes }

class function TOmniBaseQueue.RemoveLink(const ringBuffer: POmniRingBuffer): pointer;
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
      not AtomicCmpXchg4b(CurrentReference, Reference, FirstIn.Reference)
    then
      goto TryAgain;
    //Reference is set...
    CurrentFirstIn := FirstIn.PData;
    //Empty test
    if CurrentFirstIn = LastIn.PData then begin
      //Clear Reference if task own reference
      AtomicCmpXchg4b(Reference, 0, FirstIn.Reference);
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
    if not AtomicCmpXchg8b(CurrentFirstIn, Reference, NewFirstIn, 0, FirstIn) then
      goto TryAgain;
  end;
end; { TOmniBaseQueue.RemoveLink }

{ TOmniQueue }

constructor TOmniQueue.Create(numElements, elementSize: integer; options:
  TOmniContainerOptions; partlyEmptyLoadFactor, almostFullLoadFactor: real);
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
  oqOptions := options;
end; { TOmniQueue.Create }

function TOmniQueue.Dequeue(var value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Dequeue(value);
  if Result then begin
    countAfter := oqInQueueCount.Decrement;
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
    if countAfter = 0 then
      ContainerSubject.Notify(coiNotifyOnLastRemove);
    if countAfter <= oqPartlyEmptyCount then
      ContainerSubject.NotifyOnce(coiNotifyOnPartlyEmpty);
  end;
end; { TOmniQueue.Dequeue }

function TOmniQueue.Enqueue(const value): boolean;
var
  countAfter: integer;
begin
  Result := inherited Enqueue(value);
  if Result then begin
    countAfter := oqInQueueCount.Increment;
    ContainerSubject.NotifyOnce(coiNotifyOnFirstInsert);
    ContainerSubject.Notify(coiNotifyOnAllInserts);
    if countAfter >= oqAlmostFullCount then
      ContainerSubject.NotifyOnce(coiNotifyOnAlmostFull);
  end;
end; { TOmniQueue.Enqueue }

{ initialization }

procedure InitializeTimingInfo;
var
  queue: TOmniBaseQueue;
  stack: TOmniBaseStack;
begin
  stack := TOmniBaseStack.Create;
  stack.Initialize(10, 4); // enough for initialization
  FreeAndNil(stack);
  queue := TOmniBaseQueue.Create;
  queue.Initialize(10, 4); // enough for initialization
  FreeAndNil(queue);
end; { InitializeTimingInfo }

initialization
  InitializeTimingInfo;
end.

