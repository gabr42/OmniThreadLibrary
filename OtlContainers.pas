///<summary>Microlocking containers. Part of the OmniThreadLibrary project.</summary>
///<remarks>TOmni[Base]Queue requires Pentium 4 processor (or newer) unless OTL_OLDCPU is defined.</remarks>
///<author>Primoz Gabrijelcic, GJ</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2018 Primoz Gabrijelcic
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
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://plus.google.com/communities/112307748950248514961
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : GJ, Sean B. Durkin
///   Creation date     : 2008-07-13
///   Last modification : 2018-04-17
///   Version           : 3.02b
///</para><para>
///   History:
///     3.02b: 2018-04-17
///       - Fixed race condition in TOmniBaseBoundedQueue.RemoveLink which could cause
///         TOmniBaseBoundedQueue.Enqueue to return False when queue was empty.
///     3.02a: 2017-04-06
///       - Compiles with Delphi 10.2 Tokyo.
///     3.02: 2015-10-03
///       - Imported mobile support by [Sean].
///     3.01a: 2012-12-05
///       - Prevent memory leak if (Queue|Stack).Initialize is called more than once
///         (thx to [h.hasenack]).
///     3.01: 2012-11-09
///       - TOmniBaseBounded(Queue|Stack) internally aligns allocated memory to required
///         CAS granularity (8 bytes on 32-bit platforms, 16 bytes on 64-bit platforms).
///       - TOmniBaseBoundedQueue's ring buffers are internally aligned to 2*SizeOf(pointer).
///     3.0: 2011-12-19
///       - [GJ] Implemented 64-bit TOmni[Base]Bounded(Queue|Stack).
///       - Fixed TOmni[Base]Queue to work in 64-bit world.
///     2.05: 2011-08-26
///       - Implemented TOmni[Base]Queue.IsEmpty. Keep in mind that the returned value may
///         not be valid for any ammount of time if other threads are reading from/
///         writing to the queue.
///     2.04: 2010-07-01
///       - Includes OTLOptions.inc.
///     2.03a: 2010-05-06
///       - Fixed memory leak in TOmni[Base]Queue when queueing String, WideString,
///         Variant and Extended values.
///     2.03: 2010-02-18
///       - Reversed head and tail because they were used illogically.
///     2.02a: 2010-02-09
///       - Dynamically allocate head/tail structures so that they are allways 8-allocated.
///       - Optimized algorithm using atomic move instead of atomic compare-and-swap in
///         some places (thanks to GJ).
///     2.02: 2010-02-08
///       - New ABA- and MREW-free dynamic queue algorithm.
///       - Dynamic queue parameters are settable in the constructor.
///     2.01: 2010-02-04
///       - Uses CAS8 instead of CAS32.
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

unit OtlContainers;

{$I OtlOptions.inc}

{$OPTIMIZATION ON}
{$WARN SYMBOL_PLATFORM OFF}
{$IFNDEF CPUX64}
  {$DEFINE OTL_OLDCPU} // undefine if you're sure your code will only run on a CPU that supports SSE2 instruction set (more specifically, Move64 instruction)
{$ENDIF ~CPUX64}
//DEFINE DEBUG_OMNI_QUEUE to enable assertions in TOmniBaseQueue

//We don't have a platform-independant way of using cmpx8b/cmpx16b
//(5-parameter OtlSync.CAS) so we can't use bus-locking on non-Windows targets.
{$IFDEF MSWINDOWS}
  {$DEFINE OTL_HaveCmpx16b}
{$ELSE}
  {$UNDEF OTL_HaveCmpx16b}
{$ENDIF ~MSWINDOWS}

interface

uses
  Classes,
  OtlCommon,
  OtlSync,
  SyncObjs,
{$IFDEF MSWINDOWS}
  DSiWin32,
  GpStuff,
{$ENDIF}
  OtlContainerObserver;

const
  CPartlyEmptyLoadFactor = 0.8; // When an element count drops below 80%, the container is considered 'partly empty'.
  CAlmostFullLoadFactor  = 0.9; // When an element count raises above 90%, the container is considered 'almost full'.

type
  {:Lock-free, multiple writer, multiple reader, bounded stack.
  }
  IOmniStack = interface ['{F4C57327-18A0-44D6-B95D-2D51A0EF32B4}']
    procedure Empty;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    function  Pop(var value): boolean;
    function  Push(const value): boolean;
  end; { IOmniStack }

  {:Lock-free, multiple writer, multiple reader ring buffer (bounded queue).
  }
  IOmniQueue = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    function  Dequeue(var value): boolean;
    procedure Empty;
    function  Enqueue(const value): boolean;
    procedure Initialize(numElements, elementSize: integer);
    function  IsEmpty: boolean;
    function  IsFull: boolean;
  end; { IOmniQueue }

  {$IFDEF OTL_MobileSupport}
  IOmniValueQueue = interface ['{3399B817-0502-4837-B1D7-BA167E8E03A7}']
    function  GetContainerSubject: TOmniContainerSubject;
    function  IsEmpty: boolean;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
    property  ContainerSubject: TOmniContainerSubject read GetContainerSubject;
  end; { IOmniValueQueue }
  {$ENDIF OTL_MobileSupport}

  PReferencedPtr = ^TReferencedPtr;
  TReferencedPtr = record
    {$IFNDEF OTL_HaveCmpx16b}[Volatile]{$ENDIF}
    PData    : pointer;
    {$IFDEF OTL_HaveCmpx16b} // references are only used in bus-locked implementation
    Reference: NativeInt;
    {$ENDIF}
  end; { TReferencedPtr }

  TReferencedPtrBuffer = array [0..MaxInt shr 5] of TReferencedPtr;

  POmniRingBuffer = ^TOmniRingBuffer;
  TOmniRingBuffer  = packed record
    FirstIn        : TReferencedPtr;
    Dummy          : array[1..128 - SizeOf(TReferencedPtr)] of byte; // push LastIn into next cache line
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
  {$IFNDEF OTL_HaveCmpx16b}
    obsLock         : IOmniCriticalSection;
  {$ENDIF ~OTL_HaveCmpx16b}
  class var
    class var obsIsInitialized: boolean;                //default is false
    class var obsTaskPopLoops : NativeInt;
    class var obsTaskPushLoops: NativeInt;
  strict protected
    procedure Acquire; inline;
    procedure Release; inline;
    procedure MeasureExecutionTimes;
    class function  PopLink(var chain: TReferencedPtr): POmniLinkedData; static;
    class procedure PushLink(const link: POmniLinkedData; var chain: TReferencedPtr); static;
  public
    constructor Create;
    destructor  Destroy; override;
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
    osInStackCount    : TOmniAlignedInt32;
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
    obqDataBuffer       : pointer;
    obqElementSize      : integer;
    obqNumElements      : integer;
    obqPublicRingBuffer : POmniRingBuffer;
    obqPublicRingMem    : pointer;
    obqRecycleRingBuffer: POmniRingBuffer;
    obqRecycleRingMem   : pointer;
    {$IFNDEF OTL_HaveCmpx16b}
    obqLock             : IOmniCriticalSection;
    {$ENDIF ~OTL_HaveCmpx16b}
  class var
    class var obqIsInitialized  : boolean;
    class var obqTaskInsertLoops: NativeInt;             //default is false
    class var obqTaskRemoveLoops: NativeInt;
  strict protected
    procedure Acquire; inline;
    procedure Release; inline;
    class procedure InsertLink(const data: pointer; const ringBuffer: POmniRingBuffer);
      static;
    procedure MeasureExecutionTimes;
    class function  RemoveLink(const ringBuffer: POmniRingBuffer): pointer; static;
  public
    constructor Create;
    destructor  Destroy; override;
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
    oqInQueueCount    : TOmniAlignedInt32;
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
    tagEndOfList, tagExtending, tagBlockPointer, tagDestroying, tagHeader,
    tagSentinel);

  TOmniTaggedValue = packed record
    Value   : TOmniValue;    //aligned for faster data access; overlaps with header's unreleased slot count, which must be pointer-aligned
    Tag     : TOmniQueueTag;
    Offset  : word;
    {$IFDEF CPUX64}
    Stuffing: array[1..4] of byte; // make TOmniTaggedValue 3 pointers big
    {$ENDIF CPUX64}
    function CASTag(oldTag, newTag: TOmniQueueTag): boolean; inline;            // atomic on Windows, composite on other platforms
  end; { TOmniTaggedValue }
  POmniTaggedValue = ^TOmniTaggedValue;

  TOmniTaggedPointer = packed record
    Slot    : POmniTaggedValue;
    Tag     : TOmniQueueTag;
    {$IFNDEF CPUX64}
    Stuffing: array [1..3] of byte; // record size must be congruent to 0 (mod 4)
    {$ELSE CPUX64}
    Stuffing: array [1..7] of byte; // record size must be congruent to 0 (mod 8)
    {$ENDIF CPUX64}
    function  CAS(
      oldSlot: POmniTaggedValue; oldTag: TOmniQueueTag;
      newSlot: POmniTaggedValue; newTag: TOmniQueueTag): boolean; inline;       // atomic on Windows, composite on other platforms
    procedure Move(newSlot: POmniTaggedValue; newTag: TOmniQueueTag); inline;   // atomic on Windows, composite on other platforms
  end; { TOmniTaggedPointer }
  POmniTaggedPointer = ^TOmniTaggedPointer;

  ///<summary>Dynamically allocated, O(1) enqueue and dequeue, threadsafe, microlocking queue.</summary>
  TOmniBaseQueue = class
  strict private // keep 4-aligned
    obcCachedBlock: POmniTaggedValue;
  strict private
    obcBlockSize  : integer;
    obcHeadPointer: POmniTaggedPointer;
    obcMemStack   : TOmniBaseBoundedStack;
    obcNumSlots   : integer;
    obcTailPointer: POmniTaggedPointer;
    {$IFNDEF OTL_HaveCmpx16b}
    obcLock       : IOmniCriticalSection;
    {$ENDIF ~OTL_HaveCmpx16b}
  strict protected
    {$IFDEF DEBUG_OMNI_QUEUE}
    procedure Assert(condition: boolean);
    {$ENDIF DEBUG_OMNI_QUEUE}
  strict protected
    procedure Acquire; inline;
    function  AllocateBlock: POmniTaggedValue;
    procedure Cleanup; virtual;
    procedure Initialize; virtual;
    function  NextSlot(slot: POmniTaggedValue): POmniTaggedValue; inline;
    procedure PartitionMemory(memory: POmniTaggedValue);
    procedure PreallocateMemory;
    function  PrevSlot(slot: POmniTaggedValue): POmniTaggedValue; inline;
    procedure Release; inline;
    procedure ReleaseBlock(firstSlot: POmniTaggedValue; forceFree: boolean = false);
  public
    constructor Create(blockSize: integer = 65536; numCachedBlocks: integer = 4);
    destructor  Destroy; override;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  IsEmpty: boolean;
    function  TryDequeue(var value: TOmniValue): boolean;
  end; { TOmniBaseQueue }

  TOmniQueue = class(TOmniBaseQueue)
  strict private
    ocContainerSubject: TOmniContainerSubject;
  strict protected
    procedure Cleanup; override;
    procedure Initialize; override;
  public
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
    property ContainerSubject: TOmniContainerSubject read ocContainerSubject;
  end; { TOmniQueue }

{$IFDEF OTL_MobileSupport}
/// <param name="UseBusLocking">Set to true to use a spinlock. Otherwise synchronisation is achieved by a critical section.</param>
/// <param name="ThresholdForFull">The count of OmniValues to which if the queue reaches or exceeds, it is considered full.
///   Use a a value of -1 to indicate there is no threshold (and hence events like coiNotifyOnAlmostFull will never fire).</param>
function CreateOmniValueQueue(UseBusLocking: boolean; ThresholdForFull: integer = -1): IOmniValueQueue;
{$ENDIF OTL_MobileSupport}

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF OTL_MobileSupport}
  Generics.Collections,
  {$ENDIF OTL_MobileSupport}
  SysUtils;

{$IFDEF OTL_MobileSupport}
type
  TInterestSet = set of TOmniContainerObserverInterest;

  TOmniValueQueue = class(TInterfacedObject, IOmniValueQueue)
  strict private
    FAlmostFullThreshold : integer;
    FContainerSubject    : TOmniContainerSubject;
    FFullThreshold       : integer;
    FInnerQueue          : TQueue<TOmniValue>;
    FNotifiableEvents    : TInterestSet;
    FPartlyEmptyThreshold: integer;
  strict private
    procedure CollectionNotifyEvent(Sender: TObject; const Item: TOmniValue; Action: TCollectionNotification);
    function  Dequeue: TOmniValue;
    procedure DoWithCritSec( Proc: TProc);
    procedure Enqueue(const value: TOmniValue);
    function  GetContainerSubject: TOmniContainerSubject;
    function  IsEmpty: boolean;
    procedure PropagateNotifications(Events: TInterestSet);
    function  TryDequeue(var value: TOmniValue): boolean;
  protected
    procedure EnterCriticalSection; virtual; abstract;
    procedure LeaveCriticalSection; virtual; abstract;
  public
    constructor Create(AThresholdForFull: integer);
    destructor  Destroy; override;
  end; { TOmniValueQueue }

  TOmniValueQueueCS = class(TOmniValueQueue)
  strict private
    FCritSect: TFixedCriticalSection;
  protected
    procedure EnterCriticalSection; override;
    procedure LeaveCriticalSection; override;
  public
    constructor Create(AThresholdForFull: integer);
    destructor  Destroy; override;
  end; { TOmniValueQueueCS }

  TOmniValueQueueSpin = class(TOmniValueQueue)
  strict private
    FLock: TSpinLock;
  protected
    procedure EnterCriticalSection; override;
    procedure LeaveCriticalSection; override;
  public
    constructor Create(AThresholdForFull: integer);
  end; { TOmniValueQueueSpin }

function CreateOmniValueQueue(UseBusLocking: boolean; ThresholdForFull: integer = -1): IOmniValueQueue;
begin
  if UseBusLocking then
    Result := TOmniValueQueueSpin.Create(ThresholdForFull)
  else
    Result := TOmniValueQueueCS.Create(ThresholdForFull)
end; { CreateOmniValueQueue }
{$ENDIF OTL_MobileSupport}

{$IFDEF MSWINDOWS}
{$IFDEF CPUX64}
procedure AsmInt3;
asm
  .noframe
  int 3
end; { AsmInt3 }

procedure AsmPause;
asm
  .noframe
  pause;
end; { AsmPause }
{$ENDIF CPUX64}
{$ENDIF MSWINDOWS}

function RoundUpTo(value: pointer; granularity: integer): pointer;
begin
  Result := pointer((((NativeInt(value) - 1) div granularity) + 1) * granularity);
end; { RoundUpTo }

{ TOmniBaseBoundedStack }

constructor TOmniBaseBoundedStack.Create;
begin
  inherited Create;
  {$IFNDEF OTL_HaveCmpx16b}
  obsLock := CreateOmniCriticalSection;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedStack.Create }

destructor TOmniBaseBoundedStack.Destroy;
begin
  FreeMem(obsDataBuffer);
  inherited;
end; { TOmniBaseBoundedStack.Destroy }

procedure TOmniBaseBoundedStack.Acquire; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obsLock.Acquire;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedStack.Acquire }

procedure TOmniBaseBoundedStack.Release; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obsLock.Release;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedStack.Release }

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
  bufferElementSize : integer;
  currElement       : POmniLinkedData;
  dataBuffer        : pointer;
  iElement          : integer;
  nextElement       : POmniLinkedData;
  roundedElementSize: integer;
begin
  Assert(SizeOf(NativeInt) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obsNumElements := numElements;
  obsElementSize := elementSize;
  //calculate element size, round up to next aligned value
  roundedElementSize := (elementSize + SizeOf(pointer) - 1) AND NOT (SizeOf(pointer) - 1);
  //calculate buffer element size, round up to next aligned value
  bufferElementSize := ((SizeOf(TOmniLinkedData) + roundedElementSize) + SizeOf(pointer) - 1) AND NOT (SizeOf(pointer) - 1);
  //calculate DataBuffer
  FreeMem(obsDataBuffer);
  GetMem(obsDataBuffer, bufferElementSize * numElements + 2 * SizeOf(TReferencedPtr) + CASAlignment);
  dataBuffer := RoundUpTo(obsDataBuffer, CASAlignment);
  if NativeInt(dataBuffer) AND (SizeOf(pointer) - 1) <> 0 then
    raise Exception.Create('TOmniBaseContainer: obcBuffer is not aligned');
  obsPublicChainP := dataBuffer;
  inc(NativeInt(dataBuffer), SizeOf(TReferencedPtr));
  obsRecycleChainP := dataBuffer;
  inc(NativeInt(dataBuffer), SizeOf(TReferencedPtr));
  //Format buffer to recycleChain, init obsRecycleChain and obsPublicChain.
  //At the beginning, all elements are linked into the recycle chain.
  obsRecycleChainP^.PData := dataBuffer;
  currElement := obsRecycleChainP^.PData;
  for iElement := 0 to obsNumElements - 2 do begin
    nextElement := POmniLinkedData(NativeInt(currElement) + bufferElementSize);
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
  {$IFDEF MSWINDOWS}
  affinity   : string;
  {$ENDIF MSWINDOWS}
  currElement: POmniLinkedData;
  n          : integer;

begin { TOmniBaseBoundedStack.MeasureExecutionTimes }
  if not obsIsInitialized then begin
    {$IFDEF MSWINDOWS}
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
    {$ENDIF MSWINDOWS}
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obsTaskPopLoops := 1;
      obsTaskPushLoops := 1;
      for n := 1 to NumOfSamples do begin
        {$IFDEF OTL_HasTThreadYield}TThread.Yield;{$ELSE}DSiYield;{$ENDIF}
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
    {$IFDEF MSWINDOWS}
    finally DSiSetThreadAffinity(affinity); end;
    {$ENDIF MSWINDOWS}
  end;
end;  { TOmniBaseBoundedStack.MeasureExecutionTimes }

function TOmniBaseBoundedStack.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  Acquire;
  try
    linkedData := PopLink(obsPublicChainP^);
    Result := assigned(linkedData);
    if not Result then
      Exit;
    Move(linkedData.Data, value, ElementSize);
    PushLink(linkedData, obsRecycleChainP^);
  finally Release; end;
end; { TOmniBaseBoundedStack.Pop }

class function TOmniBaseBoundedStack.PopLink(var chain: TReferencedPtr): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//                                            ^------ < chainHead
var
  AtStartReference: NativeInt;
  CurrentReference: NativeInt;
  TaskCounter     : NativeInt;
  ThreadReference : NativeInt;
label
  TryAgain;
begin
  {$IFDEF OTL_HaveCmpx16b}
  ThreadReference := OtlSync.GetThreadId + 1;                           //Reference.bit0 := 1
  with chain do begin
TryAgain:
    TaskCounter := obsTaskPopLoops;
    AtStartReference := Reference OR 1;                         //Reference.bit0 := 1
    repeat
      CurrentReference := Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if (CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference) or
       not TInterlockedEx.CAS(CurrentReference, ThreadReference, Reference)
    then
      goto TryAgain;
    //Reference is set...
    Result := PData;
    //Empty test
    if result = nil then
      TInterlockedEx.CAS(ThreadReference, 0, Reference)         //Clear Reference if task own reference
    else if not CAS(Result, ThreadReference, Result.Next, 0, chain) then
      goto TryAgain;
  end; //with chain
  {$ELSE ~OTL_HaveCmpx16b}
  Result := chain.PData;
  chain.PData := Result^.Next;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedStack.PopLink }

function TOmniBaseBoundedStack.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  Acquire;
  try
    linkedData := PopLink(obsRecycleChainP^);
    Result := assigned(linkedData);
    if not Result then
      Exit;
    Move(value, linkedData.Data, ElementSize);
    PushLink(linkedData, obsPublicChainP^);
  finally Release; end;
end; { TOmniBaseBoundedStack.Push }

class procedure TOmniBaseBoundedStack.PushLink(const link: POmniLinkedData; var chain: TReferencedPtr);
var
  PMemData   : pointer;
  TaskCounter: NativeInt;
begin
  {$IFDEF OTL_HaveCmpx16b}
  with chain do begin
    for TaskCounter := 0 to obsTaskPushLoops do
      if (Reference AND 1 = 0) then
        break;
    repeat
      PMemData := PData;
      link.Next := PMemData;
    until TInterlockedEx.CAS(PMemData, link, PData);
  end;
  {$ELSE ~OTL_HaveCmpx16b}
  link.Next := chain.PData;
  chain.PData := link;
  {$ENDIF ~OTL_HaveCmpx16b}
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
    countAfter := osInStackCount.Decrement;
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
    if countAfter <= osPartlyEmptyCount then
      ContainerSubject.NotifyOnce(coiNotifyOnPartlyEmpty);
  end;
end; { TOmniBoundedStack.Pop }

function TOmniBoundedStack.Push(const value): boolean;
var
  countAfter: integer;
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

constructor TOmniBaseBoundedQueue.Create;
begin
  inherited Create;
  {$IFNDEF OTL_HaveCmpx16b}
  obqLock := CreateOmniCriticalSection;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedQueue.Create }

destructor TOmniBaseBoundedQueue.Destroy;
begin
  FreeMem(obqDataBuffer);
  FreeMem(obqPublicRingMem);
  FreeMem(obqRecycleRingMem);
  inherited;
end; { TOmniBaseBoundedQueue.Destroy }

procedure TOmniBaseBoundedQueue.Acquire; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obqLock.Acquire;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedQueue.Acquire }

procedure TOmniBaseBoundedQueue.Release; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obqLock.Release;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedQueue.Release }

function TOmniBaseBoundedQueue.Dequeue(var value): boolean;
var
  Data: pointer;
begin
  Acquire;
  try
    Data := RemoveLink(obqPublicRingBuffer);
    Result := assigned(Data);
    if not Result then
      Exit;
    Move(Data^, value, ElementSize);
    InsertLink(Data, obqRecycleRingBuffer);
  finally Release; end;
end; { TOmniBaseBoundedQueue.Dequeue }

procedure TOmniBaseBoundedQueue.Empty;
var
  Data: pointer;
begin
  Acquire;
  try
    repeat
      Data := RemoveLink(obqPublicRingBuffer);
      if assigned(Data) then
        InsertLink(Data, obqRecycleRingBuffer)
      else
        break;
    until false;
  finally Release; end;
end; { TOmniBaseBoundedQueue.Empty }

function TOmniBaseBoundedQueue.Enqueue(const value): boolean;
var
  Data: pointer;
begin
  Acquire;
  try
    Data := RemoveLink(obqRecycleRingBuffer);
    Result := assigned(Data);
    if not Result then
      Exit;
    Move(value, Data^, ElementSize);
    InsertLink(Data, obqPublicRingBuffer);
  finally Release; end;
end; { TOmniBaseBoundedQueue.Enqueue }

procedure TOmniBaseBoundedQueue.Initialize(numElements, elementSize: integer);
var
  dataBuffer        : pointer;
  n                 : integer;
  ringBufferSize    : cardinal;
  roundedElementSize: integer;
begin
  Assert(SizeOf(NativeInt) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obqNumElements := numElements;
  obqElementSize := elementSize;
  // calculate element size, round up to next aligned value
  roundedElementSize := (elementSize + SizeOf(pointer) - 1) AND NOT (SizeOf(pointer) - 1);
  // allocate obqDataBuffer
  FreeMem(obqDataBuffer);
  GetMem(obqDataBuffer, roundedElementSize * numElements + roundedElementSize + CASAlignment);
  dataBuffer := RoundUpTo(obqDataBuffer, CASAlignment);
  // allocate RingBuffers
  ringBufferSize := SizeOf(TReferencedPtr) * (numElements + 1) +
    SizeOf(TOmniRingBuffer) - SizeOf(TReferencedPtrBuffer);
  FreeMem(obqPublicRingMem);
  obqPublicRingMem := AllocMem(ringBufferSize + SizeOf(pointer) * 2);
  obqPublicRingBuffer := RoundUpTo(obqPublicRingMem, SizeOf(pointer) * 2);
  Assert(NativeInt(obqPublicRingBuffer) mod (SizeOf(pointer) * 2) = 0,
    Format('TOmniBaseContainer: obcPublicRingBuffer is not %d-aligned', [SizeOf(pointer) * 2]));
  FreeMem(obqRecycleRingMem);
  obqRecycleRingMem := AllocMem(ringBufferSize + SizeOf(pointer) * 2);
  obqRecycleRingBuffer := RoundUpTo(obqRecycleRingMem, SizeOf(pointer) * 2);
  Assert(NativeInt(obqRecycleRingBuffer) mod (SizeOf(pointer) * 2) = 0,
    Format('TOmniBaseContainer: obcRecycleRingBuffer is not %d-aligned', [SizeOf(pointer) * 2]));
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
    obqRecycleRingBuffer.Buffer[n].PData := pointer(NativeInt(dataBuffer) + NativeInt(n * roundedElementSize));
  MeasureExecutionTimes;
end; { TOmniBaseBoundedQueue.Initialize }

class procedure TOmniBaseBoundedQueue.InsertLink(const data: pointer; const ringBuffer:
  POmniRingBuffer);
//FIFO buffer logic
//Insert link to queue model with idle/busy status bit
var
  AtStartReference: NativeInt;
  CurrentLastIn   : PReferencedPtr;
  CurrentReference: NativeInt;
  NewLastIn       : PReferencedPtr;
  TaskCounter     : NativeInt;
  ThreadReference : NativeInt;
label
  TryAgain;
begin
  {$IFDEF OTL_HaveCmpx16b}
  ThreadReference := OtlSync.GetThreadId + 1;                           //Reference.bit0 := 1
  with ringBuffer^ do begin
TryAgain:
    TaskCounter := obqTaskInsertLoops;
    AtStartReference := LastIn.Reference OR 1;                  //Reference.bit0 := 1
    repeat
      CurrentReference := LastIn.Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if ((CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference))
       or (not TInterlockedEx.CAS(CurrentReference, ThreadReference, LastIn.Reference))
    then
      goto TryAgain;

    //Reference is set...
    CurrentLastIn := LastIn.PData;
    TInterlockedEx.CAS(CurrentLastIn.Reference, ThreadReference, CurrentLastIn.Reference);
    if (ThreadReference <> LastIn.Reference) or
       (not CAS(CurrentLastIn.PData, ThreadReference, data, ThreadReference, CurrentLastIn^))
    then
      goto TryAgain;    //Calculate ringBuffer next LastIn address

    NewLastIn := pointer(NativeInt(CurrentLastIn) + SizeOf(TReferencedPtr));
    if NativeInt(NewLastIn) > NativeInt(EndBuffer) then
      NewLastIn := StartBuffer;
    //Try to exchange and clear Reference if task own reference
    if not CAS(CurrentLastIn, ThreadReference, NewLastIn, 0, LastIn) then
      goto TryAgain;
  end;

  {$ELSE ~OTL_HaveCmpx16b}
  CurrentLastIn := ringBuffer^.LastIn.PData;
  CurrentLastIn^.PData := data;
  NewLastIn := pointer(NativeInt(CurrentLastIn) + SizeOf(TReferencedPtr));
  if NativeInt(NewLastIn) > NativeInt(ringBuffer^.EndBuffer) then
    NewLastIn := ringBuffer^.StartBuffer;
  ringBuffer^.LastIn.PData := NewLastIn;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniBaseBoundedQueue.InsertLink }

function TOmniBaseBoundedQueue.IsEmpty: boolean;
begin
  Result := (obqPublicRingBuffer.FirstIn.PData = obqPublicRingBuffer.LastIn.PData);
end; { TOmniBaseBoundedQueue.IsEmpty }

function TOmniBaseBoundedQueue.IsFull: boolean;
var
  NewLastIn: pointer;
begin
  Acquire;
  try
    NewLastIn := pointer(NativeInt(obqPublicRingBuffer.LastIn.PData) + SizeOf(TReferencedPtr));
    if NativeInt(NewLastIn) > NativeInt(obqPublicRingBuffer.EndBuffer) then
      NewLastIn := obqPublicRingBuffer.StartBuffer;
    result := (NativeInt(NewLastIn) = NativeInt(obqPublicRingBuffer.LastIn.PData)) or
      (obqRecycleRingBuffer.FirstIn.PData = obqRecycleRingBuffer.LastIn.PData);
  finally Release; end;
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
  {$IFDEF MSWINDOWS}
  affinity   : string;
  {$ENDIF MSWINDOWS}
  currElement: pointer;
  n          : integer;

begin { TOmniBaseBoundedQueue.MeasureExecutionTimes }
  if not obqIsInitialized then begin
    {$IFDEF MSWINDOWS}
    affinity := DSiGetThreadAffinity;
    DSiSetThreadAffinity(affinity[1]);
    try
    {$ENDIF MSWINDOWS}
      //Calculate  TaskPopDelay and TaskPushDelay counter values depend on CPU speed!!!}
      obqTaskRemoveLoops := 1;
      obqTaskInsertLoops := 1;
      for n := 1 to NumOfSamples do  begin
        {$IFDEF OTL_HasTThreadYield}TThread.Yield;{$ELSE}DSiYield;{$ENDIF}
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
    {$IFDEF MSWINDOWS}
    finally DSiSetThreadAffinity(affinity); end;
    {$ENDIF MSWINDOWS}
  end;
end; { TOmniBaseBoundedQueue.MeasureExecutionTimes }

class function TOmniBaseBoundedQueue.RemoveLink(const ringBuffer: POmniRingBuffer): pointer;
var
  AtStartReference      : NativeInt;
  CurrentFirstIn        : pointer;
  CurrentReference      : NativeInt;
  NewFirstIn            : pointer;
  Reference             : NativeInt;
  TaskCounter           : NativeInt;
label
  TryAgain;
begin
  {$IFDEF OTL_HaveCmpx16b}
  Reference := OtlSync.GetThreadId + 1;                                 //Reference.bit0 := 1
  with ringBuffer^ do begin
TryAgain:
    TaskCounter := obqTaskRemoveLoops;
    AtStartReference := FirstIn.Reference OR 1;                 //Reference.bit0 := 1
    repeat
      CurrentReference := FirstIn.Reference;
      Dec(TaskCounter);
    until (TaskCounter = 0) or (CurrentReference AND 1 = 0);
    if ((CurrentReference AND 1 <> 0) and (AtStartReference <> CurrentReference))
       or (not TInterlockedEx.CAS(CurrentReference, Reference, FirstIn.Reference))
    then
      goto TryAgain;

    //Reference is set...
    CurrentFirstIn := FirstIn.PData;
    //Empty test
    if CurrentFirstIn = pointer(TInterlockedEx.CompareExchange(PNativeInt(@LastIn.PData)^, 0, 0)) then begin // LastIn is not locked so we have to read value in a safe manner
      //Clear Reference if task own reference
      if not TInterlockedEx.CAS(Reference, 0, FirstIn.Reference) then
        goto TryAgain; // can happen due to a race condition between RemoveLink and InsertLink
      Result := nil;
      Exit;
    end;

    //Load Result
    Result := PReferencedPtr(FirstIn.PData).PData;
    //Calculate ringBuffer next FirstIn address
    NewFirstIn := pointer(NativeInt(CurrentFirstIn) + SizeOf(TReferencedPtr));
    if NativeInt(NewFirstIn) > NativeInt(EndBuffer) then
      NewFirstIn := StartBuffer;

    //Try to exchange and clear Reference if task own reference
    if not CAS(CurrentFirstIn, Reference, NewFirstIn, 0, FirstIn) then
      goto TryAgain;
  end;

  {$ELSE ~OTL_HaveCmpx16b}
  CurrentFirstIn := ringBuffer^.FirstIn.PData;
  if CurrentFirstIn = ringBuffer^.LastIn.PData then
    Exit(nil);
  Result := PReferencedPtr(ringBuffer^.FirstIn.PData)^.PData;
  NewFirstIn := pointer(NativeInt(CurrentFirstIn) + SizeOf(TReferencedPtr));
  if NativeInt(NewFirstIn) > NativeInt(ringBuffer^.EndBuffer) then
    NewFirstIn := ringBuffer^.StartBuffer;
  ringBuffer^.FirstIn.PData := NewFirstIn;
  {$ENDIF ~OTL_HaveCmpx16b}
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

//Bus-locking queue implementation.
//On non-Windows platforms same memory layout is used, but synchronisation
//is achieved using a critical section, not with bus-locking because we
//don't have a platform-independant equivalent of cmpx8b/cmpx16b.

(*
TOmniQueue
===============

tags:
  tagFree
  tagAllocating
  tagAllocated
  tagRemoving
  tagEndOfList
  tagExtending
  tagBlockPointer
  tagDestroying
  tagHeader
  tagSentinel

header contains:
  tail
    slot = 4/8 bytes (32/64-bit)
    tag  = 4/8 bytes
  head
    slot = 4/8 bytes
    tag  = 4/8 bytes
all are 4-aligned

slot contains:
  TOmniValue = 13/17 bytes
  tag        = 1 byte
  offset     = 2 bytes
  stuffing   = 0/4 bytes
TOmniValues are 4/8-aligned

block is initialized to:
[tagHeader, num slots - 1, 0] [tagSentinel, 0, 1] [tagFree 0, 2] .. [tagFree, 0, num slots - 2] [tagEndOfList, 0, num slots - 1]

Enqueue:
  repeat
      head = header.head.slot
      old_tag = header.head.tag
      if header.head.CAS(head, tagFree, head, tagAllocating) then
          break
      else if header.head.CAS(head, tagEndOfList, head, tagExtending) then
          break
      else
          yield
  forever
  if header.head.tag = tagAllocating then
      store <value, tagAllocated> into slot
      header.head.CAS(head, tagAllocating, NextSlot(head), NextSlot(head).tag)
  else
      allocate block // from cache, if possible
      next = second data slot in the new block
      set next to <tagAllocated, value>
      set last slot in the original block to <new block address, tagBlockPointer>
      header.head.CAS(head, tagExtending, next, next.tag)
      // queue is now unlocked
      preallocate block

Dequeue:
  repeat
      tail = header.tail.slot
      old_tag = header.tail.tag
      caughtTheHead := NextSlot(header.tail.slot) = header.head.slot;
      if tail.tail.CAS(tail, tagAllocated, tail, tagRemoving) then
          tail.tag = tagRemovings
          break
      else if header.tail.Tag = tagSentinel then
          if caughtTheHead then
              return false
          else if header.tail.CAS(tail, tagSentinel, tail, tagRemoving) then
              tail.tag = tagRemoving
              break
      else if header.tail.CAS(tail, tagBlockPointer, tail, tagDestrogin) then
          tail.tag = tagDestroying
          break
      else
          yield
  forever
  firstSlot = tail - tail.Offset // point to first slot
  if old_tag in [tagSentinel, tagAllocated] then
      next = NextSlot(tail)
      if tag = tagAllocated then
          fetch stored value
      if caughtTheHead then
          header.tail.CAS(tail, tagRemoving, tail, tagSentinel)
          firstSlot = nil // do not decrement the header counter
      else
          header.tail.CAS(tail, tagRemoving, next, next.tag)
  else
      next = tail.value // points to the next block's sentinel
      header.tail.CAS(tail, tagDestroying, next, tagSentinel)
      old_tag = tagSentinel // force retry
  // queue is now unlocked
  if assigned(firstSlot) and (InterlockedDecrement(firstSlot.value) = 0) then
      release block
  if old_tag = tagSentinel
      retry from beginning
*)

{$DEFINE USE_MOVEDPTR}
{$IFDEF DEBUG_OMNI_QUEUE}{$UNDEF USE_MOVEDPTR}{$ENDIF}
{$IFDEF OTL_OLDCPU}{$UNDEF USE_MOVEDPTR}{$ENDIF}
{$IFNDEF OTL_HaveCmpx16b}{$DEFINE USE_MOVEDPTR}{$ENDIF} //bus locking not supported, Move is just a move, so use it

{ TOmniTaggedValue }

function TOmniTaggedValue.CASTag(oldTag, newTag: TOmniQueueTag): boolean;
begin
  {$IFDEF OTL_HaveCmpx16b}
  Result := CAS8(Ord(oldTag), Ord(newTag), Tag);
  {$ELSE}
  Result := (Tag = oldTag);
  if Result then
    Tag := newTag;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniTaggedValue.CASTag }

{ TOmniTaggedPointer }

function TOmniTaggedPointer.CAS(
  oldSlot: POmniTaggedValue; oldTag: TOmniQueueTag;
  newSlot: POmniTaggedValue; newTag: TOmniQueueTag): boolean;
begin
  {$IFDEF OTL_HaveCmpx16b}
  Result := OtlSync.CAS(oldSlot, NativeInt(oldTag), newSlot, NativeInt(newTag), Self);
  {$ELSE}
  Result := (Slot = oldSlot) and (Tag = oldTag);
  if Result then begin
    Slot := newSlot;
    Tag := newTag;
  end;
  {$ENDIF ~OTL_HaveCmpx16b}
end; { TOmniTaggedPointer.CAS }

procedure TOmniTaggedPointer.Move(newSlot: POmniTaggedValue; newTag: TOmniQueueTag);
begin
  {$IFDEF OTL_HaveCmpx16b}
  MoveDPtr(newSlot, ord(newTag), Self);
  {$ELSE}
  Slot := newSlot;
  Tag := newTag;
  {$ENDIF OTL_HaveCmpx16b}
end; { TOmniTaggedPointer.Move }

{ TOmniBaseQueue }

constructor TOmniBaseQueue.Create(blockSize: integer; numCachedBlocks: integer);
var
  iMem  : integer;
  memory: POmniTaggedValue;
begin
  inherited Create;
  obcBlockSize := (((blockSize-1) div SizeOf(TOmniTaggedValue)) + 1) * SizeOf(TOmniTaggedValue);
  obcNumSlots := obcBlockSize div SizeOf(TOmniTaggedValue);
  if obcNumSlots > 65536 then
    raise Exception.CreateFmt('TOmniBaseQueue.Create: Maximum block size is %d',
            [65536 * SizeOf(TOmniTaggedValue)]);
  obcMemStack := TOmniBaseBoundedStack.Create;
  obcMemStack.Initialize(numCachedBlocks, SizeOf(pointer));
  for iMem := 1 to numCachedBlocks do begin
    memory := AllocMem(obcBlockSize);
    PartitionMemory(memory);
    Assert(obcMemStack.Push(memory));
  end;
  Initialize;
end; { TOmniBaseQueue.Create }

function TOmniBaseQueue.Dequeue: TOmniValue;
begin
  if not TryDequeue(Result) then
    raise Exception.Create('TOmniBaseQueue.Dequeue: Message queue is empty');
end; { TOmniBaseQueue.Dequeue }

destructor TOmniBaseQueue.Destroy;
var
  memory: pointer;
begin
  Cleanup;
  while obcMemStack.Pop(memory) do
    FreeMem(memory);
  FreeAndNil(obcMemStack);
  inherited;
end; { TOmniBaseQueue.Destroy }

procedure TOmniBaseQueue.Acquire; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obcLock.Acquire;
  {$ENDIF OTL_HaveCmpx16b}
end; { TOmniBaseQueue.Acquire }

function TOmniBaseQueue.NextSlot(slot: POmniTaggedValue): POmniTaggedValue; //inline
begin
  Result := slot;
  Inc(Result);
end; { TOmniBaseQueue.NextSlot }

procedure TOmniBaseQueue.Release; //inline
begin
  {$IFNDEF OTL_HaveCmpx16b}
  obcLock.Release;
  {$ENDIF OTL_HaveCmpx16b}
end; { TOmniBaseQueue.Release }

function TOmniBaseQueue.AllocateBlock: POmniTaggedValue;
begin
  if not obcMemStack.Pop(Result) then begin
    Result := AllocMem(obcBlockSize);
    PartitionMemory(Result);
  end;
end; { TOmniBaseQueue.AllocateBlock }

{$IFDEF DEBUG_OMNI_QUEUE}
procedure TOmniBaseQueue.Assert(condition: boolean);
begin
  if not condition then
    {$IFDEF CPUX64}AsmInt3;{$ELSE}asm int 3; end;{$ENDIF CPUX64}
end; { TOmniBaseQueue.Assert }
{$ENDIF DEBUG_OMNI_QUEUE}

procedure TOmniBaseQueue.Cleanup;
var
  pBlock: POmniTaggedValue;
  pSlot : POmniTaggedValue;
begin
  pSlot := obcTailPointer.Slot;
  while assigned(pSlot) do begin
    if pSlot.Tag in [tagBlockPointer, tagEndOfList] then begin
      pBlock := pSlot;
      pSlot := POmniTaggedValue(pSlot.Value.RawData^); //retrieveing Value.AsPointer raises exception
      Dec(pBlock, pBlock.Offset);
      ReleaseBlock(pBlock, true);
    end
    else begin
      if (pSlot.Tag = tagAllocated) then
        pSlot.Value._ReleaseAndClear;
      Inc(pSlot);
    end;
  end;
  if assigned(obcCachedBlock) then
    FreeMem(obcCachedBlock);
  FreeMem(obcTailPointer);
  FreeMem(obcHeadPointer);
end; { TOmniBaseQueue.Cleanup }

procedure TOmniBaseQueue.Enqueue(const value: TOmniValue);
var
  extension: POmniTaggedValue;
  next     : POmniTaggedValue;
  head     : POmniTaggedValue;
begin
  Acquire;
  try
    repeat
      head := obcHeadPointer.Slot;
      if (obcHeadPointer.Tag = tagFree)
         and obcHeadPointer.CAS(head, tagFree, head, tagAllocating)
      then
        break //repeat
      else if (obcHeadPointer.Tag = tagEndOfList)
              and obcHeadPointer.CAS(head, tagEndOfList, head, tagExtending)
      then
        break //repeat
      else // very temporary condition, retry quickly
        {$IFNDEF OTL_HaveCmpx16b}TThread.Yield;{$ELSE}{$IFDEF CPUX64}AsmPause;{$ELSE}asm pause; end;{$ENDIF}{$ENDIF}
    until false;
    {$IFDEF DEBUG_OMNI_QUEUE} Assert(head = obcHeadPointer.Slot); {$ENDIF}
    if obcHeadPointer.Tag = tagAllocating then begin // enqueueing
      next := NextSlot(head);
      head.Value := value; // this works because the slot was initialized to zero when allocating
      {$IFNDEF DEBUG_OMNI_QUEUE}
      head.Tag := tagAllocated; {$ELSE} Assert(head.CASTag(tagFree, tagAllocated)); {$ENDIF}
      {$IFDEF USE_MOVEDPTR} // release the lock
      obcHeadPointer.Move(next, next.Tag);
      {$ELSE}
      Assert(obcHeadPointer.CAS(head, tagAllocating, next, next.Tag));
      {$ENDIF}
    end
    else begin // allocating memory
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(obcHeadPointer.Tag = tagExtending); {$ENDIF}
      extension := AllocateBlock; // returns pointer to the header
      Inc(extension, 2);          // move over header and sentinel to the first data slot
      {$IFNDEF DEBUG_OMNI_QUEUE}
      extension.Tag := tagAllocated; {$ELSE} Assert(extension.CASTag(tagFree, tagAllocated)); {$ENDIF}
      extension.Value := value;   // this works because the slot was initialized to zero when allocating
      Dec(extension);             // forward reference points to the sentinel
      head.Value := extension;
      {$IFNDEF DEBUG_OMNI_QUEUE}
      head.Tag := tagBlockPointer; {$ELSE} Assert(head.CASTag(tagEndOfList, tagBlockPointer)); {$ENDIF}
      Inc(extension, 2); // get to the first free slot
      {$IFDEF USE_MOVEDPTR} // release the lock
      obcHeadPointer.Move(extension, extension.Tag);
      {$ELSE} Assert(obcHeadPointer.CAS(head, tagExtending, extension, extension.Tag)); {$ENDIF}
      PreallocateMemory;
    end;
  finally Release; end;
end; { TOmniBaseQueue.Enqueue }

procedure TOmniBaseQueue.Initialize;
begin
  if assigned(obcTailPointer) then
    FreeMem(obcTailPointer);
  obcTailPointer := AllocMem(SizeOf(TOmniTaggedPointer));
  if assigned(obcHeadPointer) then
    FreeMem(obcHeadPointer);
  obcHeadPointer := AllocMem(SizeOf(TOmniTaggedPointer));
  Assert(NativeInt(obcTailPointer) mod (2*SizeOf(pointer)) = 0);
  Assert(NativeInt(obcHeadPointer) mod (2*SizeOf(pointer)) = 0);
  Assert(NativeInt(@obcCachedBlock) mod SizeOf(pointer) = 0);
  obcTailPointer.Slot := NextSlot(AllocateBlock); // point to the sentinel
  obcTailPointer.Tag := obcTailPointer.Slot.Tag;
  {$IFDEF DEBUG_OMNI_QUEUE}
  Assert(obcTailPointer.Tag = tagSentinel);
  Assert(PrevSlot(obcTailPointer.Slot).Tag = tagHeader);
  {$ENDIF DEBUG_OMNI_QUEUE}
  obcHeadPointer.Slot := NextSlot(obcTailPointer.Slot);
  obcHeadPointer.Tag := obcHeadPointer.Slot.Tag;
  {$IFDEF DEBUG_OMNI_QUEUE}
  Assert(obcHeadPointer.Tag = tagFree);
  {$ENDIF DEBUG_OMNI_QUEUE}
  obcCachedBlock := AllocateBlock; // pre-allocate memory
end; { TOmniBaseQueue.Initialize }

function TOmniBaseQueue.IsEmpty: boolean;
var
  caughtTheHead: boolean;
  header       : POmniTaggedValue;
  next         : POmniTaggedValue;
  tag          : TOmniQueueTag;
  tail         : POmniTaggedValue;
begin
  Acquire;
  try
    // Basically the same as TryDequeue except that it doesn't dequeue anything.
    Result := false; // to keep compiler happy
    tag := tagSentinel;
    while tag = tagSentinel do begin
      repeat
        tail := obcTailPointer.Slot;
        caughtTheHead := NextSlot(obcTailPointer.Slot) = obcHeadPointer.Slot; // an approximation; we don't care if in a moment this won't be true anymore
        if (obcTailPointer.Tag = tagAllocated) then begin
          Result := false;
          Exit;
        end
        else if (obcTailPointer.Tag = tagSentinel) then begin
          if caughtTheHead then begin
            Result := true;
            Exit;
          end
          else if obcTailPointer.CAS(tail, tagSentinel, tail, tagRemoving) then begin
            tag := tagSentinel;
            break; //repeat
          end
        end
        else if (obcTailPointer.Tag = tagBlockPointer)
                and obcTailPointer.CAS(tail, tagBlockPointer, tail, tagDestroying) then
        begin
          tag := tagBlockPointer;
          break; //repeat
        end
        else
          {$IFNDEF OTL_HaveCmpx16b}TThread.Yield;{$ELSE}{$IFDEF CPUX64}AsmPause;{$ELSE}asm pause; end;{$ENDIF}{$ENDIF}
      until false;
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(tail = obcTailPointer.Slot); {$ENDIF}
      header := tail;
      Dec(header, header.Offset);
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(header.Tag = tagHeader); {$ENDIF}
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(tag <> tagAllocated); {$ENDIF}
      {$IFDEF DEBUG_OMNI_QUEUE} Assert(not caughtTheHead); {$ENDIF}
      if tag = tagSentinel then begin
        next := NextSlot(tail);
        {$IFDEF USE_MOVEDPTR} // release the lock
        obcTailPointer.Move(next, next.Tag);
        {$ELSE}
        Assert(obcTailPointer.CAS(tail, tagRemoving, next, next.Tag));
        {$ENDIF}
      end
      else begin // releasing memory
        {$IFDEF DEBUG_OMNI_QUEUE} Assert(tag = tagBlockPointer); {$ENDIF}
        next := POmniTaggedValue(tail.Value.AsPointer); // next points to the sentinel
        {$IFDEF DEBUG_OMNI_QUEUE} Assert(next.Tag = tagSentinel); {$ENDIF}
        {$IFDEF USE_MOVEDPTR} // release the lock
        obcTailPointer.Move(next, tagSentinel);
        {$ELSE}
        Assert(obcTailPointer.CAS(tail, tagDestroying, next, tagSentinel));
        {$ENDIF}
        tag := tagSentinel; // retry
      end;
      if assigned(header) and (TInterlockedEx.Decrement(PInteger(header)^) = 0) then
        ReleaseBlock(header);
    end; //while tag = tagSentinel
  finally Release; end;
end; { TOmniBaseQueue.IsEmpty }

procedure TOmniBaseQueue.PartitionMemory(memory: POmniTaggedValue);
var
  iSlot: integer;
begin
  Assert(Ord(tagFree) = 0);
  memory.Tag := tagHeader;
  PInteger(memory)^ := obcNumSlots - 1; // don't count the header
  Inc(memory);
  memory.Tag := tagSentinel;
  memory.Offset := 1;
  Inc(memory);
  for iSlot := 2 to obcNumSlots - 2 do begin
    memory.Offset := iSlot;
    Inc(memory);
  end;
  memory.Tag := tagEndOfList;
  memory.Offset := obcNumSlots - 1;
end; { TOmniBaseQueue.PartitionMemory }

procedure TOmniBaseQueue.PreallocateMemory;
var
  memory: POmniTaggedValue;
begin
  if obcMemStack.IsEmpty then begin
    memory := AllocMem(obcBlockSize);
    PartitionMemory(memory);
    if not obcMemStack.Push(memory) then
      FreeMem(memory);
  end;
end; { TOmniBaseQueue.PreallocateMemory }

function TOmniBaseQueue.PrevSlot(slot: POmniTaggedValue): POmniTaggedValue;
begin
  Result := slot;
  Dec(Result);
end; { TOmniBaseQueue.PrevSlot }

procedure TOmniBaseQueue.ReleaseBlock(firstSlot: POmniTaggedValue; forceFree: boolean);
begin
  {$IFDEF DEBUG_OMNI_QUEUE}Assert(firstSlot.Tag = tagHeader);{$ENDIF DEBUG_OMNI_QUEUE}
  if forceFree or obcMemStack.IsFull then
    FreeMem(firstSlot)
  else begin
    FillChar(firstSlot^, obcBlockSize, 0);
    PartitionMemory(firstSlot);
    if not obcMemStack.Push(firstSlot) then
      FreeMem(firstSlot);
  end;
end; { TOmniBaseQueue.ReleaseBlock }

function TOmniBaseQueue.TryDequeue(var value: TOmniValue): boolean;
var
  caughtTheHead: boolean;
  tail         : POmniTaggedValue;
  header       : POmniTaggedValue;
  next         : POmniTaggedValue;
  tag          : TOmniQueueTag;
begin
  Acquire;
  try
    tag := tagSentinel;
    Result := true;
    while Result and (tag = tagSentinel) do begin
      repeat
        tail := obcTailPointer.Slot;
        caughtTheHead := NextSlot(obcTailPointer.Slot) = obcHeadPointer.Slot; // an approximation; we don't care if in a moment this won't be true anymore
        if (obcTailPointer.Tag = tagAllocated)
           and obcTailPointer.CAS(tail, tagAllocated, tail, tagRemoving) then
        begin
          tag := tagAllocated;
          break; //repeat
        end
        else if (obcTailPointer.Tag = tagSentinel) then begin
          if caughtTheHead then begin
            Result := false;
            break; //repeat
          end
          else if obcTailPointer.CAS(tail, tagSentinel, tail, tagRemoving) then begin
            tag := tagSentinel;
            break; //repeat
          end
        end
        else if (obcTailPointer.Tag = tagBlockPointer)
                and obcTailPointer.CAS(tail, tagBlockPointer, tail, tagDestroying) then
        begin
          tag := tagBlockPointer;
          break; //repeat
        end
        else
          {$IFDEF CPUX64}AsmPause;{$ELSE}asm pause; end;{$ENDIF ~CPUX64}
      until false;
      if Result then begin // dequeueing
        {$IFDEF DEBUG_OMNI_QUEUE} Assert(tail = obcTailPointer.Slot); {$ENDIF}
        header := tail;
        Dec(header, header.Offset);
        {$IFDEF DEBUG_OMNI_QUEUE} Assert(header.Tag = tagHeader); {$ENDIF}
        if tag in [tagSentinel, tagAllocated] then begin
          next := NextSlot(tail);
          if tag = tagAllocated then begin // sentinel doesn't contain any useful value
            value := tail.Value;
            tail.Value._ReleaseAndClear;
          end;
          if caughtTheHead then begin
            {$IFDEF USE_MOVEDPTR} // release the lock; as this is the last element, don't move forward
            obcTailPointer.Move(tail, tagSentinel);
            {$ELSE}
            Assert(obcTailPointer.CAS(tail, tagRemoving, tail, tagSentinel));
            {$ENDIF}
            header := nil; // do NOT decrement the counter; this slot will be retagged again
          end
          else
            {$IFDEF USE_MOVEDPTR} // release the lock
            obcTailPointer.Move(next, next.Tag);
            {$ELSE} Assert(obcTailPointer.CAS(tail, tagRemoving, next, next.Tag));
            {$ENDIF}
        end
        else begin // releasing memory
          {$IFDEF DEBUG_OMNI_QUEUE} Assert(tag = tagBlockPointer); {$ENDIF}
          next := POmniTaggedValue(tail.Value.AsPointer); // next points to the sentinel
          {$IFDEF DEBUG_OMNI_QUEUE} Assert(next.Tag = tagSentinel); {$ENDIF}
          {$IFDEF USE_MOVEDPTR} // release the lock
          obcTailPointer.Move(next, tagSentinel); {$ELSE} Assert(obcTailPointer.CAS(tail, tagDestroying, next, tagSentinel)); {$ENDIF}
          tag := tagSentinel; // retry
        end;
        if assigned(header) and (TInterlockedEx.Decrement(PInteger(header)^) = 0) then
          ReleaseBlock(header);
      end;
    end; //while Result and (tag = tagSentinel)
  finally Release; end;
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

procedure TOmniQueue.Cleanup;
begin
  FreeAndNil(ocContainerSubject);
  inherited;
end; { TOmniQueue.Cleanup }

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

procedure TOmniQueue.Initialize;
begin
  inherited;
  ocContainerSubject := TOmniContainerSubject.Create;
end; { TOmniQueue.Initialize }

function TOmniQueue.TryDequeue(var value: TOmniValue): boolean;
begin
  Result := inherited TryDequeue(value);
  if Result then
    ContainerSubject.Notify(coiNotifyOnAllRemoves);
end; { TOmniQueue.TryDequeue }

{$IFDEF OTL_MobileSupport}

{ TOmniValueQueue }

constructor TOmniValueQueue.Create(AThresholdForFull: integer);
begin
  FContainerSubject := TOmniContainerSubject.Create;;
  FInnerQueue := TQueue<TOmniValue>.Create;
  FInnerQueue.OnNotify := CollectionNotifyEvent;
  FFullThreshold := AThresholdForFull;
  if FFullThreshold > 0 then begin
    FPartlyEmptyThreshold := Round(FFullThreshold * CPartlyEmptyLoadFactor);
    FAlmostFullThreshold  := Round(FFullThreshold * CAlmostFullLoadFactor);
    if FPartlyEmptyThreshold = FAlmostFullThreshold then
      Inc(FAlmostFullThreshold);
  end
  else begin
    FPartlyEmptyThreshold := -1;
    FAlmostFullThreshold  := -1
  end;
  FNotifiableEvents := [];
end; { TOmniValueQueue.Create }

destructor TOmniValueQueue.Destroy;
begin
  FreeAndNil(FContainerSubject);
  FreeAndNil(FInnerQueue);
  inherited
end; { TOmniValueQueue.Destroy }

procedure TOmniValueQueue.CollectionNotifyEvent(Sender: TObject;
  const Item: TOmniValue; Action: TCollectionNotification);
var
  AfterCount: integer;
begin
  // This method occurs within the critical section.
  AfterCount := FInnerQueue.Count;
  case Action of
    cnAdded:
      begin
        Include(FNotifiableEvents, coiNotifyOnAllInserts);
        if AfterCount = FAlmostFullThreshold then
          Include(FNotifiableEvents, coiNotifyOnAlmostFull);
      end;
    cnRemoved,
    cnExtracted:
      begin
        Include(FNotifiableEvents, coiNotifyOnAllRemoves);
        if AfterCount = FAlmostFullThreshold then
          Include(FNotifiableEvents, coiNotifyOnPartlyEmpty);
      end;
  end; //case Action
end; { TOmniValueQueue.CollectionNotifyEvent }

procedure TOmniValueQueue.DoWithCritSec(Proc: TProc);
var
  PickUp: TInterestSet;
begin
  EnterCriticalSection;
  try
    FNotifiableEvents := [];
    Proc;
    PickUp := FNotifiableEvents;
    FNotifiableEvents := []
  finally EnterCriticalSection; end;
  PropagateNotifications(PickUp);
end; { TOmniValueQueue.DoWithCritSec }

function TOmniValueQueue.Dequeue: TOmniValue;
var
  EnclosedResult: TOmniValue;
begin
  DoWithCritSec(
    procedure
    begin
      if FInnerQueue.Count > 0 then
        EnclosedResult := FInnerQueue.Dequeue
      else
        raise Exception.Create('TOmniBaseQueue.Dequeue: Message queue is empty');
    end);
  Result := EnclosedResult;
end; { TOmniValueQueue.Dequeue }

procedure TOmniValueQueue.Enqueue(const value: TOmniValue);
var
  LocalCopy: TOmniValue;
begin
  LocalCopy := value;
  DoWithCritSec(
    procedure
    begin
      FInnerQueue.Enqueue(LocalCopy);
    end);
end; { TOmniValueQueue.Enqueue }

function TOmniValueQueue.GetContainerSubject: TOmniContainerSubject;
begin
  Result := FContainerSubject;
end; { TOmniValueQueue.GetContainerSubject }

function TOmniValueQueue.IsEmpty: boolean;
begin
  EnterCriticalSection;
  Result := FInnerQueue.Count = 0;
  LeaveCriticalSection;
end; { TOmniValueQueue.IsEmpty }

procedure TOmniValueQueue.PropagateNotifications(Events: TInterestSet);
var
  Ev: TOmniContainerObserverInterest;
begin
  if assigned(FContainerSubject) and (Events <> []) then
    for Ev := Low(TOmniContainerObserverInterest) to Low(TOmniContainerObserverInterest) do
      if Ev in Events then
        FContainerSubject.Notify(Ev);
end; { TOmniValueQueue.PropagateNotifications }

function TOmniValueQueue.TryDequeue(var value: TOmniValue): boolean;
var
  EnclosedValue : TOmniValue;
  EnclosedResult: boolean;
begin
  DoWithCritSec(
    procedure
    begin
      EnclosedResult := FInnerQueue.Count > 0;
      if EnclosedResult then
        EnclosedValue := FInnerQueue.Dequeue;
    end);
  value  := EnclosedValue;
  Result := EnclosedResult;
end; { TOmniValueQueue.TryDequeue }

{ TOmniValueQueueSpin }

constructor TOmniValueQueueSpin.Create(AThresholdForFull: integer);
begin
  inherited Create(AThresholdForFull);
  FLock.Create(True);
end; { TOmniValueQueueSpin.Create }

procedure TOmniValueQueueSpin.EnterCriticalSection;
begin
  FLock.Enter;
end; { TOmniValueQueueSpin.EnterCriticalSection }

procedure TOmniValueQueueSpin.LeaveCriticalSection;
begin
  FLock.Exit(True);
end; { TOmniValueQueueSpin.LeaveCriticalSection }

{ TOmniValueQueueCS }

constructor TOmniValueQueueCS.Create(AThresholdForFull: integer);
begin
  inherited Create(AThresholdForFull);
  FCritSect := TFixedCriticalSection.Create;
end; { TOmniValueQueueCS.Create }

destructor TOmniValueQueueCS.Destroy;
begin
  FCritSect.Free;
  inherited;
end; { TOmniValueQueueCS.Destroy }

procedure TOmniValueQueueCS.EnterCriticalSection;
begin
  FCritSect.Enter;
end; { TOmniValueQueueCS.EnterCriticalSection }

procedure TOmniValueQueueCS.LeaveCriticalSection;
begin
  FCritSect.Leave;
end; { TOmniValueQueueCS.LeaveCriticalSection }

{$ENDIF OTL_MobileSupport}

initialization
  Assert(SizeOf(pointer) = SizeOf(NativeInt));
  {$IFDEF OTL_HaveCmpx16b}
  Assert(SizeOf(TOmniTaggedValue) = {$IFDEF CPUX64}3{$ELSE}4{$ENDIF}*SizeOf(pointer));
  Assert(SizeOf(TOmniTaggedPointer) = 2*SizeOf(pointer));
  InitializeTimingInfo;
  {$ENDIF OTL_HaveCmpx16b}
end.

