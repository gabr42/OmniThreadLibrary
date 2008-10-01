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
///   Version           : 0.3
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

//{$DEFINE PurePascal}

//{$DEFINE SimpleStack}

//{$DEFINE StackStaticReference}

{$DEFINE PureQueue}

//{$DEFINE QueueStaticReference}

unit OtlContainers;

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

  POmniLinkedData = ^TOmniLinkedData;
  TOmniLinkedData = packed record
    Next: POmniLinkedData;
    Data: record end; //user data, variable size
  end; { TLinkedOmniData }

  TReferencedPtr = record
    PData             : pointer;
    Reference         : cardinal;
  end; { TReferencedPtr }

  TOmniChain = packed record
    Head                : TReferencedPtr;
{$IFNDEF SimpleStack}
    TaskPopUnderflow    : cardinal;
    TaskPushUnderflow   : cardinal;
{$ENDIF SimpleStack}
  end; { TOmniChain }

  TOmniBaseContainer = class abstract(TInterfacedObject)
  strict protected
    obcBuffer      : pointer;
    obcElementSize : integer;
    obcNumElements : integer;
    obcPublicChain : TOmniChain;
    obcRecycleChain: TOmniChain;
    class function  InvertOrder(chainHead: POmniLinkedData): POmniLinkedData; static;
    class function  PopLink(var chain: TOmniChain): POmniLinkedData; static;
    class procedure PushLink(const link: POmniLinkedData; var chain: TOmniChain); static;
    class function  UnlinkAll(var chain: TOmniChain): POmniLinkedData; static;
  public
    destructor  Destroy; override;
    procedure Empty; virtual;
    procedure Initialize(numElements, elementSize: integer); virtual;
    function  IsEmpty: boolean; virtual;
    function  IsFull: boolean; virtual;
    property ElementSize: integer read obcElementSize;
    property NumElements: integer read obcNumElements;
  end; { TOmniBaseContainer }

  TOmniBaseStack = class(TOmniBaseContainer)
  public
    function  Pop(var value): boolean; virtual;
    function  Push(const value): boolean; virtual;
  end; { TOmniBaseStack }

  TOmniContainerOption = (coEnableMonitor, coEnableNotify);
  TOmniContainerOptions = set of TOmniContainerOption;

  TOmniStack = class(TOmniBaseStack, IOmniStack, IOmniNotifySupport, IOmniMonitorSupport)
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
    property Options: TOmniContainerOptions read osOptions;
  end; { TOmniStack }

{$IFDEF PureQueue}

  PReferencedPtr = ^TReferencedPtr;

  TReferencedPtrBuffer = array [0..1 shl 27] of TReferencedPtr;

  TOmniRingBuffer  = packed record
    FirstIn             : TReferencedPtr;
    LastIn              : TReferencedPtr;
    StartBuffer         : pointer;
    EndBuffer           : pointer;
    TaskPopUnderflow    : cardinal;
    TaskPushUnderflow   : cardinal;
    Buffer              : TReferencedPtrBuffer;
  end; { TOmniRingBuffer }

  POmniRingBuffer = ^TOmniRingBuffer;

  TOmniBaseQueue = class(TInterfacedObject)
  strict protected
    obcPublicRingBuffer  : POmniRingBuffer;
    obcRecycleRingBuffer : POmniRingBuffer;
    obcNumElements       : integer;
    obcElementSize       : Integer;
    obcDataBuffer        : Pointer;
    class function Pop(const ringBuffer: POmniRingBuffer): pointer; static;
    class procedure Push(const data: pointer; const ringBuffer: POmniRingBuffer); static;
  public
    destructor  Destroy; override;
    procedure Empty; virtual;
    procedure Initialize(numElements, elementSize: integer); virtual;
    function  IsEmpty: boolean; virtual;
    function  IsFull: boolean; virtual;
    property ElementSize: integer read obcElementSize;
    property NumElements: integer read obcNumElements;
    function  Dequeue(var value): boolean; virtual;
    function  Enqueue(const value): boolean; virtual;
  end; { TOmniBaseQueue }

{$ELSE PureQueue}

  TOmniBaseQueue = class(TOmniBaseContainer)
  strict protected
    obqDequeuedMessages: TOmniChain;
  public
    constructor Create;
    function  Dequeue(var value): boolean; virtual;
    procedure Empty; override;
    function  Enqueue(const value): boolean; virtual;
    function  IsEmpty: boolean; override;
  end; { TOmniBaseQueue }

{$ENDIF PureQueue}

  TOmniQueue = class(TOmniBaseQueue, IOmniQueue, IOmniNotifySupport, IOmniMonitorSupport)
  strict private
    orbMonitorSupport: IOmniMonitorSupport;
    orbNotifySupport : IOmniNotifySupport;
    orbOptions       : TOmniContainerOptions;
  public
    constructor Create(numElements, elementSize: integer;
      options: TOmniContainerOptions = [coEnableMonitor, coEnableNotify]);
    function  Dequeue(var value): boolean; override;
    function  Enqueue(const value): boolean; override;
    property MonitorSupport: IOmniMonitorSupport read orbMonitorSupport implements IOmniMonitorSupport;
    property NotifySupport: IOmniNotifySupport read orbNotifySupport implements IOmniNotifySupport;
    property Options: TOmniContainerOptions read orbOptions;
  end; { TOmniQueue }

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

type
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

{ Intel Atomic functions support }

function AtomicCmpXchg4b(const Old4b, New4b: cardinal; var Destination: cardinal): boolean;
asm
  lock cmpxchg dword ptr [Destination], New4b
  setz  al
end;

procedure AtomicAppendLink(const link: POmniLinkedData; Head: TReferencedPtr);
asm
  mov   ecx, eax
  mov   eax, [edx].TReferencedPtr.PData                         //eax = OldLink
@Spin:
  mov   [ecx].TOmniLinkedData.Next, eax                         //link.Next := OldLink
  lock cmpxchg [edx].TOmniChain.Head.PData, ecx                 //chain.Head.PData := link
  jnz   @Spin
end;

function AtomicUnlinkAll(var chain: TOmniChain): POmniLinkedData;
asm
  xor   ecx, ecx
  mov   edx, eax
  mov   eax, [edx].TOmniChain.Head.PData
@Spin:
  lock cmpxchg [edx].TOmniChain.Head.PData, ecx                 //Cut Chain.Head
  jnz   @Spin
end; { TOmniQueue.UnlinkAll }

function AtomicIncrement(var Destination: cardinal; const Count: cardinal = 1): cardinal;
asm
  mov   ecx, edx
  lock xadd [eax], edx
  lea   eax, edx + ecx
end;

function AtomicCmpXchg8b(const OldLow4b: pointer; OldHigh4b: cardinal; NewLow4b: pointer; NewHigh4b : cardinal;
  var Destination: TReferencedPtr): boolean; overload;
//try atomic DestinationHigh4b, DestinationLow4b := NewLow4b, NewHigh4b
asm
  push  edi
  push  ebx
  mov   ebx, ecx
  mov   ecx, NewHigh4b
  mov   edi, Destination
  lock cmpxchg8b qword ptr [edi]
  setz  al
  pop   ebx
  pop   edi
end;

function GetThreadId: cardinal;
asm
  mov   eax, fs:[$18]                                           //eax := thread information block
  mov   eax, [eax + $24]                                        //eax := thread id
end;

function GetTimeStamp: int64;
asm
  rdtsc
end;

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
end; { TOmniBaseStack.Destroy }

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
end; { TOmniBaseStack.Empty }

procedure TOmniBaseContainer.Initialize(numElements, elementSize: integer);
const
  NumOfSamples = 10;

var
  bufferElementSize: cardinal;
  currElement      : POmniLinkedData;
  iElement         : integer;
  nextElement      : POmniLinkedData;

{$IFNDEF SimpleStack}

  TimeTestField    : array [0..2]of array [1..NumOfSamples] of int64;
  n                : integer;

  function GetMinAndClear(Rutine: cardinal): int64;
  var
    n: integer;
    x: integer;
  begin
    x:= 1;
    result := MaxLongInt;
    for n:= 1 to NumOfSamples do
    begin
      if TimeTestField[Rutine, n] < result then
      begin
        result := TimeTestField[Rutine, n];
        x := n;
      end;
    end;
    TimeTestField[Rutine, x] := MaxLongInt;
  end;

{$ENDIF SimpleStack}

begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  if cardinal(@obcPublicChain) AND 7 <> 0 then
    raise Exception.Create('TOmniBaseContainer: obcPublicChain is not 8-aligned');
  if cardinal(@obcRecycleChain) AND 7 <> 0 then
    raise Exception.Create('TOmniBaseContainer: obcRecycleChain is not 8-aligned');
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obcNumElements := numElements;
  obcElementSize := elementSize;
  // calculate element size, round up to next 4-aligned value
  bufferElementSize := ((SizeOf(TOmniLinkedData) + elementSize) + 3) AND NOT 3;
  GetMem(obcBuffer, bufferElementSize * cardinal(numElements));
  if cardinal(obcBuffer) AND 3 <> 0 then
    raise Exception.Create('TOmniBaseContainer: obcBuffer is not 4-aligned');
  //Format buffer to recycleChain, init orbRecycleChain and orbPublicChain.
  //At the beginning, all elements are linked into the recycle chain.
  obcRecycleChain.Head.PData := obcBuffer;
  currElement := obcRecycleChain.Head.PData;
  for iElement := 0 to obcNumElements - 2 do begin
    nextElement := POmniLinkedData(cardinal(currElement) + bufferElementSize);
    currElement.Next := nextElement;
    currElement := nextElement;
  end;
  currElement.Next := nil; // terminate the chain
  obcPublicChain.Head.PData := nil;

{$IFNDEF SimpleStack}
  //Calcolate  TaskPopUnderflow and TaskPushUnderflow counter values depend on CPU speed!!!}
  obcPublicChain.TaskPopUnderflow := 1;
  obcPublicChain.TaskPushUnderflow := 1;
  obcRecycleChain.TaskPopUnderflow := 1;
  obcRecycleChain.TaskPushUnderflow := 1;
  for n := 1 to NumOfSamples do
  begin
    //Measure PopLink rutine delay
    TimeTestField[0, n] := GetTimeStamp;
    currElement := PopLink(obcRecycleChain);
    TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
    //Measure PushLink rutine delay
    TimeTestField[1, n] := GetTimeStamp;
    PushLink(currElement, obcRecycleChain);
    TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
    //Measure GetTimeStamp rutine delay
    TimeTestField[2, n] := GetTimeStamp;
    TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
  end;
  //Calcolate first 4 minimum average for GetTimeStamp
  n := ( GetMinAndClear(2) + GetMinAndClear(2) +
    GetMinAndClear(2) + GetMinAndClear(2));
  //Calcolate first 4 minimum average for PopLink rutine * 2
  obcRecycleChain.TaskPopUnderflow := ( GetMinAndClear(0) + GetMinAndClear(0) +
    GetMinAndClear(0) + GetMinAndClear(0) - n) div 4;
  obcPublicChain.TaskPopUnderflow := obcRecycleChain.TaskPopUnderflow;
  //Calcolate first 4 minimum average for PusLink rutine * 2
  obcRecycleChain.TaskPushUnderflow := ( GetMinAndClear(1) + GetMinAndClear(1) +
    GetMinAndClear(1) + GetMinAndClear(1) - n) div 1;
  obcPublicChain.TaskPushUnderflow := obcRecycleChain.TaskPushUnderflow;

{$ENDIF SimpleStack}
end; { TOmniBaseStack.Initialize }

///<summary>Invert links in a chain.</summary>
///<returns>New chain head (previous tail) or nil if chain is empty.</returns>
///<since>2008-07-13</since>
class function TOmniBaseContainer.InvertOrder(chainHead: POmniLinkedData): POmniLinkedData;
asm
  test  eax, eax
  jz    @Exit
  xor   ecx, ecx
@Walk:
  xchg  [eax].TOmniLinkedData.Next, ecx                         //Turn links
  and   ecx, ecx
  jz    @Exit
  xchg  [ecx].TOmniLinkedData.Next, eax
  and   eax, eax
  jnz   @Walk
  mov   eax, ecx
@Exit:
end; { TOmniBaseStack.InvertOrder }

function TOmniBaseContainer.IsEmpty: boolean;
begin
  Result := not assigned(obcPublicChain.Head.PData);
end; { TOmniBaseStack.IsEmpty }

function TOmniBaseContainer.IsFull: boolean;
begin
  Result := not assigned(obcRecycleChain.Head.PData);
end; { TOmniBaseStack.IsFull }

///<summary>Removes first element from the chain, atomically.</summary>
///<returns>Removed first element. If the chain is empty, returns nil.</returns>
class function TOmniBaseContainer.PopLink(var chain: TOmniChain): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                         ^------ < chainHead

{$IFDEF SimpleStack} //Old Stack version

  {$IFDEF PurePascal}

//PopLink basic version
var
  NewReference          :cardinal;

label
  TryAgain;

begin
TryAgain:
  NewReference := AtomicIncrement(Chain.Head.Reference);
  result := Chain.Head.PData;
  if result = nil then
    exit;
  if not AtomicCmpxchg8b(result, NewReference, result.Next , NewReference , Chain.Head) then
    goto TryAgain;
end;

  {$ELSE PurePascal}

//PopLink basic version
asm
  push  edi
  push  ebx
  mov   edi, eax                                                //edi = @chain
@Spin:
  mov   ecx, 1                                                  //Increment Reference for 1
  lock xadd [edi].TOmniChain.Head.Reference, ecx                //ecx := old Reference
  mov   eax, [edi].TOmniChain.Head.PData                        //eax := chain.Head.Head
  test  eax, eax
  jz    @Exit                                                   //Is Empty?
  inc   ecx                                                     //ecx := current Reference
  mov   edx, ecx
  mov   ebx, [eax].TOmniLinkedData.Next                         //ebx := chain.Head.Next
  lock cmpxchg8b qword ptr [edi].TOmniChain.Head                //Now try to xchg
  jnz   @Spin                                                   //Do spin ???
@Exit:
  pop   ebx
  pop   edi
end; { TOmniBaseContainer.PopLink }

  {$ENDIF PurePascal}

{$ELSE SimpleStack}

  {$IFDEF PurePascal}

//PopLink with Static/Dinamic reference and Reference.Bit0
var
  TaskPopUnderflowCounter       :cardinal;
  NewReference                  :cardinal;
  CurrentReference              :cardinal;
  AtStartReference              :cardinal;

label
  TryAgain;

begin
{$IFDEF StackStaticReference}
  NewReference := GetThreadId + 1; //Reference.bit0 := 1
{$ENDIF}
  with chain do
  begin
TryAgain:
    TaskPopUnderflowCounter := TaskPopUnderflow;
    AtStartReference := Head.Reference OR 1; //Reference.bit0 := 1
    repeat
      CurrentReference := Head.Reference;
      dec(TaskPopUnderflowCounter);
    until (TaskPopUnderflowCounter = 0) or (Byte(CurrentReference) AND 1 = 0);
    if (Byte(CurrentReference) AND 1 <> 0) and (AtStartReference <> CurrentReference) then
      goto TryAgain;
{$IFNDEF StackStaticReference}
    NewReference := CurrentReference OR 1 + 2; //Reference.bit0 := 1
{$ENDIF}
    if not AtomicCmpxchg4b(CurrentReference, NewReference, Head.Reference) then
      goto TryAgain;
    //NewReferenceSet
    result := Chain.Head.PData;
    if result = nil then
    begin
      //Clear Reference.bit0 if task own Reference
      AtomicCmpxchg4b(NewReference, NewReference - 1, Head.Reference);
      exit;
    end;
    if not AtomicCmpxchg8b(result, NewReference, result.Next , NewReference - 1, Chain.Head) then
      goto TryAgain;
  end;
end; { TOmniBaseContainer.PopLink }

 {$ELSE PurePascal}

//PopLink with Static/Dinamic reference and Reference.Bit0
asm
  push  edi                                                     //Store system registers
  push  ebx
  mov   edi, eax                                                //edi := TOmniChain
{$IFDEF StackStaticReference}
  mov   edx, fs:[$18]                                           //edx := thread information block
  mov   edx, [edx + $24]                                        //edx := thread id as Reference
  inc   edx                                                     //edx := Reference, Reference.bit0 := 1
{$ENDIF StackStaticReference}
@TryAgain:
  mov   ecx, [edi].TOmniChain.TaskPopUnderflow                  //Set loop counter to define task underflow
  mov   ebx, [edi].TOmniChain.Head.Reference                    //ebx := start current Reference
@Spin:
  mov   eax, [edi].TOmniChain.Head.Reference                    //eax := current Reference
  test  al, 1                                                   //Test Reference.bit0
  loopnz @Spin                                                  //Suffer boy... (loopnz is double condition jump)
  jnz   @TryToOverrideReference                                 //Task time underflow?
                                                                //Stack is idle now
{$IFNDEF StackStaticReference}
  lea   edx, eax + 3                                            //Calcolate new Reference value, Reference.bit0 = 1
{$ENDIF StackStaticReference}
  lock cmpxchg [edi].TOmniChain.Head.Reference, edx             //Try to set new Reference...
  jnz   @TryAgain                                               //...too late?
@NewReferenceSet:
  mov   eax, [edi].TOmniChain.Head.PData                        //eax := chain.Head.PData
  test  eax, eax
  jz    @Exit                                                   //Is Empty?
  mov   ebx, [eax].TOmniLinkedData.Next                         //ebx := Next
  lea   ecx, edx - 1                                            //ecx := Reference, Reference.bit0 = 0
  lock cmpxchg8b qword ptr[edi].TOmniChain.Head                 //Try to set new chain next address
{$IFDEF StackStaticReference}
  lea   edx, ecx + 1                                            //edx := Reference, Reference.bit0 = 1
{$ENDIF StackStaticReference}
  jnz   @TryAgain                                               //...no go?
  pop   ebx                                                     //Restore sytem registers
  pop   edi
  ret                                                           //...finish :)

@TryToOverrideReference:
  or    bl, 1                                                   //Force start Reference.bit0 = 1
  cmp   ebx, eax                                                //Is Reference still the same ?...
  jnz   @TryAgain                                               //...queue task has new owner
                                                                //...working task Reference counter underflow
{$IFNDEF StackStaticReference}
  lea   edx, eax + 2                                            //Calcolate new Reference value, Reference.bit0 = 1
{$ENDIF}
  lock cmpxchg [edi].TOmniChain.Head.Reference, edx             //Try to override current Reference value...
  jz    @NewReferenceSet                                        //...yes
  jmp   @TryAgain                                               //...no go

@Exit:
  mov   eax, edx                                                //eax := Reference
  dec   edx                                                     //edx := Reference, Reference.bit0 = 0
  lock cmpxchg [edi].TOmniChain.Head.Reference, edx             //Clear Reference.bit0 if task own Reference
  xor   eax, eax
  pop   ebx                                                     //Restore sytem registers
  pop   edi
  ret
end; { TOmniBaseContainer.PopLink }

  {$ENDIF PurePascal}

{$ENDIF SimpleStack}


///<summary>Inserts element at the beginning of the chain, atomically.</summary>
class procedure TOmniBaseContainer.PushLink(const link: POmniLinkedData; var chain:
  TOmniChain);
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                        ^------ < chainHead

{$IFDEF SimpleStack}

  {$IFDEF PurePascal}

//PushLink basic version
begin
  AtomicAppendLink(link, chain.Head);
end; { TOmniBaseContainer.PushLink }

  {$ELSE PurePascal}

//PushLink basic version
asm
  mov   ecx, eax
  mov   eax, [edx].TOmniChain.Head.PData                        //eax = chain.Head.PData
@Spin:
  mov   [ecx].TOmniLinkedData.Next, eax                         //link.Next := chain.Head.PData
  lock cmpxchg [edx].TOmniChain.Head.PData, ecx                 //chain.Head.PData := link
  jnz   @Spin
end; { TOmniBaseContainer.PushLink }

  {$ENDIF}

{$ELSE SimpleStack}

  {$IFDEF PurePascal}

//PushLink with Static/Dinamic reference and Reference.Bit0
var
  TaskPushUnderflowCounter       : cardinal;

begin
  with chain do
  begin
    TaskPushUnderflowCounter := TaskPushUnderflow;
    repeat
      dec(TaskPushUnderflowCounter);
    until (TaskPushUnderflowCounter = 0) or (Byte(Head.Reference) AND 1 = 0);
    AtomicAppendLink(link, Head);
  end;
end; { TOmniBaseContainer.PushLink }

  {$ELSE PurePascal}

//PushLink with Static/Dinamic reference and Reference.Bit0
asm
  mov   ecx, [edx].TOmniChain.TaskPushUnderflow                 //Set loop counter to define task underflow
@Wait:
  test   byte ptr[edx].TOmniChain.Head.Reference, 1             //If Reference.bit0 = 0 then skip
  loopnz @Wait                                                  //Suffer boy... (loopnz is double condition jump)
  mov   ecx, eax
  mov   eax, [edx].TOmniChain.Head.PData                        //eax = chain.Head
@Spin:
  mov   [ecx].TOmniLinkedData.Next, eax                         //link.Next := chain.Head
  lock cmpxchg [edx].TOmniChain.Head.PData, ecx                 //chain.Head := link
  jnz   @Spin
end; { TOmniBaseContainer.PushLink }

{$ENDIF PurePascal}

{$ENDIF SimpleStack}

///<summary>Removes all elements from a chain, atomically.</summary>
///<returns>Head of the chain.</returns>
///<since>2008-07-13</since>
class function TOmniBaseContainer.UnlinkAll(var chain: TOmniChain): POmniLinkedData;
//nil << Link.Next << Link.Next << ... << Link.Next
//FILO buffer logic                        ^------ < chain.Head

{$IFDEF PurePascal}

begin
  result := AtomicUnlinkAll(chain);
end;

{$ELSE PurePascal}

asm
  xor   ecx, ecx
  mov   edx, eax
  mov   eax, [edx].TOmniChain.Head.PData
@Spin:
  lock cmpxchg [edx].TOmniChain.Head.PData, ecx                 //Cut Chain.Head
  jnz   @Spin
end; { TOmniQueue.UnlinkAll }

{$ENDIF PurePascal}

{ TOmniBaseStack }

function TOmniBaseStack.Pop(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obcPublicChain);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obcRecycleChain);
end; { TOmniBaseStack.Pop }

function TOmniBaseStack.Push(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obcRecycleChain);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  PushLink(linkedData, obcPublicChain);
end; { TOmniBaseStack.Push }

{ TOmniStack }

constructor TOmniStack.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  osOptions := options;
  if coEnableMonitor in Options then
    osMonitorSupport := CreateOmniMonitorSupport;
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

{$IFDEF PureQueue}

{ TOmniBaseQueue }

class function TOmniBaseQueue.Pop(const ringBuffer: POmniRingBuffer): pointer;
//REMEMBER: Reference.bit0 is queue idle/busy status and is not lock bit!

{$IFDEF PurePascal}

var
  TaskPopUnderflowCounter       :cardinal;
  NewReference                  :cardinal;
  CurrentReference              :cardinal;
  AtStartReference              :cardinal;
  NewFirstIn                    :pointer;
  OldFirstIn                    :pointer;

label
  TryAgain;

//Queue Pop with Static/Dinamic reference and Reference.Bit0
begin
{$IFDEF QueueStaticReference}
  NewReference := GetThreadId + 1; //Reference.bit0 := 1
{$ENDIF}
  with ringBuffer^ do
  begin
TryAgain:
    TaskPopUnderflowCounter := TaskPopUnderflow;
    AtStartReference := FirstIn.Reference OR 1; //...Reference.bit0 := 1
    repeat
      CurrentReference := FirstIn.Reference;
      dec(TaskPopUnderflowCounter);
    until (TaskPopUnderflowCounter = 0) or (Byte(CurrentReference) AND 1 = 0);
    if (Byte(CurrentReference) AND 1 <> 0) and (AtStartReference <> CurrentReference) then
      goto TryAgain;
{$IFNDEF QueueStaticReference}
    NewReference := CurrentReference OR 1 + 2; //...Reference.bit0 := 1
{$ENDIF}
    if not AtomicCmpxchg4b(CurrentReference, NewReference, FirstIn.Reference) then
      goto TryAgain;
    //NewReferenceSet
    OldFirstIn := FirstIn.PData;
    //Empty test
    if OldFirstIn = LastIn.PData then
    begin
      //Clear Reference.bit0 if task own Reference
      AtomicCmpxchg4b(NewReference, NewReference - 1, FirstIn.Reference);
      result := nil;
      exit;
    end;
    //Load result
    result := PReferencedPtr(FirstIn.PData).PData;
    //Calcolate ringBuffer next FirstIn address
    NewFirstIn := pointer(cardinal(OldFirstIn) + SizeOf(TReferencedPtr));
    if cardinal(NewFirstIn) > cardinal(EndBuffer) then
      NewFirstIn := StartBuffer;
    //Try to exchange and clear Reference.bit0
    if not AtomicCmpXchg8b(OldFirstIn, NewReference, NewFirstIn, NewReference - 1, FirstIn) then
      goto TryAgain;
  end;
end; { TOmniBaseQueue.Pop }

{$ELSE PurePascal}

//Queue Pop with Static/Dinamic reference and Reference.Bit0
asm
  push  edi                                                     //Store system registers
  push  esi
  push  ebx
  mov   edi, eax                                                //edi := ringBuffer
{$IFDEF QueueStaticReference}
  mov   edx, fs:[$18]                                           //edx := thread information block
  mov   edx, [edx + $24]                                        //edx := thread id as Reference
  inc   edx                                                     //edx := Reference, Reference.bit0 := 1
{$ENDIF}
@TryAgain:
  mov   ecx, [edi].TOmniRingBuffer.TaskPopUnderflow             //Set loop counter to define task underflow
  mov   ebx, [edi].TOmniRingBuffer.FirstIn.Reference            //ebx := start current Reference
@Spin:
  mov   eax, [edi].TOmniRingBuffer.FirstIn.Reference            //eax := current Reference
  test  al, 1                                                   //Test Reference.bit0
  loopnz @Spin                                                  //Suffer boy... (loopnz is double condition jump)
  jnz   @TryToOverrideReference                                 //Task time underflow?
                                                                //Queue is idle now
{$IFNDEF QueueStaticReference}
  lea   edx, eax + 3                                            //Calcolate new Reference value, Reference.bit0 = 1
{$ENDIF}
  lock cmpxchg [edi].TOmniRingBuffer.FirstIn.Reference, edx     //Try to set new Reference...
  jnz   @TryAgain                                               //...too late?
@NewReferenceSet:
  mov   eax, [edi].TOmniRingBuffer.FirstIn.PData                //Ring buffer empty test
  cmp   eax, [edi].TOmniRingBuffer.LastIn.PData
  jz    @FinishEmpty                                            //Is buffer empty?
  mov   esi, [eax]                                              //esi := result
  lea   ebx, eax + 8                                            //ebx := ring buffer next address
  cmp   ebx, [edi].TOmniRingBuffer.EndBuffer                    //if ebx > EndBuffer then...
  cmova ebx, [edi].TOmniRingBuffer.StartBuffer                  //...ebx := StartBuffer
  lea   ecx, edx - 1                                            //ecx := Reference, Reference.bit0 = 0
  lock cmpxchg8b [edi].TOmniRingBuffer.FirstIn                  //Try to set new ring buffer next address
{$IFDEF QueueStaticReference}
  lea   edx, ecx + 1                                            //edx := Reference, Reference.bit0 = 1
{$ENDIF}
  jnz   @TryAgain                                               //...no go?
                                                                //...finish :)
  mov   eax, esi                                                //Result := esi
  pop   ebx                                                     //Restore sytem registers
  pop   esi
  pop   edi
  ret

@TryToOverrideReference:
  or    bl, 1                                                   //Force start Reference.bit0 = 1
  cmp   ebx, eax                                                //Is Reference still the same ?...
  jnz   @TryAgain                                               //...queue task has new owner
                                                                //...working task Reference counter underflow
{$IFNDEF QueueStaticReference}
  lea   edx, eax + 2                                            //Calcolate new Reference value, Reference.bit0 = 1
{$ENDIF}
  lock cmpxchg [edi].TOmniRingBuffer.FirstIn.Reference, edx     //Try to override current Reference value...
  jz    @NewReferenceSet                                        //...yes
  jmp   @TryAgain                                               //...no go

@FinishEmpty:
  mov   eax, edx                                                //eax := Reference
  dec   edx                                                     //edx := Reference, Reference.bit0 = 0
  lock cmpxchg [edi].TOmniRingBuffer.FirstIn.Reference, edx     //Clear Reference.bit0 if task own Reference
  xor   eax, eax                                                //Result := nil
  pop   ebx                                                     //Restore sytem registers
  pop   esi
  pop   edi
end; { TOmniBaseQueue.Pop }

{$ENDIF PurePascal}

class procedure TOmniBaseQueue.Push(const data: pointer; const ringBuffer: POmniRingBuffer);
//REMEMBER: Reference.bit0 is queue idle/busy status and is not lock bit!

  {$IFDEF PurePascal}

//Queue Push with Static/Dinamic reference and Reference.Bit0
var
  TaskPushUnderflowCounter      :cardinal;
  NewReference                  :cardinal;
  CurrentReference              :cardinal;
  AtStartReference              :cardinal;
  NewLastIn                     :PReferencedPtr;
  OldLastIn                     :PReferencedPtr;

label
  TryAgain;

begin
{$IFDEF QueueStaticReference}
  NewReference := GetThreadId + 1;
{$ENDIF}
  with ringBuffer^ do
  begin
TryAgain:
    TaskPushUnderflowCounter := TaskPushUnderflow;
    AtStartReference := LastIn.Reference OR 1; //...Reference.bit0 := 1
    repeat
      CurrentReference := LastIn.Reference;
      dec(TaskPushUnderflowCounter);
    until (TaskPushUnderflowCounter = 0) or (Byte(CurrentReference) AND 1 = 0);
    if (Byte(CurrentReference) AND 1 <> 0) and (AtStartReference <> CurrentReference) then
      goto TryAgain;
{$IFNDEF QueueStaticReference}
    NewReference := CurrentReference OR 1 + 2; //...Reference.bit0 := 1
{$ENDIF}
    if not AtomicCmpxchg4b(CurrentReference, NewReference, LastIn.Reference) then
      goto TryAgain;
    //NewReferenceSet
    OldLastIn := LastIn.PData;
    OldLastIn.Reference := NewReference;
    if (NewReference <> LastIn.Reference) or
      not AtomicCmpXchg8b(OldLastIn.PData, NewReference, data, 0, OldLastIn^) then
    begin
      //Clear Reference.bit0 if task own Reference
      AtomicCmpxchg4b(NewReference, NewReference - 1, LastIn.Reference);
      goto TryAgain;
    end;
    //Calcolate ringBuffer next LastIn address
    NewLastIn := pointer(cardinal(OldLastIn) + SizeOf(TReferencedPtr));
    if cardinal(NewLastIn) > cardinal(EndBuffer) then
      NewLastIn := StartBuffer;
    //Try to exchange and clear Reference.bit0
    if not AtomicCmpXchg8b(OldLastIn, NewReference, NewLastIn, NewReference - 1, LastIn) then
      goto TryAgain;
  end;
end; { TOmniBaseQueue.Push }

{$ELSE PurePascal}

//Queue Push with Static/Dinamic reference and Reference.Bit0
asm
  push  edi                                                     //Store system registers
  push  esi
  push  ebx
  mov   esi, eax                                                //esi := data
  mov   edi, edx                                                //edi := POmniRingBuffer
{$IFDEF QueueStaticReference}
  mov   edx, fs:[$18]                                           //edx := thread information block
  mov   edx, [edx + $24]                                        //edx := thread id as Reference
  inc   edx                                                     //edx := Reference, Reference.bit0 := 1
{$ENDIF}
@Begin:
  mov   ecx, [edi].TOmniRingBuffer.TaskPushUnderflow            //Set loop counter to define task underflow
  mov   ebx, [edi].TOmniRingBuffer.LastIn.Reference             //ebx := start current Reference
@Spin:
  mov   eax, [edi].TOmniRingBuffer.LastIn.Reference             //eax := current Reference
  test  al, 1                                                   //Reference.bit0 test
  loopnz @Spin                                                  //Suffer boy... (loopnz is double condition jump)
  jnz   @TryToOverrideReference                                 //Task time underflow?
                                                                //Queue is idle now
{$IFNDEF QueueStaticReference}
  lea   edx, eax + 3                                            //Calcolate new dinamic Reference value, Reference.bit0 = 1
{$ENDIF}
  lock cmpxchg [edi].TOmniRingBuffer.LastIn.Reference, edx      //Try to set new Reference...
  jnz   @Begin                                                  //...too late?
@NewReferenceSet:
  push  esi                                                     //Store data pointer
  mov   ebx, esi                                                //ebx := data
  xor   ecx, ecx                                                //clear Reference at data write to prevent ABA conflict
  mov   esi, [edi].TOmniRingBuffer.LastIn.PData                 //esi := TReferencedPtr
  mov   [esi].TReferencedPtr.Reference, edx                     //Set Reference Reference to TReferencedPtr
  mov   eax, [esi].TReferencedPtr.PData                         //eax := old data
  cmp   edx, [edi].TOmniRingBuffer.LastIn.Reference             //If Reference not the same then...
  jnz   @Skip                                                   //...too late?
  lock cmpxchg8b qword ptr [esi].TReferencedPtr.PData           //Try to write...
                                                                //...yes write success
  mov   eax, esi                                                //eax := LastIn
  pop   esi                                                     //Restore data pointer
  jnz   @Begin                                                  //...no go?
  lea   ecx, edx - 1                                            //ecx := Reference, Reference.bit0 = 0
  lea   ebx, eax + 8                                            //ebx := ring buffer next address
  cmp   ebx, [edi].TOmniRingBuffer.EndBuffer                    //if ebx > EndBuffer then...
  cmova ebx, [edi].TOmniRingBuffer.StartBuffer                  //...ebx := StartBuffer
  lock cmpxchg8b qword ptr [edi].TOmniRingBuffer.LastIn         //Try to set new ring buffer next address
{$IFDEF QueueStaticReference}
  lea   edx, ecx + 1                                            //edx := Reference, Reference.bit0 = 1
{$ENDIF}
  jnz   @Begin                                                  //...no go?
                                                                //...finish :)
  pop   ebx                                                     //Restore sytem registers
  pop   esi
  pop   edi
  ret

@Skip:
  pop   esi                                                     //Restore data pointer
  mov   eax, edx                                                //eax := Reference
  dec   edx                                                     //edx := Reference, Reference.bit0 = 0
  lock cmpxchg [edi].TOmniRingBuffer.LastIn.Reference, ecx      //Clear Reference.bit0 if task own Reference
  jmp   @Begin

@TryToOverrideReference:
  or    bl, 1                                                   //Force start Reference.bit0 = 1
  cmp   ebx, eax                                                //Is Reference still the same ?...
  jnz   @Begin                                                  //...queue task has new owner
                                                                //...working task Reference counter underflow
{$IFNDEF QueueStaticReference}
  lea   edx, eax + 2                                            //ecx := Reference, Reference.bit0 = 0
{$ENDIF}
  lock cmpxchg [edi].TOmniRingBuffer.LastIn.Reference, edx      //Try to override current Reference value...
  jz    @NewReferenceSet                                        //...yes
  jmp   @Begin
end; { TOmniBaseQueue.Push }

{$ENDIF PurePascal}

function TOmniBaseQueue.Dequeue(var value): boolean;
var
  Data: pointer;
begin
  Data := Pop(obcPublicRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(Data^, value, ElementSize);
  Push(Data, obcRecycleRingBuffer);
end; { TOmniQueue.Dequeue }

destructor TOmniBaseQueue.Destroy;
begin
  FreeMem(obcDataBuffer);
  FreeMem(obcPublicRingBuffer);
  FreeMem(obcRecycleRingBuffer);
end; { TOmniQueue.Destroy }

procedure TOmniBaseQueue.Empty;
var
  Data: pointer;
begin
  repeat
    Data := Pop(obcPublicRingBuffer);
    if assigned(Data) then
      Push(Data, obcRecycleRingBuffer)
    else
      break;
  until false;
end; { TOmniQueue.Empty }

function TOmniBaseQueue.Enqueue(const value): boolean;
var
  Data: pointer;
begin
  Data := Pop(obcRecycleRingBuffer);
  Result := assigned(Data);
  if not Result then
    Exit;
  Move(value, Data^, ElementSize);
  Push(Data, obcPublicRingBuffer);
end; { TOmniQueue.Enqueue }

procedure TOmniBaseQueue.Initialize(numElements, elementSize: integer);
const
  NumOfSamples = 10;

var
  RingBufferSize        : cardinal;
  n                     : integer;
  currElement           : pointer;
  TimeTestField         : array [0..2]of array [1..NumOfSamples] of int64;

  function GetMinAndClear(Rutine: cardinal): int64;
  var
    n: integer;
    x: integer;
  begin
    x:= 1;
    result := MaxLongInt;
    for n:= 1 to NumOfSamples do
    begin
      if TimeTestField[Rutine, n] < result then
      begin
        result := TimeTestField[Rutine, n];
        x := n;
      end;
    end;
    TimeTestField[Rutine, x] := MaxLongInt;
  end;

begin
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(numElements > 0);
  Assert(elementSize > 0);
  obcNumElements := numElements;
  obcElementSize := elementSize;
  // calculate element size, round up to next 4-aligned value
  obcElementSize := (elementSize + 3) AND NOT 3;
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
  //Calcolate  TaskPopUnderflow and TaskPushUnderflow counter values depend on CPU speed!!!}
  obcPublicRingBuffer.TaskPopUnderflow := 1;
  obcPublicRingBuffer.TaskPushUnderflow := 1;
  obcRecycleRingBuffer.TaskPopUnderflow := 1;
  obcRecycleRingBuffer.TaskPushUnderflow := 1;
  for n := 1 to NumOfSamples do
  begin
    //Measure Pop rutine delay
    TimeTestField[0, n] := GetTimeStamp;
    currElement := Pop(obcRecycleRingBuffer);
    TimeTestField[0, n] := GetTimeStamp - TimeTestField[0, n];
    //Measure Push rutine delay
    TimeTestField[1, n] := GetTimeStamp;
    Push(currElement, obcRecycleRingBuffer);
    TimeTestField[1, n] := GetTimeStamp - TimeTestField[1, n];
    //Measure GetTimeStamp rutine delay
    TimeTestField[2, n] := GetTimeStamp;
    TimeTestField[2, n] := GetTimeStamp - TimeTestField[2, n];
  end;
  //Calcolate first 4 minimum average for GetTimeStamp
  n := ( GetMinAndClear(2) + GetMinAndClear(2) + GetMinAndClear(2) + GetMinAndClear(2));
  //Calcolate first 4 minimum average for Pop rutine * 2
  obcRecycleRingBuffer.TaskPopUnderflow := ( GetMinAndClear(0) + GetMinAndClear(0) +
    GetMinAndClear(0) + GetMinAndClear(0) - n) div 4;
  obcPublicRingBuffer.TaskPopUnderflow := obcRecycleRingBuffer.TaskPopUnderflow;
  //Calcolate first 4 minimum average for Push rutine * 2
  obcRecycleRingBuffer.TaskPushUnderflow := (GetMinAndClear(1) + GetMinAndClear(1) +
    GetMinAndClear(1) + GetMinAndClear(1) - n) div 4;
  obcPublicRingBuffer.TaskPushUnderflow := obcRecycleRingBuffer.TaskPushUnderflow;
end; { TOmniBaseStack.Initialize }

function TOmniBaseQueue.IsEmpty: boolean;

{$IFDEF PurePascal}

begin
  result := obcPublicRingBuffer.FirstIn.PData = obcPublicRingBuffer.LastIn.PData;
end;

{$ELSE PurePascal}

asm
  //result := LastIn = FirstIn
  mov   edx, [eax].obcPublicRingBuffer
  mov   ecx, dword ptr [edx].TOmniRingBuffer.LastIn.PData       //ecx := LastIn
  cmp   ecx, dword ptr [edx].TOmniRingBuffer.FirstIn.PData
  setz  al
end; { TOmniBaseStack.IsEmpty }

 {$ENDIF PurePascal}

function TOmniBaseQueue.IsFull: boolean;

{$IFDEF PurePascal}

var
  NewLastIn: pointer;

begin
  NewLastIn := pointer(cardinal(obcPublicRingBuffer.LastIn.PData) + SizeOf(TReferencedPtr));
  if cardinal(NewLastIn) > cardinal(obcPublicRingBuffer.EndBuffer) then
    NewLastIn := obcPublicRingBuffer.StartBuffer;
  result := (cardinal(NewLastIn) > cardinal(obcPublicRingBuffer.LastIn.PData)) or
    (obcRecycleRingBuffer.FirstIn.PData = obcRecycleRingBuffer.LastIn.PData);
end;

{$ELSE PurePascal}

asm
  mov   edx, [eax].obcPublicRingBuffer
  mov   ecx, dword ptr [edx].TOmniRingBuffer.LastIn
  lea   ecx, ecx + 8                                            //ecx := ring buffer next address
  cmp   ecx, [edx].TOmniRingBuffer.EndBuffer                    //if ebx > EndBuffer then...
  cmova ecx, [edx].TOmniRingBuffer.StartBuffer                  //...ebx := StartBuffer
  cmp   ecx, dword ptr [edx].TOmniRingBuffer.FirstIn              
  setz  cl                                                      //cl.bit0 := NextAddress = FirstIn
  mov   edx, [eax].obcRecycleRingBuffer
  mov   eax, dword ptr [edx].TOmniRingBuffer.LastIn             //eax := LastIn
  cmp   eax, dword ptr [edx].TOmniRingBuffer.FirstIn              
  setz  al                                                      //al.bit0 := LastIn = FirstIn
  or    al, cl
end; { TOmniQueue.IsFull }                                      //al.bit0 := al.bit0 OR cl.bit0

{$ENDIF PurePascal}

{$ELSE PureQueue}

{ TOmniBaseQueue }

constructor TOmniBaseQueue.Create;
begin
//  Assert(SizeOf(obqDequeuedMessages) = 8);
  if cardinal(@obqDequeuedMessages) AND 7 <> 0 then
    raise Exception.Create('obqDequeuedMessages is not 8-aligned');
end; { TOmniBaseQueue.create }

function TOmniBaseQueue.Dequeue(var value): boolean;
var
  linkedData: POmniLinkedData;
begin
  if obqDequeuedMessages.Head.PData = nil then
    obqDequeuedMessages.Head.PData := InvertOrder(UnlinkAll(obcPublicChain));
  linkedData := PopLink(obqDequeuedMessages);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(linkedData.Data, value, ElementSize);
  PushLink(linkedData, obcRecycleChain);
end; { TOmniQueue.Dequeue }

procedure TOmniBaseQueue.Empty;
var
  linkedData: POmniLinkedData;
begin
  inherited;
  if assigned(obqDequeuedMessages.Head.PData) then repeat
    linkedData := PopLink(obqDequeuedMessages);
    if not assigned(linkedData) then
      break; //repeat
    PushLink(linkedData, obcRecycleChain);
  until false;
end; { TOmniQueue.Empty }

function TOmniBaseQueue.Enqueue(const value): boolean;
var
  linkedData: POmniLinkedData;
begin
  linkedData := PopLink(obcRecycleChain);
  Result := assigned(linkedData);
  if not Result then
    Exit;
  Move(value, linkedData.Data, ElementSize);
  PushLink(linkedData, obcPublicChain);
end; { TOmniQueue.Enqueue }

function TOmniBaseQueue.IsEmpty: boolean;
begin
  Result := not (assigned(obcPublicChain.Head.PData) or assigned(obqDequeuedMessages.Head.PData));
end; { TOmniQueue.IsEmpty }

{$ENDIF PureQueue}

{ TOmniQueue }

constructor TOmniQueue.Create(numElements, elementSize: integer;
  options: TOmniContainerOptions);
begin
  inherited Create;
  Initialize(numElements, elementSize);
  orbOptions := options;
  if coEnableMonitor in Options then
    orbMonitorSupport := CreateOmniMonitorSupport;
  if coEnableNotify in Options then
    orbNotifySupport := TOmniNotifySupport.Create;
end; { TOmniQueue.Create }

function TOmniQueue.Dequeue(var value): boolean;
begin
  Result := inherited Dequeue(value);
  if Result then
    if coEnableNotify in Options then
      orbNotifySupport.Signal;
end; { TOmniQueue.Dequeue }

function TOmniQueue.Enqueue(const value): boolean;
begin
  Result := inherited Enqueue(value);
  if Result then begin
    if coEnableNotify in Options then
      orbNotifySupport.Signal;
    if coEnableMonitor in Options then
      orbMonitorSupport.Notify;
  end;
end; { TOmniQueue.Enqueue }

end.
