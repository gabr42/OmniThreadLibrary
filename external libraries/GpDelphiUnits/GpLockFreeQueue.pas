///<summary>Sample implementation of a dynamically allocated, O(1) enqueue and dequeue,
///    threadsafe, microlocking queue.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010 Primoz Gabrijelcic
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
///   Creation date     : 2010-02-10
///   Last modification : 2010-10-13
///   Version           : 1.01c
///</para><para>
///   History:
///     1.01c: 2010-10-13
///       - [GJ] Alignment algorithm simplification and fix.
///     1.01b: 2010-10-11
///       - Fixed internal alignment algorithm.
///     1.01a: 2010-09-28
///       - Assumes that memory allocations are 4-aligned. 8-alignment is implemented
///         internally.
///     1.01: 2010-02-18
///       - Reversed head and tail because they were used illogically.    
///     1.0: 2010-02-10
///       - Initial implementation, based on the TOmniBaseQueue from the OmniThreadLibrary.
///</para></remarks>

// The following code requires at least Pentium 4 processor because SSE2 instructions are
// used in the Move64 function.

{$DEFINE LFQ_DontUseMove64} // defined to not use SSE2-specific stuff - apparently there's a lot of AMD CPUs without SSE2 support out there

unit GpLockFreeQueue;

{$R-,O+,A8,B-}
{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (CompilerVersion >= 17)}
    {$DEFINE USE_STRICT}
  {$IFEND}
{$ENDIF}

interface

uses
  Classes;

type
  TGpLFQueueTag = (tagFree, tagAllocating, tagAllocated, tagRemoving,
    tagEndOfList, tagExtending, tagBlockPointer, tagDestroying, tagHeader,
    tagSentinel);

  TGpLFQueueTaggedValue = packed record
    Value   : int64;    //must be 4-aligned as it is atomically decremented in the header slot
    Tag     : TGpLFQueueTag;
    Offset  : word;
    Stuffing: array [1..5] of byte;
  end; { TGpLFQueueTaggedValue }
  PGpLFQueueTaggedValue = ^TGpLFQueueTaggedValue;

  TGpLFQueueTaggedPointer = packed record
    Slot    : PGpLFQueueTaggedValue;
    Tag     : TGpLFQueueTag;
    Stuffing: array [1..3] of byte; // record size must be congruent to 0 (mod 4)
  end; { TGpLFQueueTaggedPointer }
  PGpLFQueueTaggedPointer = ^TGpLFQueueTaggedPointer;

  ///<summary>Dynamically allocated, O(1) enqueue and dequeue, threadsafe, microlocking queue.</summary>
  TGpLockFreeQueue = class
  {$IFDEF USE_STRICT} strict {$ENDIF}
    private
    obcBlockSize  : integer;
    obcCachedBlock: PGpLFQueueTaggedValue;
    obcTailBuffer : pointer;
    obcTailPointer: PGpLFQueueTaggedPointer;
    obcNumSlots   : integer;
    obcHeadBuffer : pointer;
    obcHeadPointer: PGpLFQueueTaggedPointer;
  {$IFDEF USE_STRICT} strict {$ENDIF}
  protected
    function  AllocateAligned(size, alignment: integer; var memPtr: pointer): pointer;
    function  AllocateBlock: PGpLFQueueTaggedValue;
    procedure Cleanup; virtual;
    procedure Initialize; virtual;
    function  NextSlot(slot: PGpLFQueueTaggedValue): PGpLFQueueTaggedValue; 
    procedure PartitionMemory(memory: PGpLFQueueTaggedValue);
    procedure PreallocateMemory;
    function  PrevSlot(slot: PGpLFQueueTaggedValue): PGpLFQueueTaggedValue; 
    procedure ReleaseBlock(firstSlot: PGpLFQueueTaggedValue; forceFree: boolean = false);
  public
    constructor Create(blockSize: integer = 65536; numCachedBlocks: integer = 2);
    destructor  Destroy; override;
    function  Dequeue(var value: int64): boolean;
    procedure Enqueue(const value: int64);
  end; { TGpLockFreeQueue }

implementation

uses
  Windows,
  SysUtils;

{ atomic helpers, written by GJ }

function CAS8(const oldValue, newValue: byte; var destination): boolean;
asm
  lock cmpxchg byte ptr [destination], newValue
  setz  al
end; { CAS8 }

function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

function CAS64(const oldData: pointer; oldReference: cardinal; newData: pointer;
  newReference: cardinal; var destination): boolean;
asm
  push  edi
  push  ebx
  mov   ebx, newData
  mov   ecx, newReference
  mov   edi, destination
  lock cmpxchg8b qword ptr [edi]
  setz  al
  pop   ebx
  pop   edi
end; { CAS64 }

procedure Move64(newData: pointer; newReference: cardinal; var Destination);
//Move 8 bytes atomically into 8-byte Destination!
asm
  movd  xmm0, eax
  movd  xmm1, edx
  punpckldq xmm0, xmm1
  movq  qword [Destination], xmm0
end; { Move64 }

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
    slot = 4 bytes
    tag  = 4 bytes
  head
    slot = 4 bytes
    tag  = 4 bytes
all are 4-aligned

slot contains:
  TOmniValue = 13 bytes
  tag        = 1 byte
  offset     = 2 bytes
TOmniValues are 4-aligned

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

{ TGpLockFreeQueue }

constructor TGpLockFreeQueue.Create(blockSize: integer; numCachedBlocks: integer);
begin
  inherited Create;
  obcBlockSize := (((blockSize-1) div SizeOf(TGpLFQueueTaggedValue)) + 1) * SizeOf(TGpLFQueueTaggedValue);
  obcNumSlots := obcBlockSize div SizeOf(TGpLFQueueTaggedValue);
  if obcNumSlots > 65536 then
    raise Exception.CreateFmt('TGpLockFreeQueue.Create: Maximum block size is %d',
            [65536 * SizeOf(TGpLFQueueTaggedValue)]);
  Initialize;
end; { TGpLockFreeQueue.Create }

destructor TGpLockFreeQueue.Destroy;
begin
  Cleanup;
  inherited;
end; { TGpLockFreeQueue.Destroy }

function TGpLockFreeQueue.AllocateAligned(size, alignment: integer; var memPtr: pointer):
  pointer;
begin                     
  Assert(SizeOf(cardinal) = SizeOf(pointer));
  Assert(alignment in [2{WORD-aligned}, 3{DWORD-aligned}, 4{QWORD-aligned}]);
  memPtr := AllocMem(size + (1 shl alignment));
  Result := pointer((cardinal(memPtr) + (1 shl alignment)) AND (cardinal(-1 SHL alignment)));
end; { TGpLockFreeQueue.AllocateAligned }

function TGpLockFreeQueue.AllocateBlock: PGpLFQueueTaggedValue;
var
  cached: PGpLFQueueTaggedValue;
begin
  cached := obcCachedBlock;
  if assigned(cached) and CAS32(cached, nil, obcCachedBlock) then
    Result := cached
  else begin
    Result := AllocMem(obcBlockSize);
    PartitionMemory(Result);
  end;
end; { TGpLockFreeQueue.AllocateBlock }

procedure TGpLockFreeQueue.Cleanup;
var
  pBlock: PGpLFQueueTaggedValue;
  pSlot : PGpLFQueueTaggedValue;
begin
  pSlot := obcTailPointer.Slot;
  while assigned(pSlot) do begin
    if pSlot.Tag in [tagBlockPointer, tagEndOfList] then begin
      pBlock := pSlot;
      pSlot := PGpLFQueueTaggedValue(pSlot.Value);
      Dec(pBlock, pBlock.Offset);
      ReleaseBlock(pBlock, true);
    end
    else
      Inc(pSlot);
  end;
  if assigned(obcCachedBlock) then
    FreeMem(obcCachedBlock);
  FreeMem(obcTailBuffer);
  FreeMem(obcHeadBuffer);
end; { TGpLockFreeQueue.Cleanup }

function TGpLockFreeQueue.Dequeue(var value: int64): boolean;
var
  caughtTheHead: boolean;
  tail         : PGpLFQueueTaggedValue;
  header       : PGpLFQueueTaggedValue;
  next         : PGpLFQueueTaggedValue;
  tag          : TGpLFQueueTag;
begin
  tag := tagSentinel;
  Result := true;
  while Result and (tag = tagSentinel) do begin
    repeat
      tail := obcTailPointer.Slot;
      caughtTheHead := NextSlot(obcTailPointer.Slot) = obcHeadPointer.Slot; // an approximation; we don't care if in a moment this won't be true anymore
      if (obcTailPointer.Tag = tagAllocated)
         and CAS64(tail, Ord(tagAllocated), tail, Ord(tagRemoving), obcTailPointer^) then
      begin
        tag := tagAllocated;
        break; //repeat
      end
      else if (obcTailPointer.Tag = tagSentinel) then begin
        if caughtTheHead then begin
          Result := false;
          break; //repeat
        end
        else if CAS64(tail, Ord(tagSentinel), tail, Ord(tagRemoving), obcTailPointer^) then begin
          tag := tagSentinel;
          break; //repeat
        end
      end
      else if (obcTailPointer.Tag = tagBlockPointer)
              and CAS64(tail, Ord(tagBlockPointer), tail, Ord(tagDestroying), obcTailPointer^) then
      begin
        tag := tagBlockPointer;
        break; //repeat
      end
      else
        asm pause; end;
    until false;
    if Result then begin // dequeueing
      header := tail;
      Dec(header, header.Offset);
      if tag in [tagSentinel, tagAllocated] then begin
        next := NextSlot(tail);
        if tag = tagAllocated then // sentinel doesn't contain any useful value
          value := tail.Value;
        if caughtTheHead then begin
          {$IFNDEF LFQ_DontUseMove64}
          Move64(tail, Ord(tagSentinel), obcTailPointer^); // release the lock; as this is the last element, don't move forward
          {$ELSE}
          CAS64(tail, Ord(tagRemoving), tail, Ord(tagSentinel), obcTailPointer^);
          {$ENDIF LFQ_DontUseMove64}
          header := nil; // do NOT decrement the counter; this slot will be retagged again
        end
        else
          {$IFNDEF LFQ_DontUseMove64}
          Move64(next, Ord(next.Tag), obcTailPointer^); // release the lock
          {$ELSE}
          CAS64(tail, Ord(tagRemoving), next, Ord(next.Tag), obcTailPointer^);
          {$ENDIF LFQ_DontUseMove64}
      end
      else begin // releasing memory
        next := PGpLFQueueTaggedValue(tail.Value); // next points to the sentinel
        {$IFNDEF LFQ_DontUseMove64}
        Move64(next, Ord(tagSentinel), obcTailPointer^); // release the lock
        {$ELSE}
        CAS64(tail, Ord(tagDestroying), next, Ord(tagSentinel), obcTailPointer^);
        {$ENDIF LFQ_DontUseMove64}
        tag := tagSentinel; // retry
      end;
      if assigned(header) and (InterlockedDecrement(PInteger(header)^) = 0) then
        ReleaseBlock(header);
    end;
  end; //while Result and (tag = tagSentinel)
end; { TGpLockFreeQueue.Dequeue }

procedure TGpLockFreeQueue.Enqueue(const value: int64);
var
  extension: PGpLFQueueTaggedValue;
  next     : PGpLFQueueTaggedValue;
  head     : PGpLFQueueTaggedValue;
begin
  repeat
    head := obcHeadPointer.Slot;
    if (obcHeadPointer.Tag = tagFree)
       and CAS64(head, Ord(tagFree), head, Ord(tagAllocating), obcHeadPointer^)
    then
      break //repeat
    else if (obcHeadPointer.Tag = tagEndOfList)
            and CAS64(head, Ord(tagEndOfList), head, Ord(tagExtending), obcHeadPointer^)
    then
      break //repeat
    else  // very temporary condition, retry quickly
      asm pause; end;
  until false;
  if obcHeadPointer.Tag = tagAllocating then begin // enqueueing
    next := NextSlot(head);
    head.Value := value;
    head.Tag := tagAllocated;
    {$IFNDEF LFQ_DontUseMove64}
    Move64(next, Ord(next.Tag), obcHeadPointer^); // release the lock
    {$ELSE}
    CAS64(head, Ord(tagAllocating), next, Ord(next.Tag), obcHeadPointer^);
    {$ENDIF LFQ_DontUseMove64}
  end
  else begin // allocating memory
    extension := AllocateBlock; // returns pointer to the header
    Inc(extension, 2);          // move over header and sentinel to the first data slot
    extension.Tag := tagAllocated;
    extension.Value := value;
    Dec(extension);             // forward reference points to the sentinel
    head.Value := int64(extension);
    head.Tag := tagBlockPointer;
    Inc(extension, 2); // get to the first free slot
    {$IFNDEF LFQ_DontUseMove64}
    Move64(extension, Ord(extension.Tag), obcHeadPointer^); // release the lock
    {$ELSE}
    CAS64(head, Ord(tagExtending), extension, Ord(extension.Tag), obcHeadPointer^);
    {$ENDIF LFQ_DontUseMove64}
    PreallocateMemory;
  end;
end; { TGpLockFreeQueue.Enqueue }

procedure TGpLockFreeQueue.Initialize;
begin
  obcTailPointer := AllocateAligned(SizeOf(TGpLFQueueTaggedPointer), 4, obcTailBuffer);
  obcHeadPointer := AllocateAligned(SizeOf(TGpLFQueueTaggedPointer), 4, obcHeadBuffer);
  obcCachedBlock := AllocateBlock; // pre-allocate memory
  obcTailPointer.Slot := NextSlot(AllocateBlock); // point to the sentinel
  obcTailPointer.Tag := obcTailPointer.Slot.Tag;
  obcHeadPointer.Slot := NextSlot(obcTailPointer.Slot);
  obcHeadPointer.Tag := obcHeadPointer.Slot.Tag;
  Assert(cardinal(obcTailPointer) mod 8 = 0);
  Assert(cardinal(obcHeadPointer) mod 8 = 0);
  Assert(cardinal(@obcCachedBlock) mod 4 = 0);
end; { TGpLockFreeQueue.Initialize }

function TGpLockFreeQueue.NextSlot(slot: PGpLFQueueTaggedValue): PGpLFQueueTaggedValue;
begin
  Result := slot;
  Inc(Result);
end; { TGpLockFreeQueue.NextSlot }

procedure TGpLockFreeQueue.PartitionMemory(memory: PGpLFQueueTaggedValue);
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
end; { TGpLockFreeQueue.PartitionMemory }

procedure TGpLockFreeQueue.PreallocateMemory;
var
  memory: PGpLFQueueTaggedValue;
begin
  if not assigned(obcCachedBlock) then begin
    memory := AllocMem(obcBlockSize);
    PartitionMemory(memory);
    if not CAS32(nil, memory, obcCachedBlock) then
      FreeMem(memory);
  end;
end; { TGpLockFreeQueue.PreallocateMemory }

function TGpLockFreeQueue.PrevSlot(slot: PGpLFQueueTaggedValue): PGpLFQueueTaggedValue;
begin
  Result := slot;
  Dec(Result);
end; { TGpLockFreeQueue.PrevSlot }

procedure TGpLockFreeQueue.ReleaseBlock(firstSlot: PGpLFQueueTaggedValue; forceFree: boolean);
begin
  if forceFree or assigned(obcCachedBlock) then
    FreeMem(firstSlot)
  else begin
    ZeroMemory(firstSlot, obcBlockSize);
    PartitionMemory(firstSlot);
    if not CAS32(nil, firstSlot, obcCachedBlock) then
      FreeMem(firstSlot);
  end;
end; { TGpLockFreeQueue.ReleaseBlock }

initialization
  Assert(SizeOf(TGpLFQueueTaggedValue) = 16);
  Assert(SizeOf(TGpLFQueueTaggedPointer) = 8);
  Assert(SizeOf(pointer) = SizeOf(cardinal));
end.
