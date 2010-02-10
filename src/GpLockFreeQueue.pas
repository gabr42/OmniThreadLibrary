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
///   Last modification : 2010-02-10
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2010-02-10
///       - Initial implementation, based on the TOmniBaseQueue from the OmniThreadLibrary.
///</para></remarks>

// The following code requires at least Pentium 4 processor because SSE2 instructions are
// used in the Move64 function.

unit GpLockFreeQueue;

{$R-,O+,A8,B-}
{$WARN SYMBOL_PLATFORM OFF}

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
  strict private
    obcBlockSize  : integer;
    obcCachedBlock: PGpLFQueueTaggedValue;
    obcHeadPointer: PGpLFQueueTaggedPointer;
    obcNumSlots   : integer;
    obcTailPointer: PGpLFQueueTaggedPointer;
  strict protected
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
  head
    slot = 4 bytes
    tag  = 4 bytes
  tail
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
      tail = header.tail.slot
      old_tag = header.tail.tag
      if header.tail.CAS(tail, tagFree, tail, tagAllocating) then
          break
      else if header.tail.CAS(tail, tagEndOfList, tail, tagExtending) then
          break
      else
          yield
  forever
  if header.tail.tag = tagAllocating then
      store <value, tagAllocated> into slot
      header.tail.CAS(tail, tagAllocating, NextSlot(tail), NextSlot(tail).tag)
  else
      allocate block // from cache, if possible
      next = second data slot in the new block
      set next to <tagAllocated, value>
      set last slot in the original block to <new block address, tagBlockPointer>
      header.tail.CAS(tail, tagExtending, next, next.tag)
      // queue is now unlocked
      preallocate block

Dequeue:
  repeat
      head = header.head.slot
      old_tag = header.head.tag
      caughtTheTail := NextSlot(header.head.slot) = header.tail.slot;
      if head.head.CAS(head, tagAllocated, head, tagRemoving) then
          head.tag = tagRemovings
          break
      else if header.head.Tag = tagSentinel then
          if caughtTheTail then
              return false
          else if header.head.CAS(head, tagSentinel, head, tagRemoving) then
              head.tag = tagRemoving
              break
      else if header.head.CAS(head, tagBlockPointer, head, tagDestrogin) then
          head.tag = tagDestroying
          break
      else
          yield
  forever
  firstSlot = head - head.Offset // point to first slot
  if old_tag in [tagSentinel, tagAllocated] then
      next = NextSlot(head)
      if tag = tagAllocated then
          fetch stored value
      if caughtTheTail then
          header.head.CAS(head, tagRemoving, head, tagSentinel)
          firstSlot = nil // do not decrement the header counter
      else
          header.head.CAS(head, tagRemoving, next, next.tag)
  else
      next = head.value // points to the next block's sentinel
      header.head.CAS(head, tagDestroying, next, tagSentinel)
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
  pSlot := obcHeadPointer.Slot;
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
  FreeMem(obcHeadPointer);
  FreeMem(obcTailPointer);
end; { TGpLockFreeQueue.Cleanup }

function TGpLockFreeQueue.Dequeue(var value: int64): boolean;
var
  caughtTheTail: boolean;
  head         : PGpLFQueueTaggedValue;
  header       : PGpLFQueueTaggedValue;
  next         : PGpLFQueueTaggedValue;
  tag          : TGpLFQueueTag;
begin
  tag := tagSentinel;
  Result := true;
  while Result and (tag = tagSentinel) do begin
    repeat
      head := obcHeadPointer.Slot;
      caughtTheTail := NextSlot(obcHeadPointer.Slot) = obcTailPointer.Slot; // an approximation; we don't care if in a moment this won't be true anymore
      if (obcHeadPointer.Tag = tagAllocated)
         and CAS64(head, Ord(tagAllocated), head, Ord(tagRemoving), obcHeadPointer^) then
      begin
        tag := tagAllocated;
        break; //repeat
      end
      else if (obcHeadPointer.Tag = tagSentinel) then begin
        if caughtTheTail then begin
          Result := false;
          break; //repeat
        end
        else if CAS64(head, Ord(tagSentinel), head, Ord(tagRemoving), obcHeadPointer^) then begin
          tag := tagSentinel;
          break; //repeat
        end
      end
      else if (obcHeadPointer.Tag = tagBlockPointer)
              and CAS64(head, Ord(tagBlockPointer), head, Ord(tagDestroying), obcHeadPointer^) then
      begin
        tag := tagBlockPointer;
        break; //repeat
      end
      else
        asm pause; end;
    until false;
    if Result then begin // dequeueing
      header := head;
      Dec(header, header.Offset);
      if tag in [tagSentinel, tagAllocated] then begin
        next := NextSlot(head);
        if tag = tagAllocated then // sentinel doesn't contain any useful value
          value := head.Value;
        if caughtTheTail then begin
          Move64(head, Ord(tagSentinel), obcHeadPointer^); // release the lock; as this is the last element, don't move forward
          header := nil; // do NOT decrement the counter; this slot will be retagged again
        end
        else
          Move64(next, Ord(next.Tag), obcHeadPointer^); // release the lock
      end
      else begin // releasing memory
        next := PGpLFQueueTaggedValue(head.Value); // next points to the sentinel
        Move64(next, Ord(tagSentinel), obcHeadPointer^); // release the lock
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
  tail     : PGpLFQueueTaggedValue;
begin
  repeat
    tail := obcTailPointer.Slot;
    if (obcTailPointer.Tag = tagFree)
       and CAS64(tail, Ord(tagFree), tail, Ord(tagAllocating), obcTailPointer^)
    then
      break //repeat
    else if (obcTailPointer.Tag = tagEndOfList)
            and CAS64(tail, Ord(tagEndOfList), tail, Ord(tagExtending), obcTailPointer^)
    then
      break //repeat
    else  // very temporary condition, retry quickly
      asm pause; end;
  until false;
  if obcTailPointer.Tag = tagAllocating then begin // enqueueing
    next := NextSlot(tail);
    tail.Value := value;
    tail.Tag := tagAllocated;
    Move64(next, Ord(next.Tag), obcTailPointer^); // release the lock
  end
  else begin // allocating memory
    extension := AllocateBlock; // returns pointer to the header
    Inc(extension, 2);          // move over header and sentinel to the first data slot
    extension.Tag := tagAllocated;
    extension.Value := value;
    Dec(extension);             // forward reference points to the sentinel
    tail.Value := int64(extension);
    tail.Tag := tagBlockPointer;
    Inc(extension, 2); // get to the first free slot
    Move64(extension, Ord(extension.Tag), obcTailPointer^); // release the lock
    PreallocateMemory;
  end;
end; { TGpLockFreeQueue.Enqueue }

procedure TGpLockFreeQueue.Initialize;
begin
  obcHeadPointer := AllocMem(SizeOf(TGpLFQueueTaggedPointer));
  obcTailPointer := AllocMem(SizeOf(TGpLFQueueTaggedPointer));
  Assert(cardinal(obcHeadPointer) mod 8 = 0);
  Assert(cardinal(obcTailPointer) mod 8 = 0);
  Assert(cardinal(@obcCachedBlock) mod 4 = 0);
  obcHeadPointer.Slot := NextSlot(AllocateBlock); // point to the sentinel
  obcHeadPointer.Tag := obcHeadPointer.Slot.Tag;
  obcTailPointer.Slot := NextSlot(obcHeadPointer.Slot);
  obcTailPointer.Tag := obcTailPointer.Slot.Tag;
  obcCachedBlock := AllocateBlock; // pre-allocate memory
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
