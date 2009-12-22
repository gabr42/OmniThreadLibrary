///<summary>Growable micro-locking collections. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
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
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2009-12-19
///   Last modification : 2009-12-22
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2009-12-22
///       - Released.
///</para></remarks>

unit OtlCollections;

// TODO 3 -oPrimoz Gabrijelcic : Should implement container observer

(*
TOmniCollection
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
  tagEndOfList
  tagExtending
  tagBlockPointer
  tagRemoving
  tagRemoved
  tagDestroying

CleanupGC:
  if not GC.IsEmpty then
    if removeCount = 0 then
      if CAS(removeCount, -1) then begin
        GC.Cleanup
        removeCount := 0;
      end;

Enqueue:
  CleanupGC;
  repeat
    if removeCount >= 0 then
      if CAS(removeCount, removeCount + 1) then
        break
    else
      yield
  forever

  repeat
    fetch tag from current tail
    if tag = tagFree and
       CAS(tag, tagAllocating) then
      break
    if tag = tagEndOfList and
       CAS(tag, tagExtending) then
      break
    if tag = tagExtending then
      yield
  forever
  if tag = tagFree then
    increment tail
    store value, tagAllocated
  else
    allocate and initialize new block
      last entry has tagEndOfList tag, others have tagFree
    set tail to (new block + 1 slot)
    store value, tagAllocated into new block
    store pointer to new block, tagBlockPointer, into original

  Dec(removeCount);

Dequeue:
  CleanupGC;
  repeat
    if removeCount >= 0 then
      if CAS(removeCount, removeCount + 1) then
        break
    else
      yield
  forever

  repeat
    fetch tag from current head
    if tag = tagFree then
      return Empty
    if tag in [tagAllocated, tagBlockPointer] and
       CAS(tag, tagRemoving) then
      break
    yield?
  forever
  if tag = tagAllocated then
    increment head
    get value
    store tagRemoved
  else
    if first slot in new block is allocated
      set head to (new block + 1 slot)
      get value
    else
      set head to new block
    store tagDestroying
    put last block into GC list
    retry

  Dec(removeCount);
*)

interface

uses
  Classes,
  GpStuff,
  OtlCommon,
  OtlSync;

type
  TOmniCollectionTag = (tagFree, tagAllocating, tagAllocated, tagEndOfList, tagExtending,
    tagBlockPointer, tagRemoving, tagRemoved, tagDestroying);

  TOmniTaggedValue = packed record
    Tag     : TOmniCollectionTag;
    Stuffing: word;
    Value   : TOmniValue;
    function CASTag(oldTag, newTag: TOmniCollectionTag): boolean;
  end; { TOmniTaggedValue }
  POmniTaggedValue = ^TOmniTaggedValue;

  ///<summary>Growable, threadsafe, O(1) enqueue and dequeue, microlocking queue.</summary>
  TOmniCollection = class
  strict private // keep 4-aligned
    ocHeadPointer: POmniTaggedValue;
    ocTailPointer: POmniTaggedValue;
  strict private
    ocBlockCount : integer;
    ocCachedBlock: POmniTaggedValue;
    ocGarbage    : TList;
    ocHasGCBlocks: boolean;
    ocRemoveCount: TGp4AlignedInt;
  strict protected
    procedure AddToQC(lastSlot: POmniTaggedValue);
    function  AllocateBlock: pointer;
    procedure CleanupGC;
    procedure DumpBlock(pBlock: POmniTaggedValue);
    procedure EnterReader; inline; 
    procedure LeaveReader; inline;
    procedure LeaveWriter; inline;
    function  TryEnterWriter: boolean; inline;
  {$IFDEF DEBUG}
  public
    LoopDeqAllocated: TGp4AlignedInt;
    LoopDeqOther    : TGp4AlignedInt;
    LoopDeqRemoving : TGp4AlignedInt;
    LoopEnqEOL      : TGp4AlignedInt;
    LoopEnqExtending: TGp4AlignedInt;
    LoopEnqFree     : TGp4AlignedInt;
    LoopEnqOther    : TGp4AlignedInt;
    LoopGC          : TGp4AlignedInt;
    LoopReader      : TGp4AlignedInt;
    NumDequeued     : TGp4AlignedInt;
    NumEnqueued     : TGp4AlignedInt;
    NumTrueAlloc    : TGp4AlignedInt;
    NumReusedAlloc  : TGp4AlignedInt;
  {$ENDIF DEBUG}
  public
    constructor Create;
    destructor  Destroy; override;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
  end; { TOmniList }

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

const
  CNumSlots = 4*1024;
  CBlockSize = SizeOf(TOmniTaggedValue) * CNumSlots; //64 KB

{ TOmniTaggedValue }

function TOmniTaggedValue.CASTag(oldTag, newTag: TOmniCollectionTag): boolean;
var
  newValue: DWORD;
  oldValue: DWORD;
begin
  oldValue := PDWORD(@Tag)^ AND $FFFFFF00 OR DWORD(ORD(oldTag));
  newValue := oldValue AND $FFFFFF00 OR DWORD(Ord(newTag));
  {$IFDEF Debug}
  Assert(cardinal(@Tag) mod 4 = 0);
  {$ENDIF}
  Result := CAS32(oldValue, newValue, Tag);
end; { TOmniTaggedValue.CASTag }

{ TOmniCollection }

constructor TOmniCollection.Create;
begin
  inherited;
  Assert(cardinal(ocHeadPointer) mod 4 = 0);
  Assert(cardinal(ocTailPointer) mod 4 = 0);
  ocHeadPointer := AllocateBlock;
  ocTailPointer := ocHeadPointer;
  ocGarbage := TList.Create;
end; { TOmniCollection.Create }

function TOmniCollection.Dequeue: TOmniValue;
begin
  if not TryDequeue(Result) then
    raise Exception.Create('TOmniCollection.Dequeue: Message queue is empty');
end; { TOmniCollection.Dequeue }

destructor TOmniCollection.Destroy;
begin
  while ocTailPointer.Tag <> tagEndOfList do
    Inc(ocTailPointer);
  AddToQC(ocTailPointer);
  CleanupGC;
  FreeAndNil(ocGarbage);
  FreeMem(ocCachedBlock);
  inherited;
end; { TOmniCollection.Destroy }

procedure TOmniCollection.AddToQC(lastSlot: POmniTaggedValue);
begin
  {$IFDEF Debug}
  Assert(lastSlot^.Tag in [tagEndOfList, tagDestroying]);
  {$ENDIF Debug}
  Dec(lastSlot, CNumSlots - 1); // calculate block address from the address of the last slot
  ocGarbage.Add(lastSlot);
  ocHasGCBlocks := true;
end; { TOmniCollection.AddToQC }

function TOmniCollection.AllocateBlock: pointer;
var
  iItem      : integer;
  pTaggedItem: POmniTaggedValue;
begin
  if assigned(ocCachedBlock) then begin
    {$IFDEF DEBUG}NumReusedAlloc.Increment;{$ENDIF DEBUG}
    Result := ocCachedBlock;
    ocCachedBlock := nil;
    ZeroMemory(Result, CBlockSize);
  end
  else begin
    {$IFDEF DEBUG}NumTrueAlloc.Increment;{$ENDIF DEBUG}
    Result := AllocMem(CBlockSize);
  end;
  pTaggedItem := Result;
  for iItem := 1 to (CBlockSize div SizeOf(TOmniTaggedValue)) - 1 do begin
    pTaggedItem^.tag := tagFree;
    Inc(pTaggedItem);
  end;
  pTaggedItem^.tag := tagEndOfList;
  {$IFDEF Debug}
  Assert((cardinal(pTaggedItem) - cardinal(Result)) = (CBlockSize - SizeOf(TOmniTaggedValue)));
  {$ENDIF}
end; { TOmniCollection.AllocateBlock }

procedure TOmniCollection.CleanupGC;
var
  pBlock: pointer;
begin
  if not ocHasGCBlocks then
    Exit;
  if not TryEnterWriter then
    Exit;
  for pBlock in ocGarbage do begin
    if not assigned(ocCachedBlock) then
      ocCachedBlock := pBlock
    else begin
      //DumpBlock(pBlock);
      FreeMem(pBlock);
    end;
  end;
  ocGarbage.Clear;
  ocHasGCBlocks := false;
  LeaveWriter;
end; { TOmniCollection.CleanupGC }

procedure TOmniCollection.Enqueue(const value: TOmniValue);
var
  extension: POmniTaggedValue;
  tag      : TOmniCollectionTag;
  tail     : POmniTaggedValue;
begin
  CleanupGC;
  EnterReader;
  repeat
    tail := ocTailPointer;
    tag := tail^.tag;
    if tag = tagFree then begin
      if tail^.CASTag(tag, tagAllocating) then
        break //repeat
      {$IFDEF DEBUG}else LoopEnqFree.Increment; {$ENDIF DEBUG}
    end
    else if tag = tagEndOfList then begin
      if tail^.CASTag(tag, tagExtending) then
        break //repeat
      {$IFDEF DEBUG}else LoopEnqEOL.Increment; {$ENDIF DEBUG}
    end
    else if tag = tagExtending then begin
      {$IFDEF DEBUG} LoopEnqExtending.Increment; {$ENDIF DEBUG}
      DSIYield;
    end
    else begin
      {$IFDEF DEBUG} LoopEnqOther.Increment; {$ENDIF DEBUG}
      asm pause; end;
    end;
  until false;
  {$IFDEF DEBUG} Assert(tail = ocTailPointer); NumEnqueued.Increment; {$ENDIF DEBUG}
  if tag = tagFree then begin // enqueueing
    Inc(ocTailPointer); // release the lock
    asm sfence; end;
    tail^.Value := value; // this works because the slot was initialized to zero when allocating
    {$IFDEF DEBUG}
    if not tail^.CASTag(tagAllocating, tagAllocated) then
      raise Exception.Create('Internal error');
    {$ELSE}
    tail^.Tag := tagAllocated;
    {$ENDIF DEBUG}
  end
  else begin // allocating memory
    extension := AllocateBlock;
    {$IFDEF DEBUG}
    if not extension^.CASTag(tagFree, tagAllocated) then
      raise Exception.Create('Internal error');
    {$ELSE}
    extension^.Tag := tagAllocated;
    {$ENDIF DEBUG}
    extension^.Value := value;  // this works because the slot was initialized to zero when allocating
    Inc(extension);             // skip allready allocated slot
    ocTailPointer := extension; // release the lock
    asm sfence; end;
    Dec(extension);             // link must point to the first slot
    tail^.Value := cardinal(extension);
    {$IFDEF DEBUG}
    if not tail^.CASTag(tagExtending, tagBlockPointer) then
      raise Exception.Create('Internal error');
    {$ELSE}
    tail^.Tag := tagBlockPointer;
    {$ENDIF DEBUG}
  end;
  LeaveReader;
end; { TOmniCollection.Enqueue }

procedure TOmniCollection.DumpBlock(pBlock: POmniTaggedValue);
var
  f: textfile;
  i: integer;
begin
  ocBlockCount := ocBlockCount + 1;
  AssignFile(f, Format('block_%.8x_%.3d.txt', [cardinal(Self), ocBlockCount]));
  Rewrite(f);
  for i := 1 to (CBlockSize div SizeOf(TOmniTaggedValue)) do begin
    Writeln(f, Ord(pBlock^.Tag), ' ', integer(pBlock^.Value));
    Inc(pBlock);
  end;
  CloseFile(f);
end; { TOmniCollection.DumpBlock }

procedure TOmniCollection.EnterReader;
var
  value: integer;
begin
  repeat
    value := ocRemoveCount.Value;
    if value >= 0 then
      if ocRemoveCount.CAS(value, value + 1) then
        break
      {$IFDEF DEBUG}else LoopReader.Increment {$ENDIF DEBUG}
    else begin
      DSiYield; // let the GC do its work
      {$IFDEF DEBUG} LoopGC.Increment; {$ENDIF DEBUG}
    end;
  until false;
end; { TOmniCollection.EnterReader }

procedure TOmniCollection.LeaveReader;
begin
  ocRemoveCount.Decrement;
end; { TOmniCollection.LeaveReader }

procedure TOmniCollection.LeaveWriter;
begin
  ocRemoveCount.Value := 0;
end; { TOmniCollection.LeaveWriter }

function TOmniCollection.TryDequeue(var value: TOmniValue): boolean;
var
  head: POmniTaggedValue;
  next: POmniTaggedValue;
  tag : TOmniCollectionTag;
begin
  Result := true;
  CleanupGC;
  EnterReader;
  repeat
    head := ocHeadPointer;
    tag := head^.Tag;
    if tag = tagFree then begin
      Result := false;
      break; //repeat
    end
    else if tag in [tagAllocated, tagBlockPointer] then begin
      if head^.CASTag(tag, tagRemoving) then
        break //repeat
      {$IFDEF DEBUG}else LoopDeqAllocated.Increment; {$ENDIF DEBUG}
    end
    else if tag = tagRemoving then begin
      {$IFDEF DEBUG} LoopDeqRemoving.Increment; {$ENDIF DEBUG}
      DSiYield;
    end
    else begin
      {$IFDEF DEBUG} LoopDeqOther.Increment; {$ENDIF DEBUG}
      asm pause; end;
    end;
  until false;
  if Result then begin // dequeueing
    {$IFDEF DEBUG}
    Assert(head = ocHeadPointer);
    {$ENDIF DEBUG}
    if tag = tagAllocated then begin
      Inc(ocHeadPointer); // release the lock
      asm sfence; end;
      value := head^.Value;
      if value.IsInterface then begin
        head^.Value.AsInterface._Release;
        head^.Value.RawZero;
      end;
      {$IFDEF DEBUG}
      NumDequeued.Increment;
      if not head^.CASTag(tagRemoving, tagRemoved) then
        raise Exception.Create('Internal error');
      {$ELSE}
      head^.Tag := tagRemoved;
      {$ENDIF DEBUG}
    end
    else begin // releasing memory
      // if tail pointer is incremented too soon, another thread may release the block we are working on before we are finished
      next := POmniTaggedValue(cardinal(head^.Value));
      if next^.Tag = tagAllocated then begin
        value := next^.Value;
        if value.IsInterface then begin
          next^.Value.AsInterface._Release;
          next^.Value.RawZero;
        end;
        {$IFDEF DEBUG}
        NumDequeued.Increment;
        if not next^.CASTag(tagAllocated, tagRemoved) then
          raise Exception.Create('Internal error');
        if not head^.CASTag(tagRemoving, tagDestroying) then
          raise Exception.Create('Internal error');
        {$ELSE}
        next^.Tag := tagRemoved;
        head^.Tag := tagDestroying;
        {$ENDIF DEBUG}
        Inc(next);             // skip the first slot, it's ours
        ocHeadPointer := next; // release the lock
        asm sfence; end;
//        Dec(next);             // move back to our slot
      end
      else begin
        {$IFDEF DEBUG}
        if not head^.CASTag(tagRemoving, tagDestroying) then
          raise Exception.Create('Internal error');
        {$ELSE}
        head^.Tag := tagDestroying;
        {$ENDIF DEBUG}
        ocHeadPointer := next; // release the lock
        asm sfence; end;
      end;
      AddToQC(head);
    end;
  end;
  LeaveReader;
end; { TOmniCollection.TryDequeue }

function TOmniCollection.TryEnterWriter: boolean;
begin
  Result := (ocRemoveCount.Value = 0) and (ocRemoveCount.CAS(0, -1));
end; { TOmniCollection.TryEnterWriter }

initialization
  Assert(SizeOf(TOmniValue) = 13);
  Assert(SizeOf(TOmniTaggedValue) = 16);
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(CBlockSize = 65536);
//  DSiDeleteFiles(GetCurrentDir, 'block*.txt');
end.
