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
///   Last modification : 2009-12-24
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2009-12-24
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
  tagRemoving
  tagRemoved
  tagEndOfList
  tagExtending
  tagBlockPointer
  tagDestroying
  tagDestroyed

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
    store tagRemoved
  else
    if first slot in new block is allocated
      set head to new block's slot 1
      get value
    else
      set head to new block
    leave GC
    store tagDestroying
    writelock GC
    release original block
    leave GC
    exit

  leave GC
*)

interface

uses
  Classes,
  GpStuff,
  OtlCommon,
  OtlSync;

type
  TOmniCollectionTag = (tagFree, tagAllocating, tagAllocated, tagRemoving, tagRemoved,
    tagEndOfList, tagExtending, tagBlockPointer, tagDestroying, tagDestroyed
    {$IFDEF DEBUG}, tagStartOfList, tagSentinel{$ENDIF});

  TOmniTaggedValue = packed record
    Tag     : TOmniCollectionTag;
    Stuffing: word;
    Value   : TOmniValue;
    function CASTag(oldTag, newTag: TOmniCollectionTag): boolean;
  end; { TOmniTaggedValue }
  POmniTaggedValue = ^TOmniTaggedValue;

  ///<summary>Dynamically allocated, O(1) enqueue and dequeue, threadsafe, microlocking queue.</summary>
  TOmniCollection = class
  strict private // keep 4-aligned
    ocCachedBlock: POmniTaggedValue;
    ocHeadPointer: POmniTaggedValue;
    ocTailPointer: POmniTaggedValue;
  strict private
    ocRemoveCount: TGp4AlignedInt;
  strict protected
    function  AllocateBlock: POmniTaggedValue;
    procedure EnterReader; 
    procedure LeaveReader; inline;
    procedure LeaveWriter; inline;
    procedure ReleaseBlock(lastSlot: POmniTaggedValue; forceFree: boolean = false);
    procedure EnterWriter; 
    procedure WaitForAllRemoved(const lastSlot: POmniTaggedValue);
  {$IFDEF DEBUG}
  public
    LoopDeqAllocated: TGp4AlignedInt;
    LoopDeqOther    : TGp4AlignedInt;
    LoopEnqEOL      : TGp4AlignedInt;
    LoopEnqExtending: TGp4AlignedInt;
    LoopEnqFree     : TGp4AlignedInt;
    LoopEnqOther    : TGp4AlignedInt;
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
  CNumSlots = 4*1024 {$IFDEF DEBUG} - 3 {$ENDIF};
  CBlockSize = SizeOf(TOmniTaggedValue) * CNumSlots; //64 KB

{ TOmniTaggedValue }

function TOmniTaggedValue.CASTag(oldTag, newTag: TOmniCollectionTag): boolean;
var
  newValue: DWORD;
  oldValue: DWORD;
begin
  oldValue := PDWORD(@Tag)^ AND $FFFFFF00 OR DWORD(ORD(oldTag));
  newValue := oldValue AND $FFFFFF00 OR DWORD(Ord(newTag));
  {$IFDEF Debug} Assert(cardinal(@Tag) mod 4 = 0); {$ENDIF}
  Result := CAS32(oldValue, newValue, Tag);
end; { TOmniTaggedValue.CASTag }

{ TOmniCollection }

constructor TOmniCollection.Create;
begin
  inherited;
  Assert(cardinal(ocHeadPointer) mod 4 = 0);
  Assert(cardinal(ocTailPointer) mod 4 = 0);
  Assert(cardinal(ocCachedBlock) mod 4 = 0);
  ocHeadPointer := AllocateBlock;
  ocTailPointer := ocHeadPointer;
end; { TOmniCollection.Create }

function TOmniCollection.Dequeue: TOmniValue;
begin
  if not TryDequeue(Result) then
    raise Exception.Create('TOmniCollection.Dequeue: Message queue is empty');
end; { TOmniCollection.Dequeue }

destructor TOmniCollection.Destroy;
var
  pBlock: POmniTaggedValue;
begin
  while assigned(ocHeadPointer) do begin
    if ocHeadPointer.Tag in [tagBlockPointer, tagEndOfList] then begin
      pBlock := ocHeadPointer;
      ocHeadPointer := POmniTaggedValue(ocHeadPointer.Value.AsPointer);
      ReleaseBlock(pBlock, true);
    end
    else
      Inc(ocHeadPointer);
  end;
  if assigned(ocCachedBlock) then
    FreeMem(ocCachedBlock);
  inherited;
end; { TOmniCollection.Destroy }

function TOmniCollection.AllocateBlock: POmniTaggedValue;
var
  cached: POmniTaggedValue;
  pEOL  : POmniTaggedValue;
begin
  cached := ocCachedBlock;
  if assigned(cached) and CAS32(cached, nil, ocCachedBlock) then begin
    {$IFDEF DEBUG}NumReusedAlloc.Increment;{$ENDIF DEBUG}
    Result := cached;
    ZeroMemory(Result, CBlockSize {$IFDEF DEBUG} + 3*SizeOf(TOmniTaggedValue){$ENDIF});
  end
  else begin
    {$IFDEF DEBUG}NumTrueAlloc.Increment;{$ENDIF DEBUG}
    Result := AllocMem(CBlockSize {$IFDEF DEBUG} + 3*SizeOf(TOmniTaggedValue){$ENDIF});
  end;
  Assert(Ord(tagFree) = 0);
  {$IFDEF DEBUG}
  Assert(Result^.Tag = tagFree);
  Result^.Tag := tagSentinel;
  Inc(Result);
  Assert(Result^.Tag = tagFree);
  Result^.Tag := tagStartOfList;
  Inc(Result, CNumSlots + 1);
  Assert(Result^.Tag = tagFree);    
  Result^.Tag := tagSentinel;
  Dec(Result, CNumSlots);
  {$ENDIF}
  pEOL := Result;
  Inc(pEOL, CNumSlots - 1);
  {$IFDEF DEBUG} Assert(Result^.Tag = tagFree); {$ENDIF}
  pEOL^.tag := tagEndOfList;
end; { TOmniCollection.AllocateBlock }

procedure TOmniCollection.Enqueue(const value: TOmniValue);
var
  extension: POmniTaggedValue;
  tag      : TOmniCollectionTag;
  tail     : POmniTaggedValue;
begin
  EnterReader;
  repeat
    tail := ocTailPointer;
    tag := tail^.tag;
    if tag = tagFree then begin
      if tail^.CASTag(tag, tagAllocating) then
        break //repeat
      {$IFDEF DEBUG}else LoopEnqFree.Increment; {$ENDIF}
    end
    else if tag = tagEndOfList then begin
      if tail^.CASTag(tag, tagExtending) then
        break //repeat
      {$IFDEF DEBUG}else LoopEnqEOL.Increment; {$ENDIF}
    end
    else if tag = tagExtending then begin
      {$IFDEF DEBUG} LoopEnqExtending.Increment; {$ENDIF}
      DSIYield;
    end
    else begin
      {$IFDEF DEBUG} LoopEnqOther.Increment; {$ENDIF}
      asm pause; end;
    end;
  until false;
  {$IFDEF DEBUG} Assert(tail = ocTailPointer); {$ENDIF}
  if tag = tagFree then begin // enqueueing
    Inc(ocTailPointer); // release the lock
    tail^.Value := value; // this works because the slot was initialized to zero when allocating
    {$IFDEF DEBUG} tail^.Stuffing := GetCurrentThreadID AND $FFFF; {$ENDIF}
    {$IFNDEF DEBUG} tail^.Tag := tagAllocated; {$ELSE} Assert(tail^.CASTag(tagAllocating, tagAllocated)); {$ENDIF}
  end
  else begin // allocating memory
    {$IFDEF DEBUG} Assert(tag = tagEndOfList); {$ENDIF}
    extension := AllocateBlock;
    Inc(extension);             // skip allocated slot
    ocTailPointer := extension; // release the lock
    Dec(extension);
    {$IFDEF DEBUG} // create backlink
    Dec(extension);
    extension^.Value.AsPointer := tail;
    Inc(extension);
    {$ENDIF}
    {$IFNDEF DEBUG} extension^.Tag := tagAllocated; {$ELSE} Assert(extension^.CASTag(tagFree, tagAllocated)); {$ENDIF}
    extension^.Value := value;  // this works because the slot was initialized to zero when allocating
    tail^.Value := extension;
    {$IFNDEF DEBUG} tail^.Tag := tagBlockPointer; {$ELSE} Assert(tail^.CASTag(tagExtending, tagBlockPointer)); {$ENDIF DEBUG}
  end;
  LeaveReader;
end; { TOmniCollection.Enqueue }

procedure TOmniCollection.EnterReader;
var
  value: integer;
begin
  repeat
    value := ocRemoveCount.Value;
    if value >= 0 then
      if ocRemoveCount.CAS(value, value + 1) then
        break
    else 
      DSiYield; // let the GC do its work
  until false;
end; { TOmniCollection.EnterReader }

procedure TOmniCollection.EnterWriter;
begin
  while not ((ocRemoveCount.Value = 0) and (ocRemoveCount.CAS(0, -1))) do
    asm pause; end;
end; { TOmniCollection.EnterWriter }

procedure TOmniCollection.LeaveReader;
begin
  ocRemoveCount.Decrement;
end; { TOmniCollection.LeaveReader }

procedure TOmniCollection.LeaveWriter;
begin
  ocRemoveCount.Value := 0;
end; { TOmniCollection.LeaveWriter }

procedure TOmniCollection.ReleaseBlock(lastSlot: POmniTaggedValue; forceFree: boolean);
begin
  {$IFDEF DEBUG}
  Inc(lastSlot);
  Assert(lastSlot^.Tag = tagSentinel);
  Dec(lastSlot);
  {$ENDIF}
  Dec(lastSlot, CNumSlots - 1);
  {$IFDEF DEBUG}
  Dec(lastSlot);
  Assert(lastSlot^.Tag = tagStartOfList);
  Dec(lastSlot);
  Assert(lastSlot^.Tag = tagSentinel);
  {$ENDIF};
  if forceFree or assigned(ocCachedBlock) or (not CAS32(nil, lastSlot, ocCachedBlock)) then
    FreeMem(lastSlot);
end; { TOmniCollection.ReleaseBlock }

function TOmniCollection.TryDequeue(var value: TOmniValue): boolean;
var
  head: POmniTaggedValue;
  next: POmniTaggedValue;
  tag : TOmniCollectionTag;
begin
  Result := true;
  EnterReader;
  repeat
    head := ocHeadPointer;
    tag := head^.Tag;
    if tag = tagFree then begin
      Result := false;
      break; //repeat
    end
    else if tag = tagAllocated then begin
      if head^.CASTag(tag, tagRemoving) then
        break //repeat
      {$IFDEF DEBUG}else LoopDeqAllocated.Increment; {$ENDIF}
    end
    else if tag = tagBlockPointer then begin
      if head^.CASTag(tag, tagDestroying) then
        break //repeat
      {$IFDEF DEBUG}else LoopDeqAllocated.Increment; {$ENDIF}
    end
    else begin
      {$IFDEF DEBUG} LoopDeqOther.Increment; {$ENDIF}
      DSiYield;
    end;
  until false;
  if Result then begin // dequeueing
    if tag = tagAllocated then begin
      Inc(ocHeadPointer); // release the lock
      value := head^.Value;
      if value.IsInterface then begin
        head^.Value.AsInterface._Release;
        head^.Value.RawZero;
      end;
      {$IFNDEF DEBUG} head^.Tag := tagRemoved; {$ELSE} Assert(head^.CASTag(tagRemoving, tagRemoved)); {$ENDIF}
    end
    else begin // releasing memory
      {$IFDEF DEBUG} Assert(tag = tagBlockPointer); {$ENDIF}
      next := POmniTaggedValue(head^.Value.AsPointer);
      if next^.Tag <> tagAllocated then begin
        {$IFDEF DEBUG} Assert(next^.Tag = tagFree); {$ENDIF}
        ocHeadPointer := next; // release the lock
      end
      else begin
        Inc(next);
        ocHeadPointer := next; // release the lock
        Dec(next);
        value := next^.Value;
        if value.IsInterface then begin
          next^.Value.AsInterface._Release;
          next^.Value.RawZero;
        end;
        {$IFNDEF DEBUG} next^.Tag := tagRemoved; {$ELSE} Assert(next^.CASTag(tagAllocated, tagRemoved)); {$ENDIF DEBUG}
      end;
      // At this moment, another thread may still be dequeueing from one of the previous
      // slots and memory should not yet be released!
      LeaveReader;
      {$IFNDEF DEBUG} head^.Tag := tagDestroyed; {$ELSE} Assert(head^.CASTag(tagDestroying, tagDestroyed)); {$ENDIF}
      EnterWriter;
      ReleaseBlock(head);
      LeaveWriter;
      Exit;
    end;
  end;
  LeaveReader;
end; { TOmniCollection.TryDequeue }

procedure TOmniCollection.WaitForAllRemoved(const lastSlot: POmniTaggedValue);
var
  firstRemoving: POmniTaggedValue;
  scan         : POmniTaggedValue;
  sentinel     : POmniTaggedValue;
begin
  {$IFDEF Debug}
  Assert(assigned(lastSlot));
  Assert(lastSlot^.Tag in [tagEndOfList, tagDestroying]);
  {$ENDIF Debug}
  sentinel := lastSlot;
  Dec(sentinel, CNumSlots - 1);
  repeat
    firstRemoving := nil;
    scan := lastSlot;
    Dec(scan);
    repeat
      {$IFDEF DEBUG} Assert(scan^.Tag in [tagRemoving, tagRemoved]); {$ENDIF}
      if scan^.Tag = tagRemoving then
        firstRemoving := scan;
      if scan = sentinel then
        break;
      Dec(scan);
    until false;
    sentinel := firstRemoving;
    if assigned(firstRemoving) then
      asm pause; end
    else
      break; //repeat
  until false;
end; { TOmniCollection.WaitForAllRemoved }

initialization
  Assert(SizeOf(TOmniValue) = 13);
  Assert(SizeOf(TOmniTaggedValue) = 16);
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(CBlockSize = (65536 {$IFDEF DEBUG} - 3*SizeOf(TOmniTaggedValue){$ENDIF}));
end.

