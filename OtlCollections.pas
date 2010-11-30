///<summary>Locking/blocking collections. Part of the OmniThreadLibrary project.</summary>
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
///   Creation date     : 2009-12-27
///   Last modification : 2010-11-30
///   Version           : 1.03c
///</para><para>
///   History:
///     1.03c: 2010-11-30
///       - Fixed deadlock condition in TryAdd when throttling was used.
///     1.03b: 2010-11-22
///       - Fixed an algorithm problem in TryTake. If a reader waited in TryTake for
///         something to happen and then writer scheduled a value to the blocking
///         collection and immediately called CompleteAdding, reader sometimes returned
///         from TryTake with status False (collection completed) without returning the
///         value waiting in the queue.
///       - Implemented TOmniBlockingCollection.Next.
///       - Implemented throttling support.
///     1.03a: 2010-07-21
///       - TOmniBlockingCollection.TryTake was broken. When two threads were waiting in
///         TryTake at the same time, first thread to complete the wait would remove
///         observer from the queue and that would cause next Add *not* to wake the other
///         waiting thread.
///     1.03: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.02: 2010-01-14
///       - Small changes required for the interoperability with the Parallel.ForEach.
///     1.01a: 2010-01-05
///       - Better behaviour when running on a single core.
///     1.01: 2009-12-30
///       - Number of producer/consumers can be passed to TOmniBlockingCollection
///         constructor. TryTake will then detect the deadlock state when all producer/
///         consumers are inside the TryTake and the collection is empty and will
///         automatically complete the collection.
///     1.0: 2009-12-29
///       - Released.
///</para></remarks>

unit OtlCollections;

{$I OtlOptions.inc}

interface

uses
  Windows,
  SysUtils,
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlContainers,
  OtlContainerObserver,
  OtlSync;

type
  ECollectionCompleted = class(Exception);

  ///<summary>Blocking collection
  ///- http://blogs.msdn.com/pfxteam/archive/2009/11/06/9918363.aspx
  ///- http://msdn.microsoft.com/en-us/library/dd267312(VS.100).aspx
  ///</summary>
  IOmniBlockingCollection = interface ['{208EFA15-1F8F-4885-A509-B00191145D38}']
    procedure Add(const value: TOmniValue);
    procedure CompleteAdding;
    function  GetEnumerator: IOmniValueEnumerator;
    function  IsCompleted: boolean;
    function  Next: TOmniValue;
    procedure SetThrottling(highWatermark, lowWatermark: integer);
    function  Take(var value: TOmniValue): boolean;
    function  TryAdd(const value: TOmniValue): boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
  end; { IOmniBlockingCollection }

  TOmniBlockingCollection = class(TInterfacedObject,
                                  IOmniBlockingCollection,
                                  IOmniValueEnumerable)
  strict private
    obcAccessed       : boolean;
    obcApproxCount    : TGp4AlignedInt;
    obcCollection     : TOmniQueue;
    obcCompleted      : boolean;
    obcCompletedSignal: TDSiEventHandle;
    obcHighWaterMark  : integer;
    obcLowWaterMark   : integer;
    obcNotOverflow    : THandle {event};
    obcObserver       : TOmniContainerWindowsEventObserver;
    obcResourceCount  : TOmniResourceCount;
    obcThrottling     : boolean;
  public
    constructor Create(numProducersConsumers: integer = 0);
    destructor  Destroy; override;
    procedure Add(const value: TOmniValue); inline;
    procedure CompleteAdding; inline;
    function  GetEnumerator: IOmniValueEnumerator; inline;
    function  IsCompleted: boolean; inline;
    function  Next: TOmniValue;
    procedure SetThrottling(highWaterMark, lowWaterMark: integer);
    function  Take(var value: TOmniValue): boolean; inline;
    function  TryAdd(const value: TOmniValue): boolean; inline;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
    property CompletedSignal: THandle read obcCompletedSignal;
  end; { TOmniBlockingCollection }

  TOmniBlockingCollectionEnumerator = class(TInterfacedObject, IOmniValueEnumerator)
  strict private
    obceCollection_ref: TOmniBlockingCollection;
    obceValue         : TOmniValue;
  public
    constructor Create(collection: TOmniBlockingCollection);
    function  GetCurrent: TOmniValue; inline;
    function  MoveNext: boolean; inline;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal): boolean;
    property Current: TOmniValue read GetCurrent;
  end; { TOmniBlockingCollectionEnumerator }

implementation

uses
  Classes;

{ TOmniBlockingCollectionEnumerator }

constructor TOmniBlockingCollectionEnumerator.Create(collection: TOmniBlockingCollection);
begin
  obceCollection_ref := collection;
end; { TOmniBlockingCollectionEnumerator.Create }

function TOmniBlockingCollectionEnumerator.GetCurrent: TOmniValue;
begin
  Result := obceValue;
end; { TOmniBlockingCollectionEnumerator.GetCurrent }

function TOmniBlockingCollectionEnumerator.MoveNext: boolean;
begin
  Result := obceCollection_ref.Take(obceValue);
end; { TOmniBlockingCollectionEnumerator.MoveNext }

function TOmniBlockingCollectionEnumerator.TryTake(var value: TOmniValue;
  timeout_ms: cardinal): boolean;
begin
  Result := obceCollection_ref.TryTake(value, timeout_ms);
end; { TOmniBlockingCollectionEnumerator.TryTake }

{ TOmniBlockingCollection }

///If numProducersConsumers > 0, collection will automatically enter 'completed' state
///when this number of .Take calls is simultaneously blocked because the collection is
///empty.
constructor TOmniBlockingCollection.Create(numProducersConsumers: integer);
begin
  inherited Create;
  if numProducersConsumers > 0 then
    obcResourceCount := TOmniResourceCount.Create(numProducersConsumers);
  obcCollection := TOmniQueue.Create;
  obcCompletedSignal := CreateEvent(nil, true, false, nil);
  obcObserver := CreateContainerWindowsEventObserver;
  obcCollection.ContainerSubject.Attach(obcObserver, coiNotifyOnAllInserts);
  obcNotOverflow := CreateEvent(nil, true, true, nil);
end; { TOmniBlockingCollection.Create }

destructor TOmniBlockingCollection.Destroy;
begin
  DSiCloseHandleAndNull(obcNotOverflow);
  if assigned(obcCollection) and assigned(obcObserver) then
    obcCollection.ContainerSubject.Detach(obcObserver, coiNotifyOnAllInserts);
  FreeAndNil(obcObserver);
  DSiCloseHandleAndNull(obcCompletedSignal);
  FreeAndNil(obcCollection);
  FreeAndNil(obcResourceCount);
  inherited Destroy;
end; { TOmniBlockingCollection.Destroy }

procedure TOmniBlockingCollection.Add(const value: TOmniValue);
begin
  if not TryAdd(value) then
    raise ECollectionCompleted.Create('Adding to completed collection');
end; { TOmniBlockingCollection.Add }

procedure TOmniBlockingCollection.CompleteAdding;
begin
  if not obcCompleted then begin
    obcCompleted := true;
    Win32Check(SetEvent(obcCompletedSignal));
  end;
end; { TOmniBlockingCollection.CompleteAdding }

function TOmniBlockingCollection.GetEnumerator: IOmniValueEnumerator;
begin
  Result := TOmniBlockingCollectionEnumerator.Create(Self);
end; { TOmniBlockingCollection.GetEnumerator }

function TOmniBlockingCollection.IsCompleted: boolean;
begin
  Result := obcCompleted;
end; { TOmniBlockingCollection.IsCompleted }

function TOmniBlockingCollection.Next: TOmniValue;
begin
  if not Take(Result) then
    raise ECollectionCompleted.Create('Collection is empty');
end; { TOmniBlockingCollection.Next }

///<summary>When throttling is set, Add will block if there is >= highWaterMark elements
///  in the queue. It will only unblock when number of elements drops below lowWaterMark.</summary>
procedure TOmniBlockingCollection.SetThrottling(highWaterMark, lowWaterMark: integer);
begin
  if obcAccessed then
    raise Exception.Create('Throttling cannot be set once the blocking collection has been used');
  Assert(lowWaterMark <= highWaterMark);
  obcHighWaterMark := highWaterMark;
  obcLowWaterMark := lowWaterMark;
  obcThrottling := true;
end; { TOmniBlockingCollection.SetThrottling }

function TOmniBlockingCollection.Take(var value: TOmniValue): boolean;
begin
  Result := TryTake(value, INFINITE);
end; { TOmniBlockingCollection.Take }

function TOmniBlockingCollection.TryAdd(const value: TOmniValue): boolean;
begin
  // CompleteAdding and TryAdd are not synchronised
  Result := not obcCompleted;
  if Result then begin
    obcAccessed := true;
    if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
      Win32Check(ResetEvent(obcNotOverflow));
      // it's possible that messages were removed and obcNotOverflow set *before* the
      // previous line has executed so test again ...
      if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
        if DSiWaitForTwoObjects(obcCompletedSignal, obcNotOverflow, false, INFINITE) = WAIT_OBJECT_0 then begin
          Result := false; // completed
          Exit;
        end;
      end;
    end;
    obcCollection.Enqueue(value);
    if obcThrottling then
      obcApproxCount.Increment;
  end;
end; { TOmniBlockingCollection.TryAdd }

function TOmniBlockingCollection.TryTake(var value: TOmniValue;
  timeout_ms: cardinal): boolean;
var
  awaited    : DWORD;
  startTime  : int64;
  waitHandles: array [0..2] of THandle;

  function Elapsed: boolean;
  begin
    if timeout_ms = INFINITE then
      Result := false
    else
      Result := (startTime + timeout_ms) < DSiTimeGetTime64;
  end; { Elapsed }

  function TimeLeft_ms: DWORD;
  var
    intTime: integer;
  begin
    if timeout_ms = INFINITE then
      Result := INFINITE
    else begin
      intTime := startTime + timeout_ms - DSiTimeGetTime64;
      if intTime < 0 then
        Result := 0
      else
        Result := intTime;
    end;
  end; { TimeLeft }

begin { TOmniBlockingCollection.TryTake }
  if obcCollection.TryDequeue(value) then
    Result := true
  else begin
    if assigned(obcResourceCount) then
      obcResourceCount.Allocate;
    try
      startTime := DSiTimeGetTime64;
      waitHandles[0] := obcCompletedSignal;
      waitHandles[1] := obcObserver.GetEvent;
      if assigned(obcResourceCount) then
        waitHandles[2] := obcResourceCount.Handle;
      Result := false;
      while not (IsCompleted or Elapsed) do begin
        if obcCollection.TryDequeue(value) then begin
          Result := true;
          break; //while
        end;
        awaited := WaitForMultipleObjects(2 + Ord(assigned(obcResourceCount)),
                     @waitHandles, false, TimeLeft_ms);
        if awaited <> WAIT_OBJECT_1 then begin
          if awaited = WAIT_OBJECT_2 then
            CompleteAdding;
          Result := false;
          break; //while
        end;
      end;
      if not Result then // there may still be data in completed queue
        Result := obcCollection.TryDequeue(value);
    finally
      if assigned(obcResourceCount) then
        obcResourceCount.Release;
    end;
  end;
  if obcThrottling and Result then begin
    obcApproxCount.Decrement;
    if obcApproxCount.Value <= obcLowWaterMark then
      SetEvent(obcNotOverflow);
  end;
end; { TOmniBlockingCollection.TryTake }

end.

