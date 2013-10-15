///<summary>Locking/blocking collections. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011 Primoz Gabrijelcic
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
///   Last modification : 2011-11-11
///   Version           : 1.06
///</para><para>
///   History:
///     1.06: 2011-11-11
///       - Implemented IOmniBlockingCollection.ContainerSubject which gives access
///         to the ContainerSubject property of the underlying TOmniQueue.
///     1.05: 2011-08-28
///       - Implemented IOmniBlockingCollection.ReraiseExceptions. If enabled
///         (default: disabled), [Try]Take will check if returned value for exception
///         (TOmniValue.IsException) and if true, it will reraise this exception
///         instead of returning a result.
///     1.04a: 2011-08-27
///       - [Try]Add works correctly when throttling is used. (Broken in 1.03d.)
///     1.04: 2011-08-26
///       - Implemented IOmniBlockingCollection.IsFinalized.
///     1.03d: 2011-08-25
///       - Fixed [Try]Add/CompleteAdding/[Try]Take three-thread race condition.
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

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Blocking collection
  ///	  <list type="bullet">
  ///	    <item><see href=
  ///	    "http://blogs.msdn.com/pfxteam/archive/2009/11/06/9918363.aspx" /></item>
  ///	    <item>
  ///	    http://msdn.microsoft.com/en-us/library/dd267312(VS.100).aspx</item>
  ///	  </list>
  ///	</summary>
  {$ENDREGION}
  IOmniBlockingCollection = interface ['{208EFA15-1F8F-4885-A509-B00191145D38}']
    function  GetContainerSubject: TOmniContainerSubject;
    //
    procedure Add(const value: TOmniValue);
    procedure CompleteAdding;
    function  GetEnumerator: IOmniValueEnumerator;
    function  IsCompleted: boolean;
    ///	<summary>Collection is finalized when it is both completed (i.e. CompleteAdding
    ///	was called) and empty (TryTake would fail).</summary>
    function  IsFinalized: boolean;
    function  Next: TOmniValue;
    {$REGION 'Documentation'}
    /// <summary>If enabled (default: disabled), [Try]Take will check if returned value
    /// for exception (TOmniValue.IsException) and if true, it will reraise this exception
    /// instead of returning a result.</summary>
    {$ENDREGION}
    procedure ReraiseExceptions(enable: boolean = true);
    {$REGION 'Documentation'}
    ///	<remarks>When throttling is set, Add will block if there is &gt;=
    ///	highWaterMark elements in the queue. It will only unblock when number
    ///	of elements drops below lowWaterMark.</remarks>
    {$ENDREGION}
    procedure SetThrottling(highWatermark, lowWatermark: integer);
    function  Take(var value: TOmniValue): boolean;
    function  TryAdd(const value: TOmniValue): boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
    property ContainerSubject: TOmniContainerSubject read GetContainerSubject;
  end; { IOmniBlockingCollection }

  TOmniBlockingCollection = class(TInterfacedObject,
                                  IOmniBlockingCollection,
                                  IOmniValueEnumerable)
  const
    CCompletedFlag = $40000000; // 30-bit so we don't have cardinal/integer problems
  strict private
    obcAccessed            : boolean;
    {$REGION 'Documentation'}
    ///	<summary>Combination of 'is completed' flag and 'number of active TryAdd calls'
    ///	counter. Must be kept together as those two have to be modified atomically so
    ///	that CompleteAdding and Add/TryAdd can stay synchronized.</summary>
    {$ENDREGION}
    obcAddCountAndCompleted: TGp4AlignedInt;
    obcApproxCount         : TGp4AlignedInt;
    obcCollection          : TOmniQueue;
    obcCompletedSignal     : TDSiEventHandle;
    obcHighWaterMark       : integer;
    obcLowWaterMark        : integer;
    obcNotOverflow         : THandle {event};
    obcObserver            : TOmniContainerWindowsEventObserver;
    obcReraiseExceptions   : boolean;
    obcResourceCount       : TOmniResourceCount;
    obcThrottling          : boolean;
  protected
    function  GetContainerSubject: TOmniContainerSubject;
  public
    {$REGION 'Documentation'}
    ///	<remarks>If numProducersConsumers &gt; 0, collection will automatically
    ///	enter 'completed' state when this number of .Take calls is
    ///	simultaneously blocked because the collection is empty.</remarks>
    {$ENDREGION}
    constructor Create(numProducersConsumers: integer = 0);
    destructor  Destroy; override;
    procedure Add(const value: TOmniValue); inline;
    procedure CompleteAdding;
    function  GetEnumerator: IOmniValueEnumerator; inline;
    function  IsCompleted: boolean; inline;
    function  IsFinalized: boolean;
    function  Next: TOmniValue;
    procedure ReraiseExceptions(enable: boolean = true);
    procedure SetThrottling(highWaterMark, lowWaterMark: integer);
    function  Take(var value: TOmniValue): boolean; inline;
    function  TryAdd(const value: TOmniValue): boolean; inline;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
    property CompletedSignal: THandle read obcCompletedSignal;
    property ContainerSubject: TOmniContainerSubject read GetContainerSubject;
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

{$IFDEF CPUX64}
procedure AsmPause;
asm
  .noframe
  pause
end; { AsmPause }
{$ENDIF CPUX64}

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
  repeat
    if IsCompleted then // CompleteAdding was already called
      Exit;
    if obcAddCountAndCompleted.CAS(0, CCompletedFlag) then begin // there must be no active writers
      Win32Check(SetEvent(obcCompletedSignal)); // tell blocked readers to quit
      Exit;
    end;
    {$IFDEF CPUX64}AsmPause;{$ELSE}asm pause; end;{$ENDIF CPUX64}
  until false;
end; { TOmniBlockingCollection.CompleteAdding }

function TOmniBlockingCollection.GetContainerSubject: TOmniContainerSubject;
begin
  Result := obcCollection.ContainerSubject;
end; { TOmniBlockingCollection.GetContainerSubject }

function TOmniBlockingCollection.GetEnumerator: IOmniValueEnumerator;
begin
  Result := TOmniBlockingCollectionEnumerator.Create(Self);
end; { TOmniBlockingCollection.GetEnumerator }

function TOmniBlockingCollection.IsCompleted: boolean;
begin
  Result := (obcAddCountAndCompleted.Value AND CCompletedFlag) = CCompletedFlag;
end; { TOmniBlockingCollection.IsCompleted }

function TOmniBlockingCollection.IsFinalized: boolean;
begin
  Result := IsCompleted and obcCollection.IsEmpty;
end; { TOmniBlockingCollection.IsFinalized }

function TOmniBlockingCollection.Next: TOmniValue;
begin
  if not Take(Result) then
    raise ECollectionCompleted.Create('Collection is empty');
end; { TOmniBlockingCollection.Next }

procedure TOmniBlockingCollection.ReraiseExceptions(enable: boolean);
begin
  obcReraiseExceptions := enable;
end; { TOmniBlockingCollection.ReraiseExceptions }

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
var
  awaited: cardinal;
begin
  obcAddCountAndCompleted.Increment;
  try
    // IsCompleted can not change during the execution of this function
    Result := not IsCompleted;
    if Result then begin
      obcAccessed := true;
      if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
        Win32Check(ResetEvent(obcNotOverflow));
        // it's possible that messages were removed and obcNotOverflow set *before* the
        // previous line has executed so test again ...
        if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
          obcAddCountAndCompleted.Decrement; // Leave the Add temporarily so that CompleteAdding can succeed
          awaited := DSiWaitForTwoObjects(obcCompletedSignal, obcNotOverflow, false, INFINITE);
          obcAddCountAndCompleted.Increment; // Re-enter Add; queue may be now in 'completed' state
          if (awaited = WAIT_OBJECT_0) or IsCompleted then begin
            Result := false; // completed
            Exit;
          end;
        end;
      end;
      obcCollection.Enqueue(value);
      if obcThrottling then
        obcApproxCount.Increment;
    end;
  finally obcAddCountAndCompleted.Decrement; end;
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
  else begin // must be executed even if timeout_ms = 0 or the algorithm will break
    if assigned(obcResourceCount) then
      obcResourceCount.Allocate;
    try
      startTime := DSiTimeGetTime64;
      waitHandles[0] := obcCompletedSignal;
      waitHandles[1] := obcObserver.GetEvent;
      if assigned(obcResourceCount) then
        waitHandles[2] := obcResourceCount.Handle;
      Result := false;
      repeat
        awaited := WaitForMultipleObjects(2 + Ord(assigned(obcResourceCount)),
                     @waitHandles, false, TimeLeft_ms);
        if obcCollection.TryDequeue(value) then begin // there may still be data in completed queue
          Result := true;
          break; //repeat
        end;
        if awaited <> WAIT_OBJECT_1 then begin
          if awaited = WAIT_OBJECT_2 then
            CompleteAdding;
          Result := false;
          break; //while
        end;
      until Elapsed;
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
  if Result and obcReraiseExceptions and value.IsException then
    raise value.AsException;
end; { TOmniBlockingCollection.TryTake }

end.

