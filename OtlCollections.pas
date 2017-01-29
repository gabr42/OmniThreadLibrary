///<summary>Locking/blocking collections. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017 Primoz Gabrijelcic
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
///   Contributors      : Sean B. Durkin
///   Creation date     : 2009-12-27
///   Last modification : 2017-01-29
///   Version           : 2.0
///</para><para>
///   History:
///     2.0: 2017-01-29
///       - Implemented TOmniBlockingCollection<T>. T must be simple or managed type.
///     1.09: 2016-11-18
///       - Implemented IOmniBlockingCollection.IsEmpty.
///       - Implemented IOmniBlockingCollection.Count.
///     1.08: 2015-10-04
///       - Imported mobile support by [Sean].
///     1.07a: 2015-02-04
///       - ToArrayIntf<T> and TArrayRec<T> functionality moved to ToArray<T>.
///     1.07: 2015-02-03
///       - Implemented class functions TOmniBlockingCollection.ToArray<T>,
///         TOmniBlockingCollection.ToArrayRec<T>, and
///         TOmniBlockingCollection.ToArrayIntf<T>.
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
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  DSiWin32,
  GpStuff,
  {$ELSE}
  SyncObjs,
  {$ENDIF}
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
    function  IsEmpty: boolean;
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

  TOmniBlockingCollection<T> = class;

  TOmniBlockingCollectionEnumerator<T> = class(TInterfacedObject)
  strict private
    obceCollection_ref: TOmniBlockingCollection<T>;
    obceValue         : T;
  public
    constructor Create(collection: TOmniBlockingCollection<T>);
    function  GetCurrent: T; inline;
    function  MoveNext: boolean; inline;
    function  TryTake(var value: T; timeout_ms: cardinal): boolean;
    property Current: T read GetCurrent;
  end; { TOmniBlockingCollectionEnumerator<T> }

  TOmniBlockingCollection<T> = class(TInterfacedObject)
  const
    CCompletedFlag = $40000000; // 30-bit so we don't have cardinal/integer problems
  strict private
    obcAccessed            : boolean;
    {$REGION 'Documentation'}
    ///	<summary>Combination of 'is completed' flag and 'number of active TryAdd calls'
    ///	counter. Must be kept together as those two have to be modified atomically so
    ///	that CompleteAdding and Add/TryAdd can stay synchronized.</summary>
    {$ENDREGION}
    obcAddCountAndCompleted: TOmniAlignedInt32;
    obcApproxCount         : TOmniAlignedInt32;
    obcCollection          : TOmniQueue<T>;
    obcCompletedSignal     : TOmniTransitionEvent;
    obcHighWaterMark       : integer;
    obcLowWaterMark        : integer;
    obcNotOverflow         : TOmniTransitionEvent;
    obcObserver            : {$IFDEF MSWINDOWS}TOmniContainerWindowsEventObserver{$ELSE}TOmniContainerEventObserver{$ENDIF};
    obcResourceCount       : IOmniResourceCount;
    obcThrottling          : boolean;
    {$IFNDEF MSWINDOWS}
    FCompletedWaiter       : TSynchroWaitFor;
    FTakableWaiter         : TSynchroWaitFor;
    {$ENDIF}
  protected
    function  Elapsed(startTime: int64; timeout_ms: cardinal): boolean;
    function  GetApproxCount: integer; inline;
    function  GetContainerSubject: TOmniContainerSubject;
    function  TimeLeft_ms(startTime: int64; timeout_ms: cardinal): DWORD;
  public
    {$REGION 'Documentation'}
    ///	<remarks>If numProducersConsumers &gt; 0, collection will automatically
    ///	enter 'completed' state when this number of .Take calls is
    ///	simultaneously blocked because the collection is empty.</remarks>
    {$ENDREGION}
    constructor Create(numProducersConsumers: integer = 0; blockSize: integer = 65536;
      numCachedBlocks: integer = 4);
    destructor  Destroy; override;
    procedure Add(const value: T); inline;
    procedure CompleteAdding;
    function  GetEnumerator: TOmniBlockingCollectionEnumerator<T>; inline;
    function  IsCompleted: boolean; inline;
    function  IsEmpty: boolean; inline;
    function  IsFinalized: boolean;
    function  Next: T;
    procedure SetThrottling(highWaterMark, lowWaterMark: integer);
    function  Take(var value: T): boolean; inline;
    function  ToArray: TArray<T>;
    function  TryAdd(const value: T): boolean; inline;
    function  TryTake(var value: T; timeout_ms: cardinal = 0): boolean;
    property CompletedSignal: TOmniTransitionEvent read obcCompletedSignal;
    property ContainerSubject: TOmniContainerSubject read GetContainerSubject;
    property Count: integer read GetApproxCount;
  end; { TOmniBlockingCollection<T> }

  TOmniBlockingCollection = class(TOmniBlockingCollection<TOmniValue>,
                                  IOmniBlockingCollection,
                                  IOmniValueEnumerable)
  strict private
    obcReraiseExceptions: boolean;
  public
    class function ToArray<T>(coll: IOmniBlockingCollection): TArray<T>;
    function  GetEnumerator: IOmniValueEnumerator; inline;
    procedure ReraiseExceptions(enable: boolean = true);
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
  end; { TOmniBlockingCollection }

  TOmniBlockingCollectionEnumerator = class(TOmniBlockingCollectionEnumerator<TOmniValue>,
                                            IOmniValueEnumerator)
  public
  end; { TOmniBlockingCollectionEnumerator }

  PInterface = ^IInterface;

{$IFDEF OTL_Generics}{$IFDEF OTL_HasArrayOfT}{$IFDEF OTL_ERTTI}
  //compiler requires it to be public
  function Clamp(value: integer): integer;
{$ENDIF}{$ENDIF}{$ENDIF}

implementation

uses
  Classes,
  {$IFNDEF MSWINDOWS}
  Diagnostics,
  {$ENDIF ~MSWINDOWS}
  TypInfo;

{$IFDEF MSWINDOWS}
{$IFDEF CPUX64}
procedure AsmPause;
asm
  .noframe
  pause
end; { AsmPause }
{$ENDIF CPUX64}
{$ENDIF}

function Clamp(value: integer): integer; inline;
const
  CMinIncrement = 1024;
  CMaxIncrement = 65536;
begin
  if value < CMinIncrement then
    Result := CMinIncrement
  else if value > CMaxIncrement then
    Result := CMaxIncrement
  else
    Result := (((value - 1) div CMinIncrement) + 1) * CMinIncrement;
end; { Clamp }

{ TOmniBlockingCollectionEnumerator<T> }

constructor TOmniBlockingCollectionEnumerator<T>.Create(collection: TOmniBlockingCollection<T>);
begin
  obceCollection_ref := collection;
end; { TOmniBlockingCollectionEnumerator<T>.Create }

function TOmniBlockingCollectionEnumerator<T>.GetCurrent: T;
begin
  Result := obceValue;
end; { TOmniBlockingCollectionEnumerator<T>.GetCurrent }

function TOmniBlockingCollectionEnumerator<T>.MoveNext: boolean;
begin
  Result := obceCollection_ref.Take(obceValue);
end; { TOmniBlockingCollectionEnumerator<T>.MoveNext }

function TOmniBlockingCollectionEnumerator<T>.TryTake(var value: T;
  timeout_ms: cardinal): boolean;
begin
  Result := obceCollection_ref.TryTake(value, timeout_ms);
end; { TOmniBlockingCollectionEnumerator<T>.TryTake }

{ TOmniBlockingCollection<T> }

///If numProducersConsumers > 0, collection will automatically enter 'completed' state
///when this number of .Take calls is simultaneously blocked because the collection is
///empty.
constructor TOmniBlockingCollection<T>.Create(numProducersConsumers, blockSize,
  numCachedBlocks: integer);
var
  ShareLock: IOmniCriticalSection;
begin
  inherited Create;
  // SBD: TODO: Work needs to be done here
  //  1. obcResourceCount needs to be constructed with ShareLock
  //  2. Find out what is the interaction with obcObserver. Does it block taking?
  ShareLock := CreateOmniCriticalSection;
  obcAddCountAndCompleted.Value := 0;
  obcApproxCount.Value := 0;
  if numProducersConsumers > 0 then
    obcResourceCount := CreateResourceCount(numProducersConsumers);
  obcCollection := TOmniQueue<T>.Create(blockSize, numCachedBlocks);
  obcCompletedSignal := {$IFDEF MSWINDOWS}CreateEvent(nil, true, false, nil);
                        {$ELSE}CreateOmniEvent(true, false, ShareLock);{$ENDIF}
  obcObserver := {$IFDEF MSWINDOWS}CreateContainerWindowsEventObserver;
                 {$ELSE}CreateContainerEventObserver;{$ENDIF}
  obcCollection.ContainerSubject.Attach(obcObserver, coiNotifyOnAllInserts);
  obcNotOverflow := {$IFDEF MSWINDOWS}CreateEvent(nil, true, true, nil);
                    {$ELSE}CreateOmniEvent(true, true, ShareLock);{$ENDIF}
  {$IFNDEF MSWINDOWS}
  FCompletedWaiter := TSynchroWaitFor.Create([obcCompletedSignal, obcNotOverflow], ShareLock);
  if assigned( obcResourceCount) then
      // SBD: TODO: Not sure if obcObserver needs to be included.
      FTakableWaiter := TSynchroWaitFor.Create([obcCompletedSignal, {obcObserver,
        }(obcResourceCount as IOmniSynchroObject).Synchro], ShareLock)
    else
      // FTakableWaiter := TSynchroWaitFor.Create([obcCompletedSignal, obcObserver], ShareLock);
      FTakableWaiter := nil
  {$ENDIF}
end; { TOmniBlockingCollection<T>.Create }

destructor TOmniBlockingCollection<T>.Destroy;
begin
  {$IFDEF MSWINDOWS}
  DSiCloseHandleAndNull(obcNotOverflow);
  {$ELSE}
  obcNotOverflow := nil;
  {$ENDIF MSWINDOWS}
  if assigned(obcCollection) and assigned(obcObserver) then
    obcCollection.ContainerSubject.Detach(obcObserver, coiNotifyOnAllInserts);
  FreeAndNil(obcObserver);
  {$IFDEF MSWINDOWS}
  DSiCloseHandleAndNull(obcCompletedSignal);
  {$ELSE}
  obcCompletedSignal := nil;
  {$ENDIF ~MSWINDOWS}
  FreeAndNil(obcCollection);
  obcResourceCount := nil;
  {$IFNDEF MSWINDOWS}
  FCompletedWaiter.Free;
  FTakableWaiter.Free;
  {$ENDIF}
  inherited Destroy;
end; { TOmniBlockingCollection<T>.Destroy }

procedure TOmniBlockingCollection<T>.Add(const value: T);
begin
  if not TryAdd(value) then
    raise ECollectionCompleted.Create('Adding to completed collection');
end; { TOmniBlockingCollection<T>.Add }

procedure TOmniBlockingCollection<T>.CompleteAdding;
begin
  repeat
    if IsCompleted then // CompleteAdding was already called
      Exit;
    if obcAddCountAndCompleted.CAS(0, CCompletedFlag) then begin // there must be no active writers
      {$IFDEF MSWINDOWS}
      Win32Check(SetEvent(obcCompletedSignal)); // tell blocked readers to quit
      {$ELSE}
      obcCompletedSignal.SetEvent; // tell blocked readers to quit
      {$ENDIF ~MSWINDOWS}
      Exit;
    end;
    {$IFDEF MSWINDOWS}
    Yield;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
  until false;
end; { TOmniBlockingCollection<T>.CompleteAdding }

function TOmniBlockingCollection<T>.Elapsed(startTime: int64; timeout_ms: cardinal):
  boolean;
begin
  if timeout_ms = INFINITE then
    Result := false
  else
    Result := (startTime + timeout_ms) < DSiTimeGetTime64;
end; { TOmniBlockingCollection<T>.Elapsed }

function TOmniBlockingCollection<T>.GetApproxCount: integer;
begin
  Result := obcApproxCount.Value;
end; { TOmniBlockingCollection<T>.GetApproxCount }

function TOmniBlockingCollection<T>.GetContainerSubject: TOmniContainerSubject;
begin
  Result := obcCollection.ContainerSubject;
end; { TOmniBlockingCollection<T>.GetContainerSubject }

function TOmniBlockingCollection<T>.GetEnumerator: TOmniBlockingCollectionEnumerator<T>;
begin
  Result := TOmniBlockingCollectionEnumerator<T>.Create(Self);
end; { TOmniBlockingCollection<T>.GetEnumerator }

function TOmniBlockingCollection<T>.IsCompleted: boolean;
begin
  Result := (obcAddCountAndCompleted.Value AND CCompletedFlag) = CCompletedFlag;
end; { TOmniBlockingCollection<T>.IsCompleted }

function TOmniBlockingCollection<T>.IsEmpty: boolean;
begin
  Result := obcCollection.IsEmpty;
end; { TOmniBlockingCollection }

function TOmniBlockingCollection<T>.IsFinalized: boolean;
begin
  Result := IsCompleted and obcCollection.IsEmpty;
end; { TOmniBlockingCollection<T>.IsFinalized }

function TOmniBlockingCollection<T>.Next: T;
begin
  if not Take(Result) then
    raise ECollectionCompleted.Create('Collection is empty');
end; { TOmniBlockingCollection<T>.Next }

///<summary>When throttling is set, Add will block if there is >= highWaterMark elements
///  in the queue. It will only unblock when number of elements drops below lowWaterMark.</summary>
procedure TOmniBlockingCollection<T>.SetThrottling(highWaterMark, lowWaterMark: integer);
begin
  if obcAccessed then
    raise Exception.Create('Throttling cannot be set once the blocking collection has been used');
  Assert(lowWaterMark <= highWaterMark);
  obcHighWaterMark := highWaterMark;
  obcLowWaterMark := lowWaterMark;
  obcThrottling := true;
end; { TOmniBlockingCollection<T>.SetThrottling }

function TOmniBlockingCollection<T>.Take(var value: T): boolean;
begin
  Result := TryTake(value, INFINITE);
end; { TOmniBlockingCollection<T>.Take }

function TOmniBlockingCollection<T>.TimeLeft_ms(startTime: int64; timeout_ms: cardinal):
  DWORD;
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
end; { TOmniBlockingCollection<T>.TimeLeft_ms }

function TOmniBlockingCollection<T>.ToArray: TArray<T>;
var
  ds      : integer;
  lenArr  : integer;
  maxValue: uint64;
  numEl   : integer;
  ti      : PTypeInfo;
  value   : T;
begin
  lenArr := Clamp(0);
  SetLength(Result, lenArr);
  numEl := 0;
  while TryTake(value, INFINITE) do begin
    if numEl >= lenArr then begin
      lenArr := lenArr + Clamp(Round(Length(Result)*0.5));
      SetLength(Result, lenArr);
    end;
    Result[numEl] := value;
    Inc(numEl);
  end;
  SetLength(Result, numEl);
end; { TOmniBlockingCollection<T>.ToArray<T> }

function TOmniBlockingCollection<T>.TryAdd(const value: T): boolean;
var
  {$IFDEF MSWINDOWS}
  awaited: cardinal;
  {$ELSE}
  waitResult: TWaitResult;
  Signaller: IOmniSynchro;
  {$ENDIF}
begin
  obcAddCountAndCompleted.Increment;
  try
    // IsCompleted can not change during the execution of this function
    Result := not IsCompleted;
    if Result then begin
      obcAccessed := true;
      if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
        {$IFDEF MSWINDOWS}
        Win32Check(ResetEvent(obcNotOverflow));
        {$ELSE}
        obcNotOverflow.Reset;
        {$ENDIF ~MSWINDOWS}
        // it's possible that messages were removed and obcNotOverflow set *before* the
        // previous line has executed so test again ...
        if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
          obcAddCountAndCompleted.Decrement; // Leave the Add temporarily so that CompleteAdding can succeed
          {$IFDEF MSWINDOWS}
          awaited := DSiWaitForTwoObjects(obcCompletedSignal, obcNotOverflow, false, INFINITE);
          obcAddCountAndCompleted.Increment; // Re-enter Add; queue may be now in 'completed' state
          if (awaited = WAIT_OBJECT_0) or IsCompleted then begin
          {$ELSE}
          waitResult := FCompletedWaiter.WaitAny(INFINITE,Signaller);
          obcAddCountAndCompleted.Increment;
          if ((waitResult = wrSignaled) and (Signaller = obcCompletedSignal)) or IsCompleted then begin
          {$ENDIF}
            Result := false; // completed
            Exit;
          end;
        end;
      end;
      obcCollection.Enqueue(value);
      obcApproxCount.Increment;
    end;
  finally obcAddCountAndCompleted.Decrement; end;
end; { TOmniBlockingCollection<T>.TryAdd }

{$IFDEF MSWINDOWS}
function TOmniBlockingCollection<T>.TryTake(var value: T; timeout_ms: cardinal): boolean;
var
  awaited    : DWORD;
  startTime  : int64;
  waitHandles: array [0..2] of THandle;
begin
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
                     @waitHandles, false, TimeLeft_ms(startTime, timeout_ms));
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
      until Elapsed(startTime, timeout_ms);
    finally
      if assigned(obcResourceCount) then
        obcResourceCount.Release;
    end;
  end;
  if Result then begin
    obcApproxCount.Decrement;
    if obcThrottling and (obcApproxCount.Value <= obcLowWaterMark) then
      SetEvent(obcNotOverflow);
  end;
end; { TOmniBlockingCollection<T>.TryTake }

{$ELSE}

// Non-windows version of TryTake().
function TOmniBlockingCollection<T>.TryTake(var value: T; timeout_ms: cardinal): boolean;
var
  StopWatch: TStopWatch;
  awaited: TWaitResult;
  Signaller: IOmniSynchro;

  function TimeLeft_ms: cardinal;
  var
    intTime: integer;
  begin
    if timeout_ms = INFINITE then
      Result := INFINITE
    else begin
      intTime := timeout_ms - StopWatch.ElapsedMilliseconds;
      if intTime < 0 then
        Result := 0
      else
        Result := intTime;
    end;
  end; { TimeLeft }

begin
  if obcCollection.TryDequeue(value) then
    Result := true
  else begin // must be executed even if timeout_ms = 0 or the algorithm will break
    if assigned(obcResourceCount) then
      obcResourceCount.Allocate;
    try
      StopWatch := TStopWatch.StartNew;
      Result := false;
      repeat
        if assigned(FTakableWaiter) then
          awaited := FTakableWaiter.WaitAny(TimeLeft_ms, Signaller)
        else begin
          awaited   := obcCompletedSignal.WaitFor(TimeLeft_ms);
          Signaller := obcCompletedSignal
        end;
        if obcCollection.TryDequeue(value) then begin // there may still be data in completed queue
          Result := true;
          break; //repeat
        end;
        if (awaited = wrSignaled) and (Signaller = (obcResourceCount as IOmniSynchroObject).Synchro) then
          CompleteAdding;
        if {(}awaited <> wrSignaled {) or (Signaller <> obcObserver)} then begin
          Result := false;
          break; //while
        end;
      until TimeLeft_ms = 0
    finally
      if assigned(obcResourceCount) then
        obcResourceCount.Release;
    end;
  end;
  if Result then begin
    obcApproxCount.Decrement;
    if obcThrottling and (obcApproxCount.Value <= obcLowWaterMark) then
      obcNotOverflow.SetEvent
  end;
end;
{$ENDIF}

{ TOmniBlockingCollection }

function TOmniBlockingCollection.GetEnumerator: IOmniValueEnumerator;
begin
  Result := TOmniBlockingCollectionEnumerator.Create(Self);
end; { TOmniBlockingCollection.GetEnumerator }

procedure TOmniBlockingCollection.ReraiseExceptions(enable: boolean = true);
begin
  obcReraiseExceptions := enable;
end; { TOmniBlockingCollection.ReraiseExceptions }

class function TOmniBlockingCollection.ToArray<T>(coll: IOmniBlockingCollection):
  TArray<T>;
var
  ds      : integer;
  lenArr  : integer;
  maxValue: uint64;
  numEl   : integer;
  ti      : PTypeInfo;
  value   : TOmniValue;
begin
  ds := 0;
  ti := System.TypeInfo(T);
  if assigned(ti) then
    if (ti = System.TypeInfo(byte)) or (ti = System.TypeInfo(shortint)) then
      ds := 1
    else if (ti = System.TypeInfo(word)) or (ti = System.TypeInfo(smallint)) then
      ds := 2
    else
      ds := TOmniValue_DataSize[ti^.Kind];

  maxValue := High(uint64);
  if ds > 0 then
    maxValue := uint64($FF) SHL ((ds-1) * 8);

  lenArr := Clamp(0);
  SetLength(Result, lenArr);
  numEl := 0;
  while coll.TryTake(value, INFINITE) do begin
    if numEl >= lenArr then begin
      lenArr := lenArr + Clamp(Round(Length(Result)*0.5));
      SetLength(Result, lenArr);
    end;
    if ds = 0 then begin
      case value.DataType of
        ovtInterface: PInterface(@Result[numEl])^ := value.AsInterface;
        ovtRecord:    Result[numEl] := value.ToRecord<T>;
        else          Result[numEl] := value.AsTValue.AsType<T>;
      end;
    end
    else begin
      if value.RawData^ > maxValue then
        raise EOmniValueConv.CreateFmt('Value %d is too big to fit into %s', [value.RawData^, ti^.Name]);
      Move(value.RawData^, Result[numEl], ds);
    end;
    Inc(numEl);
  end;
  SetLength(Result, numEl);
end; { TOmniBlockingCollection.ToArray }

function TOmniBlockingCollection.TryTake(var value: TOmniValue; timeout_ms: cardinal): boolean;
begin
  Result := inherited TryTake(value, timeout_ms);
  if Result and obcReraiseExceptions and value.IsException then
    raise value.AsException;
end; { TOmniBlockingCollection.TryTake }

end.

