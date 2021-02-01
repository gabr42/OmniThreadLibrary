///<summary>Locking/blocking collections. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2021 Primoz Gabrijelcic
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
///   Support           : https://en.delphipraxis.net/forum/32-omnithreadlibrary/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : Sean B. Durkin
///   Creation date     : 2009-12-27
///   Last modification : 2021-01-21
///   Version           : 2.01a
///</para><para>
///   History:
///     2.01a: 2021-01-21
///       - Better TryTake implementation for non-Windows platforms.
///     2.01: 2018-06-14
///       - TOmniTransitionEvent changed to IOmniEvent.
///     2.0: 2018-04-24
///       - Removed support for pre-XE Delphis.
///       - DSiTimeGetTime64 replaced with OtlPlatform.Time.
///     1.11: 2018-03-12
///       - Added TOmniBlockingCollection.FromArray<T> and .ToArray<T> (multiple overloads).
///     1.10a: 2017-09-26
///       - Parameter to TOmniBlockingCollection.ToArray<T> marked 'const'.
///     1.10: 2017-06-25
///       - Defined IOmniBlockingCollection.Count.
///     1.09: 2016-11-18
///       - Implemented IOmniBlockingCollection.IsEmpty.
///       - Implemented TOmniBlockingCollection.Count.
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
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  DSiWin32,
  GpStuff,
  {$ENDIF}
  System.SyncObjs,
  System.TypInfo,
  System.RTTI,
  System.Generics.Collections,
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
    function  GetApproxCount: integer;
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
    property Count: integer read GetApproxCount;
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
    obcAddCountAndCompleted: TOmniAlignedInt32;
    obcApproxCount         : TOmniAlignedInt32;
    obcCollection          : TOmniQueue;
    obcCompletedSignal     : IOmniEvent;
    obcHighWaterMark       : integer;
    obcLowWaterMark        : integer;
    obcNotOverflow         : IOmniEvent;
    obcObserver            : {$IFDEF MSWINDOWS}TOmniContainerWindowsEventObserver{$ELSE}TOmniContainerEventObserver{$ENDIF};
    obcReraiseExceptions   : boolean;
    obcResourceCount       : IOmniResourceCount;
    obcThrottling          : boolean;
    {$IFNDEF MSWINDOWS}
    FCompletedWaiter       : TSynchroWaitFor;
    FTakableWaiter         : TSynchroWaitFor;
    {$ENDIF}
  protected
    function  GetApproxCount: integer; inline;
    function  GetContainerSubject: TOmniContainerSubject;
    procedure InsertElement<T>(const value: T; ti: PTypeInfo; ds: integer);
  public
    {$REGION 'Documentation'}
    ///	<remarks>If numProducersConsumers &gt; 0, collection will automatically
    ///	enter 'completed' state when this number of .Take calls is
    ///	simultaneously blocked because the collection is empty.</remarks>
    {$ENDREGION}
    constructor Create(numProducersConsumers: integer = 0);
    destructor  Destroy; override;
    class function FromArray<T>(const values: TArray<T>): IOmniBlockingCollection; inline; //alias for FromRange
    class function FromRange<T>(const values: array of T): IOmniBlockingCollection; overload;
    class function FromRange<T>(const collection: IEnumerable<T>): IOmniBlockingCollection; overload; inline;
    class function FromRange<T>(const collection: TEnumerable<T>): IOmniBlockingCollection; overload; inline;
    class function ToArray<T>(const coll: IOmniBlockingCollection): TArray<T>;
    procedure AddRange<T>(const values: array of T); overload;
    procedure AddRange<T>(const collection: IEnumerable<T>); overload;
    procedure AddRange<T>(const collection: TEnumerable<T>); overload;
    procedure Add(const value: TOmniValue); inline;
    procedure CompleteAdding;
    function  GetEnumerator: IOmniValueEnumerator; inline;
    function  IsCompleted: boolean; inline;
    function  IsEmpty: boolean; inline;
    function  IsFinalized: boolean;
    function  Next: TOmniValue;
    procedure ReraiseExceptions(enable: boolean = true);
    procedure SetThrottling(highWaterMark, lowWaterMark: integer);
    function  Take(var value: TOmniValue): boolean; inline;
    function  TryAdd(const value: TOmniValue): boolean; inline;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
    property CompletedSignal: IOmniEvent read obcCompletedSignal;
    property ContainerSubject: TOmniContainerSubject read GetContainerSubject;
    property Count: integer read GetApproxCount;
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

  PInterface = ^IInterface;

  //compiler requires them to be public
  function Clamp(value: integer): integer;
  procedure GetTypeInformation(ti: PTypeInfo; var ds: integer; var maxValue: uint64);

implementation

uses
  System.Classes,
  System.Diagnostics,
  OtlPlatform;

{$IFDEF MSWINDOWS}
{$IFDEF CPUX64}
procedure AsmPause;
asm
  .noframe
  pause
end; { AsmPause }
{$ENDIF CPUX64}
{$ENDIF}

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
var
  shareLock: IOmniCriticalSection;
begin
  inherited Create;
  // SBD: TODO: Work needs to be done here
  //  1. obcResourceCount needs to be constructed with shareLock
  //  2. Find out what is the interaction with obcObserver. Does it block taking?
  {$IFDEF MSWINDOWS}
  shareLock := nil;
  {$ELSE}
  shareLock := CreateOmniCriticalSection;
  {$ENDIF}
  obcAddCountAndCompleted.Value := 0;
  obcApproxCount.Value := 0;
  if numProducersConsumers > 0 then
    obcResourceCount := CreateResourceCount(numProducersConsumers);
  obcCollection := TOmniQueue.Create;
  obcCompletedSignal := CreateOmniEvent(true, false, shareLock);
  obcObserver := {$IFDEF MSWINDOWS}CreateContainerWindowsEventObserver;
                 {$ELSE}CreateContainerEventObserver;{$ENDIF}
  obcCollection.ContainerSubject.Attach(obcObserver, coiNotifyOnAllInserts);
  obcNotOverflow := CreateOmniEvent(true, true, shareLock);
  {$IFNDEF MSWINDOWS}
  FCompletedWaiter := TSynchroWaitFor.Create([obcCompletedSignal, obcNotOverflow], shareLock);
  if assigned(obcResourceCount) then
    // SBD: TODO: Not sure if obcObserver needs to be included.
    FTakableWaiter := TSynchroWaitFor.Create([obcCompletedSignal, {obcObserver,}
                        (obcResourceCount as IOmniSynchroObject).Synchro], shareLock)
  else
    // FTakableWaiter := TSynchroWaitFor.Create([obcCompletedSignal, obcObserver], shareLock);
    FTakableWaiter := nil
  {$ENDIF}
end; { TOmniBlockingCollection.Create }

destructor TOmniBlockingCollection.Destroy;
begin
  obcNotOverflow := nil;
  if assigned(obcCollection) and assigned(obcObserver) then
    obcCollection.ContainerSubject.Detach(obcObserver, coiNotifyOnAllInserts);
  FreeAndNil(obcObserver);
  obcCompletedSignal := nil;
  FreeAndNil(obcCollection);
  obcResourceCount := nil;
  {$IFNDEF MSWINDOWS}
  FCompletedWaiter.Free;
  FTakableWaiter.Free;
  {$ENDIF}
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
      obcCompletedSignal.SetEvent; // tell blocked readers to quit
      Exit;
    end;
    {$IFDEF MSWINDOWS}
    {$IFDEF CPUX64}AsmPause;{$ELSE}asm pause; end;{$ENDIF CPUX64}
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
  until false;
end; { TOmniBlockingCollection.CompleteAdding }

function TOmniBlockingCollection.GetApproxCount: integer;
begin
  Result := obcApproxCount.Value;
end; { TOmniBlockingCollection.GetApproxCount }

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

function TOmniBlockingCollection.IsEmpty: boolean;
begin
  Result := obcCollection.IsEmpty;
end; { TOmniBlockingCollection }

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

procedure GetTypeInformation(ti: PTypeInfo; var ds: integer; var maxValue: uint64);
begin
  ds := 0;
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
end; { GetTypeInformation }

procedure TOmniBlockingCollection.InsertElement<T>(const value: T; ti: PTypeInfo; ds: integer);
var
  i : integer;
  ov: TOmniValue;
begin
  case ti.Kind of
    tkInteger, tkPointer:
      if ds = 8 then
        ov.AsInt64 := PInt64(@value)^
      else begin
        Assert(ds <= 4, 'TOmniBlockingCollection.InsertElement<T>: Integer data is too large');
        i := 0;
        Move(value, i, ds);
        ov.AsInteger := i;
      end;
    tkInt64:
      ov.AsInt64 := PInt64(@value)^;
    tkClass:
      ov.AsObject := PObject(@value)^;
    tkChar, tkWChar, tkString, tkWString, tkLString, tkUString:
      ov.AsString := TValue.From<T>(value).AsString;
    tkRecord:
      ov := TOmniValue.FromRecordUnsafe<T>(value);
    tkInterface:
      ov.AsInterface := PInterface(@value)^;
    else
      ov.AsTValue := TValue.From<T>(value);
  end;

  Add(ov);
end; { TOmniBlockingCollection.InsertElement<T> }

procedure TOmniBlockingCollection.AddRange<T>(const values: array of T);
var
  ds      : integer;
  el      : T;
  maxValue: uint64;
  ti      : PTypeInfo;
begin
  ti := System.TypeInfo(T);
  GetTypeInformation(ti, ds, maxValue);
  for el in values do
    InsertElement<T>(el, ti, ds);
end; { TOmniBlockingCollection.AddRange<T> }

procedure TOmniBlockingCollection.AddRange<T>(const collection: IEnumerable<T>);
var
  ds      : integer;
  el      : T;
  maxValue: uint64;
  ti      : PTypeInfo;
begin
  ti := System.TypeInfo(T);
  GetTypeInformation(ti, ds, maxValue);
  for el in collection do
    InsertElement<T>(el, ti, ds);
end; { TOmniBlockingCollection.AddRange<T> }

procedure TOmniBlockingCollection.AddRange<T>(const collection: TEnumerable<T>);
var
  ds      : integer;
  el      : T;
  maxValue: uint64;
  ti      : PTypeInfo;
begin
  ti := System.TypeInfo(T);
  GetTypeInformation(ti, ds, maxValue);
  for el in collection do
    InsertElement<T>(el, ti, ds);
end; { TOmniBlockingCollection.AddRange<T> }

class function TOmniBlockingCollection.FromRange<T>(const values: array of T): IOmniBlockingCollection;
begin
  Result := TOmniBlockingCollection.Create;
  (Result as TOmniBlockingCollection).AddRange<T>(values);
end; { TOmniBlockingCollection.FromRange<T> }

class function TOmniBlockingCollection.FromRange<T>(const collection: IEnumerable<T>): IOmniBlockingCollection;
begin
  Result := TOmniBlockingCollection.Create;
  (Result as TOmniBlockingCollection).AddRange<T>(collection);
end; { TOmniBlockingCollection.FromRange<T> }

class function TOmniBlockingCollection.FromRange<T>(const collection: TEnumerable<T>): IOmniBlockingCollection;
begin
  Result := TOmniBlockingCollection.Create;
  (Result as TOmniBlockingCollection).AddRange<T>(collection);
end; { TOmniBlockingCollection.FromRange<T> }

class function TOmniBlockingCollection.FromArray<T>(const values: TArray<T>): IOmniBlockingCollection;
begin
  Result := TOmniBlockingCollection.FromRange<T>(values);
end; { TOmniBlockingCollection.FromArray<T> }

class function TOmniBlockingCollection.ToArray<T>(const coll: IOmniBlockingCollection):
  TArray<T>;
var
  ds      : integer;
  lenArr  : integer;
  maxValue: uint64;
  numEl   : integer;
  ti      : PTypeInfo;
  value   : TOmniValue;
begin
  ti := System.TypeInfo(T);
  GetTypeInformation(ti, ds, maxValue);

  lenArr := Clamp(0);
  SetLength(Result, lenArr);
  numEl := 0;
  for value in coll do begin
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
end; { TOmniBlockingCollection.ToArray<T> }

function TOmniBlockingCollection.TryAdd(const value: TOmniValue): boolean;
var
  {$IFDEF MSWINDOWS}
  awaited: cardinal;
  {$ELSE}
  waitResult: TWaitFor.TWaitForResult;
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
        obcNotOverflow.Reset;
        // it's possible that messages were removed and obcNotOverflow set *before* the
        // previous line has executed so test again ...
        if obcThrottling and (obcApproxCount.Value >= obcHighWaterMark) then begin
          obcAddCountAndCompleted.Decrement; // Leave the Add temporarily so that CompleteAdding can succeed
          {$IFDEF MSWINDOWS}
          awaited := DSiWaitForTwoObjects(obcCompletedSignal.Handle, obcNotOverflow.Handle, false, INFINITE);
          if awaited = WAIT_OBJECT_0 then
          obcAddCountAndCompleted.Increment; // Re-enter Add; queue may be now in 'completed' state
          if (awaited = WAIT_OBJECT_0) or IsCompleted then begin
          {$ELSE}
          waitResult := FCompletedWaiter.WaitAny(INFINITE,Signaller);
          obcAddCountAndCompleted.Increment;
          if ((waitResult = waAwaited) and (Signaller = obcCompletedSignal)) or IsCompleted then begin
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
end; { TOmniBlockingCollection.TryAdd }

{$IFDEF MSWINDOWS}
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
      Result := (startTime + timeout_ms) < Time.Timestamp_ms;
  end; { Elapsed }

  function TimeLeft_ms: DWORD;
  var
    intTime: integer;
  begin
    if timeout_ms = INFINITE then
      Result := INFINITE
    else begin
      intTime := startTime + timeout_ms - Time.Timestamp_ms;
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
      startTime := Time.Timestamp_ms;
      waitHandles[0] := obcCompletedSignal.Handle;
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
  if Result then begin
    obcApproxCount.Decrement;
    if obcThrottling and (obcApproxCount.Value <= obcLowWaterMark) then
      obcNotOverflow.SetEvent;
  end;
  if Result and obcReraiseExceptions and value.IsException then
    raise value.AsException;
end; { TOmniBlockingCollection.TryTake }

{$ELSE}

// Non-windows version of TryTake().
function TOmniBlockingCollection.TryTake(
  var value: TOmniValue; timeout_ms: cardinal): boolean;
var
  StopWatch: TStopWatch;
  awaited: TWaitFor.TWaitForResult;
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
          if obcCompletedSignal.WaitFor(TimeLeft_ms) = wrSignaled then
            awaited := waAwaited
          else
            awaited := waTimeout;
          Signaller := obcCompletedSignal;
        end;
        if obcCollection.TryDequeue(value) then begin // there may still be data in completed queue
          Result := true;
          break; //repeat
        end;
        if (awaited = waAwaited) and assigned(obcResourceCount) and (Signaller = (obcResourceCount as IOmniSynchroObject).Synchro) then
          CompleteAdding;
        if (awaited = waAwaited) and (Signaller = obcCompletedSignal) then begin
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
  if Result and obcReraiseExceptions and value.IsException then
    raise value.AsException;
end;
{$ENDIF}

end.

