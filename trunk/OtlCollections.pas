///<summary>Locking/blocking collections. Part of the OmniThreadLibrary project.</summary>
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
///   Creation date     : 2009-12-27
///   Last modification : 2009-12-30
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2009-12-30
///       - Number of producer/consumers can be passed to TOmniBlockingCollection
///         constructor. TryTake will then detect the deadlock state when all producer/
///         consumers are inside the TryTake and the collection is empty and will
///         automatically complete the collection.
///     1.0: 2009-12-29
///       - Released.
///</para></remarks>

unit OtlCollections;

interface

uses
  Windows,
  SysUtils,
  DSiWin32,
  OtlCommon,
  OtlContainers, OtlSync;

type
  ECollectionCompleted = class(Exception);

  IOmniBlockingCollectionEnumerator = interface ['{7A5AA8F4-5ED8-40C3-BDC3-1F991F652F9E}']
    function  GetCurrent: TOmniValue;
    function  MoveNext: boolean;
    property Current: TOmniValue read GetCurrent;
  end; { IOmniBlockingCollectionEnumerator }

  ///<summary>Blocking collection
  ///- http://blogs.msdn.com/pfxteam/archive/2009/11/06/9918363.aspx
  ///- http://msdn.microsoft.com/en-us/library/dd267312(VS.100).aspx
  ///</summary>
  IOmniBlockingCollection = interface ['{208EFA15-1F8F-4885-A509-B00191145D38}']
    procedure Add(const value: TOmniValue);
    procedure CompleteAdding;
    function  GetEnumerator: IOmniBlockingCollectionEnumerator;
    function  IsCompleted: boolean;
    function  Take(var value: TOmniValue): boolean;
    function  TryAdd(const value: TOmniValue): boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
  end; { IOmniBlockingCollection }

  TOmniBlockingCollection = class(TInterfacedObject, IOmniBlockingCollection)
  strict private
    obcCollection     : TOmniQueue;
    obcCompleted      : boolean;
    obcCompletedSignal: TDSiEventHandle;
    obcResourceCount  : TOmniResourceCount;
  public
    constructor Create(numProducersConsumers: integer = 0);
    destructor  Destroy; override;
    procedure Add(const value: TOmniValue); inline;
    procedure CompleteAdding; inline;
    function  GetEnumerator: IOmniBlockingCollectionEnumerator; inline;
    function  IsCompleted: boolean; inline;
    function  Take(var value: TOmniValue): boolean; inline;
    function  TryAdd(const value: TOmniValue): boolean; inline;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal = 0): boolean;
    property CompletedSignal: THandle read obcCompletedSignal;
  end; { TOmniBlockingCollection }

  TOmniBlockingCollectionEnumerator = class(TInterfacedObject, IOmniBlockingCollectionEnumerator)
  strict private
    obceCollection_ref: TOmniBlockingCollection;
    obceValue         : TOmniValue;
  public
    constructor Create(collection: TOmniBlockingCollection);
    function GetCurrent: TOmniValue; inline;
    function MoveNext: boolean; inline;
    property Current: TOmniValue read GetCurrent;
  end; { TOmniBlockingCollectionEnumerator }

implementation

uses
  Classes,
  GpStuff,
  OtlContainerObserver;

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
end; { TOmniBlockingCollection.Create }

destructor TOmniBlockingCollection.Destroy;
begin
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

function TOmniBlockingCollection.GetEnumerator: IOmniBlockingCollectionEnumerator;
begin
  Result := TOmniBlockingCollectionEnumerator.Create(Self);
end; { TOmniBlockingCollection.GetEnumerator }

function TOmniBlockingCollection.IsCompleted: boolean;
begin
  Result := obcCompleted;
end; { TOmniBlockingCollection.IsCompleted }

function TOmniBlockingCollection.Take(var value: TOmniValue): boolean;
begin
  Result := TryTake(value, INFINITE);
end; { TOmniBlockingCollection.Take }

function TOmniBlockingCollection.TryAdd(const value: TOmniValue): boolean;
begin
  // CompleteAdding and TryAdd are not synchronised
  Result := not obcCompleted;
  if Result then
    obcCollection.Enqueue(value);
end; { TOmniBlockingCollection.TryAdd }

function TOmniBlockingCollection.TryTake(var value: TOmniValue;
  timeout_ms: cardinal): boolean;
var
  awaited    : DWORD;
  observer   : TOmniContainerWindowsEventObserver;
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
  else if IsCompleted or (timeout_ms = 0) then
    Result := false
  else begin
    if assigned(obcResourceCount) then
      obcResourceCount.Allocate;
    try
      observer := CreateContainerWindowsEventObserver;
      try
        obcCollection.ContainerSubject.Attach(observer, coiNotifyOnAllInserts);
        try
          startTime := DSiTimeGetTime64;
          waitHandles[0] := obcCompletedSignal;
          waitHandles[1] := observer.GetEvent;
          if assigned(obcResourceCount) then
            waitHandles[2] := obcResourceCount.Handle;
          Result := false;
          while not (IsCompleted or Elapsed) do begin
            if obcCollection.TryDequeue(value) then begin
              Result := true;
              break; //while
            end;
            awaited := WaitForMultipleObjects(IFF(assigned(obcResourceCount), 3, 2),
                         @waitHandles, false, TimeLeft_ms);
            if awaited <> WAIT_OBJECT_1 then begin
              if awaited = WAIT_OBJECT_2 then begin
// TODO 1 -oPrimoz Gabrijelcic : testing, remove!
OutputDebugString(PChar(Format('CompleteAdding', [])));
                CompleteAdding;
              end;
              Result := false;
              break; //while
            end;
          end;
        finally
          obcCollection.ContainerSubject.Detach(observer, coiNotifyOnAllInserts);
        end;
      finally FreeAndNil(observer); end;
    finally
      if assigned(obcResourceCount) then
        obcResourceCount.Release;
    end;
  end;

  // TODO 1 -oPrimoz Gabrijelcic : testing, remove!
  if not Result then
    OutputDebugString(PChar(Format('Take = false in thread %d', [GetCurrentThreadID])));

end; { TOmniBlockingCollection.TryTake }

end.

