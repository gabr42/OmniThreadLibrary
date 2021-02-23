///<summary>Simple helpers based on the OtlSync synchronization primitives. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2020, Primoz Gabrijelcic
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
///   Creation date     : 2020-09-16
///   Last modification : 2020-09-16
///   Version           : 1.0
///</para><para>
///   History:
///     1.0a: 2021-02-09
///       - IOmniSynchronizer<T>/TOmniSynchronizer<T> did not compile when T was not a string.
///     1.0: 2020-09-16
///       - Implemented TOmniSynchronizer class, useful for writing multithreaded unit tests.
///       - Requires Delphi 2009 or newer.
///</para></remarks>

unit OtlSync.Utils;

interface

uses
  Generics.Collections, SyncObjs, OtlSync;

type
  IOmniSynchronizer<T> = interface ['{631B2859-8268-464C-8A36-EB5847BE44A5}']
    function  Count: integer;
    procedure Signal(const name: T);
    function  WaitFor(const name: T; timeout: cardinal = INFINITE): boolean;
  end; { IOmniSynchronizer<T> }

  TOmniSynchronizer<T> = class(TInterfacedObject, IOmniSynchronizer<T>)
  strict private
    FEvents: TObjectDictionary<T, TEvent>;
    FLock  : TOmniMREW;
  strict protected
    function  Ensure(const name: T): TEvent;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Count: integer;
    procedure Reset(const name: T); inline;
    procedure Signal(const name: T); inline;
    function  WaitFor(const name: T; timeout: cardinal = INFINITE): boolean;
  end; { TOmniSynchronizer<T> }

  IOmniSynchronizer = IOmniSynchronizer<string>;
  TOmniSynchronizer = TOmniSynchronizer<string>;

implementation

uses
  SysUtils;

{ TOmniSynchronizer<T> }

constructor TOmniSynchronizer<T>.Create;
begin
  inherited Create;
  FEvents := TObjectDictionary<T, TEvent>.Create;
end; { TOmniSynchronizer<T>.Create }

destructor TOmniSynchronizer<T>.Destroy;
begin
  FreeAndNil(FEvents);
  inherited;
end; { TOmniSynchronizer<T>.Destroy }

function TOmniSynchronizer<T>.Count: integer;
begin
  FLock.EnterReadLock;
  try
     Result := FEvents.Count;
  finally FLock.ExitReadLock; end;
end; { TOmniSynchronizer<T>.Count }

function TOmniSynchronizer<T>.Ensure(const name: T): TEvent;
var
  event: TEvent;
begin
  FLock.EnterReadLock;
  try
    if FEvents.TryGetValue(name, Result) then
      Exit;
  finally FLock.ExitReadLock; end;

  event := TEvent.Create(nil, true, false, '');
  FLock.EnterWriteLock;
  try
    if FEvents.TryGetValue(name, Result) then
      FreeAndNil(event)
    else begin
      Result := event;
      FEvents.Add(name, Result);
    end;
  finally FLock.ExitWriteLock; end;
end; { TOmniSynchronizer<T>.Ensure }

procedure TOmniSynchronizer<T>.Reset(const name: T);
begin
  Ensure(name).ResetEvent;
end;

procedure TOmniSynchronizer<T>.Signal(const name: T);
begin
  Ensure(name).SetEvent;
end; { TOmniSynchronizer<T>.Signal }

function TOmniSynchronizer<T>.WaitFor(const name: T; timeout: cardinal): boolean;
begin
  Result := Ensure(name).WaitFor(timeout) = wrSignaled;
end; { TOmniSynchronizer<T>.WaitFor }

end.
