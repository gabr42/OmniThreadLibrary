///<summary>Platform independent atomic (bus-locking) operations.
///    Part of the OmniThreadLibrary project. Requires Delphi XE7.</summary>
///<author>Sean B. Durkin</author>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017 Sean B. Durkin, Primoz Gabrijelcic
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
///   Authors           : Seam B. Durkin, Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Creation date     : 2017-02-12
///   Last modification : 2017-02-12
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2017-02-12
///       - Imported from mobile/Otl.Parallel.Atomic.pas.

unit OtlSync.Platform.Atomic;

{$I OtlOptions.inc}

interface

uses
  System.SysUtils;

type
  /// <remarks> TVolatileInt32 is a thead-safe signed 32-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatileInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInteger;
  {$ELSE}
  integer;
  TOmniVolatileInt32Helper = record helper for TOmniVolatileInt32
  {$ENDIF}
    procedure Initialize(Value: integer);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                  inline;
    function  Increment: integer;                                        inline;
    function  Decrement: integer;                                        inline;
    function  Add(Addend: integer): integer;                             inline;
    function  CompareAndExchange(OldValue, NewValue: integer): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TOmniVolatileInt32);                        inline;
    function  Read: integer;                                             inline;
    procedure Write( const Value: integer);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: integer): TOmniVolatileInt32;          inline;
      class operator Implicit( Value: TOmniVolatileInt32): integer;          inline;
    {$ENDIF}
  end; { TOmniVolatileInt32 / TOmniVolatileInt32Helper }

  /// <remarks> TOmniVolatileInt32 is a thead-safe unsigned 32-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatileUInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PCardinal;
  {$ELSE}
  cardinal;
  TOmniVolatileUInt32Helper = record helper for TOmniVolatileUInt32
  {$ENDIF}
    procedure Initialize(Value: cardinal);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                   inline;
    function  Increment: cardinal;                                        inline;
    function  Decrement: cardinal;                                        inline;
    /// <remarks>Returns True iff was above zero and decremented.</remarks>
    function  DecrementIfAboveZero: boolean;                              inline;
    function  Add(Addend: cardinal): cardinal;                            inline;
    function  CompareAndExchange(OldValue, NewValue: cardinal): boolean;  inline;
    function  Exchange(Value: cardinal): cardinal;                        inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TOmniVolatileUInt32);                        inline;
    function  Read: cardinal;                                             inline;
    procedure Write(const Value: cardinal);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: cardinal): TOmniVolatileUInt32;          inline;
      class operator Implicit(Value: TOmniVolatileUInt32): cardinal;          inline;
    {$ENDIF}
  end; { TOmniVolatileUInt32 / TOmniVolatileUInt32Helper }

  /// <remarks> TOmniVolatileInt64 is a thead-safe signed 64-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatileInt64 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInt64;
  {$ELSE}
  int64;
  TOmniVolatileInt64Helper = record helper for TOmniVolatileInt64
  {$ENDIF}
    procedure Initialize(Value: int64);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                inline;
    function  Increment: int64;                                        inline;
    function  Decrement: int64;                                        inline;
    function  Add(Addend: int64): int64;                               inline;
    function  CompareAndExchange(OldValue, NewValue: int64): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TOmniVolatileInt64);                      inline;
    function  Read: int64;                                             inline;
    procedure Write(const Value: int64);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: int64): TOmniVolatileInt64;           inline;
      class operator Implicit(Value: TOmniVolatileInt64): int64;           inline;
    {$ENDIF}
  end; { TOmniVolatileInt64 / TOmniVolatileInt64Helper }

  /// <remarks> TOmniVolatileUInt64 is a thead-safe unsigned 64-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatileUInt64 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PUInt64;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  uint64;
  TOmniVolatileUInt64Helper = record helper for TOmniVolatileUInt64
  {$ENDIF}
    procedure Initialize(Value: uint64);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                 inline;
    function  Increment: uint64;                                        inline;
    function  Decrement: uint64;                                        inline;
    function  Add(Addend: uint64): uint64;                              inline;
    function  CompareAndExchange(OldValue, NewValue: uint64): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TOmniVolatileUInt64);                      inline;
    function  Read: uint64;                                             inline;
    procedure Write(const Value: uint64);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: uint64): TOmniVolatileUInt64;          inline;
      class operator Implicit(Value: TOmniVolatileUInt64): uint64;          inline;
    {$ENDIF}
  end; { TOmniVolatileUInt64 / TOmniVolatileUInt64Helper }

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  PNativeInteger  = ^NativeInt;
  PNativeUInteger = ^NativeUInt;
  {$ENDIF}

  {$IF SizeOf(NativeInt) = SizeOf(integer)}
  TOmniVolatileNativeInt = TOmniVolatileInt32;
  {$ELSE}
  TOmniVolatileNativeInt = TOmniVolatileInt64;
  {$IFEND}

  {$IF SizeOf(NativeUInt) = SizeOf(cardinal)}
  TOmniVolatileNativeUInt = TOmniVolatileUint32;
  {$ELSE}
  TOmniVolatileNativeUInt = TOmniVolatileUint64;
  {$IFEND}

  {$IF SizeOf(TThreadId) = SizeOf(cardinal)}
  TOmniVolatileThreadId = TOmniVolatileUint32;
  {$ELSE}
  TOmniVolatileThreadId = TOmniVolatileUint64;
  {$IFEND}

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  PObject = ^TObject;
  {$ENDIF}

  /// <remarks> TOmniVolatileObject is a thead-safe object reference. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatileObject = record
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    FStore: TBytes;
    FRef  : PObject;
  {$ELSE}
    FPayload: TObject;
  {$ENDIF}
    procedure Initialize;             {$IFNDEF USE_SLACKSPACE_ALIGNMENT} inline; {$ENDIF}
    /// <remarks> Finalize also frees the object. </remarks>
    procedure Finalize;                                                  inline;
    function  CompareAndExchange(OldValue, NewValue: TObject): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TOmniVolatileObject);                       inline;
    function  ReadObj: TObject;                                          inline;
    function  Read<T: class>: T;                                         inline;
    function  IsAssigned: boolean;                                       inline;
    procedure Write(const Value: TObject);                               inline;
    procedure Free;                                                      inline;
  end; { TOmniVolatileObject }

  /// <remarks> TOmniVolatilePointer is a thead-safe pointer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TOmniVolatilePointer =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PPointer;
  {$ELSE}
  pointer;
  TOmniVolatilePointerHelper = record helper for TOmniVolatilePointer
  {$ENDIF}
    procedure Initialize;               {$IFNDEF USE_SLACKSPACE_ALIGNMENT} inline; {$ENDIF}
    procedure Finalize;                                                    inline;
    function  CompareAndExchange(OldValue, NewValue: pointer): boolean;    inline;
    procedure Assign(var Source: TOmniVolatilePointer);                        inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    function  Read: pointer;                                               inline;
    procedure Write(const Value: pointer);                                 inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: pointer): TOmniVolatilePointer;           inline;
      class operator Implicit(Value: TOmniVolatilePointer): pointer;           inline;
    {$ENDIF}
  end; { TOmniVolatilePointer / TOmniVolatilePointerHelper }

  TOmniAtomic = class
  private
    class var [Volatile] FCacheLineSize: TOmniVolatileInt32;
  public type
    TObjectFactory = reference to function: TObject;
    TInterfaceFactory = reference to function: IInterface;

    /// <remarks>Returns True iff the datum can be read or written directly atomically.</remarks>
    class function IsReadWriteAtomicallyIntegral(DatumAddress: pointer; DatumLength: integer): boolean;   overload;
    /// <remarks>Returns True iff the datum can be read or written directly atomically.</remarks>
    class function IsReadWriteAtomicallyIntegral<T>(var Datum: T): boolean;                               overload;
    class procedure Assert_IsReadWriteAtomicallyIntegral<T>(var Datum: T);                                inline;

    /// <remarks>Returns True iff the datum is within one cache line, and does not cross cache lines.</remarks>
    class function IsWithinCacheLines(DatumAddress: pointer; DatumLength: integer): boolean;              overload;
    /// <remarks>Returns True iff the datum is within one cache line, and does not cross cache lines.</remarks>
    class function IsWithinCacheLines<T>(var Datum: T): boolean;                                          overload;
    class procedure Assert_IsWithinCacheLines<T>(var Datum: T);                                           inline;

    /// <remarks>Returns True iff we can call AtomicXXX() functions for TInterlocked class functions on
    //    the datum with absolute atomic integrity.</remarks>
    class function IsInterlockedIntegral(DatumAddress: pointer; DatumLength: integer): boolean;           overload;
    /// <remarks>Returns True iff we can call AtomicXXX() functions for TInterlocked class functions on
    //    the datum with absolute atomic integrity.</remarks>
    class function IsInterlockedIntegral<T>(var Datum: T): boolean;                                       overload;
    class procedure Assert_IsInterlockedIntegral<T>(var Datum: T);                                        inline;

    /// <remarks>Initialize() Assumes that storage is aligned within cache lines.</remarks>
    class function Initialize(var storage: TObject; Factory: TObjectFactory): TObject; overload;

    /// <remarks>Initialize() Assumes that storage is aligned within cache lines.</remarks>
    class function Initialize(var storage: IInterface; Factory: TInterfaceFactory): IInterface; overload;

    /// <remarks>Initialize() Assumes that TOmniVolatileObject.Initialize() has already been called.</remarks>
    class function InitializeVolatile(var storage: TOmniVolatileObject; Factory: TObjectFactory): TOmniVolatileObject;

    /// <remarks>FreeAndNil() ia an atomic version of SysUtils.FreeAndNil()</remarks>
    class procedure FreeAndNil(var storage: TObject);

    /// <remarks>FreeAndNilVolatile() ia an atomic version of SysUtils.FreeAndNil().
    ///   TOmniVolatileObject.Finalize() is NOT called.</remarks>
    class procedure FreeAndNilVolatile(var storage: TOmniVolatileObject);
  end; { TOmniAtomic }

{$ALIGN 8}
  [Volatile]
  TOmniAtomicSpinLock = record
    [Volatile] FEntered   : TOmniVolatileThreadId;
    [Volatile] FEntryCount: TOmniVolatileInt32;
    procedure Initialize;
    procedure Finalize;
    function  Enter( TimeOut: cardinal): boolean; overload;
    procedure Enter;                              overload;
    procedure Leave;
    procedure WithinLock( Action: System.SysUtils.TProc);
  end;
  POmniAtomicSpinLock = ^TOmniAtomicSpinLock;

implementation

uses
  {$IFDEF MSWINDOWS}
    WinApi.Windows,
  {$ENDIF}
  System.SyncObjs, System.Classes, System.RTLConsts, System.Diagnostics;

{ TOmniVolatileInt32 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileInt32.Initialize(Value: integer);
var
  addr        : NativeInt;
  misalignment: integer;
begin
  SetLength(FStore, SizeOf(Value) * 2);
  addr := NativeInt(@FStore[0]);
  misalignment := addr mod SizeOf(Value);
  if misalignment <> 0 then
    Inc(addr, SizeOf(Value) - misalignment);
  FRef  := PInteger(addr);
  FRef^ := Value;
  // TOmniAtomic.Assert_IsInterlockedIntegral<integer>(FRef^)
end; { TOmniVolatileInt32.Initialize }

procedure TOmniVolatileInt32.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TOmniVolatileInt32.Finalize }

function TOmniVolatileInt32.Add(Addend: integer): integer;
begin
  Result := TInterlocked.Add(FRef^, Addend);
end; { TOmniVolatileInt32.Add }

procedure TOmniVolatileInt32.Assign(var Source: TOmniVolatileInt32);
begin
  FRef^ := Source.FRef^;
end; { TOmniVolatileInt32.Assign }

function TOmniVolatileInt32.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TOmniVolatileInt32.CompareAndExchange }

function TOmniVolatileInt32.Decrement: integer;
begin
  Result := TInterlocked.Decrement(FRef^);
end; { TOmniVolatileInt32.Decrement }

class operator TOmniVolatileInt32.Implicit(Value: integer): TOmniVolatileInt32;
begin
  Result.FRef^ := Value;
end; { TOmniVolatileInt32.Implicit }

class operator TOmniVolatileInt32.Implicit(Value: TOmniVolatileInt32): integer;
begin
  Result := Value.FRef^;
end; { TOmniVolatileInt32.Implicit }

function TOmniVolatileInt32.Read: integer;
begin
  Result := self.FRef^;
end; { TOmniVolatileInt32.Read }

procedure TOmniVolatileInt32.Write(const Value: integer);
begin
  self.FRef^ := Value;
end; { TOmniVolatileInt32.Write }

function TOmniVolatileInt32.Increment: integer;
begin
  Result := TInterlocked.Increment(FRef^);
end; { TOmniVolatileInt32.Increment }
{$ENDIF}

{ TOmniVolatileInt32Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileInt32Helper.Initialize(Value: integer);
begin
  Self := Value;
//  TOmniAtomic.Assert_IsInterlockedIntegral<integer>(Self);
end; { TOmniVolatileInt32Helper.Initialize }

procedure TOmniVolatileInt32Helper.Finalize;
begin
  // do nothing
end; { TOmniVolatileInt32Helper.Finalize }

{$HINTS OFF}
function TOmniVolatileInt32Helper.Add(Addend: integer): integer;
begin
  Result := TInterlocked.Add(Self, Addend);
end; { TOmniVolatileInt32Helper.Add }

procedure TOmniVolatileInt32Helper.Assign(var Source: TOmniVolatileInt32);
begin
  TInterlocked.Exchange(Self, TInterlocked.CompareExchange(Source, 0, 0));
end; { TOmniVolatileInt32Helper.Assign }

function TOmniVolatileInt32Helper.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TOmniVolatileInt32Helper.CompareAndExchange }

function TOmniVolatileInt32Helper.Decrement: integer;
begin
  Result := TInterlocked.Decrement(Self);
end; { TOmniVolatileInt32Helper.Decrement }

function TOmniVolatileInt32Helper.Increment: integer;
begin
  Result := TInterlocked.Increment(Self);
end; { TOmniVolatileInt32Helper.Increment }

function TOmniVolatileInt32Helper.Read: integer;
begin
  Result := TInterlocked.CompareExchange(Self, 0, 0);
end; { TOmniVolatileInt32Helper.Read }

procedure TOmniVolatileInt32Helper.Write(const Value: integer);
begin
  TInterlocked.Exchange(Self, Value);
end; { TOmniVolatileInt32Helper.Write }
{$HINTS ON}
{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileUInt32 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileUInt32.Initialize(Value: cardinal);
var
  addr        : NativeInt;
  misalignment: integer;
begin
  SetLength(FStore, SizeOf(Value) * 2);
  addr := NativeInt(@FStore[0]);
  misalignment := addr mod SizeOf(Value);
  if misalignment <> 0 then
    Inc(addr, SizeOf(Value) - misalignment);
  FRef  := PCardinal(addr);
  FRef^ := Value;
  // TOmniAtomic.Assert_IsInterlockedIntegral<cardinal>(FRef^)
end; { TOmniVolatileUInt32.Initialize }

procedure TOmniVolatileUInt32.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TOmniVolatileUInt32.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TOmniVolatileUInt32.Add(Addend: cardinal): cardinal;
begin
  Result := cardinal(TInterlocked.Add(integer(FRef^), integer(Addend)));
end; { TOmniVolatileUInt32.Add }

function TOmniVolatileUInt32.Increment: cardinal;
begin
  Result := cardinal(TInterlocked.Increment(integer(FRef^)));
end; { TOmniVolatileUInt32.Increment }

function TOmniVolatileUInt32.Decrement: cardinal;
begin
  Result := cardinal(TInterlocked.Decrement(integer(FRef^)));
end; { TOmniVolatileUInt32.Decrement }

function TOmniVolatileUInt32.DecrementIfAboveZero: boolean;
var
  val: cardinal;
begin
  Result := False;
  repeat
    val := FRef^;
    if val > 0 then
      Result := TInterlocked.CompareExchange(integer(FRef^), integer(val - 1), integer(val)) = integer(val)
  until Result or (val = 0);
end; { TOmniVolatileUInt32.DecrementIfAboveZero }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

procedure TOmniVolatileUInt32.Assign(var Source: TOmniVolatileUInt32);
begin
  FRef^ := Source.FRef^;
end; { TOmniVolatileUInt32.Assign }

function TOmniVolatileUInt32.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  Result := TInterlocked.CompareExchange(pinteger(FRef)^, integer(NewValue), integer(OldValue)) = integer(OldValue);
end; { TOmniVolatileUInt32.CompareAndExchange }

function TOmniVolatileUInt32.Exchange(Value: cardinal): cardinal;
begin
  Result := TInterlocked.Exchange(pinteger(FRef)^, Value);
end; { TOmniVolatileUInt32.Exchange }

class operator TOmniVolatileUInt32.Implicit(Value: cardinal): TOmniVolatileUInt32;
begin
  Result.FRef^ := Value;
end; { TOmniVolatileUInt32.Implicit }

class operator TOmniVolatileUInt32.Implicit(Value: TOmniVolatileUInt32): cardinal;
begin
  Result := Value.FRef^;
end; { TOmniVolatileUInt32.Implicit }

function TOmniVolatileUInt32.Read: cardinal;
begin
  Result := Self.FRef^;
end; { TOmniVolatileUInt32.Read }

procedure TOmniVolatileUInt32.Write(const Value: cardinal);
begin
  Self.FRef^ := Value;
end; { TOmniVolatileUInt32.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileUInt32Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileUInt32Helper.Initialize(Value: cardinal);
begin
  Self := Value;
//  TOmniAtomic.Assert_IsInterlockedIntegral<cardinal>(Self);
end; { TOmniVolatileUInt32Helper.Initialize }

procedure TOmniVolatileUInt32Helper.Finalize;
begin
  // do nothing
end; { TOmniVolatileUInt32Helper.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$HINTS OFF}
function TOmniVolatileUInt32Helper.Add(Addend: cardinal): cardinal;
begin
  Result := cardinal(TInterlocked.Add(integer(Self), integer(Addend)));
end; { TOmniVolatileUInt32Helper.Add }

function TOmniVolatileUInt32Helper.Decrement: cardinal;
begin
  Result := cardinal(TInterlocked.Decrement(integer(Self)));
end; { TOmniVolatileUInt32Helper.Decrement }

function TOmniVolatileUInt32Helper.Increment: cardinal;
begin
  Result := cardinal(TInterlocked.Increment(integer(Self)));
end; { TOmniVolatileUInt32Helper.Increment }

function TOmniVolatileUInt32Helper.DecrementIfAboveZero: boolean;
var
  val: cardinal;
begin
  Result := False;
  repeat
    val := Self;
    if val > 0 then
      Result := TInterlocked.CompareExchange(integer(Self), integer(val - 1), integer(val)) = integer(val)
  until Result or (val = 0);
end; { TOmniVolatileUInt32Helper.DecrementIfAboveZero }
{$HINTS ON}

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{ TOmniVolatileUInt32Helper }

{$HINTS OFF}
procedure TOmniVolatileUInt32Helper.Assign(var Source: TOmniVolatileUInt32);
begin
  TInterlocked.Exchange(integer(Self), TInterlocked.CompareExchange(integer(Source), 0, 0));
end; { TOmniVolatileUInt32Helper.Assign }

function TOmniVolatileUInt32Helper.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  Result := TInterlocked.CompareExchange(integer(Self), integer(NewValue), integer(OldValue)) = integer(OldValue);
end; { TOmniVolatileUInt32Helper.CompareAndExchange }

function TOmniVolatileUInt32Helper.Exchange(Value: cardinal): cardinal;
begin
  Result := TInterlocked.Exchange(integer(Self), Value);
end; { TOmniVolatileUInt32Helper.Exchange }

function TOmniVolatileUInt32Helper.Read: cardinal;
begin
  Result := cardinal(TInterlocked.CompareExchange(integer(Self), 0, 0));
end; { TOmniVolatileUInt32Helper.Read }

procedure TOmniVolatileUInt32Helper.Write(const Value: cardinal);
begin
  TInterlocked.Exchange(integer(Self), integer(Value));
end; { TOmniVolatileUInt32Helper.Write }

{$HINTS ON}

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileInt64 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileInt64.Initialize(Value: int64);
var
  addr        : NativeInt;
  misalignment: integer;
begin
  SetLength(FStore, SizeOf(Value) * 2);
  addr := NativeInt(@FStore[0]);
  misalignment := addr mod SizeOf(Value);
  if misalignment <> 0 then
    Inc(addr, SizeOf(Value) - misalignment);
  FRef  := PInt64(addr);
  FRef^ := Value;
  // TOmniAtomic.Assert_IsInterlockedIntegral<int64>(FRef^)
end; { TOmniVolatileInt64.Initialize }

procedure TOmniVolatileInt64.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TOmniVolatileInt64.Finalize }

function TOmniVolatileInt64.Add(Addend: int64): int64;
begin
  Result := TInterlocked.Add(FRef^, Addend);
end; { TOmniVolatileInt64.Add }

function TOmniVolatileInt64.CompareAndExchange(
  OldValue, NewValue: int64): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TOmniVolatileInt64.CompareAndExchange }

function TOmniVolatileInt64.Decrement: int64;
begin
  Result := TInterlocked.Decrement(FRef^);
end; { TOmniVolatileInt64.Decrement }

class operator TOmniVolatileInt64.Implicit(Value: int64): TOmniVolatileInt64;
begin
  {$IFDEF CPU64BITS}
    Result.FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(Result.FRef^, Value);
  {$ENDIF}
end; { TOmniVolatileInt64.Implicit }

class operator TOmniVolatileInt64.Implicit(Value: TOmniVolatileInt64): int64;
begin
  {$IFDEF CPU64BITS}
    Result := Value.FRef^;
  {$ELSE}
    Result := TInterlocked.Read(Value.FRef^);
  {$ENDIF}
end; { TOmniVolatileInt64.Implicit }

function TOmniVolatileInt64.Increment: int64;
begin
  Result := TInterlocked.Increment(FRef^);
end; { TOmniVolatileInt64.Increment }

procedure TOmniVolatileInt64.Assign(var Source: TOmniVolatileInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^;
  {$ELSE}
    TInterlocked.Exchange(FRef^, TInterlocked.CompareExchange(Source.FRef^, 0, 0));
  {$ENDIF}
end; { TOmniVolatileInt64.Assign }

function TOmniVolatileInt64.Read: int64;
begin
  {$IFDEF CPU64BITS}
    Result := FRef^;
  {$ELSE}
    Result := TInterlocked.Read(FRef^);
  {$ENDIF}
end; { TOmniVolatileInt64.Read }

procedure TOmniVolatileInt64.Write(const Value: int64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(FRef^, Value);
  {$ENDIF}
end; { TOmniVolatileInt64.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileInt64Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileInt64Helper.Initialize(Value: int64);
begin
  Self := Value;
//  TOmniAtomic.Assert_IsInterlockedIntegral<int64>(Self);
end; { TOmniVolatileInt64Helper.Initialize }

procedure TOmniVolatileInt64Helper.Finalize;
begin
  // do nothing
end; { TOmniVolatileInt64Helper.Finalize }

{$HINTS OFF}
function TOmniVolatileInt64Helper.Increment: int64;
begin
  Result := TInterlocked.Increment(Self);
end; { TOmniVolatileInt64Helper.Increment }

function TOmniVolatileInt64Helper.Decrement: int64;
begin
  Result := TInterlocked.Decrement(Self);
end; { TOmniVolatileInt64Helper.Decrement }

function TOmniVolatileInt64Helper.Add(Addend: int64): int64;
begin
  Result := TInterlocked.Add(Self, Addend);
end; { TOmniVolatileInt64Helper.Add }

function TOmniVolatileInt64Helper.CompareAndExchange(OldValue, NewValue: int64): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TOmniVolatileInt64Helper.CompareAndExchange }

procedure TOmniVolatileInt64Helper.Assign(var Source: TOmniVolatileInt64);
begin
  TInterlocked.Exchange(Self, TInterlocked.Read(Source));
end; { TOmniVolatileInt64Helper.Assign }

function TOmniVolatileInt64Helper.Read: int64;
begin
  Result := TInterlocked.Read(Self);
end; { TOmniVolatileInt64Helper.Read }

procedure TOmniVolatileInt64Helper.Write(const Value: int64);
begin
  TInterlocked.Exchange(Self, Value);
end; { TOmniVolatileInt64Helper.Write }

{$HINTS OFF}

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileUInt64 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileUInt64.Initialize(Value: uint64);
var
  addr        : NativeInt;
  misalignment: integer;
begin
  SetLength(FStore, SizeOf(Value) * 2);
  addr := NativeInt(@FStore[0]);
  misalignment := addr mod SizeOf(Value);
  if misalignment <> 0 then
    Inc(addr, SizeOf(Value) - misalignment);
  FRef  := PUInt64(addr);
  FRef^ := Value;
  // TOmniAtomic.Assert_IsInterlockedIntegral<uint64>(FRef^);
end; { TOmniVolatileUInt64.Initialize }

procedure TOmniVolatileUInt64.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TOmniVolatileUInt64.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TOmniVolatileUInt64.Add(Addend: uint64): uint64;
begin
  Result := uint64(TInterlocked.Add(pint64(FRef)^, int64(Addend)));
end; { TOmniVolatileUInt64.Add }

function TOmniVolatileUInt64.Decrement: uint64;
begin
  Result := uint64(TInterlocked.Decrement(pint64(FRef)^));
end; { TOmniVolatileUInt64.Decrement }

function TOmniVolatileUInt64.Increment: uint64;
begin
  Result := uint64(TInterlocked.Increment(pint64(FRef)^));
end; { TOmniVolatileUInt64.Increment }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TOmniVolatileUInt64.CompareAndExchange(
  OldValue, NewValue: uint64): boolean;
begin
  Result := TInterlocked.CompareExchange(pint64(FRef)^, int64(NewValue), int64(OldValue)) = int64(OldValue);
end; { TOmniVolatileUInt64.CompareAndExchange }

class operator TOmniVolatileUInt64.Implicit(Value: uint64): TOmniVolatileUInt64;
begin
  {$IFDEF CPU64BITS}
    Result.FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(pint64(Result.FRef)^, int64(Value));
  {$ENDIF}
end; { TOmniVolatileUInt64.Implicit }

class operator TOmniVolatileUInt64.Implicit(Value: TOmniVolatileUInt64): uint64;
begin
  {$IFDEF CPU64BITS}
    Result := Value.FRef^;
  {$ELSE}
    Result := uint64(TInterlocked.Read(pint64(Value.FRef)^));
  {$ENDIF}
end; { TOmniVolatileUInt64.Implicit }

procedure TOmniVolatileUInt64.Assign(var Source: TOmniVolatileUInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^;
  {$ELSE}
    TInterlocked.Exchange(pint64(FRef)^, TInterlocked.CompareExchange(pint64(Source.FRef)^, 0, 0));
  {$ENDIF}
end; { TOmniVolatileUInt64.Assign }

function TOmniVolatileUInt64.Read: uint64;
begin
  {$IFDEF CPU64BITS}
    Result := FRef^;
  {$ELSE}
    Result := uint64(TInterlocked.Read(pint64(FRef)^));
  {$ENDIF}
end; { TOmniVolatileUInt64.Read }

procedure TOmniVolatileUInt64.Write(const Value: uint64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(pint64(FRef)^, int64(Value));
  {$ENDIF}
end; { TOmniVolatileUInt64.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileUInt64Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatileUInt64Helper.Initialize(Value: uint64);
begin
  Self := Value;
//  TOmniAtomic.Assert_IsInterlockedIntegral<uint64>(Self);
end; { TOmniVolatileUInt64Helper.Initialize }

procedure TOmniVolatileUInt64Helper.Finalize;
begin
  // do nothing
end; { TOmniVolatileUInt64Helper.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TOmniVolatileUInt64Helper.Increment: uint64;
begin
  Result := uint64(TInterlocked.Increment(int64(Self)));
end; { TOmniVolatileUInt64Helper.Increment }

function TOmniVolatileUInt64Helper.Decrement: uint64;
begin
  Result := uint64(TInterlocked.Decrement(int64(Self)));
end; { TOmniVolatileUInt64Helper.Decrement }

function TOmniVolatileUInt64Helper.Add(Addend: uint64): uint64;
begin
  Result := uint64(TInterlocked.Add(int64(Self), int64(Addend)));
end; { TOmniVolatileUInt64Helper.Add }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TOmniVolatileUInt64Helper.CompareAndExchange(OldValue, NewValue: uint64): boolean;
begin
  Result := TInterlocked.CompareExchange(int64(Self), int64(NewValue), int64(OldValue)) = int64(OldValue);
end; { TOmniVolatileUInt64Helper.CompareAndExchange }

procedure TOmniVolatileUInt64Helper.Assign(var Source: TOmniVolatileUInt64);
begin
  TInterlocked.Exchange(int64(Self), TInterlocked.Read(int64(Source)));
end; { TOmniVolatileUInt64Helper.Assign }

function TOmniVolatileUInt64Helper.Read: uint64;
begin
  Result := TInterlocked.Read(int64(Self));
end; { TOmniVolatileUInt64Helper.Read }

procedure TOmniVolatileUInt64Helper.Write(const Value: uint64);
begin
  TInterlocked.Exchange(int64(Self), int64(Value));
end; { TOmniVolatileUInt64Helper.Write }

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatilePointer }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatilePointer.Initialize;
var
  addr        : NativeInt;
  misalignment: integer;
begin
  SetLength(FStore, SizeOf(pointer) * 2);
  addr := NativeInt(@FStore[0]);
  misalignment := addr mod SizeOf(pointer);
  if misalignment <> 0 then
    Inc(addr, SizeOf(pointer) - misalignment);
  FRef  := PPointer(addr);
  FRef^ := nil;
  // TOmniAtomic.Assert_IsInterlockedIntegral<pointer>(FRef^);
end; { TOmniVolatilePointer.Initialize }

procedure TOmniVolatilePointer.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TOmniVolatilePointer.Finalize }

procedure TOmniVolatilePointer.Assign(var Source: TOmniVolatilePointer);
begin
  FRef^ := Source.FRef^;
end; { TOmniVolatilePointer.Assign }

function TOmniVolatilePointer.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TOmniVolatilePointer.CompareAndExchange }

class operator TOmniVolatilePointer.Implicit(Value: pointer): TOmniVolatilePointer;
begin
  Result.FRef^ := Value;
end; { TOmniVolatilePointer.Implicit }

class operator TOmniVolatilePointer.Implicit(Value: TOmniVolatilePointer): pointer;
begin
  Result := Value.FRef^;
end; { TOmniVolatilePointer.Implicit }

function TOmniVolatilePointer.Read: pointer;
begin
  Result := Self.FRef^;
end; { TOmniVolatilePointer.Read }

procedure TOmniVolatilePointer.Write(const Value: pointer);
begin
  Self.FRef^ := Value;
end; { TOmniVolatilePointer.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatilePointerHelper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TOmniVolatilePointerHelper.Initialize;
begin
  Self := nil;
//  TOmniAtomic.Assert_IsInterlockedIntegral<pointer>(Self);
end; { TOmniVolatilePointerHelper.Initialize }

procedure TOmniVolatilePointerHelper.Finalize;
begin
  // do nothing
end; { TOmniVolatilePointerHelper.Finalize }

procedure TOmniVolatilePointerHelper.Assign(var Source: pointer);
begin
  TInterlocked.Exchange(Self, TInterlocked.CompareExchange(Source, nil, nil));
end; { TOmniVolatilePointerHelper.Assign }

function TOmniVolatilePointerHelper.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TOmniVolatilePointerHelper.CompareAndExchange }

function TOmniVolatilePointerHelper.Read: pointer;
begin
  Result := TInterlocked.CompareExchange(Self, nil, nil);
end; { TOmniVolatilePointerHelper.Read }

procedure TOmniVolatilePointerHelper.Write(const Value: pointer);
begin
  TInterlocked.Exchange(Self, Value);
end; { TOmniVolatilePointerHelper.Write }
{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TOmniVolatileObject }

procedure TOmniVolatileObject.Initialize;
{$IFDEF USE_SLACKSPACE_ALIGNMENT}
var
  addr        : NativeInt;
  misalignment: integer;
{$ENDIF}
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    SetLength(FStore, SizeOf(TObject) * 2);
    addr := NativeInt(@FStore[0]);
    misalignment := addr mod SizeOf(TObject);
    if misalignment <> 0 then
      Inc(addr, SizeOf(TObject) - misalignment);
    FRef  := PObject(addr);
    FRef^ := nil;
    // TOmniAtomic.Assert_IsInterlockedIntegral<TObject>(FRef^);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    FPayload := nil;
//    TOmniAtomic.Assert_IsInterlockedIntegral<TObject>(FPayload);
  {$ENDIF}
end; { TOmniVolatileObject.Initialize }

procedure TOmniVolatileObject.Finalize;
var
  obj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    obj := TInterlocked.Exchange(FRef^, TObject(nil));
    SetLength(FStore, 0);
    FRef := nil;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    obj := TInterlocked.Exchange(FPayload, TObject(nil));
  {$ENDIF}
  obj.Free;
end; { TOmniVolatileObject.Finalize }

function TOmniVolatileObject.CompareAndExchange(OldValue, NewValue: TObject): boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FPayload, NewValue, OldValue) = OldValue;
  {$ENDIF}
end; { TOmniVolatileObject.CompareAndExchange }

procedure TOmniVolatileObject.Assign(var Source: TOmniVolatileObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FRef^, TInterlocked.CompareExchange(Source.FRef^, TObject(nil), TObject(nil)));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FPayload, TInterlocked.CompareExchange(Source.FPayload, TObject(nil), TObject(nil)));
  {$ENDIF}
end; { TOmniVolatileObject.Assign }

function TOmniVolatileObject.ReadObj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FRef^, TObject(nil), TObject(nil));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FPayload, TObject(nil), TObject(nil));
  {$ENDIF}
end; { TOmniVolatileObject.ReadObj }

function TOmniVolatileObject.IsAssigned: boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := Assigned(FRef^);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := Assigned(FPayload);
  {$ENDIF}
end; { TOmniVolatileObject.IsAssigned }

function TOmniVolatileObject.Read< T>: T;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := T(TInterlocked.CompareExchange(FRef^, TObject(nil), TObject(nil)));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := T(TInterlocked.CompareExchange(FPayload, TObject(nil), TObject(nil)));
  {$ENDIF}
end; { TOmniVolatileObject.Read }

procedure TOmniVolatileObject.Write(const Value: TObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FRef^, Value);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FPayload, Value);
  {$ENDIF}
end; { TOmniVolatileObject.Write }

procedure TOmniVolatileObject.Free;
var
  obj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    obj := TInterlocked.Exchange(FRef^, TObject(nil));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    obj := TInterlocked.Exchange(FPayload, TObject(nil));
  {$ENDIF}
  obj.Free;
end; { TOmniVolatileObject.Free }

{ TOmniAtomic }

class function TOmniAtomic.IsReadWriteAtomicallyIntegral(DatumAddress: pointer; DatumLength: integer): boolean;
begin
  Result :=
    ((DatumLength = 1)
      or (DatumLength = 2)
      or (DatumLength = 4)
      {$IFDEF CPU64BITS}
      or (DatumLength = 8)
      {$ENDIF}
    )
    and ((NativeInt(DatumAddress) mod DatumLength) = 0);
end; { TOmniAtomic.IsReadWriteAtomicallyIntegral }

class function TOmniAtomic.IsReadWriteAtomicallyIntegral<T>(var Datum: T): boolean;
begin
  Result := IsReadWriteAtomicallyIntegral(@Datum, SizeOf(Datum));
end; { TOmniAtomic.IsReadWriteAtomicallyIntegral }

{$IFDEF MSWINDOWS}
function Compute_GetCacheLineSize: Integer;
var
  info: array of TSystemLogicalProcessorInformation;
  j   : integer;
  len : DWORD;
  sz  : DWORD;
begin
  Result := 64;
  len    := 0;
  if (GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetLogicalProcessorInformation') <> nil)
     and (not GetLogicalProcessorInformation(nil, len))
     and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    sz := (len + SizeOf(TSystemLogicalProcessorInformation) - 1) div SizeOf(TSystemLogicalProcessorInformation);
    SetLength(info, sz);
    len := sz * SizeOf(TSystemLogicalProcessorInformation);
    if (sz > 0) and GetLogicalProcessorInformation(@info[0], len) then
      for j := 0 to sz - 1 do begin
        if (info[j].Relationship <> RelationCache)
           or (info[j].Cache.Level <> 1)
        then
          continue;
        Result := info[j].Cache.LineSize;
        break;
      end;
  end;
end; { Compute_GetCacheLineSize }

{$ELSEIF defined(ANDROID)}

function Compute_GetCacheLineSize: Integer;
begin
  Result := 64;
end; { Compute_GetCacheLineSize }

{$ELSEIF defined(POSIX)}

function Compute_GetCacheLineSize: Integer;
var
  lineSize: uint64;
  sz      : integer;
begin
  sz := SizeOf(lineSize);
  if sysctlbyname('hw.cachelinesize', @lineSize, @sz, nil, 0) = 0 then
    Result := lineSize
  else
    Result := 64;
end; { Compute_GetCacheLineSize }

{$IFEND}

class function TOmniAtomic.IsWithinCacheLines<T>(var Datum: T): boolean;
begin
  Result := IsWithinCacheLines(@Datum, SizeOf(Datum));
end; { TOmniAtomic.IsWithinCacheLines }

class function TOmniAtomic.IsWithinCacheLines(DatumAddress: pointer; DatumLength: integer): boolean;
var
  cacheLineSize: integer;
begin
  cacheLineSize := FCacheLineSize.Read;
  if cacheLineSize = 0 then begin
    cacheLineSize := Compute_GetCacheLineSize;
    FCacheLineSize.Write(cacheLineSize);
  end;
  Result := ((NativeInt(DatumAddress) mod
    {$IFDEF CPU32BITS} 4 {$ENDIF}  {$IFDEF CPU64BITS} 8 {$ENDIF}
       ) + DatumLength) <=
    {$IFDEF CPU32BITS} 4 {$ENDIF}  {$IFDEF CPU64BITS} 8 {$ENDIF}
end; { TOmniAtomic.IsWithinCacheLines }

class function TOmniAtomic.IsInterlockedIntegral(DatumAddress: pointer; DatumLength: integer): boolean;
begin
  {$IFDEF CPUINTEL}
    Result := True;
  {$ENDIF}

  {$IFDEF CPUARM}
    Result := IsWithinCacheLines(DatumAddress, DatumLength);
  {$ENDIF}
end; { TOmniAtomic.IsInterlockedIntegral }

class function TOmniAtomic.IsInterlockedIntegral<T>(var Datum: T): boolean;
begin
  Result := IsInterlockedIntegral(@Datum, SizeOf(Datum));
end; { TOmniAtomic.IsInterlockedIntegral }

class procedure TOmniAtomic.Assert_IsReadWriteAtomicallyIntegral<T>(var Datum: T);
begin
  Assert(TOmniAtomic.IsReadWriteAtomicallyIntegral<T>(Datum),
    'Datum is not aligned as required for atomic read/write integrity.');
end; { TOmniAtomic.Assert_IsReadWriteAtomicallyIntegral }

class procedure TOmniAtomic.Assert_IsWithinCacheLines<T>(var Datum: T);
begin
  Assert(TOmniAtomic.IsWithinCacheLines<T>(Datum),
    'Datum does not fit within a single L1 CPU cache line.');
end; { TOmniAtomic.Assert_IsWithinCacheLines }

class procedure TOmniAtomic.Assert_IsInterlockedIntegral<T>(var Datum: T);
begin
  Assert(TOmniAtomic.IsInterlockedIntegral<T>(Datum),
    'Datum is not aligned as required for AtomicXXX()/ TInterlocked-class function integrity.');
end; { TOmniAtomic.Assert_IsInterlockedIntegral }

class function TOmniAtomic.Initialize(var storage: TObject; Factory: TObjectFactory): TObject;
var
  newValue: TObject;
  oldValue: NativeInt;
begin
//  Assert_IsInterlockedIntegral<TObject>(Storage);
  if TInterlocked.CompareExchange(pointer(Storage), nil, nil) <> nil then
    Result := Storage
  else begin
    newValue := Factory();
    {$IFDEF AUTOREFCOUNT}
      if assigned(newValue) then
        newValue.__ObjAddRef;
    {$ENDIF AUTOREFCOUNT}
    pointer(oldValue) := TInterlocked.CompareExchange(pointer(Storage), pointer(newValue), nil);
    if oldValue = 0 then
      Result := newValue
    else begin
      Result := TObject(oldValue);
      {$IFDEF AUTOREFCOUNT}
        if assigned(newValue) then
          newValue.__ObjRelease;
      {$ELSE}
        newValue.Free;
      {$ENDIF AUTOREFCOUNT}
      end
    end
end; { TOmniAtomic.Initialize }

class function TOmniAtomic.InitializeVolatile(var storage: TOmniVolatileObject; Factory: TObjectFactory): TOmniVolatileObject;
var
  newValue: TObject;
begin
  // Assume Storage Storage.Initialize() already called at this point.
  Result := Storage;
  if Result.IsAssigned then
    Exit;
  newValue := Factory();
  if not Storage.CompareAndExchange(TObject(nil), newValue) then
    newValue.Free;
end; { TOmniAtomic.InitializeVolatile }

class function TOmniAtomic.Initialize(var Storage: IInterface; Factory: TInterfaceFactory): IInterface;
var
  newValue: IInterface;
  oldValue: NativeInt;
begin
//  Assert_IsInterlockedIntegral<IInterface>(Storage);
  if TInterlocked.CompareExchange(pointer(Storage), nil, nil) <> nil then
    Result := Storage
  else begin
    newValue := Factory();
    if assigned(newValue) then
      newValue._AddRef;
    pointer(oldValue) := TInterlocked.CompareExchange(pointer(Storage), pointer(newValue), nil);
    if oldValue = 0 then
      Result := newValue
    else begin
      Result := IInterface(oldValue);
      if assigned(newValue) then
        newValue._Release;
    end
  end;
end; { TOmniAtomic.Initialize }

class procedure TOmniAtomic.FreeAndNil(var Storage: TObject);
var
  oldValue: pointer;
begin
  oldValue := TInterlocked.CompareExchange(pointer(Storage), nil, nil);
  if assigned(oldValue)
     and (oldValue = TInterlocked.CompareExchange(pointer(Storage), nil, pointer(oldValue)))
  then
    {$IFDEF AUTOREFCOUNT}
      TObject(oldValue).__ObjRelease;
    {$ELSE}
      TObject(oldValue).Free;
    {$ENDIF AUTOREFCOUNT}
end; { TOmniAtomic.FreeAndNil }

class procedure TOmniAtomic.FreeAndNilVolatile(var Storage: TOmniVolatileObject);
var
  oldValue: TObject;
begin
  oldValue := Storage.ReadObj;
  if not assigned(oldValue) then
    Exit;
  if Storage.CompareAndExchange(oldValue, nil) then
    {$IFDEF AUTOREFCOUNT}
      oldValue.__ObjRelease;
    {$ELSE}
      oldValue.Free;
    {$ENDIF AUTOREFCOUNT}
end; { TOmniAtomic.FreeAndNilVolatile }

procedure TOmniAtomicSpinLock.Initialize;
begin
  FEntered.Initialize(0);
  FEntryCount.Initialize(0);
end; { TOmniAtomicSpinLock.Initialize }

procedure TOmniAtomicSpinLock.Finalize;
begin
  FEntered.Finalize;
  FEntryCount.Finalize;
end; { TOmniAtomicSpinLock.Finalize }

const
  MaxCardinal: cardinal = cardinal(-1);

function TOmniAtomicSpinLock.Enter(TimeOut: cardinal): boolean;
const
  InitialYieldCount = 4;
  MaxYieldCount     = 10000;
  InitialSleepTime  = 1; // milliseconds;
  MaxSleepTime      = 100;
var
  sleepTime : integer;
  spinCount : cardinal;
  this      : TThreadId;
  timer     : TStopwatch;
  yieldCount: integer;
begin
  this := TThread.CurrentThread.ThreadID;
  Result := FEntered.Read = this;
  if not Result then begin
    spinCount  := 0;
    yieldCount := InitialYieldCount;
    sleepTime  := InitialSleepTime;
    if TimeOut <> INFINITE then begin
      // Don't use TStopwatch.StartNew; for performance reasons.
    timer.Reset;
    timer.Start
    end;
    repeat
      if spinCount = MaxCardinal then
        spinCount := 0
      else
        Inc(spinCount);

      if (spinCount mod 1000) = 0 then begin
        TThread.Sleep(sleepTime);
        sleepTime := sleepTime shl 1;
        if sleepTime  > MaxSleepTime then
         sleepTime := MaxSleepTime
      end

      else if (spinCount mod 100) = 0 then begin
        TThread.SpinWait(yieldCount);
        yieldCount := yieldCount shl 1;
        if yieldCount >= MaxYieldCount then
          yieldCount := InitialYieldCount;
        end;
    until (FEntered.CompareAndExchange(0, this))
          or ((TimeOut <> INFINITE) and (timer.ElapsedMilliseconds >= Timeout));
    Result := FEntered.Read = this
  end;
  if Result then
    FEntryCount.Increment;
end; { TOmniAtomicSpinLock.Enter }

procedure TOmniAtomicSpinLock.Enter;
begin
  Enter(INFINITE);
end; { TOmniAtomicSpinLock.Enter }

procedure TOmniAtomicSpinLock.Leave;
var
  this: TThreadId;
begin
  this := TThread.CurrentThread.ThreadID;
  if FEntered.Read <> this then
    raise ELockException.CreateRes(@SSpinLockNotOwned);
  if FEntryCount.Decrement <= 0 then
    FEntered.CompareAndExchange(this, 0);
end; { TOmniAtomicSpinLock.Leave }

procedure TOmniAtomicSpinLock.WithinLock(Action: System.SysUtils.TProc);
begin
  Enter(INFINITE);
  Action;
  Leave;
end; { TOmniAtomicSpinLock.WithinLock }

{ initialization }

procedure InitUnit_Atomic;
begin
  TStopWatch.Create;
  TOmniAtomic.FCacheLineSize.Initialize(0);
end; { InitUnit_Atomic }

procedure DoneUnit_Atomic;
begin
  TOmniAtomic.FCacheLineSize.Finalize;
end; { DoneUnit_Atomic }

initialization
  InitUnit_Atomic;
finalization
  DoneUnit_Atomic;
end.
