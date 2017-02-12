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
  TVolatileInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInteger;
  {$ELSE}
  integer;
  TVolatileInt32Helper = record helper for TVolatileInt32
  {$ENDIF}
    procedure Initialize(Value: integer);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                  inline;
    function  Increment: integer;                                        inline;
    function  Decrement: integer;                                        inline;
    function  Add(Addend: integer): integer;                             inline;
    function  CompareAndExchange(OldValue, NewValue: integer): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TVolatileInt32);                        inline;
    function  Read: integer;                                             inline;
    procedure Write( const Value: integer);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: integer): TVolatileInt32;          inline;
      class operator Implicit( Value: TVolatileInt32): integer;          inline;
    {$ENDIF}
  end; { TVolatileInt32 / TVolatileInt32Helper }

  /// <remarks> TVolatileInt32 is a thead-safe unsigned 32-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileUInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PCardinal;
  {$ELSE}
  cardinal;
  TVolatileUInt32Helper = record helper for TVolatileUInt32
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
    procedure Assign(var Source: TVolatileUInt32);                        inline;
    function  Read: cardinal;                                             inline;
    procedure Write(const Value: cardinal);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: cardinal): TVolatileUInt32;          inline;
      class operator Implicit(Value: TVolatileUInt32): cardinal;          inline;
    {$ENDIF}
  end; { TVolatileUInt32 / TVolatileUInt32Helper }

  /// <remarks> TVolatileInt64 is a thead-safe signed 64-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileInt64 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInt64;
  {$ELSE}
  int64;
  TVolatileInt64Helper = record helper for TVolatileInt64
  {$ENDIF}
    procedure Initialize(Value: int64);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                inline;
    function  Increment: int64;                                        inline;
    function  Decrement: int64;                                        inline;
    function  Add(Addend: int64): int64;                               inline;
    function  CompareAndExchange(OldValue, NewValue: int64): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TVolatileInt64);                      inline;
    function  Read: int64;                                             inline;
    procedure Write(const Value: int64);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: int64): TVolatileInt64;           inline;
      class operator Implicit(Value: TVolatileInt64): int64;           inline;
    {$ENDIF}
  end; { TVolatileInt64 / TVolatileInt64Helper }

  /// <remarks> TVolatileUInt64 is a thead-safe unsigned 64-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileUInt64 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PUInt64;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  uint64;
  TVolatileUInt64Helper = record helper for TVolatileUInt64
  {$ENDIF}
    procedure Initialize(Value: uint64);                   {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                 inline;
    function  Increment: uint64;                                        inline;
    function  Decrement: uint64;                                        inline;
    function  Add(Addend: uint64): uint64;                              inline;
    function  CompareAndExchange(OldValue, NewValue: uint64): boolean;  inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign(var Source: TVolatileUInt64);                      inline;
    function  Read: uint64;                                             inline;
    procedure Write(const Value: uint64);                               inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: uint64): TVolatileUInt64;          inline;
      class operator Implicit(Value: TVolatileUInt64): uint64;          inline;
    {$ENDIF}
  end; { TVolatileUInt64 / TVolatileUInt64Helper }

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  PNativeInteger  = ^NativeInt;
  PNativeUInteger = ^NativeUInt;
  {$ENDIF}

  {$IF SizeOf(NativeInt) = SizeOf(integer)}
  TVolatileNativeInt = TVolatileInt32;
  {$ELSE}
  TVolatileNativeInt = TVolatileInt64;
  {$IFEND}

  {$IF SizeOf(NativeUInt) = SizeOf(cardinal)}
  TVolatileNativeUInt = TVolatileUint32;
  {$ELSE}
  TVolatileNativeUInt = TVolatileUint64;
  {$IFEND}

  {$IF SizeOf(TThreadId) = SizeOf(cardinal)}
  TVolatileThreadId = TVolatileUint32;
  {$ELSE}
  TVolatileThreadId = TVolatileUint64;
  {$IFEND}

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  PObject = ^TObject;
  {$ENDIF}

  /// <remarks> TVolatileObject is a thead-safe object reference. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileObject = record
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
    procedure Assign(var Source: TVolatileObject);                       inline;
    function  ReadObj: TObject;                                          inline;
    function  Read<T: class>: T;                                         inline;
    function  IsAssigned: boolean;                                       inline;
    procedure Write(const Value: TObject);                               inline;
    procedure Free;                                                      inline;
  end; { TVolatileObject }

  /// <remarks> TVolatilePointer is a thead-safe pointer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatilePointer =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PPointer;
  {$ELSE}
  pointer;
  TVolatilePointerHelper = record helper for TVolatilePointer
  {$ENDIF}
    procedure Initialize;               {$IFNDEF USE_SLACKSPACE_ALIGNMENT} inline; {$ENDIF}
    procedure Finalize;                                                    inline;
    function  CompareAndExchange(OldValue, NewValue: pointer): boolean;    inline;
    procedure Assign(var Source: TVolatilePointer);                        inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    function  Read: pointer;                                               inline;
    procedure Write(const Value: pointer);                                 inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit(Value: pointer): TVolatilePointer;           inline;
      class operator Implicit(Value: TVolatilePointer): pointer;           inline;
    {$ENDIF}
  end; { TVolatilePointer / TVolatilePointerHelper }

  TAtomic = class
  private
    class var [Volatile] FCacheLineSize: TVolatileInt32;
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

    /// <remarks>Initialize() Assumes that TVolatileObject.Initialize() has already been called.</remarks>
    class function InitializeVolatile(var storage: TVolatileObject; Factory: TObjectFactory): TVolatileObject;

    /// <remarks>FreeAndNil() ia an atomic version of SysUtils.FreeAndNil()</remarks>
    class procedure FreeAndNil(var storage: TObject);

    /// <remarks>FreeAndNilVolatile() ia an atomic version of SysUtils.FreeAndNil().
    ///   TVolatileObject.Finalize() is NOT called.</remarks>
    class procedure FreeAndNilVolatile(var storage: TVolatileObject);
  end; { TAtomic }

{$ALIGN 8}
  [Volatile]
  TAtomicSpinLock = record
    [Volatile] FEntered   : TVolatileThreadId;
    [Volatile] FEntryCount: TVolatileInt32;
    procedure Initialize;
    procedure Finalize;
    function  Enter( TimeOut: cardinal): boolean; overload;
    procedure Enter;                              overload;
    procedure Leave;
    procedure WithinLock( Action: System.SysUtils.TProc);
  end;
  PAtomicSpinLock = ^TAtomicSpinLock;

implementation

uses
  {$IFDEF MSWINDOWS}
    WinApi.Windows,
  {$ENDIF}
  System.SyncObjs, System.Classes, System.RTLConsts, System.Diagnostics;

{ TVolatileInt32 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt32.Initialize(Value: integer);
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
  // TAtomic.Assert_IsInterlockedIntegral<integer>(FRef^)
end; { TVolatileInt32.Initialize }

procedure TVolatileInt32.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TVolatileInt32.Finalize }

function TVolatileInt32.Add(Addend: integer): integer;
begin
  Result := TInterlocked.Add(FRef^, Addend);
end; { TVolatileInt32.Add }

procedure TVolatileInt32.Assign(var Source: TVolatileInt32);
begin
  FRef^ := Source.FRef^;
end; { TVolatileInt32.Assign }

function TVolatileInt32.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TVolatileInt32.CompareAndExchange }

function TVolatileInt32.Decrement: integer;
begin
  Result := TInterlocked.Decrement(FRef^);
end; { TVolatileInt32.Decrement }

class operator TVolatileInt32.Implicit(Value: integer): TVolatileInt32;
begin
  Result.FRef^ := Value;
end; { TVolatileInt32.Implicit }

class operator TVolatileInt32.Implicit(Value: TVolatileInt32): integer;
begin
  Result := Value.FRef^;
end; { TVolatileInt32.Implicit }

function TVolatileInt32.Read: integer;
begin
  Result := self.FRef^;
end; { TVolatileInt32.Read }

procedure TVolatileInt32.Write(const Value: integer);
begin
  self.FRef^ := Value;
end; { TVolatileInt32.Write }

function TVolatileInt32.Increment: integer;
begin
  Result := TInterlocked.Increment(FRef^);
end; { TVolatileInt32.Increment }
{$ENDIF}

{ TVolatileInt32Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt32Helper.Initialize(Value: integer);
begin
  Self := Value;
//  TAtomic.Assert_IsInterlockedIntegral<integer>(Self);
end; { TVolatileInt32Helper.Initialize }

procedure TVolatileInt32Helper.Finalize;
begin
  // do nothing
end; { TVolatileInt32Helper.Finalize }

{$HINTS OFF}
function TVolatileInt32Helper.Add(Addend: integer): integer;
begin
  Result := TInterlocked.Add(Self, Addend);
end; { TVolatileInt32Helper.Add }

procedure TVolatileInt32Helper.Assign(var Source: TVolatileInt32);
begin
  TInterlocked.Exchange(Self, TInterlocked.CompareExchange(Source, 0, 0));
end; { TVolatileInt32Helper.Assign }

function TVolatileInt32Helper.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TVolatileInt32Helper.CompareAndExchange }

function TVolatileInt32Helper.Decrement: integer;
begin
  Result := TInterlocked.Decrement(Self);
end; { TVolatileInt32Helper.Decrement }

function TVolatileInt32Helper.Increment: integer;
begin
  Result := TInterlocked.Increment(Self);
end; { TVolatileInt32Helper.Increment }

function TVolatileInt32Helper.Read: integer;
begin
  Result := TInterlocked.CompareExchange(Self, 0, 0);
end; { TVolatileInt32Helper.Read }

procedure TVolatileInt32Helper.Write(const Value: integer);
begin
  TInterlocked.Exchange(Self, Value);
end; { TVolatileInt32Helper.Write }
{$HINTS ON}
{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TVolatileUInt32 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt32.Initialize(Value: cardinal);
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
  // TAtomic.Assert_IsInterlockedIntegral<cardinal>(FRef^)
end; { TVolatileUInt32.Initialize }

procedure TVolatileUInt32.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TVolatileUInt32.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt32.Add(Addend: cardinal): cardinal;
begin
  Result := cardinal(TInterlocked.Add(integer(FRef^), integer(Addend)));
end; { TVolatileUInt32.Add }

function TVolatileUInt32.Increment: cardinal;
begin
  Result := cardinal(TInterlocked.Increment(integer(FRef^)));
end; { TVolatileUInt32.Increment }

function TVolatileUInt32.Decrement: cardinal;
begin
  Result := cardinal(TInterlocked.Decrement(integer(FRef^)));
end; { TVolatileUInt32.Decrement }

function TVolatileUInt32.DecrementIfAboveZero: boolean;
var
  val: cardinal;
begin
  Result := False;
  repeat
    val := FRef^;
    if val > 0 then
      Result := TInterlocked.CompareExchange(integer(FRef^), integer(val - 1), integer(val)) = integer(val)
  until Result or (val = 0);
end; { TVolatileUInt32.DecrementIfAboveZero }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

procedure TVolatileUInt32.Assign(var Source: TVolatileUInt32);
begin
  FRef^ := Source.FRef^;
end; { TVolatileUInt32.Assign }

function TVolatileUInt32.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  Result := TInterlocked.CompareExchange(pinteger(FRef)^, integer(NewValue), integer(OldValue)) = integer(OldValue);
end; { TVolatileUInt32.CompareAndExchange }

function TVolatileUInt32.Exchange(Value: cardinal): cardinal;
begin
  Result := TInterlocked.Exchange(pinteger(FRef)^, Value);
end; { TVolatileUInt32.Exchange }

class operator TVolatileUInt32.Implicit(Value: cardinal): TVolatileUInt32;
begin
  Result.FRef^ := Value;
end; { TVolatileUInt32.Implicit }

class operator TVolatileUInt32.Implicit(Value: TVolatileUInt32): cardinal;
begin
  Result := Value.FRef^;
end; { TVolatileUInt32.Implicit }

function TVolatileUInt32.Read: cardinal;
begin
  Result := Self.FRef^;
end; { TVolatileUInt32.Read }

procedure TVolatileUInt32.Write(const Value: cardinal);
begin
  Self.FRef^ := Value;
end; { TVolatileUInt32.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TVolatileUInt32Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt32Helper.Initialize(Value: cardinal);
begin
  Self := Value;
//  TAtomic.Assert_IsInterlockedIntegral<cardinal>(Self);
end; { TVolatileUInt32Helper.Initialize }

procedure TVolatileUInt32Helper.Finalize;
begin
  // do nothing
end; { TVolatileUInt32Helper.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$HINTS OFF}
function TVolatileUInt32Helper.Add(Addend: cardinal): cardinal;
begin
  Result := cardinal(TInterlocked.Add(integer(Self), integer(Addend)));
end; { TVolatileUInt32Helper.Add }

function TVolatileUInt32Helper.Decrement: cardinal;
begin
  Result := cardinal(TInterlocked.Decrement(integer(Self)));
end; { TVolatileUInt32Helper.Decrement }

function TVolatileUInt32Helper.Increment: cardinal;
begin
  Result := cardinal(TInterlocked.Increment(integer(Self)));
end; { TVolatileUInt32Helper.Increment }

function TVolatileUInt32Helper.DecrementIfAboveZero: boolean;
var
  val: cardinal;
begin
  Result := False;
  repeat
    val := Self;
    if val > 0 then
      Result := TInterlocked.CompareExchange(integer(Self), integer(val - 1), integer(val)) = integer(val)
  until Result or (val = 0);
end; { TVolatileUInt32Helper.DecrementIfAboveZero }
{$HINTS ON}

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{ TVolatileUInt32Helper }

{$HINTS OFF}
procedure TVolatileUInt32Helper.Assign(var Source: TVolatileUInt32);
begin
  TInterlocked.Exchange(integer(Self), TInterlocked.CompareExchange(integer(Source), 0, 0));
end; { TVolatileUInt32Helper.Assign }

function TVolatileUInt32Helper.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  Result := TInterlocked.CompareExchange(integer(Self), integer(NewValue), integer(OldValue)) = integer(OldValue);
end; { TVolatileUInt32Helper.CompareAndExchange }

function TVolatileUInt32Helper.Exchange(Value: cardinal): cardinal;
begin
  Result := TInterlocked.Exchange(integer(Self), Value);
end; { TVolatileUInt32Helper.Exchange }

function TVolatileUInt32Helper.Read: cardinal;
begin
  Result := cardinal(TInterlocked.CompareExchange(integer(Self), 0, 0));
end; { TVolatileUInt32Helper.Read }

procedure TVolatileUInt32Helper.Write(const Value: cardinal);
begin
  TInterlocked.Exchange(integer(Self), integer(Value));
end; { TVolatileUInt32Helper.Write }

{$HINTS ON}

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TVolatileInt64 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt64.Initialize(Value: int64);
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
  // TAtomic.Assert_IsInterlockedIntegral<int64>(FRef^)
end; { TVolatileInt64.Initialize }

procedure TVolatileInt64.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TVolatileInt64.Finalize }

function TVolatileInt64.Add(Addend: int64): int64;
begin
  Result := TInterlocked.Add(FRef^, Addend);
end; { TVolatileInt64.Add }

function TVolatileInt64.CompareAndExchange(
  OldValue, NewValue: int64): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TVolatileInt64.CompareAndExchange }

function TVolatileInt64.Decrement: int64;
begin
  Result := TInterlocked.Decrement(FRef^);
end; { TVolatileInt64.Decrement }

class operator TVolatileInt64.Implicit(Value: int64): TVolatileInt64;
begin
  {$IFDEF CPU64BITS}
    Result.FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(Result.FRef^, Value);
  {$ENDIF}
end; { TVolatileInt64.Implicit }

class operator TVolatileInt64.Implicit(Value: TVolatileInt64): int64;
begin
  {$IFDEF CPU64BITS}
    Result := Value.FRef^;
  {$ELSE}
    Result := TInterlocked.Read(Value.FRef^);
  {$ENDIF}
end; { TVolatileInt64.Implicit }

function TVolatileInt64.Increment: int64;
begin
  Result := TInterlocked.Increment(FRef^);
end; { TVolatileInt64.Increment }

procedure TVolatileInt64.Assign(var Source: TVolatileInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^;
  {$ELSE}
    TInterlocked.Exchange(FRef^, TInterlocked.CompareExchange(Source.FRef^, 0, 0));
  {$ENDIF}
end; { TVolatileInt64.Assign }

function TVolatileInt64.Read: int64;
begin
  {$IFDEF CPU64BITS}
    Result := FRef^;
  {$ELSE}
    Result := TInterlocked.Read(FRef^);
  {$ENDIF}
end; { TVolatileInt64.Read }

procedure TVolatileInt64.Write(const Value: int64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(FRef^, Value);
  {$ENDIF}
end; { TVolatileInt64.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TVolatileInt64Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt64Helper.Initialize(Value: int64);
begin
  Self := Value;
//  TAtomic.Assert_IsInterlockedIntegral<int64>(Self);
end; { TVolatileInt64Helper.Initialize }

procedure TVolatileInt64Helper.Finalize;
begin
  // do nothing
end; { TVolatileInt64Helper.Finalize }

{$HINTS OFF}
function TVolatileInt64Helper.Increment: int64;
begin
  Result := TInterlocked.Increment(Self);
end; { TVolatileInt64Helper.Increment }

function TVolatileInt64Helper.Decrement: int64;
begin
  Result := TInterlocked.Decrement(Self);
end; { TVolatileInt64Helper.Decrement }

function TVolatileInt64Helper.Add(Addend: int64): int64;
begin
  Result := TInterlocked.Add(Self, Addend);
end; { TVolatileInt64Helper.Add }

function TVolatileInt64Helper.CompareAndExchange(OldValue, NewValue: int64): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TVolatileInt64Helper.CompareAndExchange }

procedure TVolatileInt64Helper.Assign(var Source: TVolatileInt64);
begin
  TInterlocked.Exchange(Self, TInterlocked.Read(Source));
end; { TVolatileInt64Helper.Assign }

function TVolatileInt64Helper.Read: int64;
begin
  Result := TInterlocked.Read(Self);
end; { TVolatileInt64Helper.Read }

procedure TVolatileInt64Helper.Write(const Value: int64);
begin
  TInterlocked.Exchange(Self, Value);
end; { TVolatileInt64Helper.Write }

{$HINTS OFF}

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TVolatileUInt64 }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt64.Initialize(Value: uint64);
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
  // TAtomic.Assert_IsInterlockedIntegral<uint64>(FRef^);
end; { TVolatileUInt64.Initialize }

procedure TVolatileUInt64.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TVolatileUInt64.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64.Add(Addend: uint64): uint64;
begin
  Result := uint64(TInterlocked.Add(pint64(FRef)^, int64(Addend)));
end; { TVolatileUInt64.Add }

function TVolatileUInt64.Decrement: uint64;
begin
  Result := uint64(TInterlocked.Decrement(pint64(FRef)^));
end; { TVolatileUInt64.Decrement }

function TVolatileUInt64.Increment: uint64;
begin
  Result := uint64(TInterlocked.Increment(pint64(FRef)^));
end; { TVolatileUInt64.Increment }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64.CompareAndExchange(
  OldValue, NewValue: uint64): boolean;
begin
  Result := TInterlocked.CompareExchange(pint64(FRef)^, int64(NewValue), int64(OldValue)) = int64(OldValue);
end; { TVolatileUInt64.CompareAndExchange }

class operator TVolatileUInt64.Implicit(Value: uint64): TVolatileUInt64;
begin
  {$IFDEF CPU64BITS}
    Result.FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(pint64(Result.FRef)^, int64(Value));
  {$ENDIF}
end; { TVolatileUInt64.Implicit }

class operator TVolatileUInt64.Implicit(Value: TVolatileUInt64): uint64;
begin
  {$IFDEF CPU64BITS}
    Result := Value.FRef^;
  {$ELSE}
    Result := uint64(TInterlocked.Read(pint64(Value.FRef)^));
  {$ENDIF}
end; { TVolatileUInt64.Implicit }

procedure TVolatileUInt64.Assign(var Source: TVolatileUInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^;
  {$ELSE}
    TInterlocked.Exchange(pint64(FRef)^, TInterlocked.CompareExchange(pint64(Source.FRef)^, 0, 0));
  {$ENDIF}
end; { TVolatileUInt64.Assign }

function TVolatileUInt64.Read: uint64;
begin
  {$IFDEF CPU64BITS}
    Result := FRef^;
  {$ELSE}
    Result := uint64(TInterlocked.Read(pint64(FRef)^));
  {$ENDIF}
end; { TVolatileUInt64.Read }

procedure TVolatileUInt64.Write(const Value: uint64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value;
  {$ELSE}
    TInterlocked.Exchange(pint64(FRef)^, int64(Value));
  {$ENDIF}
end; { TVolatileUInt64.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TVolatileUInt64Helper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt64Helper.Initialize(Value: uint64);
begin
  Self := Value;
//  TAtomic.Assert_IsInterlockedIntegral<uint64>(Self);
end; { TVolatileUInt64Helper.Initialize }

procedure TVolatileUInt64Helper.Finalize;
begin
  // do nothing
end; { TVolatileUInt64Helper.Finalize }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64Helper.Increment: uint64;
begin
  Result := uint64(TInterlocked.Increment(int64(Self)));
end; { TVolatileUInt64Helper.Increment }

function TVolatileUInt64Helper.Decrement: uint64;
begin
  Result := uint64(TInterlocked.Decrement(int64(Self)));
end; { TVolatileUInt64Helper.Decrement }

function TVolatileUInt64Helper.Add(Addend: uint64): uint64;
begin
  Result := uint64(TInterlocked.Add(int64(Self), int64(Addend)));
end; { TVolatileUInt64Helper.Add }

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64Helper.CompareAndExchange(OldValue, NewValue: uint64): boolean;
begin
  Result := TInterlocked.CompareExchange(int64(Self), int64(NewValue), int64(OldValue)) = int64(OldValue);
end; { TVolatileUInt64Helper.CompareAndExchange }

procedure TVolatileUInt64Helper.Assign(var Source: TVolatileUInt64);
begin
  TInterlocked.Exchange(int64(Self), TInterlocked.Read(int64(Source)));
end; { TVolatileUInt64Helper.Assign }

function TVolatileUInt64Helper.Read: uint64;
begin
  Result := TInterlocked.Read(int64(Self));
end; { TVolatileUInt64Helper.Read }

procedure TVolatileUInt64Helper.Write(const Value: uint64);
begin
  TInterlocked.Exchange(int64(Self), int64(Value));
end; { TVolatileUInt64Helper.Write }

{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TVolatilePointer }

{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatilePointer.Initialize;
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
  // TAtomic.Assert_IsInterlockedIntegral<pointer>(FRef^);
end; { TVolatilePointer.Initialize }

procedure TVolatilePointer.Finalize;
begin
  SetLength(FStore, 0);
  FRef := nil;
end; { TVolatilePointer.Finalize }

procedure TVolatilePointer.Assign(var Source: TVolatilePointer);
begin
  FRef^ := Source.FRef^;
end; { TVolatilePointer.Assign }

function TVolatilePointer.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
end; { TVolatilePointer.CompareAndExchange }

class operator TVolatilePointer.Implicit(Value: pointer): TVolatilePointer;
begin
  Result.FRef^ := Value;
end; { TVolatilePointer.Implicit }

class operator TVolatilePointer.Implicit(Value: TVolatilePointer): pointer;
begin
  Result := Value.FRef^;
end; { TVolatilePointer.Implicit }

function TVolatilePointer.Read: pointer;
begin
  Result := Self.FRef^;
end; { TVolatilePointer.Read }

procedure TVolatilePointer.Write(const Value: pointer);
begin
  Self.FRef^ := Value;
end; { TVolatilePointer.Write }

{$ENDIF USE_SLACKSPACE_ALIGNMENT}

{ TVolatilePointerHelper }

{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatilePointerHelper.Initialize;
begin
  Self := nil;
//  TAtomic.Assert_IsInterlockedIntegral<pointer>(Self);
end; { TVolatilePointerHelper.Initialize }

procedure TVolatilePointerHelper.Finalize;
begin
  // do nothing
end; { TVolatilePointerHelper.Finalize }

procedure TVolatilePointerHelper.Assign(var Source: pointer);
begin
  TInterlocked.Exchange(Self, TInterlocked.CompareExchange(Source, nil, nil));
end; { TVolatilePointerHelper.Assign }

function TVolatilePointerHelper.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  Result := TInterlocked.CompareExchange(Self, NewValue, OldValue) = OldValue;
end; { TVolatilePointerHelper.CompareAndExchange }

function TVolatilePointerHelper.Read: pointer;
begin
  Result := TInterlocked.CompareExchange(Self, nil, nil);
end; { TVolatilePointerHelper.Read }

procedure TVolatilePointerHelper.Write(const Value: pointer);
begin
  TInterlocked.Exchange(Self, Value);
end; { TVolatilePointerHelper.Write }
{$ENDIF ~USE_SLACKSPACE_ALIGNMENT}

{ TVolatileObject }

procedure TVolatileObject.Initialize;
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
    // TAtomic.Assert_IsInterlockedIntegral<TObject>(FRef^);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    FPayload := nil;
//    TAtomic.Assert_IsInterlockedIntegral<TObject>(FPayload);
  {$ENDIF}
end; { TVolatileObject.Initialize }

procedure TVolatileObject.Finalize;
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
end; { TVolatileObject.Finalize }

function TVolatileObject.CompareAndExchange(OldValue, NewValue: TObject): boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FRef^, NewValue, OldValue) = OldValue;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FPayload, NewValue, OldValue) = OldValue;
  {$ENDIF}
end; { TVolatileObject.CompareAndExchange }

procedure TVolatileObject.Assign(var Source: TVolatileObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FRef^, TInterlocked.CompareExchange(Source.FRef^, TObject(nil), TObject(nil)));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FPayload, TInterlocked.CompareExchange(Source.FPayload, TObject(nil), TObject(nil)));
  {$ENDIF}
end; { TVolatileObject.Assign }

function TVolatileObject.ReadObj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FRef^, TObject(nil), TObject(nil));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := TInterlocked.CompareExchange(FPayload, TObject(nil), TObject(nil));
  {$ENDIF}
end; { TVolatileObject.ReadObj }

function TVolatileObject.IsAssigned: boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := Assigned(FRef^);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := Assigned(FPayload);
  {$ENDIF}
end; { TVolatileObject.IsAssigned }

function TVolatileObject.Read< T>: T;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Result := T(TInterlocked.CompareExchange(FRef^, TObject(nil), TObject(nil)));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Result := T(TInterlocked.CompareExchange(FPayload, TObject(nil), TObject(nil)));
  {$ENDIF}
end; { TVolatileObject.Read }

procedure TVolatileObject.Write(const Value: TObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FRef^, Value);
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange(FPayload, Value);
  {$ENDIF}
end; { TVolatileObject.Write }

procedure TVolatileObject.Free;
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
end; { TVolatileObject.Free }

{ TAtomic }

class function TAtomic.IsReadWriteAtomicallyIntegral(DatumAddress: pointer; DatumLength: integer): boolean;
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
end; { TAtomic.IsReadWriteAtomicallyIntegral }

class function TAtomic.IsReadWriteAtomicallyIntegral<T>(var Datum: T): boolean;
begin
  Result := IsReadWriteAtomicallyIntegral(@Datum, SizeOf(Datum));
end; { TAtomic.IsReadWriteAtomicallyIntegral }

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

class function TAtomic.IsWithinCacheLines<T>(var Datum: T): boolean;
begin
  Result := IsWithinCacheLines(@Datum, SizeOf(Datum));
end; { TAtomic.IsWithinCacheLines }

class function TAtomic.IsWithinCacheLines(DatumAddress: pointer; DatumLength: integer): boolean;
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
end; { TAtomic.IsWithinCacheLines }

class function TAtomic.IsInterlockedIntegral(DatumAddress: pointer; DatumLength: integer): boolean;
begin
  {$IFDEF CPUINTEL}
    Result := True;
  {$ENDIF}

  {$IFDEF CPUARM}
    Result := IsWithinCacheLines(DatumAddress, DatumLength);
  {$ENDIF}
end; { TAtomic.IsInterlockedIntegral }

class function TAtomic.IsInterlockedIntegral<T>(var Datum: T): boolean;
begin
  Result := IsInterlockedIntegral(@Datum, SizeOf(Datum));
end; { TAtomic.IsInterlockedIntegral }

class procedure TAtomic.Assert_IsReadWriteAtomicallyIntegral<T>(var Datum: T);
begin
  Assert(TAtomic.IsReadWriteAtomicallyIntegral<T>(Datum),
    'Datum is not aligned as required for atomic read/write integrity.');
end; { TAtomic.Assert_IsReadWriteAtomicallyIntegral }

class procedure TAtomic.Assert_IsWithinCacheLines<T>(var Datum: T);
begin
  Assert(TAtomic.IsWithinCacheLines<T>(Datum),
    'Datum does not fit within a single L1 CPU cache line.');
end; { TAtomic.Assert_IsWithinCacheLines }

class procedure TAtomic.Assert_IsInterlockedIntegral<T>(var Datum: T);
begin
  Assert(TAtomic.IsInterlockedIntegral<T>(Datum),
    'Datum is not aligned as required for AtomicXXX()/ TInterlocked-class function integrity.');
end; { TAtomic.Assert_IsInterlockedIntegral }

class function TAtomic.Initialize(var storage: TObject; Factory: TObjectFactory): TObject;
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
end; { TAtomic.Initialize }

class function TAtomic.InitializeVolatile(var storage: TVolatileObject; Factory: TObjectFactory): TVolatileObject;
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
end; { TAtomic.InitializeVolatile }

class function TAtomic.Initialize(var Storage: IInterface; Factory: TInterfaceFactory): IInterface;
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
end; { TAtomic.Initialize }

class procedure TAtomic.FreeAndNil(var Storage: TObject);
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
end; { TAtomic.FreeAndNil }

class procedure TAtomic.FreeAndNilVolatile(var Storage: TVolatileObject);
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
end; { TAtomic.FreeAndNilVolatile }

procedure TAtomicSpinLock.Initialize;
begin
  FEntered.Initialize(0);
  FEntryCount.Initialize(0);
end; { TAtomicSpinLock.Initialize }

procedure TAtomicSpinLock.Finalize;
begin
  FEntered.Finalize;
  FEntryCount.Finalize;
end; { TAtomicSpinLock.Finalize }

const
  MaxCardinal: cardinal = cardinal(-1);

function TAtomicSpinLock.Enter(TimeOut: cardinal): boolean;
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
end; { TAtomicSpinLock.Enter }

procedure TAtomicSpinLock.Enter;
begin
  Enter(INFINITE);
end; { TAtomicSpinLock.Enter }

procedure TAtomicSpinLock.Leave;
var
  this: TThreadId;
begin
  this := TThread.CurrentThread.ThreadID;
  if FEntered.Read <> this then
    raise ELockException.CreateRes(@SSpinLockNotOwned);
  if FEntryCount.Decrement <= 0 then
    FEntered.CompareAndExchange(this, 0);
end; { TAtomicSpinLock.Leave }

procedure TAtomicSpinLock.WithinLock(Action: System.SysUtils.TProc);
begin
  Enter(INFINITE);
  Action;
  Leave;
end; { TAtomicSpinLock.WithinLock }

{ initialization }

procedure InitUnit_Atomic;
begin
  TStopWatch.Create;
  TAtomic.FCacheLineSize.Initialize(0);
end; { InitUnit_Atomic }

procedure DoneUnit_Atomic;
begin
  TAtomic.FCacheLineSize.Finalize;
end; { DoneUnit_Atomic }

initialization
  InitUnit_Atomic;
finalization
  DoneUnit_Atomic;
end.
