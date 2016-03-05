unit Otl.Parallel.Atomic;

{$I OtlOptions.inc}

interface
uses System.SysUtils;

{$I HintReporting.inc}

type
  /// <remarks> TVolatileInt32 is a thead-safe signed 32-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInteger;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  integer;
  TVolatileInt32Helper = record helper for TVolatileInt32
  {$ENDIF}
    procedure Initialize( Value: integer);                  {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                  inline;
    function  Increment: integer;                                        inline;
    function  Decrement: integer;                                        inline;
    function  Add( Addend: integer): integer;                            inline;
    function  CompareAndExchange( OldValue, NewValue: integer): boolean; inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign( var Source: TVolatileInt32);                       inline;
    function  Read: integer;                                             inline;
    procedure Write( const Value: integer);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: integer): TVolatileInt32;          inline;
      class operator Implicit( Value: TVolatileInt32): integer;          inline;
    {$ENDIF}
  end;


  /// <remarks> TVolatileInt32 is a thead-safe unsigned 32-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileUInt32 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PCardinal;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  cardinal;
  TVolatileUInt32Helper = record helper for TVolatileUInt32
  {$ENDIF}
    procedure Initialize( Value: cardinal);                  {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                   inline;
    function  Increment: cardinal;                                        inline;
    function  Decrement: cardinal;                                        inline;
    /// <remarks>Returns True iff was above zero and decremented.</remarks>
    function  DecrementIfAboveZero: boolean;                              inline;
    function  Add( Addend: cardinal): cardinal;                           inline;
    function  CompareAndExchange( OldValue, NewValue: cardinal): boolean; inline;
    function  Exchange( Value: cardinal): cardinal;                       inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign( var Source: TVolatileUInt32);                       inline;
    function  Read: cardinal;                                             inline;
    procedure Write( const Value: cardinal);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: cardinal): TVolatileUInt32;         inline;
      class operator Implicit( Value: TVolatileUInt32): cardinal;         inline;
    {$ENDIF}
  end;


  /// <remarks> TVolatileInt32 is a thead-safe signed 64-bit integer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileInt64 =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PInt64;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  int64;
  TVolatileInt64Helper = record helper for TVolatileInt64
  {$ENDIF}
    procedure Initialize( Value: int64);                  {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                inline;
    function  Increment: int64;                                        inline;
    function  Decrement: int64;                                        inline;
    function  Add( Addend: int64): int64;                              inline;
    function  CompareAndExchange( OldValue, NewValue: int64): boolean; inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign( var Source: TVolatileInt64);                     inline;
    function  Read: int64;                                             inline;
    procedure Write( const Value: int64);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: int64): TVolatileInt64;          inline;
      class operator Implicit( Value: TVolatileInt64): int64;          inline;
    {$ENDIF}
  end;


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
    procedure Initialize( Value: uint64);                  {$IFDEF ARM} inline; {$ENDIF}
    procedure Finalize;                                                 inline;
    function  Increment: uint64;                                        inline;
    function  Decrement: uint64;                                        inline;
    function  Add( Addend: uint64): uint64;                             inline;
    function  CompareAndExchange( OldValue, NewValue: uint64): boolean; inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign( var Source: TVolatileUInt64);                     inline;
    function  Read: uint64;                                             inline;
    procedure Write( const Value: uint64);                              inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: uint64): TVolatileUInt64;         inline;
      class operator Implicit( Value: TVolatileUInt64): uint64;         inline;
    {$ENDIF}
  end;


  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    PNativeInteger  = ^NativeInt;
    PNativeUInteger = ^NativeUInt;
  {$ENDIF}

  {$IF SizeOf( NativeInt) = SizeOf( integer)}
    TVolatileNativeInt = TVolatileInt32;
  {$ELSE}
    TVolatileNativeInt = TVolatileInt64;
  {$ENDIF}

  {$IF SizeOf( NativeUInt) = SizeOf( cardinal)}
    TVolatileNativeUInt = TVolatileUint32;
  {$ELSE}
    TVolatileNativeUInt = TVolatileUint64;
  {$ENDIF}

  {$IF SizeOf( TThreadId) = SizeOf( cardinal)}
    TVolatileThreadId = TVolatileUint32;
  {$ELSE}
    TVolatileThreadId = TVolatileUint64;
  {$ENDIF}

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    PObject = ^TObject;
  {$ENDIF}

  /// <remarks> TVolatileObject is a thead-safe object reference. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatileObject = record
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      FStore: TBytes;
      FRef  : PObject;
    {$ENDIF}
    {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
      FPayload: TObject;
    {$ENDIF}
    procedure Initialize;                              {$IFNDEF USE_SLACKSPACE_ALIGNMENT} inline; {$ENDIF}
    /// <remarks> Finalize also frees the object. </remarks>
    procedure Finalize;                                                  inline;
    function  CompareAndExchange( OldValue, NewValue: TObject): boolean; inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    procedure Assign( var Source: TVolatileObject);                      inline;
    function  ReadObj: TObject;                                          inline;
    function  Read<T: class>: T;                                         inline;
    function  IsAssigned: boolean;                                       inline;
    procedure Write( const Value: TObject);                              inline;
    procedure Free;                                                      inline;
  end;


  /// <remarks> TVolatileObject is a thead-safe pointer. Integrity does not rely on alignment.</remarks>
  [Volatile]
  TVolatilePointer =
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
  record
    FStore: TBytes;
    FRef  : PPointer;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
  pointer;
  TVolatilePointerHelper = record helper for TVolatilePointer
  {$ENDIF}
    procedure Initialize;                                {$IFNDEF USE_SLACKSPACE_ALIGNMENT} inline; {$ENDIF}
    procedure Finalize;                                                    inline;
    function  CompareAndExchange( OldValue, NewValue: pointer): boolean;   inline;
    procedure Assign( var Source: TVolatilePointer);                       inline;
    /// <remarks> Assign() is atomic in terms of reading the source, and writing the destination, but not both as together.</remarks>
    function  Read: pointer;                                               inline;
    procedure Write( const Value: pointer);                                inline;
    {$IFDEF USE_SLACKSPACE_ALIGNMENT}
      class operator Implicit( Value: pointer): TVolatilePointer;          inline;
      class operator Implicit( Value: TVolatilePointer): pointer;          inline;
    {$ENDIF}
  end;


  TAtomic = class
  private
    class var [Volatile] FCacheLineSize: TVolatileInt32;

  public
    /// <remarks>Returns True iff the datum can be read or written directly atomically.</remarks>
    class function isReadWriteAtomicallyIntegral( DatumAddress: pointer; DatumLength: integer): boolean;  overload;
    /// <remarks>Returns True iff the datum can be read or written directly atomically.</remarks>
    class function isReadWriteAtomicallyIntegral<T>( var Datum: T): boolean;                              overload;
    class procedure Assert_isReadWriteAtomicallyIntegral<T>( var Datum: T);                               inline;

    /// <remarks>Returns True iff the datum is within one cache line, and does not cross cache lines.</remarks>
    class function isWithinCacheLines( DatumAddress: pointer; DatumLength: integer): boolean;             overload;
    /// <remarks>Returns True iff the datum is within one cache line, and does not cross cache lines.</remarks>
    class function isWithinCacheLines<T>( var Datum: T): boolean;                                         overload;
    class procedure Assert_isWithinCacheLines<T>( var Datum: T);                                          inline;

    /// <remarks>Returns True iff we can call AtomicXXX() functions for TInterlocked class functions on
    //    the datum with absolute atomic integrity.</remarks>
    class function isInterlockedIntegral( DatumAddress: pointer; DatumLength: integer): boolean;          overload;
    /// <remarks>Returns True iff we can call AtomicXXX() functions for TInterlocked class functions on
    //    the datum with absolute atomic integrity.</remarks>
    class function isInterlockedIntegral<T>( var Datum: T): boolean;                                      overload;
    class procedure Assert_isInterlockedIntegral<T>( var Datum: T);                                       inline;

  public
    type TObjectFactory = reference to function: TObject;
    type TInterfaceFactory = reference to function: IInterface;

    /// <remarks>Initialize() Assumes that storage is aligned within cache lines.</remarks>
    class function Initialize        ( var storage: TObject        ; Factory: TObjectFactory   ): TObject;    overload;

    /// <remarks>Initialize() Assumes that TVolatileObject.Initialize() has already been called.</remarks>
    class function InitializeVolatile( var storage: TVolatileObject; Factory: TObjectFactory): TVolatileObject;

    /// <remarks>Initialize() Assumes that storage is aligned within cache lines.</remarks>
    class function Initialize        ( var storage: IInterface     ; Factory: TInterfaceFactory): IInterface; overload;

    /// <remarks>FreeAndNil() ia an atomic version of SysUtils.FreeAndNil()</remarks>
    class procedure FreeAndNil( var storage: TObject);

    /// <remarks>FreeAndNilVolatile() ia an atomic version of SysUtils.FreeAndNil().
    ///   TVolatileObject.Finalize() is NOT called.</remarks>
    class procedure FreeAndNilVolatile( var storage: TVolatileObject);

  end;


{$ALIGN 8}
  [Volatile]
  TSBDSpinLock = record
    [Volatile] FEntered   : TVolatileThreadId;
    [Volatile] FEntryCount: TVolatileInt32;
    procedure Initialize;
    procedure Finalize;
    function  Enter( TimeOut: cardinal): boolean; overload;
    procedure Enter;                              overload;
    procedure Leave;
    procedure WithinLock( Action: System.SysUtils.TProc);
  end;
  PSBDSpinLock = ^TSBDSpinLock;

implementation






uses System.SyncObjs, System.Classes, System.RTLConsts, System.Diagnostics
     {$IFDEF MSWINDOWS}
       , WinApi.Windows
     {$ENDIF}
     ;


procedure InitUnit_Atomic;
begin
  TStopWatch.Create;
  TAtomic.FCacheLineSize.Initialize( 0)
end;


procedure DoneUnit_Atomic;
begin
  TAtomic.FCacheLineSize.Finalize
end;


{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt32.Initialize( Value: integer);
var
  Addr: NativeInt;
  Misalignment: integer;
begin
  SetLength( FStore, SizeOf( Value) * 2);
  Addr := NativeInt( @FStore[0]);
  Misalignment := Addr mod SizeOf( Value);
  if Misalignment <> 0 then
    Inc( Addr, SizeOf( Value) - Misalignment);
  FRef  := PInteger( Addr);
  FRef^ := Value;
  // TAtomic.Assert_isInterlockedIntegral<integer>( FRef^)
end;

procedure TVolatileInt32.Finalize;
begin
  SetLength( FStore, 0);
  FRef := nil
end;

function TVolatileInt32.Add( Addend: integer): integer;
begin
  result := TInterlocked.Add( FRef^, Addend)
end;

procedure TVolatileInt32.Assign( var Source: TVolatileInt32);
begin
  FRef^ := Source.FRef^
end;

function TVolatileInt32.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  result := TInterlocked.CompareExchange( FRef^, NewValue, OldValue) = OldValue
end;

function TVolatileInt32.Decrement: integer;
begin
  result := TInterlocked.Decrement( FRef^)
end;

class operator TVolatileInt32.Implicit( Value: integer): TVolatileInt32;
begin
  result.FRef^ := Value
end;

class operator TVolatileInt32.Implicit( Value: TVolatileInt32): integer;
begin
  result := Value.FRef^
end;


function TVolatileInt32.Read: integer;
begin
  result := self.FRef^
end;

procedure TVolatileInt32.Write( const Value: integer);
begin
  self.FRef^ := Value
end;




function TVolatileInt32.Increment: integer;
begin
  result := TInterlocked.Increment( FRef^)
end;
{$ENDIF}





{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt32Helper.Initialize( Value: integer);
begin
  self := Value;
  TAtomic.Assert_isInterlockedIntegral<integer>( self)
end;

procedure TVolatileInt32Helper.Finalize;
begin
end;

{$HINTS OFF}
function TVolatileInt32Helper.Add( Addend: integer): integer;
begin
  result := TInterlocked.Add( self, Addend)
end;

procedure TVolatileInt32Helper.Assign( var Source: TVolatileInt32);
begin
  TInterlocked.Exchange( self, TInterlocked.CompareExchange( Source, 0, 0))
end;

function TVolatileInt32Helper.CompareAndExchange(
  OldValue, NewValue: integer): boolean;
begin
  result := TInterlocked.CompareExchange( self, NewValue, OldValue) = OldValue
end;

function TVolatileInt32Helper.Decrement: integer;
begin
  result := TInterlocked.Decrement( self)
end;

function TVolatileInt32Helper.Increment: integer;
begin
  result := TInterlocked.Increment( self)
end;

function TVolatileInt32Helper.Read: integer;
begin
  result := TInterlocked.CompareExchange( self, 0, 0)
end;

procedure TVolatileInt32Helper.Write( const Value: integer);
begin
  TInterlocked.Exchange( self, Value)
end;
{$HINTS ON}
{$ENDIF}



{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt32.Initialize( Value: cardinal);
var
  Addr: NativeInt;
  Misalignment: integer;
begin
  SetLength( FStore, SizeOf( Value) * 2);
  Addr := NativeInt( @FStore[0]);
  Misalignment := Addr mod SizeOf( Value);
  if Misalignment <> 0 then
    Inc( Addr, SizeOf( Value) - Misalignment);
  FRef  := PCardinal( Addr);
  FRef^ := Value;
  // TAtomic.Assert_isInterlockedIntegral<cardinal>( FRef^)
end;

procedure TVolatileUInt32.Finalize;
begin
  SetLength( FStore, 0);
  FRef := nil
end;

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt32.Add( Addend: cardinal): cardinal;
begin
  result := cardinal( TInterlocked.Add( integer( FRef^), integer( Addend)))
end;

function TVolatileUInt32.Increment: cardinal;
begin
  result := cardinal( TInterlocked.Increment( integer( FRef^)))
end;

function TVolatileUInt32.Decrement: cardinal;
begin
  result := cardinal( TInterlocked.Decrement( integer( FRef^)))
end;

function TVolatileUInt32.DecrementIfAboveZero: boolean;
var
  Val: cardinal;
begin
  result := False;
  repeat
    Val := FRef^;
    if Val > 0 then
      result := TInterlocked.CompareExchange( integer( FRef^), integer( Val - 1), integer( Val)) = integer( Val)
  until result or (Val = 0)
end;

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

procedure TVolatileUInt32.Assign( var Source: TVolatileUInt32);
begin
  FRef^ := Source.FRef^
end;

function TVolatileUInt32.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  result := TInterlocked.CompareExchange( pinteger( FRef)^, integer( NewValue), integer( OldValue)) = integer( OldValue)
end;

function TVolatileUInt32.Exchange( Value: cardinal): cardinal;
begin
  result := TInterlocked.Exchange( pinteger( FRef)^, Value)
end;

class operator TVolatileUInt32.Implicit( Value: cardinal): TVolatileUInt32;
begin
  result.FRef^ := Value
end;

class operator TVolatileUInt32.Implicit( Value: TVolatileUInt32): cardinal;
begin
  result := Value.FRef^
end;


function TVolatileUInt32.Read: cardinal;
begin
  result := self.FRef^
end;

procedure TVolatileUInt32.Write( const Value: cardinal);
begin
  self.FRef^ := Value
end;

{$ENDIF}





{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt32Helper.Initialize( Value: cardinal);
begin
  self := Value;
  TAtomic.Assert_isInterlockedIntegral<cardinal>( self)
end;

procedure TVolatileUInt32Helper.Finalize;
begin
end;

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$HINTS OFF}
function TVolatileUInt32Helper.Add( Addend: cardinal): cardinal;
begin
  result := cardinal( TInterlocked.Add( integer( self), integer( Addend)))
end;

function TVolatileUInt32Helper.Decrement: cardinal;
begin
  result := cardinal( TInterlocked.Decrement( integer( self)))
end;

function TVolatileUInt32Helper.Increment: cardinal;
begin
  result := cardinal( TInterlocked.Increment( integer( self)))
end;

function TVolatileUInt32Helper.DecrementIfAboveZero: boolean;
var
  Val: cardinal;
begin
  result := False;
  repeat
    Val := self;
    if Val > 0 then
      result := TInterlocked.CompareExchange( integer( self), integer( Val - 1), integer( Val)) = integer( Val)
  until result or (Val = 0)
end;
{$HINTS ON}


{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}


{$HINTS OFF}
procedure TVolatileUInt32Helper.Assign( var Source: TVolatileUInt32);
begin
  TInterlocked.Exchange( integer( self), TInterlocked.CompareExchange( integer( Source), 0, 0))
end;

function TVolatileUInt32Helper.CompareAndExchange(
  OldValue, NewValue: cardinal): boolean;
begin
  result := TInterlocked.CompareExchange( integer( self), integer( NewValue), integer( OldValue)) = integer( OldValue)
end;

function TVolatileUInt32Helper.Exchange( Value: cardinal): cardinal;
begin
  result := TInterlocked.Exchange( integer( self), Value)
end;

function TVolatileUInt32Helper.Read: cardinal;
begin
  result := cardinal( TInterlocked.CompareExchange( integer( self), 0, 0))
end;

procedure TVolatileUInt32Helper.Write( const Value: cardinal);
begin
  TInterlocked.Exchange( integer( self), integer( Value))
end;
{$HINTS ON}
{$ENDIF}



{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt64.Initialize( Value: int64);
var
  Addr: NativeInt;
  Misalignment: integer;
begin
  SetLength( FStore, SizeOf( Value) * 2);
  Addr := NativeInt( @FStore[0]);
  Misalignment := Addr mod SizeOf( Value);
  if Misalignment <> 0 then
    Inc( Addr, SizeOf( Value) - Misalignment);
  FRef  := PInt64( Addr);
  FRef^ := Value;
  // TAtomic.Assert_isInterlockedIntegral<int64>( FRef^)
end;

procedure TVolatileInt64.Finalize;
begin
  SetLength( FStore, 0);
  FRef := nil
end;

function TVolatileInt64.Add( Addend: int64): int64;
begin
  result := TInterlocked.Add( FRef^, Addend)
end;

function TVolatileInt64.CompareAndExchange(
  OldValue, NewValue: int64): boolean;
begin
  result := TInterlocked.CompareExchange( FRef^, NewValue, OldValue) = OldValue
end;

function TVolatileInt64.Decrement: int64;
begin
  result := TInterlocked.Decrement( FRef^)
end;

class operator TVolatileInt64.Implicit( Value: int64): TVolatileInt64;
begin
  {$IFDEF CPU64BITS}
    result.FRef^ := Value
  {$ELSE}
    TInterlocked.Exchange( result.FRef^, Value)
  {$ENDIF}
end;

class operator TVolatileInt64.Implicit( Value: TVolatileInt64): int64;
begin
  {$IFDEF CPU64BITS}
    result := Value.FRef^
  {$ELSE}
    result := TInterlocked.Read( Value.FRef^)
  {$ENDIF}
end;

function TVolatileInt64.Increment: int64;
begin
  result := TInterlocked.Increment( FRef^)
end;


procedure TVolatileInt64.Assign( var Source: TVolatileInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^
  {$ELSE}
    TInterlocked.Exchange( FRef^, TInterlocked.CompareExchange( Source.FRef^, 0, 0))
  {$ENDIF}
end;

function TVolatileInt64.Read: int64;
begin
  {$IFDEF CPU64BITS}
    result := FRef^
  {$ELSE}
    result := TInterlocked.Read( FRef^)
  {$ENDIF}
end;

procedure TVolatileInt64.Write( const Value: int64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value
  {$ELSE}
    TInterlocked.Exchange( FRef^, Value)
  {$ENDIF}
end;

{$ENDIF}


{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileInt64Helper.Initialize( Value: int64);
begin
  self := Value;
  TAtomic.Assert_isInterlockedIntegral<int64>( self)
end;

procedure TVolatileInt64Helper.Finalize;
begin
end;

{$HINTS OFF}
function TVolatileInt64Helper.Increment: int64;
begin
  result := TInterlocked.Increment( self)
end;

function TVolatileInt64Helper.Decrement: int64;
begin
  result := TInterlocked.Decrement( self)
end;

function TVolatileInt64Helper.Add( Addend: int64): int64;
begin
  result := TInterlocked.Add( self, Addend)
end;

function TVolatileInt64Helper.CompareAndExchange( OldValue, NewValue: int64): boolean;
begin
  result := TInterlocked.CompareExchange( self, NewValue, OldValue) = OldValue
end;

procedure TVolatileInt64Helper.Assign( var Source: TVolatileInt64);
begin
  TInterlocked.Exchange( self, TInterlocked.Read( Source))
end;

function TVolatileInt64Helper.Read: int64;
begin
  result := TInterlocked.Read( self)
end;

procedure TVolatileInt64Helper.Write( const Value: int64);
begin
  TInterlocked.Exchange( self, Value)
end;
{$HINTS OFF}
{$ENDIF}


{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt64.Initialize( Value: uint64);
var
  Addr: NativeInt;
  Misalignment: integer;
begin
  SetLength( FStore, SizeOf( Value) * 2);
  Addr := NativeInt( @FStore[0]);
  Misalignment := Addr mod SizeOf( Value);
  if Misalignment <> 0 then
    Inc( Addr, SizeOf( Value) - Misalignment);
  FRef  := PUInt64( Addr);
  FRef^ := Value;
  // TAtomic.Assert_isInterlockedIntegral<uint64>( FRef^)
end;

procedure TVolatileUInt64.Finalize;
begin
  SetLength( FStore, 0);
  FRef := nil
end;

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64.Add( Addend: uint64): uint64;
begin
  result := uint64( TInterlocked.Add( pint64( FRef)^, int64( Addend)))
end;

function TVolatileUInt64.Decrement: uint64;
begin
  result := uint64( TInterlocked.Decrement( pint64( FRef)^))
end;


function TVolatileUInt64.Increment: uint64;
begin
  result := uint64( TInterlocked.Increment( pint64( FRef)^))
end;
{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64.CompareAndExchange(
  OldValue, NewValue: uint64): boolean;
begin
  result := TInterlocked.CompareExchange( pint64( FRef)^, int64( NewValue), int64( OldValue)) = int64( OldValue)
end;

class operator TVolatileUInt64.Implicit( Value: uint64): TVolatileUInt64;
begin
  {$IFDEF CPU64BITS}
    result.FRef^ := Value
  {$ELSE}
    TInterlocked.Exchange( pint64( result.FRef)^, int64( Value))
  {$ENDIF}
end;

class operator TVolatileUInt64.Implicit( Value: TVolatileUInt64): uint64;
begin
  {$IFDEF CPU64BITS}
    result := Value.FRef^
  {$ELSE}
    result := uint64( TInterlocked.Read( pint64( Value.FRef)^))
  {$ENDIF}
end;


procedure TVolatileUInt64.Assign( var Source: TVolatileUInt64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Source.FRef^
  {$ELSE}
    TInterlocked.Exchange( pint64( FRef)^, TInterlocked.CompareExchange( pint64( Source.FRef)^, 0, 0))
  {$ENDIF}
end;

function TVolatileUInt64.Read: uint64;
begin
  {$IFDEF CPU64BITS}
    result := FRef^
  {$ELSE}
    result := uint64( TInterlocked.Read( pint64( FRef)^))
  {$ENDIF}
end;

procedure TVolatileUInt64.Write( const Value: uint64);
begin
  {$IFDEF CPU64BITS}
    FRef^ := Value
  {$ELSE}
    TInterlocked.Exchange( pint64( FRef)^, int64( Value))
  {$ENDIF}
end;

{$ENDIF}


{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatileUInt64Helper.Initialize( Value: uint64);
begin
  self := Value;
  TAtomic.Assert_isInterlockedIntegral<uint64>( self)
end;

procedure TVolatileUInt64Helper.Finalize;
begin
end;

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$OVERFLOWCHECKS OFF}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64Helper.Increment: uint64;
begin
  result := uint64( TInterlocked.Increment( int64( self)))
end;

function TVolatileUInt64Helper.Decrement: uint64;
begin
  result := uint64( TInterlocked.Decrement( int64( self)))
end;

function TVolatileUInt64Helper.Add( Addend: uint64): uint64;
begin
  result := uint64( TInterlocked.Add( int64( self), int64( Addend)))
end;

{$IFDEF OVERFLOW_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

function TVolatileUInt64Helper.CompareAndExchange( OldValue, NewValue: uint64): boolean;
begin
  result := TInterlocked.CompareExchange( int64( self), int64( NewValue), int64( OldValue)) = int64( OldValue)
end;

procedure TVolatileUInt64Helper.Assign( var Source: TVolatileUInt64);
begin
  TInterlocked.Exchange( int64( self), TInterlocked.Read( int64( Source)))
end;

function TVolatileUInt64Helper.Read: uint64;
begin
  result := TInterlocked.Read( int64( self))
end;

procedure TVolatileUInt64Helper.Write( const Value: uint64);
begin
  TInterlocked.Exchange( int64( self), int64( Value))
end;
{$ENDIF}










{$IFNDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatilePointerHelper.Initialize;
begin
  self := nil;
  TAtomic.Assert_isInterlockedIntegral<pointer>( self)
end;

procedure TVolatilePointerHelper.Finalize;
begin
end;



procedure TVolatilePointerHelper.Assign( var Source: pointer);
begin
  TInterlocked.Exchange( self, TInterlocked.CompareExchange( Source, nil, nil))
end;

function TVolatilePointerHelper.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  result := TInterlocked.CompareExchange( self, NewValue, OldValue) = OldValue
end;


function TVolatilePointerHelper.Read: pointer;
begin
  result := TInterlocked.CompareExchange( self, nil, nil)
end;

procedure TVolatilePointerHelper.Write( const Value: pointer);
begin
  TInterlocked.Exchange( self, Value)
end;
{$ENDIF}



{$IFDEF USE_SLACKSPACE_ALIGNMENT}
procedure TVolatilePointer.Initialize;
var
  Addr: NativeInt;
  Misalignment: integer;
begin
  SetLength( FStore, SizeOf( pointer) * 2);
  Addr := NativeInt( @FStore[0]);
  Misalignment := Addr mod SizeOf( pointer);
  if Misalignment <> 0 then
    Inc( Addr, SizeOf( pointer) - Misalignment);
  FRef  := PPointer( Addr);
  FRef^ := nil;
  // TAtomic.Assert_isInterlockedIntegral<pointer>( FRef^)
end;

procedure TVolatilePointer.Finalize;
begin
  SetLength( FStore, 0);
  FRef := nil
end;


procedure TVolatilePointer.Assign( var Source: TVolatilePointer);
begin
  FRef^ := Source.FRef^
end;

function TVolatilePointer.CompareAndExchange(
  OldValue, NewValue: pointer): boolean;
begin
  result := TInterlocked.CompareExchange( FRef^, NewValue, OldValue) = OldValue
end;

class operator TVolatilePointer.Implicit( Value: pointer): TVolatilePointer;
begin
  result.FRef^ := Value
end;

class operator TVolatilePointer.Implicit( Value: TVolatilePointer): pointer;
begin
  result := Value.FRef^
end;


function TVolatilePointer.Read: pointer;
begin
  result := self.FRef^
end;

procedure TVolatilePointer.Write( const Value: pointer);
begin
  self.FRef^ := Value
end;
{$ENDIF}


procedure TVolatileObject.Initialize;
{$IFDEF USE_SLACKSPACE_ALIGNMENT}
var
  Addr: NativeInt;
  Misalignment: integer;
{$ENDIF}
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    SetLength( FStore, SizeOf( TObject) * 2);
    Addr := NativeInt( @FStore[0]);
    Misalignment := Addr mod SizeOf( TObject);
    if Misalignment <> 0 then
      Inc( Addr, SizeOf( TObject) - Misalignment);
    FRef  := PObject( Addr);
    FRef^ := nil;
    // TAtomic.Assert_isInterlockedIntegral<TObject>( FRef^)
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    FPayload := nil;
    TAtomic.Assert_isInterlockedIntegral<TObject>( FPayload)
  {$ENDIF}
end;

procedure TVolatileObject.Finalize;
var
  Obj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Obj := TInterlocked.Exchange( FRef^, TObject( nil));
    SetLength( FStore, 0);
    FRef := nil;
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Obj := TInterlocked.Exchange( FPayload, TObject( nil));
  {$ENDIF}
  Obj.Free
end;

function TVolatileObject.CompareAndExchange( OldValue, NewValue: TObject): boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    result := TInterlocked.CompareExchange( FRef^, NewValue, OldValue) = OldValue
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    result := TInterlocked.CompareExchange( FPayload, NewValue, OldValue) = OldValue
  {$ENDIF}
end;

procedure TVolatileObject.Assign( var Source: TVolatileObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange( FRef^, TInterlocked.CompareExchange( Source.FRef^, TObject( nil), TObject( nil)))
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange( FPayload, TInterlocked.CompareExchange( Source.FPayload, TObject( nil), TObject( nil)))
  {$ENDIF}
end;

function TVolatileObject.ReadObj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    result := TInterlocked.CompareExchange( FRef^, TObject( nil), TObject( nil))
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    result := TInterlocked.CompareExchange( FPayload, TObject( nil), TObject( nil))
  {$ENDIF}
end;

function TVolatileObject.IsAssigned: boolean;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    result := Assigned( FRef^)
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    result := Assigned( FPayload)
  {$ENDIF}
end;

function TVolatileObject.Read< T>: T;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    result := T( TInterlocked.CompareExchange( FRef^, TObject( nil), TObject( nil)))
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    result := T( TInterlocked.CompareExchange( FPayload, TObject( nil), TObject( nil)))
  {$ENDIF}
end;

procedure TVolatileObject.Write( const Value: TObject);
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange( FRef^, Value)
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    TInterlocked.Exchange( FPayload, Value)
  {$ENDIF}
end;

procedure TVolatileObject.Free;
var
  Obj: TObject;
begin
  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    Obj := TInterlocked.Exchange( FRef^, TObject( nil));
  {$ENDIF}
  {$IFNDEF USE_SLACKSPACE_ALIGNMENT}
    Obj := TInterlocked.Exchange( FPayload, TObject( nil));
  {$ENDIF}
  Obj.Free
end;




class function TAtomic.isReadWriteAtomicallyIntegral( DatumAddress: pointer; DatumLength: integer): boolean;
begin
  result := ((DatumLength = 1) or
             (DatumLength = 2) or
             (DatumLength = 4)
            {$IFDEF CPU64BITS}
             or (DatumLength = 8)
            {$ENDIF}
              ) and
            ((NativeInt( DatumAddress) mod DatumLength) = 0)
end;

class function TAtomic.isReadWriteAtomicallyIntegral<T>( var Datum: T): boolean;
begin
  result := isReadWriteAtomicallyIntegral( @Datum, SizeOf( Datum))
end;


{$IFDEF MSWINDOWS}
function Compute_GetCacheLineSize: Integer;
var
  Info: array of TSystemLogicalProcessorInformation;
  Len, Sz: DWORD;
  j: integer;
begin
  result := 64;
  Len    := 0;
  if (GetProcAddress( GetModuleHandle( 'kernel32.dll'), 'GetLogicalProcessorInformation') <> nil) and
    not GetLogicalProcessorInformation( nil, Len) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
    begin
      Sz := (Len + SizeOf( TSystemLogicalProcessorInformation) - 1) div SizeOf( TSystemLogicalProcessorInformation);
      SetLength( Info, Sz);
      Len := Sz * SizeOf( TSystemLogicalProcessorInformation);
      if (Sz > 0) and GetLogicalProcessorInformation( @Info[0], Len) then
        for j := 0 to Sz - 1 do
          begin
          if (Info[j].Relationship <> RelationCache) or
              (Info[j].Cache.Level <> 1) then
            continue;
          result := Info[j].Cache.LineSize;
          break
          end
    end
end;
{$ENDIF}


{$IFDEF ANDROID}
function Compute_GetCacheLineSize: Integer;
begin
  result := 64;
end;

{$ELSEIF defined(POSIX)}
function Compute_GetCacheLineSize: Integer;
var
  LineSize: uint64;
  Sz: integer;
begin
  Sz := SizeOf( LineSize);
  if sysctlbyname( 'hw.cachelinesize', @LineSize, @Sz, nil, 0) = 0 then
      result := LineSize
    else
      result := 64
end;
{$ENDIF}


class function TAtomic.isWithinCacheLines<T>( var Datum: T): boolean;
begin
  result := isWithinCacheLines( @Datum, SizeOf( Datum))
end;


class function TAtomic.isWithinCacheLines( DatumAddress: pointer; DatumLength: integer): boolean;
var
  CacheLineSize: integer;
begin
  CacheLineSize := FCacheLineSize.Read;
  if CacheLineSize = 0 then
    begin
    CacheLineSize := Compute_GetCacheLineSize;
    FCacheLineSize.Write( CacheLineSize);
    end;
  result := ((NativeInt( DatumAddress) mod
    {$IFDEF CPU32BITS} 4 {$ENDIF}  {$IFDEF CPU64BITS} 8 {$ENDIF}
       ) + DatumLength) <=
    {$IFDEF CPU32BITS} 4 {$ENDIF}  {$IFDEF CPU64BITS} 8 {$ENDIF}
end;


class function TAtomic.isInterlockedIntegral( DatumAddress: pointer; DatumLength: integer): boolean;
begin
  {$IFDEF CPUINTEL}
    result := True
  {$ENDIF}

  {$IFDEF CPUARM}
    result := isWithinCacheLines( DatumAddress, DatumLength)
  {$ENDIF}
end;


class function TAtomic.isInterlockedIntegral<T>( var Datum: T): boolean;
begin
  result := isInterlockedIntegral( @Datum, SizeOf( Datum))
end;


class procedure TAtomic.Assert_isReadWriteAtomicallyIntegral<T>( var Datum: T);
begin
  Assert( TAtomic.isReadWriteAtomicallyIntegral<T>( Datum),
    'Datum is not aligned as required for atomic read/write integrity.')
end;

class procedure TAtomic.Assert_isWithinCacheLines<T>( var Datum: T);
begin
  Assert( TAtomic.isWithinCacheLines<T>( Datum),
    'Datum does not fit within a single L1 CPU cache line.')
end;

class procedure TAtomic.Assert_isInterlockedIntegral<T>( var Datum: T);
begin
  Assert( TAtomic.isInterlockedIntegral<T>( Datum),
    'Datum is not aligned as required for AtomicXXX()/ TInterlocked-class function integrity.')
end;


class function TAtomic.Initialize( var storage: TObject; Factory: TObjectFactory): TObject;
var
  OldValue: NativeInt;
  NewValue: TObject;
begin
  Assert_isInterlockedIntegral<TObject>( Storage);
  if TInterlocked.CompareExchange( pointer( Storage), nil, nil) <> nil then
      result := Storage
    else
      begin
      NewValue := Factory();
      {$IFDEF AUTOREFCOUNT}
        if assigned( NewValue) then
          NewValue.__ObjAddRef;
      {$ENDIF AUTOREFCOUNT}
      pointer( OldValue) := TInterlocked.CompareExchange( pointer( Storage), pointer( NewValue), nil);
      if OldValue = 0 then
          result := NewValue
        else
          begin
          result := TObject( OldValue);
          {$IFDEF AUTOREFCOUNT}
            if assigned( NewValue) then
              NewValue.__ObjRelease;
          {$ELSE}
            NewValue.Free
          {$ENDIF AUTOREFCOUNT}
          end
      end
end;

class function TAtomic.InitializeVolatile( var storage: TVolatileObject; Factory: TObjectFactory): TVolatileObject;
var
  NewValue: TObject;
begin
  // Assume Storage Storage.Initialize() already called at this point.
  result := Storage;
  if result.IsAssigned then exit;
  NewValue := Factory();
  if not Storage.CompareAndExchange( TObject( nil), NewValue) then
    NewValue.Free
end;

class function TAtomic.Initialize( var Storage: IInterface; Factory: TInterfaceFactory): IInterface;
var
  OldValue: NativeInt;
  NewValue: IInterface;
begin
  Assert_isInterlockedIntegral<IInterface>( Storage);
  if TInterlocked.CompareExchange( pointer( Storage), nil, nil) <> nil then
      result := Storage
    else
      begin
      NewValue := Factory();
      if assigned( NewValue) then
        NewValue._AddRef;
      pointer( OldValue) := TInterlocked.CompareExchange( pointer( Storage), pointer( NewValue), nil);
      if OldValue = 0 then
          result := NewValue
        else
          begin
          result := IInterface( OldValue);
          if assigned( NewValue) then
            NewValue._Release
          end
      end
end;

class procedure TAtomic.FreeAndNil( var Storage: TObject);
var
  OldValue: pointer;
begin
  OldValue := TInterlocked.CompareExchange( pointer( Storage), nil, nil);
  if assigned( OldValue) and
     (OldValue = TInterlocked.CompareExchange( pointer( Storage), nil, pointer( OldValue))) then
    {$IFDEF AUTOREFCOUNT}
      TObject( OldValue).__ObjRelease
    {$ELSE}
      TObject( OldValue).Free
    {$ENDIF AUTOREFCOUNT}
end;

class procedure TAtomic.FreeAndNilVolatile( var Storage: TVolatileObject);
var
  OldValue: TObject;
begin
  OldValue := Storage.ReadObj;
  if not assigned( OldValue) then exit;
  if Storage.CompareAndExchange( OldValue, nil) then
    {$IFDEF AUTOREFCOUNT}
      OldValue.__ObjRelease
    {$ELSE}
      OldValue.Free
    {$ENDIF AUTOREFCOUNT}
end;


procedure TSBDSpinLock.Initialize;
begin
  FEntered.Initialize( 0);
  FEntryCount.Initialize( 0)
end;

procedure TSBDSpinLock.Finalize;
begin
  FEntered.Finalize;
  FEntryCount.Finalize
end;

const
  MaxCardinal: cardinal = cardinal( -1);

function TSBDSpinLock.Enter( TimeOut: cardinal): boolean;
const
  InitialYieldCount = 4;
  MaxYieldCount = 10000;
  InitialSleepTime = 1; // milliseconds;
  MaxSleepTime = 100;
var
  This: TThreadId;
  SpinCount: cardinal;
  YieldCount: integer;
  Timer: TStopwatch;
  SleepTime: integer;
begin
  This := TThread.CurrentThread.ThreadID;
  result := FEntered.Read = This;
  if not result then
    begin
    SpinCount  := 0;
    YieldCount := InitialYieldCount;
    SleepTime  := InitialSleepTime;
    if TimeOut <> INFINITE then
      begin
      // Don't use TStopwatch.StartNew; for performance reasons.
      Timer.Reset;
      Timer.Start
      end;
    repeat
      if SpinCount = MaxCardinal then
          SpinCount := 0
        else
          Inc( SpinCount);

      if (SpinCount mod 1000) = 0 then
        begin
        TThread.Sleep( SleepTime);
        SleepTime := SleepTime shl 1;
        if SleepTime  > MaxSleepTime then
           SleepTime := MaxSleepTime
        end

      else if (SpinCount mod 100) = 0 then
        begin
        TThread.SpinWait( YieldCount);
        YieldCount := YieldCount shl 1;
        if YieldCount >= MaxYieldCount then
          YieldCount := InitialYieldCount
        end;
    until (FEntered.CompareAndExchange( 0, This)) or
          ((TimeOut <> INFINITE) and (Timer.ElapsedMilliseconds >= Timeout));
    result := FEntered.Read = This
    end;
  if result then
    FEntryCount.Increment
end;


procedure TSBDSpinLock.Enter;
begin
  Enter( INFINITE)
end;

procedure TSBDSpinLock.Leave;
var
  This: TThreadId;
begin
  This := TThread.CurrentThread.ThreadID;
  if FEntered.Read <> This then
    raise ELockException.CreateRes( @SSpinLockNotOwned);
  if FEntryCount.Decrement <= 0 then
    FEntered.CompareAndExchange( This, 0)
end;

procedure TSBDSpinLock.WithinLock( Action: System.SysUtils.TProc);
begin
  Enter( INFINITE);
  Action;
  Leave
end;


initialization
InitUnit_Atomic;

finalization
DoneUnit_Atomic;


end.
