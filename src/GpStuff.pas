(*:Various stuff with no other place to go.
   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2018 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2006-09-25
   Last modification : 2018-04-11
   Version           : 1.68
</pre>*)(*
   History:
     1.68: 2018-04-11
       - Implemented IGpBuffer.Current.
     1.67: 2018-02-20
       - Implemented ClassNameEx.
     1.66: 2017-10-25
       - Implemented ExceptionsInDebugger which can be used to temporarily
         prevent debugger from breaking on exception. 
       - Implemented two OutputDebugString overloads.
     1.65a: 2017-09-05
       - Fixed three occasions where pointer was incorrectly cast as integer instead of
         NativeUInt and one where it was cast as NativeInt.
     1.65: 2017-05-18
       - Implemented IGpBuffer.GetByteAddr.
     1.64: 2017-02-23
       - Added overloaded AddToList for ansi strings.
     1.63a: 2017-01-28
       - RoundUpTo was incorrectly casting pointer to NativeInt instead of NativeUInt.
     1.63: 2016-11-10
       - Added StoreValue<T>.
     1.62: 2016-09-07
       - Implemented IGpBuffer.Append.
       - Implemented IGpBuffer.SetSize.
     1.61: 2016-07-27
       - Added type TCFunc<T1,T2,TResult>.
     1.60: 2016-07-21
       - Added IndexOfList.
     1.59: 2016-07-18
       - Defined anonymous record TRec<T1,T2,T3,T4,T5>.
     1.58: 2016-06-30
       - TGpTraceable no longer descends from TInterfacedObject and implements
         refcounting internally.
     1.57: 2016-03-29
       - Added type TCFunc.
       - Added two EnumList overloads acception filter with signature (const string): string.
     1.56: 2016-03-05
       - [bero] Added 'const' to IFF(boolean, string, string) overload.
     1.55: 2015-09-21
       - Added interface identification to IGpBuffer.
     1.54: 2015-07-24
       - Added function ParamArray which returns all command-line parameters
         (except ParamStr(0)) as TArray<string>.
     1.52: 2015-07-15
       - Implemented function IsInList(string, array of string).
       - Join renamed to JoinList.
     1.51: 2015-06-15
       - Defined anonymous records TRec<T1...T4>.
     1.50: 2015-05-20
       - Added optional filter proc parameter to EnumList.
     1.49: 2015-05-11
       - Implemented Join(TArray<string>).
     1.48: 2015-04-10
       - Implemented AddToList function.
     1.47: 2015-04-03
       - Implemented SplitList function.
     1.46: 2015-02-16
       - Implemented RoundDownTo function.
       - RoundUpTo marked as inline.
     1.45a: 2015-02-06
       - Compiles with D2007.
     1.45: 2015-01-22
       - Added Ternary<T> record for generic IFF operations.
     1.44: 2014-05-19
       - Implemented IGpBuffer.AsStream.
     1.43: 2014-05-16
       - Implemented IGpBuffer.AsString.
     1.42: 2014-04-21
       - Added parameter ignoreDottedFolders to EnumFiles.
     1.41: 2014-03-19
       - Added default property ByteVal[] to IGpBuffer.
     1.40: 2014-01-15
       - TGpBuffer can be initialized from a stream.
     1.39: 2014-01-10
       - Implemented IGpAutoDestroyObject.Detach.
     1.38: 2014-01-06
       - Implemented TGpInterfacedPersistent.
       - IGpBuffer reimplemented using TMemoryStream.
       - Implemented IGpBuffer.Add.
     1.37: 2013-12-24
       - Added IGpBuffer.
     1.36: 2013-09-24
       - Added SplitURL function.
     1.35: 2013-04-04
       - Added EnumList overload.
     1.34: 2012-11-27
       - Implemented IGpStringBuilder and BuildString.
     1.33: 2012-11-09
       - Implemented RoundUpTo function.
     1.32a: 2012-09-25
       - Workaround for an internal error in D2009.
     1.32: 2012-05-23
       - Implemented FletcherChecksum function.
     1.31: 2012-01-23
       - Implemented EnumFiles enumerator.
     1.30: 2012-01-12
       - Added TGpMemoryBuffer - preallocated, growable caching memory buffer for one
         specific memory size.
     1.29: 2011-12-16
       - X64 compatible.
       - [GJ] Assembler implementation of the x64 implementation of the TableFind* functions.
     1.28: 2011-11-22
       - Implemented IGpTraceable.LogReferences.
     1.27: 2011-11-05
       - Implemented IGpAutoDestroyObject.
     1.26: 2011-01-28
       - Implemented procedure DontOptimize. Call DontOptimize(x) if you want to prevent
         compiler from optimizing out the variable x.
     1.25: 2010-12-15
       - DebugBreak accepts parameter.
       - DebugBreak is only compiled if DEBUG conditional symbol is defined.
       - Asgn overload taking Ansi/Wide strings.
     1.24: 2010-12-14
       - Implemented function DebugBreak.
     1.23: 2010-09-21
       - Implemented function DisableHandler. 
     1.22: 2010-07-09
       - Added IFF overload with AnsiString parameters (Unicode Delphi only).
     1.21: 2010-04-13
       - Implemented overloads for Increment and Decrement in TGp4AlignedInt and
         TGp8AlignedInt64.
       - Implemented Add/Subtract methods in TGp4AlignedInt and TGp8AlignedInt64.
     1.20: 2010-02-01
       - OpenArrayToVarArray supports vtUnicodeString variant type.
     1.19a: 2009-11-16
       - IGpTraceable needs GUID.
     1.19: 2009-07-15
       - Added EnumPairs string array enumerator.
       - Added EnumList string enumerator.
     1.18: 2009-05-08
       - TGp4AlignedInt fully changed from working with Cardinal to Integer.
       - Implemented function CAS (compare and swap) in TGp4AlignedInt and
         TGp8AlignedInt64 records. 
     1.17a: 2009-04-21
       - InterlockedIncrement/InterlockedDecrement deal with integers, therefore
         TGp4AlignedInt.Increment/Decrement must return integers.
     1.17: 2009-03-29
       - Implemented IGpTraceable interface.
     1.16: 2009-03-16
       - TGp8AlignedInt.Addr must be PInt64, not PCardinal.
       - TGp8AlignedInt renamed to TGp8AlignedInt64.
     1.15: 2009-03-11
       - Implemented EnumStrings enumerator.
     1.14: 2008-10-26
       - Implemented EnumValues enumerator.
     1.13: 2008-09-09
       - Added >= and <= operators to the TGp4AlignedInt.
     1.12: 2008-07-28
       - Added int64 support to the OpenArrayToVarArray.
     1.11: 2008-07-20
       - Implemented 8-aligned integer, TGp8AlignedInt.
       - TGp4AlignedInt and TGp8AlignedInt ifdef'd to be only available on D2007+.
     1.10: 2008-07-08
       - [GJ] ReverseWord/ReverseDWord rewritten in assembler.
     1.09: 2008-06-19
       - Implemented 4-aligned integer, TGp4AlignedInt.
     1.08: 2008-04-08
       - Declared MaxInt64 constant.
     1.07: 2008-03-31
       - Added function OpenArrayToVarArray, written by Thomas Schubbauer.
     1.06: 2007-06-13
       - ReverseCardinal renamed to ReverseDWord.
       - Added function ReverseWord.
     1.05: 2007-04-12
       - Added function ReverseCardinal.
     1.04: 2007-02-26
       - Added function TableFindEQ.
       - Changed result semantics for TableFind*.
       - Inlined everying that could be inlined.
       - Changed Asgn implementation so that it doesn't trigger BDS2006 codegen bug.
       - Added OffsetPtr method.
     1.03: 2007-02-20
       - Added function TableFindNE.
     1.02: 2007-01-08
       - Implemented IFF64 function.
     1.01: 2007-01-06
       - Moved in stuff from GpIFF.
     1.0: 2006-09-25
       - Released.
*)

unit GpStuff;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  DSiWin32;

{$IFDEF ConditionalExpressions}
  {$IF CompilerVersion >= 25} //DXE4+
    {$LEGACYIFEND ON}
  {$IFEND}
  {$IF CompilerVersion >= 17} //D2005+
    {$DEFINE USE_STRICT}
  {$IFEND}
  {$IF CompilerVersion >= 26} //DXE5+
    {$DEFINE GpStuff_AnsiStrings}
  {$IFEND}
  {$IF CompilerVersion >= 18} //D2006+
    {$DEFINE GpStuff_Inline}
    {$DEFINE GpStuff_AlignedInt}
    {$DEFINE GpStuff_ValuesEnumerators}
    {$DEFINE GpStuff_Helpers}
  {$IFEND}
  {$IF CompilerVersion >= 20} //D2009+
    {$DEFINE GpStuff_Anonymous}
    {$DEFINE GpStuff_Generics}
  {$IFEND}
  {$IF CompilerVersion >= 21} //D2010+
    {$DEFINE GpStuff_NativeInt}
  {$IFEND}
  {$IF CompilerVersion >= 22} //XE
    {$DEFINE GpStuff_RegEx}
    {$DEFINE GpStuff_TArrayOfT}
  {$IFEND}
  {$IF CompilerVersion >= 23} //XE2
    {$DEFINE GpStuff_FullAnonymous}
  {$IFEND}
{$ENDIF}

const
  MaxInt64 = $7FFFFFFFFFFFFFFF;

{$IFDEF GpStuff_AlignedInt}
type
  TGp4AlignedInt = record
  strict private
    aiData: int64;
    function  GetValue: integer; inline;
    procedure SetValue(value: integer); inline;
  public
    function  Add(value: integer): integer; inline;
    function  Addr: PInteger; inline;
    function  CAS(oldValue, newValue: integer): boolean;
    function  Decrement: integer; overload; inline;
    function  Decrement(value: integer): integer; overload; inline;
    function  Increment: integer; overload; inline;
    function  Increment(value: integer): integer; overload; inline;
    function  Subtract(value: integer): integer; inline;
    class operator Add(const ai: TGp4AlignedInt; i: integer): cardinal; inline;
    class operator Equal(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator GreaterThan(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator GreaterThanOrEqual(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator Implicit(const ai: TGp4AlignedInt): integer; inline;
    class operator Implicit(const ai: TGp4AlignedInt): cardinal; inline;
    class operator Implicit(const ai: TGp4AlignedInt): PInteger; inline;
    class operator LessThan(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator LessThanOrEqual(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator NotEqual(const ai: TGp4AlignedInt; i: integer): boolean; inline;
    class operator Subtract(ai: TGp4AlignedInt; i: integer): cardinal; inline;
    property Value: integer read GetValue write SetValue;
  end; { TGp4AlignedInt }

  TGp8AlignedInt64 = record
  strict private
    aiData: packed record
      DataLo, DataHi: int64;
    end;
    function  GetValue: int64; inline;
    procedure SetValue(value: int64); inline;
  public
    function  Add(value: int64): int64; inline;
    function  Addr: PInt64; inline;
    function  CAS(oldValue, newValue: int64): boolean;
    function  Decrement: int64; overload; inline;
    function  Decrement(value: int64): int64; overload; inline;
    function  Increment: int64; overload; inline;
    function  Increment(value: int64): int64; overload; inline;
    function  Subtract(value: int64): int64; inline;
    property Value: int64 read GetValue write SetValue;
  end; { TGp8AlignedInt64 }

  TGpObjectListHelper = class helper for TObjectList
  public
    function  CardCount: cardinal;
  end; { TGpObjectListHelper }
{$ENDIF GpStuff_AlignedInt}

type
  ///	<summary>
  ///	  TPersistent descanding implementing IInterface.
  ///	</summary>
  TGpInterfacedPersistent = class(TPersistent, IInterface)
  protected
    FRefCount: Integer;
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end; { TGpInterfacedPersistent }

  IGpTraceable = interface(IInterface)
    ['{EA2316AC-B5FA-45EA-86E0-9016CD51C336}']
    function  GetLogReferences: boolean; stdcall;
    function  GetTraceReferences: boolean; stdcall;
    procedure SetTraceReferences(const value: boolean); stdcall;
    procedure SetLogReferences(const value: boolean); stdcall;
  //
    function  _AddRef: integer; stdcall;
    function  _Release: integer; stdcall;
    function  GetRefCount: integer; stdcall;
    property LogReferences: boolean read GetLogReferences write SetLogReferences;
    property TraceReferences: boolean read GetTraceReferences write SetTraceReferences;
  end; { IGpTraceable }

  TGpTraceable = class(TObject, IGpTraceable)
  private
    gtLogRef  : boolean;
    gtRefCount: TGp4AlignedInt;
    gtTraceRef: boolean;
  protected
    function  GetLogReferences: boolean; stdcall;
    function  GetTraceReferences: boolean; stdcall;
    procedure SetLogReferences(const value: boolean); stdcall;
    procedure SetTraceReferences(const value: boolean); stdcall;
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    destructor  Destroy; override;
    function  _AddRef: integer; stdcall;
    function  _Release: integer; stdcall;
    function  GetRefCount: integer; stdcall;
    property LogReferences: boolean read GetLogReferences write SetLogReferences;
    property TraceReferences: boolean read GetTraceReferences write SetTraceReferences;
  end; { TGpTraceable }

  IGpAutoDestroyObject = interface ['{17A1E78B-69EF-42EE-A64B-DA4EA81A2C2C}']
    function  GetObj: TObject;
  //
    function  Detach: TObject;
    procedure Free;
    property Obj: TObject read GetObj;
  end; { IGpAutoDestroyObject }

  {$IFDEF GpStuff_Anonymous}
  IGpAutoExecute = interface ['{A6B38DDB-25F0-4789-BFC4-25787722CBAE}']
  end; { IGpAutoExecute }
  {$ENDIF GpStuff_Anonymous}

  ///	<summary>Preallocated, growable caching memory buffer for one specific memory size.</summary>
  TGpMemoryBuffer = class
  {$IFDEF USE_STRICT} strict {$ENDIF} private
    FBaseBlock   : pointer;
    FBaseBlockEnd: pointer;
    FBufferSize  : integer;
    FList        : pointer;
  public
    constructor Create(bufferSize, preallocateCount: integer);
    destructor  Destroy; override;
    procedure Allocate(var buf: pointer); overload;
    procedure Release(buf: pointer); overload;      {$IFDEF GpStuff_Inline}inline;{$ENDIF}
  end; { TGpMemoryBuffer }

  IGpBuffer = interface ['{0B9FF0FC-492B-412D-B716-618355908550}']
    function  GetAsAnsiString: AnsiString;
    function  GetAsStream: TStream;
    function  GetAsString: string;
    function  GetByteAddr(idx: integer): pointer;
    function  GetByteVal(idx: integer): byte;
    function  GetCurrent: pointer;
    function  GetSize: integer;
    function  GetValue: pointer;
    procedure SetAsAnsiString(const value: AnsiString);
    procedure SetAsString(const value: string);
    procedure SetByteVal(idx: integer; const value: byte);
    procedure SetCurrent(const value: pointer);
    procedure SetSize(const value: integer);
  //
    procedure Add(b: byte); overload;
    procedure Add(ch: AnsiChar); overload;
    procedure Allocate(size: integer);
    procedure Append(data: pointer; size: integer);
    procedure Assign(data: pointer; size: integer);
    procedure Clear;
    function  IsEmpty: boolean;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsStream: TStream read GetAsStream;
    property AsString: string read GetAsString write SetAsString;
    property ByteAddr[idx: integer]: pointer read GetByteAddr;
    property ByteVal[idx: integer]: byte read GetByteVal write SetByteVal; default;
    property Current: pointer read GetCurrent write SetCurrent;
    property Size: integer read GetSize write SetSize;
    property Value: pointer read GetValue;
  end; { IGpBuffer }

  TGpBuffer = class(TInterfacedObject, IGpBuffer)
  strict private
    FData: TMemoryStream;
  protected
    function  GetAsAnsiString: AnsiString; inline;
    function  GetAsStream: TStream; inline;
    function  GetAsString: string; inline;
    function  GetByteAddr(idx: integer): pointer; inline;
    function  GetByteVal(idx: integer): byte; inline;
    function  GetCurrent: pointer; inline;
    function  GetSize: integer; inline;
    function  GetValue: pointer; inline;
    procedure SetAsAnsiString(const value: AnsiString); inline;
    procedure SetAsString(const value: string); inline;
    procedure SetByteVal(idx: integer; const value: byte); inline;
    procedure SetCurrent(const value: pointer); inline;
    procedure SetSize(const value: integer); inline;
  public
    constructor Create; overload;
    constructor Create(data: pointer; size: integer); overload;
    constructor Create(stream: TStream); overload;
    destructor  Destroy; override;
    procedure Add(b: byte); overload; inline;
    procedure Add(ch: AnsiChar); overload; inline;
    procedure Allocate(size: integer); inline;
    procedure Append(data: pointer; size: integer); inline;
    procedure Assign(data: pointer; size: integer); inline;
    procedure Clear; inline;
    function  IsEmpty: boolean; inline;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsStream: TStream read GetAsStream;
    property AsString: string read GetAsString write SetAsString;
    property ByteAddr[idx: integer]: pointer read GetByteAddr;
    property ByteVal[idx: integer]: byte read GetByteVal write SetByteVal; default;
    property Current: pointer read GetCurrent write SetCurrent;
    property Size: integer read GetSize write SetSize;
    property Value: pointer read GetValue;
  end; { TGpBuffer }

  PMethod = ^TMethod;

function  Asgn(var output: boolean; const value: boolean): boolean; overload; {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  Asgn(var output: string; const value: string): string; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  Asgn(var output: integer; const value: integer): integer; overload; {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  Asgn(var output: real; const value: real): real; overload;          {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  Asgn64(var output: int64; const value: int64): int64; overload;     {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$IFDEF Unicode}
function  Asgn(var output: AnsiString; const value: AnsiString): AnsiString; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$ENDIF Unicode}
function  Asgn(var output: WideString; const value: WideString): WideString; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}

function  IFF(condit: boolean; const iftrue, iffalse: string): string; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: integer): integer; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: real): real; overload;        {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: boolean): boolean; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: pointer): pointer; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: TDateTime): TDateTime; overload; {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF64(condit: boolean; iftrue, iffalse: int64): int64;              {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$IFDEF Unicode}
function  IFF(condit: boolean; iftrue, iffalse: AnsiString): AnsiString; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$ENDIF Unicode}

function  AssignValue(var assignTo: string; const assignFrom: string): boolean; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  AssignValue(var assignTo: byte; const assignFrom: byte): boolean; overload;      {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  AssignValue(var assignTo: word; const assignFrom: word): boolean; overload;      {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  AssignValue(var assignTo: longint; const assignFrom: longint): boolean; overload;{$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  AssignValue(var assignTo: DWORD; const assignFrom: DWORD): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  AssignValue(var assignTo: int64; const assignFrom: int64): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}

{$IFDEF GpStuff_Generics}
type
  Ternary<T> = record
    class function IFF(condit: boolean; iftrue, iffalse: T): T; static;       {$IFDEF GpStuff_Inline}inline;{$ENDIF}
  end;

  TRec<T1,T2> = record
    Field1: T1;
    Field2: T2;
    constructor Create(Value1: T1; Value2: T2);
  end;

  TRec<T1,T2,T3> = record
    Field1: T1;
    Field2: T2;
    Field3: T3;
    constructor Create(Value1: T1; Value2: T2; Value3: T3);
  end;

  TRec<T1,T2,T3,T4> = record
    Field1: T1;
    Field2: T2;
    Field3: T3;
    Field4: T4;
    constructor Create(Value1: T1; Value2: T2; Value3: T3; Value4: T4);
  end;

  TRec<T1,T2,T3,T4,T5> = record
    Field1: T1;
    Field2: T2;
    Field3: T3;
    Field4: T4;
    Field5: T5;
    constructor Create(Value1: T1; Value2: T2; Value3: T3; Value4: T4; Value5: T5);
  end;
{$ENDIF GpStuff_Generics}

function  OffsetPtr(ptr: pointer; offset: NativeInt): pointer;                  {$IFDEF GpStuff_Inline}inline;{$ENDIF}

///<summary>Reverses byte order in a 4-byte number.</summary>
function  ReverseDWord(dw: DWORD): DWORD;
///<summary>Reverses byte order in a 2-byte number.</summary>
function  ReverseWord(w: word): word;

///<summary>Locates specified value in a buffer.</summary>
///<returns>Offset of found value (0..dataLen-1) or -1 if value was not found.</returns>
///<since>2007-02-22</since>
function  TableFindEQ(value: byte; data: PChar; dataLen: integer): integer; assembler;

///<summary>Locates a byte different from the specified value in a buffer.</summary>
///<returns>Offset of first differing value (0..dataLen-1) or -1 if buffer contains only specified values.</returns>
///<since>2007-02-22</since>
function  TableFindNE(value: byte; data: PChar; dataLen: integer): integer; assembler;

///<summary>Converts open variant array to COM variant array.<para>
///  Written by Thomas Schubbauer and published in borland.public.delphi.objectpascal on
///  Thu, 7 May 1998 09:53:33 +0200. Original function name was MakeVariant.</para><para>
///  2008-03-31, Gp: Extended to support vtObject type.
///  2008-07-28, Gp: Extended to support vtInt64 type.</para></summary>
function  OpenArrayToVarArray(aValues: array of const): Variant;

function  FormatDataSize(value: int64): string;

function  AutoDestroyObject(obj: TObject): IGpAutoDestroyObject;

{$IFDEF GpStuff_Anonymous}
function  AutoExecute(proc: TProc): IGpAutoExecute;
{$ENDIF GpStuff_Anonymous}

///<summary>Stops execution if the program is running in the debugger.</summary>
procedure DebugBreak(triggerBreak: boolean = true);

procedure DontOptimize(var data);

function FletcherChecksum(const buffer; size: integer): word;

{$IFDEF GpStuff_NativeInt}
function RoundDownTo(value: NativeInt; granularity: integer): NativeInt; overload; {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function RoundUpTo(value: NativeInt; granularity: integer): NativeInt; overload;   {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$ELSE}
function RoundDownTo(value: integer; granularity: integer): integer; overload;     {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function RoundUpTo(value: integer; granularity: integer): integer; overload;       {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$ENDIF GpStuff_NativeInt}
function RoundDownTo(value: pointer; granularity: integer): pointer; overload;     {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function RoundUpTo(value: pointer; granularity: integer): pointer; overload;       {$IFDEF GpStuff_Inline}inline;{$ENDIF}

{$IFDEF GpStuff_ValuesEnumerators}
type
  IGpIntegerValueEnumerator = interface 
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { IGpIntegerValueEnumerator }

  IGpIntegerValueEnumeratorFactory = interface
    function  GetEnumerator: IGpIntegerValueEnumerator;
  end; { IGpIntegerValueEnumeratorFactory }

  IGpStringValueEnumerator = interface
    function  GetCurrent: string;
    function  MoveNext: boolean;
    property Current: string read GetCurrent;
  end; { IGpStringValueEnumerator }

  IGpStringValueEnumeratorFactory = interface
    function  GetEnumerator: IGpStringValueEnumerator;
  end; { IGpStringValueEnumeratorFactory }

  TGpStringPair = class
  private
    kvKey  : string;
    kvValue: string;
  public
    property Key: string read kvKey write kvKey;
    property Value: string read kvValue write kvValue;
  end; { TGpStringPair }

  IGpStringPairEnumerator = interface
    function  GetCurrent: TGpStringPair;
    function  MoveNext: boolean;
    property Current: TGpStringPair read GetCurrent;
  end; { IGpStringPairEnumerator }

  IGpStringPairEnumeratorFactory = interface
    function  GetEnumerator: IGpStringPairEnumerator;
  end; { IGpStringPairEnumeratorFactory }

  IGpDisableHandler = interface
    procedure Restore;
  end; { IGpDisableHandler }

{$IFDEF GpStuff_FullAnonymous}
  TCFunc<T,TResult> = reference to function (const Arg1: T): TResult;
  TCFunc<T1,T2,TResult> = reference to function (const Arg1: T1; const Arg2: T2): TResult;
{$ENDIF}

function EnumValues(const aValues: array of integer): IGpIntegerValueEnumeratorFactory;
function EnumStrings(const aValues: array of string): IGpStringValueEnumeratorFactory;
function EnumPairs(const aValues: array of string): IGpStringPairEnumeratorFactory;
function EnumList(const aList: string; delim: char; const quoteChar: string = '';
  stripQuotes: boolean = true{$IFDEF GpStuff_FullAnonymous};
  filter: TFunc<string,string> = nil{$ENDIF GpStuff_FullAnonymous}): IGpStringValueEnumeratorFactory; overload;
function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string = '';
  stripQuotes: boolean = true{$IFDEF GpStuff_FullAnonymous};
  filter: TFunc<string,string> = nil{$ENDIF GpStuff_FullAnonymous}): IGpStringValueEnumeratorFactory; overload;
{$IFDEF GpStuff_FullAnonymous}
function EnumList(const aList: string; delim: char; const quoteChar: string;
  stripQuotes: boolean; filter: TCFunc<string,string>): IGpStringValueEnumeratorFactory; overload;
function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string;
  stripQuotes: boolean; filter: TCFunc<string,string>): IGpStringValueEnumeratorFactory; overload;
{$ENDIF}
function EnumFiles(const fileMask: string; attr: integer; returnFullPath: boolean = false;
  enumSubfolders: boolean = false; maxEnumDepth: integer = 0;
  ignoreDottedFolders: boolean = false): IGpStringValueEnumeratorFactory;

function AddToList(const aList, delim, newElement: string): string; overload;
{$IFDEF Unicode}
function AddToList(const aList, delim, newElement: AnsiString): AnsiString; overload;
{$ENDIF}
function IsInList(const value: string; const values: array of string; caseSensitive: boolean = false): boolean;
function IndexOfList(const value: string; const values: array of string; caseSensitive: boolean = false): integer;

{$IFDEF GpStuff_TArrayOfT}
function SplitList(const aList: string; delim: string; const quoteChar: string = '';
  stripQuotes: boolean = true): TArray<string>; overload;
function SplitList(const aList: string; delim: TSysCharSet; const quoteChar: string = '';
  stripQuotes: boolean = true): TArray<string>; overload;
function JoinList(const strings: TArray<string>; const delimiter: string): string;

//returns command-line parameters (from 1 to ParamCount) as TArray<string>
function ParamArray: TArray<string>;
{$ENDIF GpStuff_TArrayOfT}

///Usage:
///  with DisableHandler(@@cbDisableInterface.OnClick) do begin
///    cbDisableInterface.Checked := newValue;
///    Restore;
///  end;
function DisableHandler(const handler: PMethod): IGpDisableHandler;
{$ENDIF GpStuff_ValuesEnumerators}

{$IFDEF GpStuff_RegEx}
function ParseURL(const url: string; var proto, host: string; var port: integer;
  var path: string): boolean;
{$ENDIF GpStuff_RegEx}

type
  IGpStringBuilder = interface
    function Add(const s: string): IGpStringBuilder; overload;
    function Add(const s: string; const param: array of const): IGpStringBuilder; overload;
    function AsString: string;
  end; { IGpStringBuilder }

function BuildString: IGpStringBuilder;

function GetRefCount(const intf: IInterface): integer;

procedure OutputDebugString(const msg: string); overload; inline;
procedure OutputDebugString(const msg: string; const params: array of const); overload;

{$IFDEF GpStuff_Generics}
type
  TStoredValue<T> = record
  public
    StoredValue: T;
  end;

  StoreValue<T> = class
    class function Create(const value: T): TStoredValue<T>; static;
  end;
{$ENDIF GpStuff_Generics}

type
  IIgnoreExceptionsInDebugger = interface ['{3F16180B-B3B6-48CF-B6F0-1708E79580DB}']
    procedure Handle;
  end; { IIgnoreExceptionsInDebugger }

  ExceptionsInDebugger = class
  public
    class function Ignore: IIgnoreExceptionsInDebugger;
  end;

function ClassNameEx(obj: TObject): string;

implementation

uses
{$IFDEF GpStuff_AnsiStrings}
  System.AnsiStrings,
{$ENDIF}
{$IFDEF GpStuff_RegEx}
  RegularExpressions{$IFDEF ConditionalExpressions},{$ELSE};{$ENDIF}
{$ENDIF GpStuff_RegEx}
{$IFDEF ConditionalExpressions}
  Variants;
{$ENDIF ConditionalExpressions}

{$IFDEF ConditionalExpressions}
{$IF CompilerVersion <= 20} //D2009 or older
type
  NativeInt  = integer;
  NativeUInt = cardinal;
{$IFEND}
{$ELSE}
type
  NativeInt  = integer;
  NativeUInt = cardinal;
{$ENDIF}

{$IFDEF GpStuff_ValuesEnumerators}
type
  TGpIntegerValueEnumerator = class(TInterfacedObject, IGpIntegerValueEnumerator)
  private
    iveIndex    : integer;
    iveNumValues: integer;
    iveValues   : PIntegerArray;
  public
    constructor Create(values: PIntegerArray; numValues: integer);
    destructor  Destroy; override;
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { TGpIntegerValueEnumerator }

  TGpIntegerValueEnumeratorFactory = class(TInterfacedObject, IGpIntegerValueEnumeratorFactory)
  private
    ivefNumValues: integer;
    ivefValues   : PIntegerArray;
  public
    constructor Create(const aValues: array of integer);
    function GetEnumerator: IGpIntegerValueEnumerator;
  end; { TGpIntegerValueEnumeratorFactory }

  TGpStringValueEnumerator = class(TInterfacedObject, IGpStringValueEnumerator)
  private
    sveIndex : integer;
    sveValues: TStringList;
  public
    constructor Create(values: TStringList);
    destructor  Destroy; override;
    function  GetCurrent: string;
    function  MoveNext: boolean;
    property Current: string read GetCurrent;
  end; { TGpStringValueEnumerator }

  TGpStringValueEnumeratorFactory = class(TInterfacedObject, IGpStringValueEnumeratorFactory)
  private
    svefValues_ref: TStringList;
  public
    constructor Create(values: TStringList);
    function  GetEnumerator: IGpStringValueEnumerator;
  end; { TGpStringValueEnumeratorFactory }

  TGpStringPairEnumerator = class(TInterfacedObject, IGpStringPairEnumerator)
  private
    sveCurrentPair: TGpStringPair;
    sveIndex      : integer;
    svePairs      : TStringList;
  public
    constructor Create(values: TStringList);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringPair;
    function  MoveNext: boolean;
    property Current: TGpStringPair read GetCurrent;
  end; { TGpStringPairEnumerator }

  TGpStringPairEnumeratorFactory = class(TInterfacedObject, IGpStringPairEnumeratorFactory)
  private
    svefPairs_ref: TStringList;
  public
    constructor Create(values: TStringList);
    function  GetEnumerator: IGpStringPairEnumerator;
  end; { TGpStringPairEnumeratorFactory }
{$ENDIF GpStuff_ValuesEnumerators}

type
  TDelimiters = array of integer;

  {$IFDEF GpStuff_ValuesEnumerators}
  TGpDisableHandler = class(TInterfacedObject, IGpDisableHandler)
  strict private
    FHandler   : PMethod;
    FOldHandler: TMethod;
  public
    constructor Create(const handler: PMethod);
    destructor  Destroy; override;
    procedure Restore;
  end; { TGpDisableHandler }
  {$ENDIF}

  TGpAutoDestroyObject = class(TInterfacedObject, IGpAutoDestroyObject)
  {$IFDEF USE_STRICT}strict {$ENDIF}  private
    FObject: TObject;
  protected
    function GetObj: TObject;
  public
    constructor Create(obj: TObject);
    destructor  Destroy; override;
    function Detach: TObject;
    procedure Free;
    property Obj: TObject read GetObj;
  end; { IGpAutoDestroyObject }

  {$IFDEF GpStuff_Anonymous}
  TGpAutoExecute = class(TInterfacedObject, IGpAutoExecute)
  strict private
    FProc: TProc;
  public
    constructor Create(proc: TProc);
    destructor  Destroy; override;
    procedure Run;
  end; { TGpAutoExecute }
  {$ENDIF GpStuff_Anonymous}

  TGpStringBuilder = class(TInterfacedObject, IGpStringBuilder)
  strict private
    FString: string;
  public
    function Add(const s: string): IGpStringBuilder; overload; inline;
    function Add(const s: string; const param: array of const): IGpStringBuilder; overload;
    function AsString: string; inline;
  end; { TGpStringBuilder }

  EUnusedException = class(Exception)
  end; { EUnusedException }

  TIgnoreExceptionsInDebugger = class(TInterfacedObject, IIgnoreExceptionsInDebugger)
  strict private
    FOriginalClass: TClass;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Handle;
  end; { TIgnoreExceptionsInDebugger }

function BuildString: IGpStringBuilder;
begin
  Result := TGpStringBuilder.Create;
end; { BuildString }

function AutoDestroyObject(obj: TObject): IGpAutoDestroyObject;
begin
  Result := TGpAutoDestroyObject.Create(obj);
end; { AutoDestroyObject }

{$IFDEF GpStuff_Anonymous}
function AutoExecute(proc: TProc): IGpAutoExecute;
begin
  Result := TGpAutoExecute.Create(proc);
end; { AutoExecute }
{$ENDIF GpStuff_Anonymous}

//copied from GpString unit
procedure GetDelimiters(const list: string; delim: string; const quoteChar: string;
  addTerminators: boolean; var delimiters: TDelimiters); overload;
var
  chk   : boolean;
  i     : integer;
  idx   : integer;
  lDelim: integer;
  quote : char;
  skip  : boolean;
begin
  SetLength(delimiters, Length(list)+2); // leave place for terminators
  idx := 0;
  lDelim := Length(delim);
  Assert(lDelim > 0);
  if addTerminators then begin
    delimiters[idx] := 1 - lDelim;
    Inc(idx);
  end;
  skip := false;
  if quoteChar = '' then begin
    chk := false;
    quote := #0; //to keep compiler happy
  end
  else begin
    chk   := true;
    quote := quoteChar[1];
  end;
  for i := 1 to Length(list) do begin
    if chk and (list[i] = quote) then
      skip := not skip
    else if not skip then begin
      if ((lDelim = 1) and (list[i] = delim))
         or ((lDelim > 1) and (Copy(list, i, lDelim) = delim)) then
      begin
        delimiters[idx] := i;
        Inc(idx);
      end;
    end;
  end; //for
  if addTerminators then begin
    delimiters[idx] := Length(list)+1;
    Inc(idx);
  end;
  SetLength(delimiters,idx);
end; { GetDelimiters }

procedure GetDelimiters(const list: string; delim: TSysCharSet; const quoteChar: string;
  addTerminators: boolean; var delimiters: TDelimiters); overload;
var
  chk  : boolean;
  i    : integer;
  idx  : integer;
  quote: char;
  skip : boolean;
begin
  SetLength(delimiters, Length(list)+2); // leave place for terminators
  idx := 0;
  if addTerminators then begin
    delimiters[idx] := 0;
    Inc(idx);
  end;
  skip := false;
  if quoteChar = '' then begin
    chk := false;
    quote := #0; //to keep compiler happy
  end
  else begin
    chk   := true;
    quote := quoteChar[1];
  end;
  for i := 1 to Length(list) do begin
    if chk and (list[i] = quote) then
      skip := not skip
    else if not skip then begin
      if AnsiChar(list[i]) in delim then begin
        delimiters[idx] := i;
        Inc(idx);
      end;
    end;
  end; //for
  if addTerminators then begin
    delimiters[idx] := Length(list)+1;
    Inc(idx);
  end;
  SetLength(delimiters,idx);
end; { GetDelimiters }

{ exports }

function Asgn(var output: boolean; const value: boolean): boolean; overload;
begin
  output := value;
  Result := output;
end; { Asgn }

function Asgn(var output: string; const value: string): string; overload;
begin
  output := value;
  Result := output;
end; { Asgn }

{$IFDEF Unicode}
function  Asgn(var output: AnsiString; const value: AnsiString): AnsiString;
begin
  output := value;
  Result := output;
end; { Asgn }
{$ENDIF Unicode}

function  Asgn(var output: WideString; const value: WideString): WideString;
begin
  output := value;
  Result := output;
end; { Asgn }

function Asgn(var output: real; const value: real): real; overload;
begin
  output := value;
  Result := output;
end; { Asgn }

function Asgn(var output: integer; const value: integer): integer; overload;
begin
  output := value;
  Result := output;
end; { Asgn }

function Asgn64(var output: int64; const value: int64): int64; overload;
begin
  output := value;
  Result := output;
end; { Asgn64 }

function IFF(condit: boolean; const iftrue, iffalse: string): string;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF(condit: boolean; iftrue, iffalse: integer): integer;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF(condit: boolean; iftrue, iffalse: real): real;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF(condit: boolean; iftrue, iffalse: boolean): boolean;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF(condit: boolean; iftrue, iffalse: pointer): pointer;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF(condit: boolean; iftrue, iffalse: TDateTime): TDateTime;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }

function IFF64(condit: boolean; iftrue, iffalse: int64): int64;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF64 }

{$IFDEF Unicode}
function IFF(condit: boolean; iftrue, iffalse: AnsiString): AnsiString;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { IFF }
{$ENDIF Unicode}

function AssignValue(var assignTo: string; const assignFrom: string): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function AssignValue(var AssignTo: Byte; const AssignFrom: Byte): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function AssignValue(var AssignTo: Word; const AssignFrom: Word): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function AssignValue(var AssignTo: Longint; const AssignFrom: Longint): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function AssignValue(var AssignTo: DWORD; const AssignFrom: DWORD): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function AssignValue(var AssignTo: Int64; const AssignFrom: Int64): boolean; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
begin
  Result := (assignFrom <> assignTo);
  if Result then
    assignTo := assignFrom;
end; { AssignValue }

function OffsetPtr(ptr: pointer; offset: NativeInt): pointer;
begin
  Result := pointer({$IFDEF Unicode}NativeUInt{$ELSE}cardinal{$ENDIF}(int64(ptr) + offset));
end; { OffsetPtr }

function OpenArrayToVarArray(aValues: array of const): Variant;
var
  i: integer;
begin
  Result := VarArrayCreate([Low(aValues), High(aValues)], varVariant);
  for i := Low(aValues) to High(aValues) do begin
    with aValues[i] do begin
      case VType of
        vtInteger:    Result[i] := VInteger;
        vtBoolean:    Result[i] := VBoolean;
        vtChar:       Result[i] := VChar;
        vtExtended:   Result[i] := VExtended^;
        vtString:     Result[i] := VString^;
        vtPointer:    Result[i] := NativeUInt(VPointer);
        vtPChar:      Result[i] := {$IFDEF GpStuff_AnsiStrings}System.AnsiStrings.{$ENDIF}StrPas(VPChar);
        vtAnsiString: Result[i] := string(VAnsiString);
        vtCurrency:   Result[i] := VCurrency^;
        vtVariant:    Result[i] := VVariant^;
        vtObject:     Result[i] := NativeUInt(VObject);
        vtInterface:  Result[i] := NativeUInt(VInterface);
        vtWideString: Result[i] := WideString(VWideString);
        vtInt64:      Result[i] := VInt64^;
        {$IFDEF UNICODE}
        vtUnicodeString:
                      Result[i] := string(VUnicodeString);
        {$ENDIF UNICODE}
      else
        raise Exception.Create ('OpenArrayToVarArray: invalid data type')
      end; //case
    end; //with
  end; //for i
end; { OpenArrayToVarArray }

function FormatDataSize(value: int64): string;
begin
  if value < 1024*1024 then
    Result := Format('%.1f KB', [value/1024])
  else if value < 1024*1024*1024 then
    Result := Format('%.1f MB', [value/1024/1024])
  else
    Result := Format('%.1f GB', [value/1024/1024/1024]);
end; { FormatDataSize }

{$IFDEF CPUX64}
procedure X64AsmBreak;
asm
  .NOFRAME
  INT 3
end; { X64AsmBreak }
{$ENDIF CPUX64}

procedure DebugBreak(triggerBreak: boolean = true);
begin
  {$IFDEF DEBUG}
  {$WARN SYMBOL_PLATFORM OFF}
  if triggerBreak {$IFDEF MSWINDOWS}and (DebugHook <> 0){$ENDIF} then
  {$WARN SYMBOL_PLATFORM ON}
    {$IFDEF CPUX64}
    X64AsmBreak;
    {$ELSE}
    asm int 3 end;
    {$ENDIF ~CPUX64}
  {$ENDIF DEBUG}
end; { DebugBreak }

function ReverseDWord(dw: cardinal): cardinal;
asm
  {$IFDEF CPUX64}
  mov rax, rcx
  {$ENDIF}
  bswap eax
end; { ReverseDWord }

function ReverseWord(w: word): word;
asm
   {$IFDEF CPUX64}
   mov rax, rcx
   {$ENDIF}
   xchg   al, ah
end; { ReverseWord }

function TableFindEQ(value: byte; data: PChar; dataLen: integer): integer; assembler;
asm
{$IFDEF WIN64}
// value - RCX, data - RDX, dataLen - R8
      PUSH  rDI
      mov   al, value
      MOV   rDI, data
      xor   rcx, rcx
      mov   ecx, dataLen
      REPNE SCASB
      MOV   rAX, -1
      JNE   @@1
      MOV   rAX,rDI
      SUB   rAX,rDX
      DEC   rAX
@@1:  POP   rDI
{$ELSE WIN64}
      PUSH  EDI
      MOV   EDI,EDX
      REPNE SCASB
      MOV   EAX,$FFFFFFFF
      JNE   @@1
      MOV   EAX,EDI
      SUB   EAX,EDX
      DEC   EAX
@@1:  POP   EDI
{$ENDIF WIN64}
end; { TableFindEQ }

function TableFindNE(value: byte; data: PChar; dataLen: integer): integer; assembler;
asm
{$IFDEF WIN64}
      PUSH  rDI
      mov   al, value
      MOV   rDI, data
      xor   rcx, rcx
      mov   ecx, dataLen
      REPNE SCASB
      MOV   rAX, -1
      JE    @@1
      MOV   rAX,rDI
      SUB   rAX,rDX
      DEC   rAX
@@1:  POP   rDI
{$ELSE WIN64}
      PUSH  EDI
      MOV   EDI,EDX
      REPE  SCASB
      MOV   EAX,$FFFFFFFF
      JE    @@1
      MOV   EAX,EDI
      SUB   EAX,EDX
      DEC   EAX
@@1:  POP   EDI
{$ENDIF WIN64}
end; { TableFindNE }


{$IFDEF GpStuff_AlignedInt}

{ TGpAlignedInt }

function TGp4AlignedInt.Add(value: integer): integer;
begin
  Result := InterlockedExchangeAdd(Addr^, value);
end; { TGp4AlignedInt.Add }

function TGp4AlignedInt.Addr: PInteger;
begin
  Result := PInteger((NativeUInt(@aiData) + 3) AND NOT 3);
end; { TGp4AlignedInt.Addr }

function TGp4AlignedInt.CAS(oldValue, newValue: integer): boolean;
begin
  Result := InterlockedCompareExchange(Addr^, newValue, oldValue) = oldValue; 
end; { TGp4AlignedInt.CAS }

function TGp4AlignedInt.Decrement: integer;
begin
  Result := InterlockedDecrement(Addr^);
end; { TGp4AlignedInt.Decrement }

function TGp4AlignedInt.Decrement(value: integer): integer;
begin
  Result := Subtract(value) - value;
end; { TGp4AlignedInt.Decrement }

function TGp4AlignedInt.GetValue: integer;
begin
  Result := Addr^;
end; { TGp4AlignedInt.GetValue }

function TGp4AlignedInt.Increment: integer;
begin
  Result := InterlockedIncrement(Addr^);
end; { TGp4AlignedInt.Increment }

function TGp4AlignedInt.Increment(value: integer): integer;
begin
  Result := Add(value) + value;
end; { TGp4AlignedInt.Increment }

procedure TGp4AlignedInt.SetValue(value: integer);
begin
  Addr^ := value;
end; { TGp4AlignedInt.SetValue }

function TGp4AlignedInt.Subtract(value: integer): integer;
begin
  Result := InterlockedExchangeAdd(Addr^, -value);
end; { TGp4AlignedInt.Subtract }

class operator TGp4AlignedInt.Add(const ai: TGp4AlignedInt; i: integer): cardinal;
begin
  Result := cardinal(int64(ai.Value) + i);
end; { TGp4AlignedInt.Add }

class operator TGp4AlignedInt.Equal(const ai: TGp4AlignedInt; i: integer): boolean;
begin
  Result := (ai.Value = i);
end; { TGp4AlignedInt.Equal }

class operator TGp4AlignedInt.GreaterThan(const ai: TGp4AlignedInt; i: integer): boolean;
begin
  Result := (ai.Value > i);
end; { TGp4AlignedInt.GreaterThan }

class operator TGp4AlignedInt.GreaterThanOrEqual(const ai: TGp4AlignedInt; i: integer):
  boolean;
begin
  Result := (ai.Value >= i);
end; { TGp4AlignedInt.GreaterThanOrEqual }

class operator TGp4AlignedInt.Implicit(const ai: TGp4AlignedInt): PInteger;
begin
  Result := ai.Addr;
end; { TGp4AlignedInt.Implicit }

class operator TGp4AlignedInt.Implicit(const ai: TGp4AlignedInt): cardinal;
begin
  Result := ai.Value;
end; { TGp4AlignedInt.Implicit }

class operator TGp4AlignedInt.Implicit(const ai: TGp4AlignedInt): integer;
begin
  Result := integer(ai.Value);
end; { TGp4AlignedInt.Implicit }

class operator TGp4AlignedInt.LessThan(const ai: TGp4AlignedInt; i: integer): boolean;
begin
  Result := (ai.Value < i);
end; { TGp4AlignedInt.LessThan }

class operator TGp4AlignedInt.LessThanOrEqual(const ai: TGp4AlignedInt; i: integer):
  boolean;
begin
  Result := (ai.Value <= i);
end; { TGp4AlignedInt.LessThanOrEqual }

class operator TGp4AlignedInt.NotEqual(const ai: TGp4AlignedInt; i: integer): boolean;
begin
  Result := (ai.Value <> i);
end; { TGp4AlignedInt.NotEqual }

class operator TGp4AlignedInt.Subtract(ai: TGp4AlignedInt; i: integer): cardinal;
begin
  Result := cardinal(int64(ai.Value) - i);
end; { TGp4AlignedInt.Subtract }

{ TGp8AlignedInt64 }

function TGp8AlignedInt64.Add(value: int64): int64;
begin
  Result := DSiInterlockedExchangeAdd64(Addr^, value);
end; { TGp8AlignedInt64.Add }

function TGp8AlignedInt64.Addr: PInt64;
begin
  Assert(SizeOf(pointer) = SizeOf(NativeInt));
  Result := PInt64((NativeInt(@aiData) + 7) AND NOT 7);
end; { TGp8AlignedInt64.Addr }

function TGp8AlignedInt64.CAS(oldValue, newValue: int64): boolean;
begin
  Result := DSiInterlockedCompareExchange64(Addr, newValue, oldValue) = oldValue;
end; { TGp8AlignedInt64.CAS }

function TGp8AlignedInt64.Decrement: int64;
begin
  Result := DSiInterlockedDecrement64(Addr^);
end; { TGp8AlignedInt64.Decrement }

function TGp8AlignedInt64.Decrement(value: int64): int64;
begin
  Result := Subtract(value) - value;
end; { TGp8AlignedInt64.Decrement }

function TGp8AlignedInt64.GetValue: int64;
begin
  Result := Addr^;
end; { TGp8AlignedInt64.GetValue }

function TGp8AlignedInt64.Increment: int64;
begin
  Result := DSiInterlockedIncrement64(Addr^);
end; { TGp8AlignedInt64.Increment }

function TGp8AlignedInt64.Increment(value: int64): int64;
begin
  Result := Add(value) + value;
end; { TGp8AlignedInt64.Increment }

procedure TGp8AlignedInt64.SetValue(value: int64);
begin
  Addr^ := value;
end; { TGp8AlignedInt64.SetValue }

function TGp8AlignedInt64.Subtract(value: int64): int64;
begin
  Result := DSiInterlockedExchangeAdd64(Addr^, -value);
end; { TGp8AlignedInt64.Subtract }

{$ENDIF GpStuff_AlignedInt}

{$IFDEF GpStuff_AlignedInt}
function TGpObjectListHelper.CardCount: cardinal;
begin
  Result := cardinal(Count);
end; { TGpObjectListHelper.CardCount }
{$ENDIF GpStuff_AlignedInt}

{$IFDEF GpStuff_ValuesEnumerators}

{ TGpIntegerValueEnumerator }

constructor TGpIntegerValueEnumerator.Create(values: PIntegerArray; numValues: integer);
begin
  iveValues := values;
  iveNumValues := numValues;
  iveIndex := -1;
end; { TGpIntegerValueEnumerator.Create }

destructor TGpIntegerValueEnumerator.Destroy;
begin
  FreeMem(iveValues);
  inherited;
end; { pIntegerValueEnumerator.Destroy }

function TGpIntegerValueEnumerator.GetCurrent: integer;
begin
  Result := iveValues^[iveIndex];
end; { TGpIntegerValueEnumerator.GetCurrent }

function TGpIntegerValueEnumerator.MoveNext: boolean;
begin
  Inc(iveIndex);
  Result := (iveIndex < iveNumValues);
end; { TGpIntegerValueEnumerator.MoveNext }

{ TGpStringValueEnumeratorFactory }

constructor TGpIntegerValueEnumeratorFactory.Create(const aValues: array of integer);
var
  dataSize: integer;
begin
  inherited Create;
  ivefNumValues := Length(aValues);
  Assert(ivefNumValues > 0);
  dataSize := ivefNumValues * SizeOf(aValues[0]);
  GetMem(ivefValues, dataSize);
  Move(aValues[0], ivefValues^[0], dataSize);
end; { TGpIntegerValueEnumeratorFactory.Create }

function TGpIntegerValueEnumeratorFactory.GetEnumerator: IGpIntegerValueEnumerator;
begin
  Result := TGpIntegerValueEnumerator.Create(ivefValues, ivefNumValues);
end; { TGpIntegerValueEnumeratorFactory.GetEnumerator }

{ TGpStringValueEnumerator }

constructor TGpStringValueEnumerator.Create(values: TStringList);
begin
  sveValues := values;
  sveIndex := -1;
end; { TGpStringValueEnumerator.Create }

destructor TGpStringValueEnumerator.Destroy;
begin
  FreeAndNil(sveValues);
  inherited;
end; { pIntegerValueEnumerator.Destroy }

function TGpStringValueEnumerator.GetCurrent: string;
begin
  Result := sveValues[sveIndex];
end; { TGpStringValueEnumerator.GetCurrent }

function TGpStringValueEnumerator.MoveNext: boolean;
begin
  Inc(sveIndex);
  Result := (sveIndex < sveValues.Count);
end; { TGpStringValueEnumerator.MoveNext }

{ TGpStringPairEnumeratorFactory }

constructor TGpStringValueEnumeratorFactory.Create(values: TStringList);
begin
  inherited Create;
  svefValues_ref := values;
end; { TGpStringValueEnumeratorFactory.Create }

function TGpStringValueEnumeratorFactory.GetEnumerator: IGpStringValueEnumerator;
begin
  Result := TGpStringValueEnumerator.Create(svefValues_ref); //enumerator takes ownership
end; { TGpStringValueEnumeratorFactory.GetEnumerator }

{ TGpStringPairEnumerator }

constructor TGpStringPairEnumerator.Create(values: TStringList);
begin
  svePairs := values;
  sveIndex := -2;
  sveCurrentPair := TGpStringPair.Create;
end; { TGpStringPairEnumerator.Create }

destructor TGpStringPairEnumerator.Destroy;
begin
  FreeAndNil(sveCurrentPair);
  FreeAndNil(svePairs);
  inherited;
end; { pIntegerPairEnumerator.Destroy }

function TGpStringPairEnumerator.GetCurrent: TGpStringPair;
begin
  sveCurrentPair.Key := svePairs[sveIndex];
  sveCurrentPair.Value := svePairs[sveIndex+1];
  Result := sveCurrentPair;
end; { TGpStringPairEnumerator.GetCurrent }

function TGpStringPairEnumerator.MoveNext: boolean;
begin
  Inc(sveIndex, 2);
  Result := (sveIndex < svePairs.Count);
end; { TGpStringPairEnumerator.MoveNext }

{ TGpStringPairEnumeratorFactory }

constructor TGpStringPairEnumeratorFactory.Create(values: TStringList);
begin
  inherited Create;
  svefPairs_ref := values;
end; { TGpStringPairEnumeratorFactory.Create }

function TGpStringPairEnumeratorFactory.GetEnumerator: IGpStringPairEnumerator;
begin
  Result := TGpStringPairEnumerator.Create(svefPairs_ref); //enumerator takes ownership
end; { TGpStringPairEnumeratorFactory.GetEnumerator }

{ exports }

function EnumValues(const aValues: array of integer): IGpIntegerValueEnumeratorFactory;
begin
  Result := TGpIntegerValueEnumeratorFactory.Create(aValues);
end; { EnumValues }

function EnumStrings(const aValues: array of string): IGpStringValueEnumeratorFactory;
var
  s : string;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for s in aValues do
    sl.Add(s);
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumValues }

function EnumPairs(const aValues: array of string): IGpStringPairEnumeratorFactory;
var
  s : string;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for s in aValues do
    sl.Add(s);
  Result := TGpStringPairEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumPairs }

function EnumList(const aList: string; delim: char; const quoteChar: string;
  stripQuotes: boolean{$IFDEF GpStuff_FullAnonymous};
  filter: TFunc<string,string>{$ENDIF GpStuff_FullAnonymous}): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
  s         : string;
  sl        : TStringList;
begin
  sl := TStringList.Create;
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + 1] = quote) and
         (aList[delimiters[iDelim+1] - 1] = quote)
      then
        s := Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3)
      else
        s := Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1);
      {$IFDEF GpStuff_FullAnonymous}
      if assigned(filter) then
        s := filter(s);
      {$ENDIF GpStuff_FullAnonymous}
      sl.Add(s);
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }

function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string;
  stripQuotes: boolean{$IFDEF GpStuff_FullAnonymous};
  filter: TFunc<string,string>{$ENDIF GpStuff_FullAnonymous}): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
  s         : string;
  sl        : TStringList;
begin
  sl := TStringList.Create;
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + 1] = quote) and
         (aList[delimiters[iDelim+1] - 1] = quote)
      then
        s := Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3)
      else
        s := Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1);
      {$IFDEF GpStuff_FullAnonymous}
      if assigned(filter) then
        s := filter(s);
      {$ENDIF GpStuff_FullAnonymous}
      sl.Add(s);
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }

{$IFDEF GpStuff_FullAnonymous}
function EnumList(const aList: string; delim: char; const quoteChar: string;
  stripQuotes: boolean; filter: TCFunc<string,string>): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
  s         : string;
  sl        : TStringList;
begin
  sl := TStringList.Create;
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + 1] = quote) and
         (aList[delimiters[iDelim+1] - 1] = quote)
      then
        s := Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3)
      else
        s := Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1);
      if assigned(filter) then
        s := filter(s);
      sl.Add(s);
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }

function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string;
  stripQuotes: boolean; filter: TCFunc<string,string>): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
  s         : string;
  sl        : TStringList;
begin
  sl := TStringList.Create;
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + 1] = quote) and
         (aList[delimiters[iDelim+1] - 1] = quote)
      then
        s := Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3)
      else
        s := Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1);
      if assigned(filter) then
        s := filter(s);
      sl.Add(s);
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }
{$ENDIF}

{$IFDEF GpStuff_TArrayOfT}
function SplitList(const aList: string; delim: string; const quoteChar: string = '';
  stripQuotes: boolean = true): TArray<string>;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  lDelim    : integer;
  quote     : char;
begin
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    lDelim := Length(delim);
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    SetLength(Result, High(delimiters) - Low(delimiters));
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + lDelim] = quote) and
         (aList[delimiters[iDelim+1] - 1]      = quote)
      then
        Result[iDelim-Low(delimiters)] := Copy(aList, delimiters[iDelim] + lDelim + 1, delimiters[iDelim+1] - delimiters[iDelim] - lDelim - 2)
      else
        Result[iDelim-Low(delimiters)] := Copy(aList, delimiters[iDelim] + lDelim, delimiters[iDelim+1] - delimiters[iDelim] - lDelim);
    end;
  end;
end; { SplitList }

function SplitList(const aList: string; delim: TSysCharSet; const quoteChar: string = '';
  stripQuotes: boolean = true): TArray<string>;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
begin
  if aList <> '' then begin
    if stripQuotes and (quoteChar <> '') then
      quote := quoteChar[1]
    else begin
      stripQuotes := false;
      quote := #0; //to keep compiler happy;
    end;
    GetDelimiters(aList, delim, quoteChar, true, delimiters);
    SetLength(Result, High(delimiters) - Low(delimiters));
    for iDelim := Low(delimiters) to High(delimiters) - 1 do begin
      if stripQuotes and
         (aList[delimiters[iDelim  ] + 1] = quote) and
         (aList[delimiters[iDelim+1] - 1] = quote)
      then
        Result[iDelim-Low(delimiters)] := Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3)
      else
        Result[iDelim-Low(delimiters)] := Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1);
    end;
  end;
end; { SplitList }

function JoinList(const strings: TArray<string>; const delimiter: string): string;
var
  i       : integer;
  lDelim  : integer;
  lStrings: integer;
  pResult : PChar;
  s       : string;
begin
  lDelim := Length(delimiter);
  lStrings := 0;
  for i := Low(strings) to High(strings) do begin
    Inc(lStrings, Length(strings[i]));
    if i < High(strings) then
      Inc(lStrings, lDelim);
  end;
  SetLength(Result, lStrings);
  pResult := @(Result[1]);
  for i := Low(strings) to High(strings) do begin
    s := strings[i];
    if s <> '' then begin
      Move(s[1], pResult^, Length(s) * SizeOf(char));
      Inc(pResult, Length(s));
    end;
    if lDelim > 0 then begin
      Move(delimiter[1], pResult^, lDelim * SizeOf(char));
      Inc(pResult, lDelim);
    end;
  end;
end; { Join }

function ParamArray: TArray<string>;
var
  i: integer;
begin
  SetLength(Result, ParamCount);
  for i := 1 to ParamCount do
    Result[i-1] := ParamStr(i);
end; { ParamArray }
{$ENDIF GpStuff_TArrayOfT}

function EnumFiles(const fileMask: string; attr: integer; returnFullPath: boolean;
  enumSubfolders: boolean; maxEnumDepth: integer; ignoreDottedFolders: boolean): IGpStringValueEnumeratorFactory;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  DSiEnumFilesToSL(fileMask, attr, sl, returnFullPath, enumSubfolders, maxEnumDepth, ignoreDottedFolders);
  Result := TGpStringValueEnumeratorFactory.Create(sl);
end; { EnumFiles }

function IsInList(const value: string; const values: array of string; caseSensitive: boolean): boolean;
begin
  Result := (IndexOfList(value, values, caseSensitive) >= 0);
end; { IsInList }

function IndexOfList(const value: string; const values: array of string; caseSensitive: boolean = false): integer;
var
  s: string;
begin
  for Result := Low(values) to High(values) do begin
    s := values[Result];
    if caseSensitive then begin
      if SameStr(value, s) then
        Exit;
    end
    else if SameText(value, s) then
      Exit;
  end;
  Result := -1;
end; { IndexOfList }
{$ENDIF GpStuff_ValuesEnumerators}

function AddToList(const aList, delim, newElement: string): string;
begin
  Result := aList;
  if Result <> '' then
    Result := Result + delim;
  Result := Result + newElement;
end; { AddToList }

{$IFDEF Unicode}
function AddToList(const aList, delim, newElement: AnsiString): AnsiString;
begin
  Result := aList;
  if Result <> '' then
    Result := Result + delim;
  Result := Result + newElement;
end; { AddToList }
{$ENDIF Unicode}

{$IFDEF GpStuff_RegEx}
function ParseURL(const url: string; var proto, host: string; var port: integer;
  var path: string): boolean;
const
  CShortURLRegEx = '(?:(http|https)://|)([\d.a-z-]+)(?::(\d{1,5}+))?';
  CURLRegEx = CShortURLRegEx + '(?:/([!$''()*+,._a-z-]++){0,9}(?:/[!$''()*+,._a-z-]*)?(?:\?[!$&''()*+,.=_a-z-]*)?)';
var
  match: TMatch;
begin
  proto := ''; host := ''; port := 80; path := '';
  match := TRegEx.Match(url, CURLRegEx, [roIgnoreCase]);
  Result := match.Success;
  if Result then begin
    if match.Groups.Count > 1 then
      proto := match.Groups[1].Value;
    if match.Groups.Count > 2then
      host := match.Groups[2].Value;
    if match.Groups.Count > 3 then
      port := StrToIntDef(match.Groups[3].Value, 80);
    if match.Groups.Count > 4 then
      path := match.Groups[4].Value;
  end
  else begin // try the short url without a path
    match := TRegEx.Match(url, CShortURLRegEx, [roIgnoreCase]);
    Result := match.Success;
    if Result then begin
      if match.Groups.Count > 1 then
        proto := match.Groups[1].Value;
      if match.Groups.Count > 2then
        host := match.Groups[2].Value;
      if match.Groups.Count > 3 then
        port := StrToIntDef(match.Groups[3].Value, 80);
    end;
  end;
  if proto = '' then
    proto := 'http';
end; { ParseURL }
{$ENDIF}

{ TIgnoreExceptionsInDebugger }

constructor TIgnoreExceptionsInDebugger.Create;
begin
  inherited Create;
  FOriginalClass := ExceptionClass;
  ExceptionClass := EUnusedException;
end; { TIgnoreExceptionsInDebugger.Create }

destructor TIgnoreExceptionsInDebugger.Destroy;
begin
  Handle;
  inherited;
end; { TIgnoreExceptionsInDebugger.Destroy }

procedure TIgnoreExceptionsInDebugger.Handle;
begin
  if ExceptionClass = EUnusedException then
    ExceptionClass := FOriginalClass;
end; { TIgnoreExceptionsInDebugger.Handle }

{ ExceptionsInDebugger }

class function ExceptionsInDebugger.Ignore: IIgnoreExceptionsInDebugger;
begin
  Result := TIgnoreExceptionsInDebugger.Create;
end; { ExceptionsInDebugger.Ignore }

{ TGpStringBuilder }

function TGpStringBuilder.Add(const s: string): IGpStringBuilder;
begin
  FString := FString + s;
  Result := Self;
end; { TGpStringBuilder.Add }

function TGpStringBuilder.Add(const s: string;
  const param: array of const): IGpStringBuilder;
begin
  Result := Add(Format(s, param));
end; { TGpStringBuilder.Add }

function TGpStringBuilder.AsString: string;
begin
  Result := FString;
end; { TGpStringBuilder.AsString }

{ TGpTraceable }

destructor TGpTraceable.Destroy;
begin
  if gtLogRef then
    OutputDebugString(PChar(Format('TGpTraceable.Destroy: [%s]', [ClassName])));
  DebugBreak(gtTraceRef);
  inherited;
end; { TGpTraceable.Destroy }

function TGpTraceable.GetLogReferences: boolean;
begin
  Result := gtLogRef;
end; { TGpTraceable.GetLogReferences }

function TGpTraceable.GetRefCount: integer;
begin
  Result := gtRefCount.Value;
end; { TGpTraceable.GetRefCount }

function TGpTraceable.GetTraceReferences: boolean;
begin
  Result := gtTraceRef;
end; { TGpTraceable.GetTraceReferences }

function TGpTraceable.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TGpTraceable.QueryInterface }

procedure TGpTraceable.SetLogReferences(const value: boolean);
begin
  gtLogRef := value;
end; { TGpTraceable.SetLogReferences }

procedure TGpTraceable.SetTraceReferences(const value: boolean);
begin
  gtTraceRef := value;
end; { TGpTraceable.SetTraceReferences }

function TGpTraceable._AddRef: integer;
begin
  Result := gtRefCount.Increment;
  if gtLogRef then
    OutputDebugString(PChar(Format('TGpTraceable._AddRef: [%s] %d', [ClassName, Result])));
  DebugBreak(gtTraceRef);
end; { TGpTraceable._AddRef }

function TGpTraceable._Release: integer;
begin
  DebugBreak(gtTraceRef);
  Result := gtRefCount.Decrement;
  if gtLogRef then
    OutputDebugString(PChar(Format('TGpTraceable._Release: [%s] %d', [ClassName, Result])));
  if Result = 0 then
    Destroy;
end; { TGpTraceable._Release }

{$IFDEF GpStuff_ValuesEnumerators}
{ TGpDisableHandler }

function DisableHandler(const handler: PMethod): IGpDisableHandler;
begin
  Result := TGpDisableHandler.Create(handler);
end; { DisableHandler }

{$ENDIF}

procedure DontOptimize(var data);
begin
  // do nothing
end; { DontOptimize }

function FletcherChecksum(const buffer; size: integer): word;
var
  iData: integer;
  pData: PByte;
  sum1 : byte;
  sum2 : byte;
begin
  {$R-,Q-}
  sum1 := 0;
  sum2 := 0;
  pData := @buffer;
  for iData := 1 to size do begin
    sum1 := (word(sum1) + pData^) mod 255;
    sum2 := (word(sum2) + sum1) mod 255;
    Inc(pData);
  end;
  Result := (word(sum2) shl 8) OR sum1;
  {$R+,Q+}
end; { FletcherChecksum }

{$IFDEF GpStuff_NativeInt}
function RoundDownTo(value: NativeInt; granularity: integer): NativeInt;
{$ELSE}
function RoundDownTo(value: integer; granularity: integer): integer;
{$ENDIF GpStuff_NativeInt}
begin
  Result := (value div granularity) * granularity;
end; { RoundDownTo }

function RoundDownTo(value: pointer; granularity: integer): pointer;
begin
  Result := pointer((NativeInt(value) div granularity) * granularity);
end; { RoundDownTo }

{$IFDEF GpStuff_NativeInt}
function RoundUpTo(value: NativeInt; granularity: integer): NativeInt;
{$ELSE}
function RoundUpTo(value: integer; granularity: integer): integer;
{$ENDIF GpStuff_NativeInt}
begin
  if value = 0 then
    Result := 0
  else
    Result := (((value - 1) div granularity) + 1) * granularity;
end; { RoundUpTo }

function RoundUpTo(value: pointer; granularity: integer): pointer;
begin
  Result := pointer((((NativeUInt(value) - 1) div NativeUInt(granularity)) + 1) * NativeUInt(granularity));
end; { RoundUpTo }

function GetRefCount(const intf: IInterface): integer;
begin
  Result := intf._AddRef - 1;
  intf._Release;
end; { GetRefCount }

procedure OutputDebugString(const msg: string);
begin
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then
    Windows.OutputDebugString(PChar(msg));
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
end; { OutputDebugString }

procedure OutputDebugString(const msg: string; const params: array of const);
begin
{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook <> 0 then
    OutputDebugString(Format(msg, params));
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
end; { OutputDebugString }

function ClassNameEx(obj: TObject): string;
begin
  if assigned(obj) then
    Result := obj.ClassName
  else
    Result := '<>';
end; { ClassNameEx }

{ TGpDisableHandler }
{$IFDEF GpStuff_ValuesEnumerators}

constructor TGpDisableHandler.Create(const handler: PMethod);
const
  CNilMethod: TMethod = (Code: nil; Data: nil);
begin
  inherited Create;
  FHandler := handler;
  FOldHandler := handler^;
  handler^ := CNilMethod;
end; { TGpDisableHandler.Create }

destructor TGpDisableHandler.Destroy;
begin
  Restore;
  inherited;
end; { TGpDisableHandler.Destroy }

procedure TGpDisableHandler.Restore;
begin
  FHandler^ := FOldHandler;
end; { TGpDisableHandler.Restore }
{$ENDIF}

{ TGpAutoDestroyObject }

constructor TGpAutoDestroyObject.Create(obj: TObject);
begin
  inherited Create;
  FObject := obj;
end; { TGpAutoDestroyObject.Create }

destructor TGpAutoDestroyObject.Destroy;
begin
  Free;
  inherited;
end; { TGpAutoDestroyObject.Destroy }

function TGpAutoDestroyObject.Detach: TObject;
begin
  Result := FObject;
  FObject := nil;
end; { TGpAutoDestroyObject.Detach }

procedure TGpAutoDestroyObject.Free;
begin
  FreeAndNil(FObject);
end; { TGpAutoDestroyObject.Free }

function TGpAutoDestroyObject.GetObj: TObject;
begin
  Result := FObject;
end; { TGpAutoDestroyObject.GetObj }

{ TGpAutoExecute }

{$IFDEF GpStuff_Anonymous}
constructor TGpAutoExecute.Create(proc: TProc);
begin
  inherited Create;
  FProc := proc;
end; { TGpAutoExecute.Create }

destructor TGpAutoExecute.Destroy;
begin
  Run;
  inherited;
end; { TGpAutoExecute.Destroy }

procedure TGpAutoExecute.Run;
begin
  if assigned(FProc) then
    FProc;
  FProc := nil;
end; { TGpAutoExecute.Run }
{$ENDIF GpStuff_Anonymous}

{ TGpMemoryBuffer }

constructor TGpMemoryBuffer.Create(bufferSize, preallocateCount: integer);
var
  baseBlockSize: NativeUInt;
  iBuffer      : integer;
  next         : pointer;
  previous     : pointer;
  stride       : integer;
begin
  inherited Create;
  Assert(bufferSize >= SizeOf(pointer));
  FBufferSize := bufferSize;
  stride := (((bufferSize - 1) div 16) + 1) * 16;
  baseBlockSize := NativeUInt(stride) * NativeUInt(preallocateCount);
  GetMem(FBaseBlock, baseBlockSize);
  Assert(assigned(FBaseBlock));
  FBaseBlockEnd := pointer(NativeUInt(FBaseBlock) + baseBlockSize - 1);
  previous := nil;
  next := FBaseBlock;
  for iBuffer := 1 to preallocateCount do begin
    PPointer(next)^ := previous;
    previous := next;
    next := pointer(NativeUInt(next) + NativeUInt(stride));
  end;
  FList := previous;
end; { TGpMemoryBuffer.Create }

destructor TGpMemoryBuffer.Destroy;
var
  next    : pointer;
  previous: pointer;
begin
  next := FList;
  while assigned(next) do begin
    previous := PPointer(next)^;
    if (NativeUInt(next) < NativeUInt(FBaseBlock)) or
       (NativeUInt(next) > NativeUInt(FBaseBlockEnd))
    then
      FreeMem(next);
    next := previous;
  end;
  FreeMem(FBaseBlock);
  inherited;
end; { TGpMemoryBuffer.Destroy }

procedure TGpMemoryBuffer.Allocate(var buf: pointer);
begin
  if assigned(FList) then begin
    buf := FList;
    FList := PPointer(FList)^;
  end
  else
    GetMem(buf, FBufferSize);
end; { TGpMemoryBuffer.Allocate }

procedure TGpMemoryBuffer.Release(buf: pointer);
begin
  PPointer(buf)^ := FList;
  FList := buf;
end; { TGpMemoryBuffer.Release }

{ TGpBuffer }

constructor TGpBuffer.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create;
end; { TGpBuffer.Create }

constructor TGpBuffer.Create(data: pointer; size: integer);
begin
  Create;
  Assign(data, size);
end; { TGpBuffer.Create }

constructor TGpBuffer.Create(stream: TStream);
begin
  Create;
  if stream.Size > 0 then
    FData.CopyFrom(stream, 0);
end; { TGpBuffer.Create }

function TGpBuffer.GetCurrent: pointer;
begin
  Result := OffsetPtr(FData.Memory, FData.Position);
end; { TGpBuffer.GetCurrent }

destructor TGpBuffer.Destroy;
begin
  FreeAndNil(FData);
end; { TGpBuffer.Destroy }

procedure TGpBuffer.Add(b: byte);
begin
  FData.Write(b, 1);
end; { TGpBuffer.Add }

procedure TGpBuffer.Add(ch: AnsiChar);
begin
  Add(byte(ch));
end; { TGpBuffer.Add }

procedure TGpBuffer.Allocate(size: integer);
begin
  Assert(size >= 0);
  FData.Size := size;
end; { TGpBuffer.Allocate }

procedure TGpBuffer.Append(data: pointer; size: integer);
begin
  FData.Write(data^, size);
end; { TGpBuffer.Append }

procedure TGpBuffer.Assign(data: pointer; size: integer);
begin
  Allocate(size);
  if size > 0 then
    Move(data^, Value^, size);
end; { TGpBuffer.Assign }

procedure TGpBuffer.Clear;
begin
  Allocate(0);
end; { TGpBuffer.Clear }

function TGpBuffer.GetAsAnsiString: AnsiString;
begin
  SetLength(Result, Size);
  if Size > 0 then
    Move(Value^, Result[1], Size);
end; { TGpBuffer.GetAsAnsiString }

function TGpBuffer.GetAsStream: TStream;
begin
  Result := FData;
end; { TGpBuffer.GetAsStream }

function TGpBuffer.GetAsString: string;
begin
  SetLength(Result, Size div SizeOf(char));
  if Size > 0 then
    Move(Value^, Result[1], Size);
end; { TGpBuffer.GetAsString }

function TGpBuffer.GetByteAddr(idx: integer): pointer;
begin
  Assert((idx >= 0) and (idx < Size));
  Result := pointer(NativeUInt(Value) + NativeUInt(idx));
end; { TGpBuffer.GetByteAddr }

function TGpBuffer.GetByteVal(idx: integer): byte;
begin
  Result := PByte(ByteAddr[idx])^;
end; { TGpBuffer.GetByteVal }

function TGpBuffer.GetSize: integer;
begin
  Result := FData.Size;
end; { TGpBuffer.GetSize }

function TGpBuffer.GetValue: pointer;
begin
  Result := FData.Memory;
end; { TGpBuffer.GetValue }

function TGpBuffer.IsEmpty: boolean;
begin
  Result := (Size = 0);
end; { TGpBuffer.IsEmpty }

procedure TGpBuffer.SetAsAnsiString(const value: AnsiString);
begin
  if value = '' then
    Clear
  else
    Assign(@value[1], Length(value));
end; { TGpBuffer.SetAsAnsiString }

procedure TGpBuffer.SetAsString(const value: string);
begin
  if value = '' then
    Clear
  else
    Assign(@value[1], Length(value) * SizeOf(char));
end; { TGpBuffer.SetAsString }

procedure TGpBuffer.SetByteVal(idx: integer; const value: byte);
begin
  Assert((idx >= 0) and (idx < Size));
  PByte(NativeUInt(Value) + NativeUInt(idx))^ := value;
end; { TGpBuffer.SetByteVal }

procedure TGpBuffer.SetCurrent(const value: pointer);
begin
  FData.Position := {$IFDEF Unicode}NativeUInt{$ELSE}cardinal{$ENDIF}(value) -
                    {$IFDEF Unicode}NativeUInt{$ELSE}cardinal{$ENDIF}(FData.Memory);
end; { TGpBuffer.SetCurrent }

procedure TGpBuffer.SetSize(const value: integer);
begin
  FData.Size := value;
end; { TGpBuffer.SetSize }

{ TGpInterfacedPersistent }

procedure TGpInterfacedPersistent.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end; { TGpInterfacedPersistent.AfterConstruction }

procedure TGpInterfacedPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise Exception.Create('TGpInterfacedPersistent: RefCount <> 0');
end; { TGpInterfacedPersistent.BeforeDestruction }

class function TGpInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TGpInterfacedPersistent(Result).FRefCount := 1;
end; { TGpInterfacedPersistent.NewInstance }

function TGpInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TGpInterfacedPersistent.QueryInterface }

function TGpInterfacedPersistent._AddRef: integer;
begin
  Result := InterlockedIncrement(FRefCount);
end; { TGpInterfacedPersistent._AddRef }

function TGpInterfacedPersistent._Release: integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end; { TGpInterfacedPersistent._Release }

{$IFDEF GpStuff_Generics}
class function Ternary<T>.IFF(condit: boolean; iftrue, iffalse: T): T;
begin
  if condit then
    Result := iftrue
  else
    Result := iffalse;
end; { Ternary<T>.IFF }

constructor TRec<T1, T2>.Create(Value1: T1; Value2: T2);
begin
  Field1 := Value1;
  Field2 := Value2;
end; { TRec<T1, T2>.Create }

constructor TRec<T1, T2, T3>.Create(Value1: T1; Value2: T2; Value3: T3);
begin
  Field1 := Value1;
  Field2 := Value2;
  Field3 := Value3;
end; { TRec<T1, T2, T3>.Create }

constructor TRec<T1, T2, T3, T4>.Create(Value1: T1; Value2: T2; Value3: T3; Value4: T4);
begin
  Field1 := Value1;
  Field2 := Value2;
  Field3 := Value3;
  Field4 := Value4;
end; { TRec<T1, T2, T3, T4>.Create }

constructor TRec<T1, T2, T3, T4, T5>.Create(Value1: T1; Value2: T2; Value3: T3;
  Value4: T4; Value5: T5);
begin
  Field1 := Value1;
  Field2 := Value2;
  Field3 := Value3;
  Field4 := Value4;
  Field5 := Value5;
end; { TRec<T1, T2, T3, T4, T5>.Create }

{ StoreValue<T> }

class function StoreValue<T>.Create(const value: T): TStoredValue<T>;
begin
  Result.StoredValue := value;
end; { StoreValue<T>.Create }
{$ENDIF GpStuff_Generics}

end.

