(*:Various stuff with no other place to go.
   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2013 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2006-09-25
   Last modification : 2013-09-24
   Version           : 1.36
</pre>*)(*
   History:
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
       - Implemented function DisableHandler. Usage:
           with DisableHandler(@@cbDisableInterface.OnClick) do begin
             cbDisableInterface.Checked := newValue;
             Restore;
           end;
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
  {$IFEND}
  {$IF CompilerVersion >= 21} //D2010+
    {$DEFINE GpStuff_NativeInt}
  {$IFEND}
  {$IF CompilerVersion >= 22} //XE
    {$DEFINE GpStuff_RegEx}
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

  TGpTraceable = class(TInterfacedObject, IGpTraceable)
  private
    gtLogRef  : boolean;
    gtTraceRef: boolean;
  protected
    function  GetLogReferences: boolean; stdcall;
    function  GetRefCount: integer; stdcall;
    function  GetTraceReferences: boolean; stdcall;
    procedure SetLogReferences(const value: boolean); stdcall;
    procedure SetTraceReferences(const value: boolean); stdcall;
  public
    destructor  Destroy; override;
    function  _AddRef: integer; stdcall;
    function  _Release: integer; stdcall;
    property LogReferences: boolean read GetLogReferences write SetLogReferences;
    property TraceReferences: boolean read GetTraceReferences write SetTraceReferences;
  end; { TGpTraceable }

  IGpAutoDestroyObject = interface ['{17A1E78B-69EF-42EE-A64B-DA4EA81A2C2C}']
    function  GetObj: TObject;
  //
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

function  IFF(condit: boolean; iftrue, iffalse: string): string; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: integer): integer; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: real): real; overload;        {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: boolean): boolean; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF(condit: boolean; iftrue, iffalse: pointer): pointer; overload;  {$IFDEF GpStuff_Inline}inline;{$ENDIF}
function  IFF64(condit: boolean; iftrue, iffalse: int64): int64;              {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$IFDEF Unicode}
function  IFF(condit: boolean; iftrue, iffalse: AnsiString): AnsiString; overload;    {$IFDEF GpStuff_Inline}inline;{$ENDIF}
{$ENDIF Unicode}

function  OffsetPtr(ptr: pointer; offset: integer): pointer;                  {$IFDEF GpStuff_Inline}inline;{$ENDIF}

///<summary>Reverses byte order in a 4-byte number.</summary>
function  ReverseDWord(dw: DWORD): DWORD;
///<summary>Reverses byte order in a 2-byte number.</summary>
function  ReverseWord(w: word): word;

{$IFNDEF CPUX64}
///<summary>Locates specified value in a buffer.</summary>
///<returns>Offset of found value (0..dataLen-1) or -1 if value was not found.</returns>
///<since>2007-02-22</since>
function  TableFindEQ(value: byte; data: PChar; dataLen: integer): integer; assembler;

///<summary>Locates a byte different from the specified value in a buffer.</summary>
///<returns>Offset of first differing value (0..dataLen-1) or -1 if buffer contains only specified values.</returns>
///<since>2007-02-22</since>
function  TableFindNE(value: byte; data: PChar; dataLen: integer): integer; assembler;
{$ENDIF ~CPUX64}

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
function RoundUpTo(value: NativeInt; granularity: integer): NativeInt; overload;
{$ELSE}
function RoundUpTo(value: integer; granularity: integer): integer; overload;
{$ENDIF GpStuff_NativeInt}
function RoundUpTo(value: pointer; granularity: integer): pointer; overload;

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

function EnumValues(const aValues: array of integer): IGpIntegerValueEnumeratorFactory;
function EnumStrings(const aValues: array of string): IGpStringValueEnumeratorFactory;
function EnumPairs(const aValues: array of string): IGpStringPairEnumeratorFactory;
function EnumList(const aList: string; delim: char; const quoteChar: string = '';
  stripQuotes: boolean = true): IGpStringValueEnumeratorFactory; overload;
function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string = '';
  stripQuotes: boolean = true): IGpStringValueEnumeratorFactory; overload;
function EnumFiles(const fileMask: string; attr: integer; returnFullPath: boolean = false;
  enumSubfolders: boolean = false; maxEnumDepth: integer = 0): IGpStringValueEnumeratorFactory;

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
procedure GetDelimiters(const list: string; delim: char; const quoteChar: string;
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
      if list[i] = delim then begin
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

function IFF(condit: boolean; iftrue, iffalse: string): string;
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

function OffsetPtr(ptr: pointer; offset: integer): pointer;
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
        vtPointer:    Result[i] := integer(VPointer);
        vtPChar:      Result[i] := {$IFDEF GpStuff_AnsiStrings}System.AnsiStrings.{$ENDIF}StrPas(VPChar);
        vtAnsiString: Result[i] := string(VAnsiString);
        vtCurrency:   Result[i] := VCurrency^;
        vtVariant:    Result[i] := VVariant^;
        vtObject:     Result[i] := integer(VObject);
        vtInterface:  Result[i] := integer(VInterface);
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
  if triggerBreak and (DebugHook <> 0) then
    {$IFDEF CPUX64}
    X64AsmBreak;
    {$ELSE}
    asm int 3 end;
    {$ENDIF ~CPUX64}
  {$ENDIF DEBUG}
end; { DebugBreak }

function ReverseDWord(dw: cardinal): cardinal;
asm
  bswap eax
end; { ReverseDWord }

function ReverseWord(w: word): word;
asm
   xchg   al, ah
end; { ReverseWord }

{$IFNDEF CPUX64}
function TableFindEQ(value: byte; data: PChar; dataLen: integer): integer; assembler;
asm
{$IFDEF WIN64}
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
      MOV   rDI,rDX
      mov   ecx, dataLen
      REPE  SCASB
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
{$ENDIF ~CPUX64}

{$IFDEF GpStuff_AlignedInt}

{ TGpAlignedInt }

function TGp4AlignedInt.Add(value: integer): integer;
begin
  Result := InterlockedExchangeAdd(Addr^, value);
end; { TGp4AlignedInt.Add }

function TGp4AlignedInt.Addr: PInteger;
begin
  Result := PInteger((NativeInt(@aiData) + 3) AND NOT 3);
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
  stripQuotes: boolean): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
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
        sl.Add(Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3))
      else
        sl.Add(Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1));
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }

function EnumList(const aList: string; delim: TSysCharSet; const quoteChar: string;
  stripQuotes: boolean): IGpStringValueEnumeratorFactory;
var
  delimiters: TDelimiters;
  iDelim    : integer;
  quote     : char;
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
        sl.Add(Copy(aList, delimiters[iDelim] + 2, delimiters[iDelim+1] - delimiters[iDelim] - 3))
      else
        sl.Add(Copy(aList, delimiters[iDelim] + 1, delimiters[iDelim+1] - delimiters[iDelim] - 1));
    end;
  end;
  Result := TGpStringValueEnumeratorFactory.Create(sl); //factory takes ownership
end; { EnumList }

function EnumFiles(const fileMask: string; attr: integer; returnFullPath: boolean;
  enumSubfolders: boolean; maxEnumDepth: integer): IGpStringValueEnumeratorFactory;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  DSiEnumFilesToSL(fileMask, attr, sl, returnFullPath, enumSubfolders, maxEnumDepth);
  Result := TGpStringValueEnumeratorFactory.Create(sl);
end; { EnumFiles }

{$ENDIF GpStuff_ValuesEnumerators}

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
  Result := RefCount;
end; { TGpTraceable.GetRefCount }

function TGpTraceable.GetTraceReferences: boolean;
begin
  Result := gtTraceRef;
end; { TGpTraceable.GetTraceReferences }

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
  Result := inherited _AddRef;
  if gtLogRef then
    OutputDebugString(PChar(Format('TGpTraceable._AddRef: [%s] %d', [ClassName, Result])));
  DebugBreak(gtTraceRef);
end; { TGpTraceable._AddRef }

function TGpTraceable._Release: integer;
begin
  DebugBreak(gtTraceRef);
  Result := inherited _Release;
  if gtLogRef then
    OutputDebugString(PChar(Format('TGpTraceable._Release: [%s] %d', [ClassName, Result])));
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
function RoundUpTo(value: NativeInt; granularity: integer): NativeInt;
{$ELSE}
function RoundUpTo(value: NativeInt; granularity: integer): NativeInt;
{$ENDIF GpStuff_NativeInt}
begin
  Result := (((value - 1) div granularity) + 1) * granularity;
end;

function RoundUpTo(value: pointer; granularity: integer): pointer;
begin
  Result := pointer((((NativeInt(value) - 1) div granularity) + 1) * granularity);
end;

{$IFDEF   GpStuff_ValuesEnumerators}
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

end.
