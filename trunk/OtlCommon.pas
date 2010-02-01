///<summary>Stuff common to the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010, Primoz Gabrijelcic
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
///   Home              : http://otl.17slon.com
///   Support           : http://otl.17slon.com/forum/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///     Web             : http://gp.17slon.com
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2008-06-12
///   Last modification : 2010-02-01
///   Version           : 1.09
///</para><para>
///   History:
///     1.09: 2010-02-01
///       - TOmniValue getters know how to process empty TOmniValue.
///       - Added Environment.Thread interface.
///       - Environment.SystemAffinity moved to Environment.System.Affinity.
///     1.08: 2010-01-14
///       - Added TOmniValue.IsInteger.
///       - Refactored and enhanced TOmniValueContainer.
///       - Defined IOmniValueEnumerable interface.
///     1.07: 2010-01-05
///       - Renamed: IInterfaceDictionary -> IOmniInterfaceDictionary,
///         IInterfaceDictionaryEnumerator -> IOmniInterfaceDictionaryEnumerator,
///         TInterfaceDictionaryPair -> TOmniInterfaceDictionaryPair.
///       - Implemented IOmniEnvironment interface and function Environment returning
///         some information on system and process.
///     1.06: 2009-12-21
///       - Added pointer conversions and AsPointer cast to TOmniValue.
///     1.05: 2009-11-15
///       - Removed lots of stuff that is now implemented using container observers.
///     1.04: 2009-04-18
///       - Added WideString support to TOmniValue.
///     1.03a: 2009-04-05
///       - Bug fixed: TInterfaceDictionaryEnumerator was ignoring first bucket.
///     1.03: 2009-03-30
///       - TOmniCS and IOmniCriticalSection moved to the OtlSync unit.
///     1.02a: 2009-02-09
///       - Simplified TOmniCS.Initialize.
///     1.02: 2009-02-03
///       - Added accessor to the internal critical section to the TOmniCS record.
///     1.01: 2009-01-26
///       - Implemented TOmniCS critical section wrapper.
///       - Added TOmniWaitableValue class.
///     1.0d: 2008-10-05
///       - Use GetGoodHashSize from GpStringHash unit.
///     1.0c: 2008-09-26
///       - Check PostMessage result.
///     1.0b: 2008-09-19
///       - Bug fixed: TOmniValue.Null was not really initialized to Null.
///     1.0a: 2008-09-02
///       - Fixed memory leak that could occur in TOmniMonitorSupport.Notify (in fact it
///         was possible to cause it in demo 11).
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

{$WARN SYMBOL_PLATFORM OFF}

unit OtlCommon;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Variants,
  DSiWin32,
  GpStuff;

const
  // reserved exit statuses
  EXIT_OK                        = 0;
  EXIT_EXCEPTION                 = integer($80000000);
  EXIT_THREADPOOL_QUEUE_TOO_LONG = EXIT_EXCEPTION + 1;
  EXIT_THREADPOOL_STALE_TASK     = EXIT_EXCEPTION + 2;
  EXIT_THREADPOOL_CANCELLED      = EXIT_EXCEPTION + 3;
  EXIT_THREADPOOL_INTERNAL_ERROR = EXIT_EXCEPTION + 4;

type
  TOmniValue = packed record
  private
    ovData: int64;
    ovIntf: IInterface;
    ovType: (ovtNull, ovtBoolean, ovtInteger, ovtDouble, ovtExtended, ovtString,
             ovtObject, ovtInterface, ovtVariant, ovtWideString,
             ovtPointer);
    function  GetAsBoolean: boolean; inline;
    function  GetAsCardinal: cardinal; inline;
    function  GetAsDouble: Double;
    function  GetAsExtended: Extended;
    function  GetAsInt64: int64; inline;
    function  GetAsInteger: integer; inline;
    function  GetAsInterface: IInterface; inline;
    function  GetAsObject: TObject; inline;
    function  GetAsPointer: pointer;
    function  GetAsString: string;
    function  GetAsVariant: Variant;
    function  GetAsVariantArr(idx: integer): Variant; inline;
    function  GetAsWideString: WideString;
    procedure SetAsBoolean(const value: boolean); inline;
    procedure SetAsCardinal(const value: cardinal); inline;
    procedure SetAsDouble(value: Double); inline;
    procedure SetAsExtended(value: Extended);
    procedure SetAsInt64(const value: int64); inline;
    procedure SetAsInteger(const value: integer); inline;
    procedure SetAsInterface(const value: IInterface); inline;
    procedure SetAsObject(const value: TObject); inline;
    procedure SetAsPointer(const value: pointer); inline;
    procedure SetAsString(const value: string);
    procedure SetAsVariant(const value: Variant);
    procedure SetAsWideString(const value: WideString);
  public
    procedure Clear; inline;
    function  IsBoolean: boolean; inline;
    function  IsEmpty: boolean; inline;
    function  IsFloating: boolean; inline;
    function  IsInteger: boolean; inline;
    function  IsInterface: boolean; inline;
    function  IsObject: boolean; inline;
    function  IsPointer: boolean; inline;
    function  IsString: boolean; inline;
    function  IsVariant: boolean; inline;
    function  IsWideString: boolean; inline;
    class function Null: TOmniValue; static;
    function  RawData: PInt64; inline;
    procedure RawZero; inline;
    class operator Equal(const a: TOmniValue; i: integer): boolean; inline;
    class operator Equal(const a: TOmniValue; const s: string): boolean; inline;
    class operator Implicit(const a: boolean): TOmniValue; inline;
    class operator Implicit(const a: Double): TOmniValue; inline;
    class operator Implicit(const a: Extended): TOmniValue; inline;
    class operator Implicit(const a: integer): TOmniValue; inline;
    class operator Implicit(const a: int64): TOmniValue; inline;
    class operator Implicit(const a: pointer): TOmniValue; inline;
    class operator Implicit(const a: string): TOmniValue; inline;
    class operator Implicit(const a: IInterface): TOmniValue; inline;
    class operator Implicit(const a: TObject): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): int64; inline;
    class operator Implicit(const a: TOmniValue): TObject; inline;
    class operator Implicit(const a: TOmniValue): Double; inline;
    class operator Implicit(const a: TOmniValue): Extended; inline;
    class operator Implicit(const a: TOmniValue): string; inline;
    class operator Implicit(const a: TOmniValue): integer; inline;
    class operator Implicit(const a: TOmniValue): pointer; inline;
    class operator Implicit(const a: TOmniValue): WideString; inline;
    class operator Implicit(const a: TOmniValue): boolean; inline;
    class operator Implicit(const a: TOmniValue): IInterface; inline;
    class operator Implicit(const a: WideString): TOmniValue; inline;
    class operator Implicit(const a: Variant): TOmniValue; inline;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsCardinal: cardinal read GetAsCardinal write SetAsCardinal;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInt64: int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsPointer: pointer read GetAsPointer write SetAsPointer;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsVariantArr[idx: integer]: Variant read GetAsVariantArr; default;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end; { TOmniValue }

  IOmniValueEnumerator = interface ['{F60EBBD8-2F87-4ACD-A014-452F296F4699}']
    function  GetCurrent: TOmniValue;
    function  MoveNext: boolean;
    function  Take(var value: TOmniValue): boolean;
    property Current: TOmniValue read GetCurrent;
  end; { IOmniValueEnumerator }

  IOmniValueEnumerable = interface ['{50C1C176-C61F-41F5-AA0B-6FD215E5159F}']
    function  GetEnumerator: IOmniValueEnumerator;
  end; { IOmniValueEnumerable }

  TOmniWaitableValue = class
  public
    Handle: THandle;
    Value : TOmniValue;
    constructor Create;
    destructor  Destroy; override;
    procedure Reset; inline;
    procedure Signal; overload; inline; 
    procedure Signal(const data: TOmniValue); overload; inline;
    function  WaitFor(maxWait_ms: cardinal = INFINITE): boolean; inline;
  end; { TOmniWaitableValue }

  TOmniValueContainer = class
  strict private
    ovcCanModify: boolean;
    ovcNames    : array of string;
    ovcValues   : array of TOmniValue;
    ovcCount    : integer;
  strict protected
    procedure Clear;
    procedure Grow(requiredIdx: integer = -1);
  public
    constructor Create;
    procedure Add(const paramValue: TOmniValue; paramName: string = '');
    procedure Assign(const parameters: array of TOmniValue);
    function  IndexOfName(const paramName: string): integer;
    procedure Insert(paramIdx: integer; const value: TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    function  ParamByIdx(paramIdx: integer): TOmniValue;
    function  ParamByName(const paramName: string): TOmniValue;
  end; { TOmniValueContainer }

  IOmniCounter = interface ['{3A73CCF3-EDC5-484F-8459-532B8C715E3C}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  //
    function  Increment: integer;
    function  Decrement: integer;
    property Value: integer read GetValue write SetValue;
  end; { IOmniCounter }

  TOmniInterfaceDictionaryPair = class
  strict private
    idpKey  : int64;
    idpValue: IInterface;
  protected
    procedure SetKeyValue(const key: int64; const value: IInterface);
  public
    property Key: int64 read idpKey;
    property Value: IInterface read idpValue;
  end; { TOmniInterfaceDictionaryPair }

  IOmniInterfaceDictionaryEnumerator = interface
    function  GetCurrent: TOmniInterfaceDictionaryPair;
    function  MoveNext: boolean;
    property Current: TOmniInterfaceDictionaryPair read GetCurrent;
  end; { IOmniInterfaceDictionaryEnumerator }

  IOmniInterfaceDictionary = interface ['{619FCCF3-E810-4DCF-B902-1EF1A5A72DB5}']
    function  GetEnumerator: IOmniInterfaceDictionaryEnumerator;
  //
    procedure Add(const key: int64; const value: IInterface);
    procedure Clear;
    function  Count: integer; 
    procedure Remove(const key: int64);
    function  ValueOf(const key: int64): IInterface;
  end; { IOmniInterfaceDictionary }

  IOmniAffinity = interface ['{8A6DDC70-F705-4577-869B-6810E776132B}']
    function  GetAsString: string;
    function  GetCount: integer;
    function  GetMask: DWORD;
    procedure SetAsString(const value: string);
    procedure SetCount(const value: integer);
    procedure SetMask(const value: DWORD);
  //
    property AsString: string read GetAsString write SetAsString;
    property Count: integer read GetCount write SetCount;
    property Mask: DWORD read GetMask write SetMask;
  end; { IOmniAffinity }

  TOmniProcessMemoryCounters = TProcessMemoryCounters;

  TOmniProcessTimes = record
    CreationTime: TDateTime;
    UserTime    : int64;
    KernelTime  : int64;
  end; { TOmniProcessTimes }

  TOmniProcessPriorityClass = (pcIdle, pcBelowNormal, pcNormal, pcAboveNormal, pcHigh,
    pcRealtime);

  IOmniProcessEnvironment = interface ['{98D6BDA3-840B-4E19-B01D-633E6A239FE9}']
    function  GetAffinity: IOmniAffinity;
    function  GetMemory: TOmniProcessMemoryCounters;
    function  GetPriorityClass: TOmniProcessPriorityClass;
    function  GetTimes: TOmniProcessTimes;
  //
    property Affinity: IOmniAffinity read GetAffinity;
    property Memory: TOmniProcessMemoryCounters read GetMemory;
    property PriorityClass: TOmniProcessPriorityClass read GetPriorityClass;
    property Times: TOmniProcessTimes read GetTimes;
  end; { IOmniProcessEnvironment }

  IOmniSystemEnvironment = interface ['{9BE1EFE3-4ABB-4C2F-B2A4-B014D0949FEC}']
    function GetAffinity: IOmniAffinity;
  //
    property Affinity: IOmniAffinity read GetAffinity;
  end; { IOmniSystemEnvironment }

  IOmniThreadEnvironment = interface ['{5C11FEC7-9FBE-423F-B30E-543C8240E3A3}']
    function  GetAffinity: IOmniAffinity;
    function  GetID: cardinal;
  //
    property Affinity: IOmniAffinity read GetAffinity;
    property ID: cardinal read GetID;
  end; { IOmniThreadEnvironment }

  IOmniEnvironment = interface ['{4F9594E2-8B88-483C-9616-85B50493406D}']
    function  GetProcess: IOmniProcessEnvironment;
    function  GetSystem: IOmniSystemEnvironment;
    function  GetThread: IOmniThreadEnvironment;
  //
    property Process: IOmniProcessEnvironment read GetProcess;
    property System: IOmniSystemEnvironment read GetSystem;
    property Thread: IOmniThreadEnvironment read GetThread;
  end; { IOmniEnvironment }

  function  CreateCounter(initialValue: integer = 0): IOmniCounter;
  function  CreateEnumerableRange(low, high: int64): IOmniValueEnumerable;
  function  CreateInterfaceDictionary: IOmniInterfaceDictionary;
  function  Environment: IOmniEnvironment;
  procedure SetThreadName(const name: string);
  function  VarToObj(const v: Variant): TObject; inline;

var
  OtlUID: TGp8AlignedInt64;

implementation

uses
  GpStringHash;

type
  IOmniStringData = interface ['{21E52E56-390C-4066-B9FC-83862FFBCBF3}']
    function  GetValue: string;
    procedure SetValue(const value: string);
    property Value: string read GetValue write SetValue;
  end; { IOmniStringData }

  TOmniStringData = class(TInterfacedObject, IOmniStringData)
  strict private
    osdValue: string;
  public
    constructor Create(const value: string);
    function  GetValue: string;
    procedure SetValue(const value: string);
    property Value: string read GetValue write SetValue;
  end; { TOmniStringData }

  IOmniWideStringData = interface ['{B303DB23-4A06-4D25-814A-8A9EDC90D066}']
    function  GetValue: WideString;
    procedure SetValue(const value: WideString);
    property Value: WideString read GetValue write SetValue;
  end; { IOmniWideStringData }

  TOmniWideStringData = class(TInterfacedObject, IOmniWideStringData)
  strict private
    osdValue: WideString;
  public
    constructor Create(const value: WideString);
    function  GetValue: WideString;
    procedure SetValue(const value: WideString);
    property Value: WideString read GetValue write SetValue;
  end; { TOmniWideStringData }

  IOmniVariantData = interface ['{65311D7D-67F1-452E-A0BD-C90596671FC8}']
    function  GetValue: Variant;
    procedure SetValue(const value: Variant);
    property Value: Variant read GetValue write SetValue;
  end; { IOmniVariantData }

  TOmniVariantData = class(TInterfacedObject, IOmniVariantData)
  strict private
    ovdValue: Variant;
  public
    constructor Create(const value: Variant);
    function  GetValue: Variant;
    procedure SetValue(const value: Variant);
    property Value: Variant read GetValue write SetValue;
  end; { TOmniVariantData }

  IOmniExtendedData = interface ['{B6CD371F-A461-436A-8767-9BCA194B1D0E}']
    function  GetValue: Extended;
    procedure SetValue(const value: Extended);
    property Value: Extended read GetValue write SetValue;
  end; { IOmniExtendedData }

  TOmniExtendedData = class(TInterfacedObject, IOmniExtendedData)
  strict private
    oedValue: Extended;
  public
    constructor Create(const value: Extended);
    function  GetValue: Extended;
    procedure SetValue(const value: Extended);
    property Value: Extended read GetValue write SetValue;
  end; { TOmniExtendedData }

  TOmniCounter = class(TInterfacedObject, IOmniCounter)
  strict private
    ocValue: TGp4AlignedInt;
  protected
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  public
    constructor Create(initialValue: integer);
    function Decrement: integer;
    function Increment: integer;
    property Value: integer read GetValue write SetValue;
  end; { TOmniCounter }

  PGp4AlignedInt = ^TGp4AlignedInt;

  TOmniRangeEnumerator = class(TInterfacedObject, IOmniValueEnumerator)
  strict private
    oreCurrent  : int64;
    oreHigh     : int64;
    oreIncrement: boolean;
    oreLow      : PGp4AlignedInt;
  public
    constructor Create(low: PGp4AlignedInt; high: int64; increment: boolean);
    function  GetCurrent: TOmniValue;
    function  MoveNext: boolean;
    function  Take(var value: TOmniValue): boolean;
    property Current: TOmniValue read GetCurrent;
  end; { TOmniRangeEnumerator }

  TOmniEnumerableRange = class(TInterfacedObject, IOmniValueEnumerable)
    function  GetEnumerator: IOmniValueEnumerator;
  strict private
    oerHigh     : int64;
    oerIncrement: boolean;
    oerLow      : TGp4AlignedInt;
  public
    constructor Create(low, high: int64);
  end; { TOmniEnumerableRange }

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next : PHashItem;
    Key  : int64;
    Value: IInterface;
  end; { THashItem }

  TBucketArray = array of PHashItem;
  PBucketArray = ^TBucketArray;

  TOmniInterfaceDictionaryEnumerator = class(TInterfacedObject, IOmniInterfaceDictionaryEnumerator)
  strict private
    ideBuckets  : PBucketArray;
    ideBucketIdx: integer;
    ideCurrent  : PHashItem;
    ideItem     : PHashItem;
    idePair     : TOmniInterfaceDictionaryPair;
  public
    constructor Create(buckets: PBucketArray);
    destructor  Destroy; override;
    function  GetCurrent: TOmniInterfaceDictionaryPair;
    function  MoveNext: boolean;
    property Current: TOmniInterfaceDictionaryPair read GetCurrent;
  end; { IOmniInterfaceDictionaryEnumerator }

  TOmniInterfaceDictionary = class(TInterfacedObject, IOmniInterfaceDictionary)
  strict private
    idBuckets: TBucketArray;
    idCount  : integer;
  strict protected
    function  Find(const key: int64): PPHashItem;
    function  HashOf(const key: int64): integer; inline;
    procedure Resize(size: Cardinal);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const key: int64; const value: IInterface);
    procedure Clear;
    function  Count: integer; inline;
    function  GetEnumerator: IOmniInterfaceDictionaryEnumerator;
    procedure Remove(const key: int64);
    function  ValueOf(const key: int64): IInterface;
  end; { TOmniInterfaceDictionary }

  TOmniAffinityTarget = (atSystem, atProcess, atThread);

  TOmniAffinity = class(TInterfacedObject, IOmniAffinity)
  strict private
    oaTarget: TOmniAffinityTarget;
  protected
    function GetAsString: string;
    function  GetCount: integer;
    function  GetMask: DWORD;
    procedure SetAsString(const value: string);
    procedure SetCount(const value: integer);
    procedure SetMask(const value: DWORD);
  public
    constructor Create(target: TOmniAffinityTarget);
    property AsString: string read GetAsString write SetAsString;
    property Count: integer read GetCount write SetCount;
    property Mask: DWORD read GetMask write SetMask;
  end; { TOmniAffinity }

  TOmniProcessEnvironment = class(TInterfacedObject, IOmniProcessEnvironment)
  strict private
    opeAffinity: IOmniAffinity;
  protected
    function  GetAffinity: IOmniAffinity;
    function  GetMemory: TOmniProcessMemoryCounters;
    function  GetPriorityClass: TOmniProcessPriorityClass;
    function  GetTimes: TOmniProcessTimes;
  public
    constructor Create;
    property Affinity: IOmniAffinity read GetAffinity;
    property Memory: TOmniProcessMemoryCounters read GetMemory;
    property PriorityClass: TOmniProcessPriorityClass read GetPriorityClass;
    property Times: TOmniProcessTimes read GetTimes;
  end; { TOmniProcessEnvironment }

  TOmniSystemEnvironment = class(TInterfacedObject, IOmniSystemEnvironment)
  strict private
    oseAffinity: IOmniAffinity;
  protected
    function  GetAffinity: IOmniAffinity;
  public
    constructor Create;
    property Affinity: IOmniAffinity read GetAffinity;
  end; { TOmniSystemEnvironment }

  TOmniThreadEnvironment = class(TInterfacedObject, IOmniThreadEnvironment)
  strict private
    oteAffinity: IOmniAffinity;
    oteThreadID: cardinal;
  protected
    function  GetAffinity: IOmniAffinity;
    function  GetID: cardinal;
  public
    constructor Create;
    property Affinity: IOmniAffinity read GetAffinity;
    property ID: cardinal read GetID;
  end; { TOmniThreadEnvironment }

  TOmniEnvironment = class(TInterfacedObject, IOmniEnvironment)
  strict private
    oeProcessEnv: IOmniProcessEnvironment;
    oeSystemEnv : IOmniSystemEnvironment;
    oeThreadEnv : IOmniThreadEnvironment;
  protected
    function  GetProcess: IOmniProcessEnvironment;
    function  GetSystem: IOmniSystemEnvironment;
    function  GetThread: IOmniThreadEnvironment;
  public
    constructor Create;
    property Process: IOmniProcessEnvironment read GetProcess;
    property System: IOmniSystemEnvironment read GetSystem;
    property Thread: IOmniThreadEnvironment read GetThread;
  end; { TOmniEnvironment }

var
  GEnvironment: IOmniEnvironment;

{ exports }

function CreateCounter(initialValue: integer): IOmniCounter;
begin
  Result := TOmniCounter.Create(initialValue);
end; { CreateCounter }

function CreateEnumerableRange(low, high: int64): IOmniValueEnumerable;
begin
  Result := TOmniEnumerableRange.Create(low, high);
end; { CreateEnumerableRange }

function CreateInterfaceDictionary: IOmniInterfaceDictionary;
begin
  Result := TOmniInterfaceDictionary.Create;
end; { CreateInterfaceDictionary }

function Environment: IOmniEnvironment;
begin
  Result := GEnvironment;
end; { Environment }

procedure SetThreadName(const name: string);
type
  TThreadNameInfo = record
    FType    : LongWord; // must be 0x1000
    FName    : PAnsiChar;// pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags   : LongWord; // reserved for future use, must be zero
  end; { TThreadNameInfo }
var
  ansiName      : AnsiString;
  threadNameInfo: TThreadNameInfo;
begin
  if DebugHook <> 0 then begin
    ansiName := AnsiString(name);
    threadNameInfo.FType := $1000;
    threadNameInfo.FName := PAnsiChar(ansiName);
    threadNameInfo.FThreadID := $FFFFFFFF;
    threadNameInfo.FFlags := 0;
    try
      RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
    except {ignore} end;
  end;
end; { SetThreadName }

function VarToObj(const v: Variant): TObject;
begin
  Result := TObject(cardinal(v));
end; { VarToObj }

{ TOmniValueContainer }

constructor TOmniValueContainer.Create;
begin
  inherited Create;
  ovcCanModify := true;
  ovcCount := 0;
end; { TOmniValueContainer.Create }

procedure TOmniValueContainer.Add(const paramValue: TOmniValue; paramName: string);
var
  idxParam: integer;
  newParam: boolean;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Locked');
  newParam := (paramName = '') or (Asgn(idxParam, IndexOfName(paramName)) < 0);
  if newParam then begin
    idxParam := ovcCount;
    Inc(ovcCount);
  end;
  if idxParam > High(ovcValues) then
    Grow;
  if newParam then
    ovcNames[idxParam] := paramName;
  ovcValues[idxParam] := paramValue;
end; { TOmniValueContainer.Add }

procedure TOmniValueContainer.Assign(const parameters: array of TOmniValue);
var
  value: TOmniValue;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  Clear;
  Grow(Length(parameters)-1);
  for value in parameters do
    Add(value);
end; { TOmniValueContainer.Assign }

procedure TOmniValueContainer.Clear;
begin
  SetLength(ovcNames, 0);
  SetLength(ovcValues, 0);
  ovcCount := 0;
end; { TOmniValueContainer.Clear }

procedure TOmniValueContainer.Grow(requiredIdx: integer = -1);
var
  iValue   : integer;
  newLength: integer;
  tmpNames : array of string;
  tmpValues: array of TOmniValue;
begin
  Assert(Length(ovcNames) = Length(ovcValues));
  SetLength(tmpNames, Length(ovcNames));
  SetLength(tmpValues, Length(ovcValues));
  for iValue := 0 to High(ovcValues) - 1 do begin
    tmpNames[iValue] := ovcNames[iValue];
    tmpValues[iValue] := ovcValues[iValue];
  end;
  newLength := 2*Length(ovcValues)+1;
  if newLength <= requiredIdx then
    newLength := requiredIdx + 1;
  SetLength(ovcNames, newLength);
  SetLength(ovcValues, newLength);
  for iValue := 0 to High(tmpValues) - 1 do begin
    ovcNames[iValue] := tmpNames[iValue];
    ovcValues[iValue] := tmpValues[iValue];
  end;
end; { TOmniValueContainer.Grow }

function TOmniValueContainer.IndexOfName(const paramName: string): integer;
begin
  for Result := 0 to ovcCount - 1 do
    if SameText(paramName, ovcNames[Result]) then
      Exit;
  Result := -1;
end; { TOmniValueContainer.IndexOfName }

procedure TOmniValueContainer.Insert(paramIdx: integer; const value: TOmniValue);
begin
  if paramIdx > High(ovcValues) then 
    Grow(paramIdx);
  ovcValues[paramIdx] := value;
end; { TOmniValueContainer.Insert }

function TOmniValueContainer.IsLocked: boolean;
begin
  Result := not ovcCanModify;
end; { TOmniValueContainer.IsLocked }

procedure TOmniValueContainer.Lock;
begin
  ovcCanModify := false;
end; { TOmniValueContainer.Lock }

function TOmniValueContainer.ParamByIdx(paramIdx: integer): TOmniValue;
begin
  Result := ovcValues[paramIdx];
end; { TOmniValueContainer.ParamByIdx }

function TOmniValueContainer.ParamByName(const paramName: string): TOmniValue;
begin
  Result := ovcValues[IndexOfName(paramName)];
end; { TOmniValueContainer.ParamByName }

{ TOmniCounter }

constructor TOmniCounter.Create(initialValue: integer);
begin
  Value := initialValue;
end; { TOmniCounter.Create }

function TOmniCounter.Decrement: integer;
begin
  Result := ocValue.Decrement;
end; { TOmniCounter.Decrement }

function TOmniCounter.GetValue: integer;
begin
  Result := ocValue;
end; { TOmniCounter.GetValue }

function TOmniCounter.Increment: integer;
begin
  Result := ocValue.Increment;
end; { TOmniCounter.Increment }

procedure TOmniCounter.SetValue(const value: integer);
begin
  ocValue.Value := value;
end; { TOmniCounter.SetValue }

{ TOmniRangeEnumerator }

constructor TOmniRangeEnumerator.Create(low: PGp4AlignedInt; high: int64; increment:
  boolean);
begin
  inherited Create;
  oreLow := low;
  oreHigh := high;
  oreIncrement := increment;
end; { TOmniRangeEnumerator.Create }

function TOmniRangeEnumerator.GetCurrent: TOmniValue;
begin
  Result := oreCurrent;
end; { TOmniRangeEnumerator.GetCurrent }

function TOmniRangeEnumerator.MoveNext: boolean;
begin
  if oreIncrement then begin
    oreCurrent := oreLow^.Increment;
    Result := (oreCurrent <= oreHigh);
  end
  else begin
    oreCurrent := oreLow^.Decrement;
    Result := (oreCurrent >= oreHigh);
  end;
end; { TOmniRangeEnumerator.MoveNext }

function TOmniRangeEnumerator.Take(var value: TOmniValue): boolean;
begin
  Result := MoveNext;
  if Result then
    value := oreCurrent;
end; { TOmniRangeEnumerator.Take }

{ TOmniEnumerableRange }

constructor TOmniEnumerableRange.Create(low, high: int64);
begin
  inherited Create;
  oerLow.Value := low;
  oerHigh := high;
  oerIncrement := (low <= high);
  if oerIncrement then
    oerLow.Decrement
  else
    oerLow.Increment;
end; { TOmniEnumerableRange.Create }

function TOmniEnumerableRange.GetEnumerator: IOmniValueEnumerator;
begin
  Result := TOmniRangeEnumerator.Create(@oerLow, oerHigh, oerIncrement);
end; { TOmniEnumerableRange.GetEnumerator }

{ TOmniInterfaceDictionaryPair }

procedure TOmniInterfaceDictionaryPair.SetKeyValue(const key: int64; const value: IInterface);
begin
  idpKey := key;
  idpValue := value;
end; { TOmniInterfaceDictionaryPair.SetKeyValue }

{ TOmniInterfaceDictionaryEnumerator }

constructor TOmniInterfaceDictionaryEnumerator.Create(buckets: PBucketArray);
begin
  ideBuckets := buckets;
  ideBucketIdx := Low(ideBuckets^);
  ideItem := nil;
  idePair := TOmniInterfaceDictionaryPair.Create;
end; { TOmniInterfaceDictionaryEnumerator.Create }

destructor TOmniInterfaceDictionaryEnumerator.Destroy;
begin
  FreeAndNil(idePair);
  inherited Destroy;
end; { TOmniInterfaceDictionaryEnumerator.Destroy }

function TOmniInterfaceDictionaryEnumerator.GetCurrent: TOmniInterfaceDictionaryPair;
begin
  idePair.SetKeyValue(ideCurrent^.Key, ideCurrent^.Value);
  Result := idePair;
end; { TOmniInterfaceDictionaryEnumerator.GetCurrent }

function TOmniInterfaceDictionaryEnumerator.MoveNext: boolean;
begin
  Result := false;
  while not assigned(ideItem) do begin
    Inc(ideBucketIdx);
    if ideBucketIdx > High(ideBuckets^) then
      Exit;
    ideItem := ideBuckets^[ideBucketIdx];
  end;
  ideCurrent := ideItem;
  ideItem := ideItem^.Next;
  Result := true;
end; { TOmniInterfaceDictionaryEnumerator.MoveNext }

{ TInterfaceHash }

constructor TOmniInterfaceDictionary.Create;
begin
  inherited Create;
  Resize(1);
end; { TOmniInterfaceDictionary.Create }

destructor TOmniInterfaceDictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end; { TOmniInterfaceDictionary.Destroy }

procedure TOmniInterfaceDictionary.Add(const key: int64; const value: IInterface);
var
  bucket: PHashItem;
  hash  : integer;
begin
  hash := HashOf(key);
  New(bucket);
  bucket^.Key := key;
  bucket^.Value := value;
  bucket^.Next := idBuckets[hash];
  idBuckets[hash] := bucket;
  Inc(idCount);
  if idCount > (1.5 * Length(idBuckets)) then
    Resize(idCount * 2);
end; { TOmniInterfaceDictionary.Add }

procedure TOmniInterfaceDictionary.Clear;
var
  bucket : PHashItem;
  iBucket: integer;
  next   : PHashItem;
begin
  for iBucket := 0 to Length(idBuckets) - 1 do begin
    bucket := idBuckets[iBucket];
    while bucket <> nil do begin
      next := bucket^.Next;
      Dispose(bucket);
      bucket := next;
    end;
    idBuckets[iBucket] := nil;
  end;
  idCount := 0;
end; { TOmniInterfaceDictionary.Clear }

function TOmniInterfaceDictionary.Count: integer;
begin
  Result := idCount;
end; { TOmniInterfaceDictionary.Count }

function TOmniInterfaceDictionary.Find(const key: int64): PPHashItem;
var
  hash: integer;
begin
  hash := HashOf(key);
  Result := @idBuckets[hash];
  while Result^ <> nil do begin
    if Result^.key = key then
      Exit
    else
      Result := @Result^.Next;
  end;
end; { TOmniInterfaceDictionary.Find }

function TOmniInterfaceDictionary.GetEnumerator: IOmniInterfaceDictionaryEnumerator;
begin
  Result := TOmniInterfaceDictionaryEnumerator.Create(@idBuckets);
end; { TOmniInterfaceDictionary.GetEnumerator }

function TOmniInterfaceDictionary.HashOf(const key: int64): integer;
begin
  Result := key mod Length(idBuckets);
end; { TOmniInterfaceDictionary.HashOf }

procedure TOmniInterfaceDictionary.Remove(const key: int64);
var
  bucket    : PHashItem;
  bucketHead: PPHashItem;
begin
  bucketHead := Find(key);
  bucket := bucketHead^;
  if assigned(bucket) then begin
    bucketHead^ := bucket^.Next;
    Dispose(bucket);
    Dec(idCount);
  end;
end; { TOmniInterfaceDictionary.Remove }

procedure TOmniInterfaceDictionary.Resize(size: Cardinal);
var
  bucket    : PHashItem;
  iBucket   : integer;
  oldBuckets: TBucketArray;
begin
  if Cardinal(Length(idBuckets)) >= size then
    Exit;
  oldBuckets := idBuckets;
  idBuckets := nil;
  SetLength(idBuckets, GetGoodHashSize(size));
  for iBucket := 0 to High(oldBuckets) do begin
    bucket := oldBuckets[iBucket];
    while assigned(bucket) do begin
       Add(bucket.Key, bucket.Value);
       bucket := bucket.Next;
    end;
  end;
end; { TOmniInterfaceDictionary.Resize }

function TOmniInterfaceDictionary.ValueOf(const key: int64): IInterface;
var
  bucketHead: PHashItem;
begin
  bucketHead := Find(key)^;
  if bucketHead <> nil then
    Result := bucketHead^.Value
  else
    Result := nil;
end; { TOmniInterfaceDictionary.ValueOf }

{ TOmniValue }

procedure TOmniValue.Clear;
begin
  ovData := 0;
  ovIntf := nil;
  ovType := ovtNull;
end; { TOmniValue.Clear }

function TOmniValue.GetAsBoolean: boolean;
begin
  if ovType <> ovtBoolean then
    raise Exception.Create('TOmniValue cannot be converted to boolean');
  Result := PByte(RawData)^ <> 0;
end; { TOmniValue.GetAsBoolean }

function TOmniValue.GetAsCardinal: cardinal;
begin
  Result := AsInt64;
end; { TOmniValue.GetAsCardinal }

function TOmniValue.GetAsDouble: Double;
begin
  case ovType of
    ovtInteger:  Result := AsInt64;
    ovtDouble:   Result := PDouble(RawData)^;
    ovtExtended: Result := (ovIntf as IOmniExtendedData).Value;
    else raise Exception.Create('TOmniValue cannot be converted to double');
  end;
end; { TOmniValue.GetAsDouble }

function TOmniValue.GetAsExtended: Extended;
begin
  case ovType of
    ovtInteger:  Result := AsInt64;
    ovtDouble:   Result := PDouble(RawData)^;
    ovtExtended: Result := (ovIntf as IOmniExtendedData).Value;
    else raise Exception.Create('TOmniValue cannot be converted to extended');
  end;
end; { TOmniValue.GetAsExtended }

function TOmniValue.GetAsInt64: int64;
begin
  if IsInteger then
    Result := ovData
  else if IsEmpty then
    Result := 0
  else
    raise Exception.Create('TOmniValue cannot be converted to int64');
end; { TOmniValue.GetAsInt64 }

function TOmniValue.GetAsInteger: integer;
begin
  Result := AsInt64;
end; { TOmniValue.GetAsInteger }

function TOmniValue.GetAsInterface: IInterface;
begin
  if IsInterface then
    Result := ovIntf
  else if IsEmpty then
    Result := nil
  else
    raise Exception.Create('TOmniValue cannot be converted to interface');
end; { TOmniValue.GetAsInterface }

function TOmniValue.GetAsObject: TObject;
begin
  if IsObject then
    Result := TObject(RawData^)
  else if IsEmpty then
    Result := nil
  else
    raise Exception.Create('TOmniValue cannot be converted to object');
end; { TOmniValue.GetAsObject }

function TOmniValue.GetAsPointer: pointer;
begin
  if IsPointer or IsObject then
    Result := pointer(RawData^)
  else if IsEmpty then
    Result := nil
  else
    raise Exception.Create('TOmniValue cannot be converted to pointer');
end; { TOmniValue.GetAsPointer }

function TOmniValue.GetAsString: string;
begin
  case ovType of
    ovtNull:       Result := '';
    ovtBoolean:    Result := BoolToStr(AsBoolean, true);
    ovtInteger:    Result := IntToStr(ovData);
    ovtDouble,
    ovtExtended:   Result := FloatToStr(AsExtended);
    ovtString:     Result := (ovIntf as IOmniStringData).Value;
    ovtWideString: Result := (ovIntf as IOmniWideStringData).Value;
    else raise Exception.Create('TOmniValue cannot be converted to string');
  end;
end; { TOmniValue.GetAsString }

function TOmniValue.GetAsVariant: Variant;
begin
  if IsVariant then
    Result := (ovIntf as IOmniVariantData).Value
  else if IsEmpty then
    Result := Variants.Null
  else
    raise Exception.Create('TOmniValue cannot be converted to variant');
end; { TOmniValue.GetAsVariant }

function TOmniValue.GetAsVariantArr(idx: integer): Variant;
begin
  Result := AsVariant[idx];
end; { TOmniValue.GetAsVariantArr }

function TOmniValue.GetAsWideString: WideString;
begin
  if ovType = ovtWideString then
    Result := (ovIntf as IOmniWideStringData).Value
  else
    Result := GetAsString;
end; { TOmniValue.GetAsWideString }

function TOmniValue.IsBoolean: boolean;
begin
  Result := (ovType = ovtBoolean);
end; { TOmniValue.IsBoolean }

function TOmniValue.IsEmpty: boolean;
begin
  Result := (ovType = ovtNull);
end; { TOmniValue.IsEmpty }

function TOmniValue.IsFloating: boolean;
begin
  Result := (ovType in [ovtDouble, ovtExtended]);
end; { TOmniValue.IsFloating }

function TOmniValue.IsInteger: boolean;
begin
  Result := (ovType = ovtInteger);
end; { TOmniValue.IsInteger }

function TOmniValue.IsInterface: boolean;
begin
  Result := (ovType = ovtInterface);
end; { TOmniValue.IsInterface }

function TOmniValue.IsObject: boolean;
begin
  Result := (ovType = ovtObject);
end; { TOmniValue.IsObject }

function TOmniValue.IsPointer: boolean;
begin
  Result := (ovType = ovtPointer);
end; { TOmniValue.IsPointer }

function TOmniValue.IsString: boolean;
begin
  Result := (ovType = ovtString);
end; { TOmniValue.IsString }

function TOmniValue.IsVariant: boolean;
begin
  Result := (ovType = ovtVariant);
end; { TOmniValue.IsVariant }

function TOmniValue.IsWideString: boolean;
begin
  Result := (ovType = ovtWideString);
end; { TOmniValue.IsWideString }

class function TOmniValue.Null: TOmniValue;
begin
  Result.ovType := ovtNull;
end; { TOmniValue.Null }

function TOmniValue.RawData: PInt64;
begin
  Result := @ovData;
end; { TOmniValue.RawData }

procedure TOmniValue.RawZero;
begin
  ovData := 0;
  pointer(ovIntf) := nil;
  ovType := ovtNull;
end; { TOmniValue.RawZero }

procedure TOmniValue.SetAsBoolean(const value: boolean);
begin
  PByte(RawData)^ := Ord(value);
  ovType := ovtBoolean;
end; { TOmniValue.SetAsBoolean }

procedure TOmniValue.SetAsCardinal(const value: cardinal);
begin
  AsInt64 := value;
end; { TOmniValue.SetAsCardinal }

procedure TOmniValue.SetAsDouble(value: Double);
begin
  PDouble(RawData)^ := value;
  ovType := ovtDouble;
end; { TOmniValue.SetAsDouble }

procedure TOmniValue.SetAsExtended(value: Extended);
begin
  ovIntf := TOmniExtendedData.Create(value);
  ovType := ovtExtended;
end; { TOmniValue.SetAsExtended }

procedure TOmniValue.SetAsInt64(const value: int64);
begin
  ovData := value;
  ovType := ovtInteger;
end; { TOmniValue.SetAsInt64 }

procedure TOmniValue.SetAsInteger(const value: integer);
begin
  AsInt64 := value;
end; { TOmniValue.SetAsInteger }

procedure TOmniValue.SetAsInterface(const value: IInterface);
begin
  ovIntf := value;
  ovType := ovtInterface;
end; { TOmniValue.SetAsInterface }

procedure TOmniValue.SetAsObject(const value: TObject);
begin
  RawData^ := int64(value);
  ovType := ovtObject;
end; { TOmniValue.SetAsObject }

procedure TOmniValue.SetAsPointer(const value: pointer);
begin
  RawData^ := int64(value);
  ovType := ovtPointer;
end; { TOmniValue.SetAsPointer }

procedure TOmniValue.SetAsString(const value: string);
begin
  ovIntf := TOmniStringData.Create(value);
  ovType := ovtString;
end; { TOmniValue.SetAsString }

procedure TOmniValue.SetAsVariant(const value: Variant);
begin
  ovIntf := TOmniVariantData.Create(value);
  ovType := ovtVariant;
end; { TOmniValue.SetAsVariant }

procedure TOmniValue.SetAsWideString(const value: WideString);
begin
  ovIntf := TOmniWideStringData.Create(value);
  ovType := ovtWideString;
end; { TOmniValue.SetAsWideString }

class operator TOmniValue.Equal(const a: TOmniValue; i: integer): boolean;
begin
  Result := (a.AsInteger = i);
end; { TOmniValue.Equal }

class operator TOmniValue.Equal(const a: TOmniValue; const s: string): boolean;
begin
  Result := (a.AsString = s);
end; { TOmniValue.Equal }

class operator TOmniValue.Implicit(const a: boolean): TOmniValue;
begin
  Result.AsBoolean := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: Double): TOmniValue;
begin
  Result.AsDouble := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: Extended): TOmniValue;
begin
  Result.AsExtended := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: integer): TOmniValue;
begin
  Result.AsInteger := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: int64): TOmniValue;
begin
  Result.AsInt64 := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: string): TOmniValue;
begin
  Result.AsString := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: IInterface): TOmniValue;
begin
  Result.AsInterface := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TObject): TOmniValue;
begin
  Result.AsObject := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): WideString;
begin
  Result := a.AsWideString;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): Extended;
begin
  Result := a.AsExtended;
end; { TOmniValue.Implicit }            

class operator TOmniValue.Implicit(const a: TOmniValue): int64;
begin
  Result := a.AsInt64;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): boolean;
begin
  Result := a.AsBoolean;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): Double;
begin
  Result := a.AsDouble;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): integer;
begin
  Result := a.AsInteger;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): IInterface;
begin
  Result := a.AsInterface;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): TObject;
begin
  Result := a.AsObject;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): string;
begin
  Result := a.AsString;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: WideString): TOmniValue;
begin
  Result.AsWideString := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: Variant): TOmniValue;
begin
  Result.AsVariant := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: pointer): TOmniValue;
begin
  Result.AsPointer := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): pointer;
begin
  Result := a.AsPointer;
end; { TOmniValue.Implicit }

{ TOmniWaitableValue }

constructor TOmniWaitableValue.Create;
begin
  Handle := CreateEvent(nil, false, false, nil);
  Value := TOmniValue.Null;
end; { TOmniWaitableValue.Create }

destructor TOmniWaitableValue.Destroy;
begin
  DSiCloseHandleAndInvalidate(Handle);
end; { TOmniWaitableValue.Destroy }

procedure TOmniWaitableValue.Reset;
begin
  WaitForSingleObject(Handle, 0);
end; { TOmniWaitableValue.Reset }

procedure TOmniWaitableValue.Signal;
begin
  SetEvent(Handle);
end; { TOmniWaitableValue.Signal }

procedure TOmniWaitableValue.Signal(const data: TOmniValue);
begin
  Value := data;
  Signal;
end; { TOmniWaitableValue.Signal }

function TOmniWaitableValue.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := (WaitForSingleObject(Handle, maxWait_ms) = WAIT_OBJECT_0);
end; { TOmniWaitableValue.WaitFor }

{ TOmniStringData }

constructor TOmniStringData.Create(const value: string);
begin
  inherited Create;
  osdValue := value;
end; { TOmniStringData.Create }

function TOmniStringData.GetValue: string;
begin
  Result := osdValue;
end; { TOmniStringData.GetValue }

procedure TOmniStringData.SetValue(const value: string);
begin
  osdValue := value;
end; { TOmniStringData.SetValue }

{ TOmniWideStringData }

constructor TOmniWideStringData.Create(const value: WideString);
begin
  inherited Create;
  osdValue := value;
end; { TOmniWideStringData.Create }

function TOmniWideStringData.GetValue: WideString;
begin
  Result := osdValue;
end; { TOmniWideStringData.GetValue }

procedure TOmniWideStringData.SetValue(const value: WideString);
begin
  osdValue := value;
end; { TOmniWideStringData.SetValue }

{ TOmniVariantData }

constructor TOmniVariantData.Create(const value: Variant);
begin
  inherited Create;
  ovdValue := value;
end; { TOmniVariantData.Create }

function TOmniVariantData.GetValue: Variant;
begin
  Result := ovdValue;
end; { TOmniVariantData.GetValue }

procedure TOmniVariantData.SetValue(const value: Variant);
begin
  ovdValue := value;
end; { TOmniVariantData.SetValue }

{ TOmniExtendedData }

constructor TOmniExtendedData.Create(const value: Extended);
begin
  inherited Create;
  oedValue := value;
end; { TOmniExtendedData.Create }

function TOmniExtendedData.GetValue: Extended;
begin
  Result := oedValue;
end; { TOmniExtendedData.GetValue }

procedure TOmniExtendedData.SetValue(const value: Extended);
begin
  oedValue := value;
end; { TOmniExtendedData.SetValue }

{ TOmniAffinity }

constructor TOmniAffinity.Create(target: TOmniAffinityTarget);
begin
  Assert(target in [atSystem, atProcess, atThread]);
  inherited Create;
  oaTarget := target;
end; { TOmniAffinity.Create }

function TOmniAffinity.GetAsString: string;
begin
  Result := DSiAffinityMaskToString(Mask);
end; { TOmniAffinity.GetAsString }

function TOmniAffinity.GetCount: integer;
var
  affMask: DWORD;
begin
  Result := 0;
  affMask := Mask;
  while affMask <> 0 do begin
    if Odd(affMask) then
      Inc(Result);
    affMask := affMask SHR 1;
  end;
end; { TOmniAffinity.GetCount }

function TOmniAffinity.GetMask: DWORD;
begin
  case oaTarget of
    atSystem:
      Result := DSiGetSystemAffinityMask;
    atProcess:
      Result := DSiGetProcessAffinityMask;
    atThread:
      Result := DSiGetThreadAffinityMask;
    else
      Result := 0; // to keep compiler happy
  end;
end; { TOmniAffinity.GetMask }

procedure TOmniAffinity.SetAsString(const value: string);
begin
  case oaTarget of
    atSystem:
      raise Exception.Create('TOmniAffinity.SetMask: Cannot modify system affinity mask.');
    atProcess:
      DSiSetProcessAffinity(value);
    atThread:
      DSiSetThreadAffinity(value);
  end;
end; { TOmniAffinity.SetAsString }

procedure TOmniAffinity.SetCount(const value: integer);
var
  affMask: string;
  numCore: integer;
  pCore  : integer;
  sysMask: string;
begin
  sysMask := DSiGetSystemAffinity;
  affMask := '';
  numCore := value;
  while (numCore > 0) and (sysMask <> '') do begin
    pCore := Random(Length(sysMask)) + 1;
    affMask := affMask + sysMask[pCore];
    Delete(sysMask, pCore, 1);
    Dec(numCore);
  end;
  AsString := affMask;
end; { TOmniAffinity.SetCount }

procedure TOmniAffinity.SetMask(const value: DWORD);
begin
  AsString := DSiAffinityMaskToString(value);
end; { TOmniAffinity.SetMask }

{ TOmniProcessEnvironment }

constructor TOmniProcessEnvironment.Create;
begin
  inherited Create;
  opeAffinity := TOmniAffinity.Create(atProcess);
end; { TOmniProcessEnvironment.Create }

function TOmniProcessEnvironment.GetAffinity: IOmniAffinity;
begin
  Result := opeAffinity;
end; { TOmniProcessEnvironment.GetAffinity }

function TOmniProcessEnvironment.GetMemory: TOmniProcessMemoryCounters;
begin
  if not DSiGetProcessMemory(Result) then
    FillChar(Result, SizeOf(Result), 0);
end; { TOmniProcessEnvironment.GetMemory }

function TOmniProcessEnvironment.GetPriorityClass: TOmniProcessPriorityClass;
var
  priority: DWORD;
begin
  priority := Windows.GetPriorityClass(GetCurrentProcess);
  if priority = $8000 then
    Result := pcAboveNormal
  else if priority = $4000 then
    Result := pcBelowNormal
  else if priority = $80 then
    Result := pcHigh
  else if priority = $40 then
    Result := pcIdle
  else if priority = $100 then
    Result := pcRealtime
  else
    Result := pcNormal;
end; { TOmniProcessEnvironment.GetPriorityClass }

function TOmniProcessEnvironment.GetTimes: TOmniProcessTimes;
begin
  if not DSiGetProcessTimes(Result.CreationTime, Result.UserTime, Result.KernelTime) then
    FillChar(Result, SizeOf(Result), 0);
end; { TOmniProcessEnvironment.GetTimes }

{ TOmniSystemEnvironment }

constructor TOmniSystemEnvironment.Create;
begin
  oseAffinity := TOmniAffinity.Create(atSystem);
end; { TOmniSystemEnvironment.Create }

function TOmniSystemEnvironment.GetAffinity: IOmniAffinity;
begin
  Result := oseAffinity;
end; { TOmniSystemEnvironment.GetAffinity }

{ TOmniThreadEnvironment }

constructor TOmniThreadEnvironment.Create;
begin
  oteAffinity := TOmniAffinity.Create(atThread);
  oteThreadID := GetCurrentThreadID;
end; { TOmniThreadEnvironment.Create }

function TOmniThreadEnvironment.GetAffinity: IOmniAffinity;
begin
  Result := oteAffinity;
end; { TOmniThreadEnvironment.GetAffinity }

function TOmniThreadEnvironment.GetID: cardinal;
begin
  Result := oteThreadID;
end; { TOmniThreadEnvironment.GetID }

{ TOmniEnvironment }

constructor TOmniEnvironment.Create;
begin
  inherited Create;
  oeProcessEnv := TOmniProcessEnvironment.Create;
  oeSystemEnv := TOmniSystemEnvironment.Create;
end; { TOmniEnvironment.Create }

function TOmniEnvironment.GetProcess: IOmniProcessEnvironment;
begin
  Result := oeProcessEnv;
end; { TOmniEnvironment.GetProcess }

function TOmniEnvironment.GetSystem: IOmniSystemEnvironment;
begin
  Result := oeSystemEnv;
end; { TOmniEnvironment.GetSystem }

function TOmniEnvironment.GetThread: IOmniThreadEnvironment;
begin
  if (not assigned(oeThreadEnv)) or (oeThreadEnv.ID <> GetCurrentThreadID) then
    oeThreadEnv := TOmniThreadEnvironment.Create;
  Result := oeThreadEnv;
end; { TOmniEnvironment.GetThread }

initialization
  Assert(SizeOf(TObject) = SizeOf(cardinal)); //in VarToObj
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(SizeOf(pointer) = 4);
  GEnvironment := TOmniEnvironment.Create;
end.
