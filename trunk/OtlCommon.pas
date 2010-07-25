//<summary>Stuff common to the OmniThreadLibrary project.</summary>
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
///   Last modification : 2010-07-08
///   Version           : 1.17a
///</para><para>
///   History:
///     1.17a: 2010-07-08
///       - TOmniValue.CastAs<T> and .CastFrom<T> are partially supported in D2009.
///     1.17: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.16: 2010-05-12
///       - TOmniValue can be cast as Int64.
///       - Implemented TOmniValue.CastFrom<T> and .CastAs<T>.
///     1.15: 2010-05-08
///       - Implemented conversions from/to TOmniValue to/from TValue (Delphi 2010 and newer).
///     1.14: 2010-05-06
///       - Implemented TOmniValue._AddRef, _Release, _ReleaseAndClear.
///     1.13: 2010-04-14
///       - Removed TOmniEnumerableRange and associated code.
///     1.12: 2010-03-16
///       - Implemented TOmniMessageID record, used internally to implement timers.
///     1.11: 2010-03-10
///       - Implemented TOmniCounter, auto-initialized wrapper around the IOmniCounter.
///     1.10b: 2010-03-03
///       - Replacement AnonCopy, by Serg.
///     1.10a: 2010-02-22
///       - D2009-compatible way of setting a delegate in TOmniExecutable.
///     1.10: 2010-02-09
///       - Implemented TOmniExecutor - a record that can store TProcedure, TMethod, or
///         TProc.
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

// TODO 1 -oPrimoz Gabrijelcic : Can CastAs<> and CastFrom<> work in Delphi 2009?

unit OtlCommon;

{$I OTLOptions.inc}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Variants,
  TypInfo,
  {$IFDEF OTL_ERTTI}
  RTTI,
  {$ENDIF OTL_ERTTI}
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
  //:TOmniValue conversion exception.
  EOmniValueConv = class(Exception);

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
    constructor Create(const values: array of const);
    procedure _AddRef; inline;
    procedure _Release; inline;
    procedure _ReleaseAndClear; inline;
    function  CastAsInt64: int64; inline;
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
  {$IFDEF OTL_Generics}
  public
    class function CastFrom<T>(const value: T): TOmniValue; static;
    function CastAs<T>: T;
  {$ENDIF OTL_Generics}
  {$IFDEF OTL_ERTTI}
  private
    function  GetAsTValue: TValue;
    procedure SetAsTValue(const value: TValue);
  public
    class operator Implicit(const a: TValue): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): TValue; inline;
    property AsTValue: TValue read GetAsTValue write SetAsTValue;
  {$ENDIF OTL_ERTTI}
  end; { TOmniValue }

  ///<summary>Slightly different from the IEnumerable:
  ///    - Returns TOmniValue.
  ///    - Must ensure correct operation of multiple simultaneous enumerators.
  ///    - TryTake must be implemented to support mutable collections (as TOmniBlockingCollection).
  ///      For non-mutable collections TryTake can simply return false if the collection
  ///      is empty.
  ///    - TryTake must be threadsafe - when used in Parallel.For, data manager will call
  ///      it simultaneously from multiple threads at the same time.
  ///</summary>
  IOmniValueEnumerator = interface ['{F60EBBD8-2F87-4ACD-A014-452F296F4699}']
    function  GetCurrent: TOmniValue;
    function  MoveNext: boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: cardinal): boolean;
    property Current: TOmniValue read GetCurrent;
  end; { IOmniValueEnumerator }

  IOmniValueEnumerable = interface ['{50C1C176-C61F-41F5-AA0B-6FD215E5159F}']
    function  GetEnumerator: IOmniValueEnumerator;
  end; { IOmniValueEnumerable }

  ///<summary>Abstract enumerator class, used as a base for internal classes passed to the
  ///    OtlDataManager.</summary>
  ///<since>2010-05-07</since>
  TOmniValueEnumerator = class abstract
    function  GetCurrent: TOmniValue; virtual; abstract;
    function  MoveNext: boolean; virtual; abstract;
    property Current: TOmniValue read GetCurrent;
  end; { TOmniValueEnumerator }

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
    ovcCount    : integer;
    ovcNames    : array of string;
    ovcValues   : array of TOmniValue;
  strict protected
    procedure Clear;
    function  GetItem(paramIdx: integer): TOmniValue; overload;
    function  GetItem(const paramName: string): TOmniValue; overload;
    function GetItem(const param: TOmniValue): TOmniValue; overload;
    procedure Grow(requiredIdx: integer = -1);
  public
    constructor Create;
    procedure Add(const paramValue: TOmniValue; paramName: string = '');
    procedure Assign(const parameters: array of TOmniValue);
    function  ByName(const paramName: string): TOmniValue; overload;
    function  ByName(const paramName: string; const defValue: TOmniValue): TOmniValue; overload;
    function  Count: integer;
    function  Exists(const paramName: string): boolean;
    function  IndexOf(const paramName: string): integer;
    procedure Insert(paramIdx: integer; const value: TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    property Item[paramIdx: integer]: TOmniValue read GetItem; default;
    property Item[const paramName: string]: TOmniValue read GetItem; default;
    property Item[const param: TOmniValue]: TOmniValue read GetItem; default;
  end; { TOmniValueContainer }

  IOmniCounter = interface ['{3A73CCF3-EDC5-484F-8459-532B8C715E3C}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  //
    function  Increment: integer;
    function  Decrement: integer;
    property Value: integer read GetValue write SetValue;
  end; { IOmniCounter }

  TOmniCounter = record
  strict private
    ocCounter: IOmniCounter;
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  public
    procedure Initialize;
    function  Increment: integer;
    function  Decrement: integer;
    property Value: integer read GetValue write SetValue;
  end; { TOmniCounter }

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

  TOmniExecutableKind = (oekNull, oekProcedure, oekMethod {$IFDEF OTL_Anonymous},
    oekDelegate{$ENDIF});

  TOmniExecutable = record
  strict private
    {$IFDEF OTL_Anonymous}
    oeDelegate: TProc;
    function  GetDelegate: TProc; inline;
    {$ENDIF OTL_Anonymous}
  strict private
    oeMethod   : TMethod;
    oeProcedure: TProcedure;
    oeKind     : TOmniExecutableKind;
    procedure CheckKind(kind: TOmniExecutableKind); inline;
    function  GetMethod: TMethod; inline;
    function  GetProc: TProcedure; inline;
    procedure SetMethod(const value: TMethod); inline;
    procedure SetProc(const value: TProcedure); inline;
  public
    class operator Explicit(const a: TOmniExecutable): TMethod; inline;
    class operator Explicit(const a: TOmniExecutable): TProcedure; inline;
    class operator Explicit(const a: TMethod): TOmniExecutable; inline;
    class operator Explicit(const a: TProcedure): TOmniExecutable; inline;
    class operator Implicit(const a: TMethod): TOmniExecutable; inline;
    class operator Implicit(const a: TProcedure): TOmniExecutable; inline;
    class operator Implicit(const a: TOmniExecutable): TMethod; inline;
    class operator Implicit(const a: TOmniExecutable): TProcedure; inline;
    procedure Clear; inline;
    function  IsNull: boolean; inline;
    property Kind: TOmniExecutableKind read oeKind;
    property Method: TMethod read GetMethod write SetMethod;
    property Proc: TProcedure read GetProc write SetProc;
  public
    {$IFDEF OTL_Anonymous}
    class procedure AnonCopy(var Dest; const Source); static;
    class operator Explicit(const a: TOmniExecutable): TProc; inline;
    class operator Explicit(const a: TProc): TOmniExecutable; inline;
    class operator Implicit(const a: TOmniExecutable): TProc; inline;
    class operator Implicit(const a: TProc): TOmniExecutable; inline;
    procedure SetDelegate(const source);
    property Delegate: TProc read GetDelegate;
    {$ENDIF OTL_Anonymous}
  end; { TOmniExecutable }

  TOmniMessageIDType = (mitInteger, mitString, mitPointer);

  ///<summary>Describes 'smart' IOmniTaskControl message (either message ID, method name,
  ///    or method pointer.</summary>
  ///<since>2010-03-16</since>
  TOmniMessageID = record
  strict private
    omidInteger    : integer;
    omidMessageType: TOmniMessageIDType;
    omidPointer    : pointer;
    omidString     : string;
  public
    class operator Implicit(const a: integer): TOmniMessageID; inline;
    class operator Implicit(const a: pointer): TOmniMessageID; inline;
    class operator Implicit(const a: string): TOmniMessageID; inline;
    class operator Implicit(const a: TOmniMessageID): integer; inline;
    class operator Implicit(const a: TOmniMessageID): string; inline;
    class operator Implicit(const a: TOmniMessageID): pointer; inline;
    property MessageType: TOmniMessageIDType read omidMessageType;
  end; { TOmniMessageID }

  function  CreateCounter(initialValue: integer = 0): IOmniCounter;
  function  CreateInterfaceDictionary: IOmniInterfaceDictionary;
  function  Environment: IOmniEnvironment;
  procedure SetThreadName(const name: string);
  function  VarToObj(const v: Variant): TObject; inline;

var
  OtlUID: TGp8AlignedInt64;

  {$IFDEF OTL_Generics} // must not be local due to compiler restrictions
  TOmniValue_DataSize: array [TTypeKind] of integer;
  {$ENDIF OTL_Generics}

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

  TOmniCounterImpl = class(TInterfacedObject, IOmniCounter)
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
  end; { TOmniCounterImpl }

  PGp4AlignedInt = ^TGp4AlignedInt;

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
  Result := TOmniCounterImpl.Create(initialValue);
end; { CreateCounter }

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
//      RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
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
  newParam := (paramName = '') or (Asgn(idxParam, IndexOf(paramName)) < 0);
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

function TOmniValueContainer.ByName(const paramName: string): TOmniValue;
begin
  Result := ByName(paramName, TOmniValue.Null);
end; { TOmniValueContainer.ByName }

function TOmniValueContainer.ByName(const paramName: string; const defValue: TOmniValue):
  TOmniValue;
var
  idxParam: integer;
begin
  idxParam := IndexOf(paramName);
  if idxParam >= 0 then
    Result := GetItem(idxParam)
  else
    Result := defValue;
end; { TOmniValueContainer.ByName }

procedure TOmniValueContainer.Clear;
begin
  SetLength(ovcNames, 0);
  SetLength(ovcValues, 0);
  ovcCount := 0;
end; { TOmniValueContainer.Clear }

function TOmniValueContainer.Count: integer;
begin
  Result := ovcCount;
end; { TOmniValueContainer.Count }

function TOmniValueContainer.Exists(const paramName: string): boolean;
begin
  Result := (IndexOf(paramName) >= 0);
end; { TOmniValueContainer.Exists }

function TOmniValueContainer.GetItem(paramIdx: integer): TOmniValue;
begin
  Result := ovcValues[paramIdx];
end; { TOmniValueContainer.GetItem }

function TOmniValueContainer.GetItem(const paramName: string): TOmniValue;
var
  idxParam: integer;
begin
  idxParam := IndexOf(paramName);
  if idxParam >= 0 then
    Result := GetItem(idxParam)
  else
    raise Exception.CreateFmt('TOmniValueContainer.GetItem: Parameter %s does not exist', [paramName]);
end; { TOmniValueContainer.GetItem }

function TOmniValueContainer.GetItem(const param: TOmniValue): TOmniValue;
begin
  if param.IsInteger then
    Result := GetItem(param.AsInteger)
  else if param.IsString then
    Result := GetItem(param.AsString)
  else
    raise Exception.Create('TOmniValueContainer.GetItem: Container can only be indexed by integer or string.');  
end; { TOmniValueContainer.GetItem }

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

function TOmniValueContainer.IndexOf(const paramName: string): integer;
begin
  for Result := 0 to ovcCount - 1 do
    if SameText(paramName, ovcNames[Result]) then
      Exit;
  Result := -1;
end; { TOmniValueContainer.IndexOf }

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

{ TOmniCounter }

function TOmniCounter.Decrement: integer;
begin
  Initialize;
  Result := ocCounter.Decrement;
end; { TOmniCounter.Decrement }

function TOmniCounter.GetValue: integer;
begin
  Initialize;
  Result := ocCounter.GetValue;
end;

function TOmniCounter.Increment: integer;
begin
  Initialize;
  Result := ocCounter.Increment;
end; { TOmniCounter.Increment }

procedure TOmniCounter.Initialize;
var
  countIntf: IOmniCounter;
begin
  Assert(cardinal(@ocCounter) mod 4 = 0, 'TOmniCS.Initialize: ocsSync is not 4-aligned!');
  if not assigned(ocCounter) then begin
    countIntf := CreateCounter;
    if InterlockedCompareExchange(PInteger(@ocCounter)^, integer(countIntf), 0) = 0 then
      pointer(countIntf) := nil;
  end;
end; { TOmniCounter.Initialize }

procedure TOmniCounter.SetValue(const value: integer);
begin
  Initialize;
  ocCounter.SetValue(value);
end; { TOmniCounter.SetValue }

{ TOmniCounterImpl }

constructor TOmniCounterImpl.Create(initialValue: integer);
begin
  Value := initialValue;
end; { TOmniCounterImpl.Create }

function TOmniCounterImpl.Decrement: integer;
begin
  Result := ocValue.Decrement;
end; { TOmniCounterImpl.Decrement }

function TOmniCounterImpl.GetValue: integer;
begin
  Result := ocValue;
end; { TOmniCounterImpl.GetValue }

function TOmniCounterImpl.Increment: integer;
begin
  Result := ocValue.Increment;
end; { TOmniCounterImpl.Increment }

procedure TOmniCounterImpl.SetValue(const value: integer);
begin
  ocValue.Value := value;
end; { TOmniCounterImpl.SetValue }

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

constructor TOmniValue.Create(const values: array of const);
begin
  AsVariant := OpenArrayToVarArray(values);
end; { TOmniValue.Create }

function TOmniValue.CastAsInt64: int64;
begin
  case ovType of
    ovtInterface, ovtExtended, ovtString, ovtWideString, ovtDouble:
      raise Exception.Create('TOmniValue cannot be cast to Int64');
    ovtVariant:
      Result := AsVariant;
    else
      Result := ovData;
  end;
end; { TOmniValue.CastAsInt64 }

{$IFDEF OTL_Generics}
function TOmniValue.CastAs<T>: T;
var
  ds      : integer;
  maxValue: uint64;
  ti      : PTypeInfo;
begin
  ds := 0;
  ti := System.TypeInfo(T);
  if assigned(ti) then
    ds := TOmniValue_DataSize[ti^.Kind];
  if ds = 0 then // complicated stuff
    {$IFDEF OTL_ERTTI}
    Result := AsTValue.AsType<T>
    {$ELSE}
    raise Exception.Create('Only casting to simple types is supported in Delphi 2009')
    {$ENDIF OTL_ERTTI}
  else begin // simple types
    if ds < 8 then begin
      maxValue := uint64($FF) SHL ((ds-1) * 8);
      if ovData > maxValue then
        raise EOmniValueConv.CreateFmt('Value %d is too big to fit into %s', [ovData, ti^.Name]);
    end;
    Move(ovData, Result, ds);
  end;
end; { TOmniValue.CastAs }

class function TOmniValue.CastFrom<T>(const value: T): TOmniValue;
var
  data: int64;
  ds  : integer;
  ti  : PTypeInfo;
begin
  ds := 0;
  ti := System.TypeInfo(T);
  if assigned(ti) then
    ds := TOmniValue_DataSize[ti^.Kind];
  if ds = 0 then // complicated stuff
    {$IFDEF OTL_ERTTI}
    Result.AsTValue := TValue.From<T>(value)
    {$ELSE}
    raise Exception.Create('Only casting from simple types is supported in Delphi 2009')
    {$ENDIF OTL_ERTTI}
  else begin // simple types
    data := 0;
    Move(value, data, ds);
    case ti^.Kind of
      tkInteger:
        Result.AsInteger := data;
      tkClass:
        Result.AsObject := TObject(data);
      tkMethod:
        Result.AsInt64 := data;
      tkInt64:
        Result.AsInt64 := data;
      {$IFDEF OTL_HasTkPointer}
      tkPointer:
        Result.AsPointer := pointer(data);
      {$ENDIF OTL_HasTkPointer}
      else
        raise Exception.CreateFmt('TOmniValue: CastFrom<%s> is broken!', [ti^.Name]);
    end;
  end;
end; { TOmniValue.CastFrom }
{$ENDIF OTL_Generics}

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

{$IFDEF OTL_ERTTI}
function TOmniValue.GetAsTValue: TValue;
begin
  case ovType of
    ovtNull:
      Result := nil;
    ovtBoolean:
      Result := AsBoolean;
    ovtInteger:
      Result := AsInteger;
    ovtDouble,
    ovtExtended:
      Result := AsExtended;
    ovtString:
      Result := AsString;
    ovtObject:
      Result := AsObject;
    ovtInterface:
      Result := TValue.From<IInterface>(AsInterface);
    ovtVariant:
      Result := TValue.FromVariant(AsVariant);
    ovtWideString:
      Result := AsWideString;
    ovtPointer:
      Result := AsPointer;
  end;
end; { TOmniValue.GetAsTValue }
{$ENDIF OTL_ERRTI}

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

{$IFDEF OTL_ERTTI}
procedure TOmniValue.SetAsTValue(const value: TValue);
begin
  case value.Kind of
    tkInteger:
      AsInteger := value.AsInteger;
    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString:
      AsString := value.AsString;
    tkFloat:
      AsExtended := value.AsExtended;
    tkVariant:
      AsVariant := value.AsVariant;
    tkInterface:
      AsInterface := value.AsInterface;
    tkInt64:
      AsInt64 := value.AsInt64;
    tkPointer:
    begin
      Assert(SizeOf(pointer) = SizeOf(integer));
      AsPointer := pointer(value.GetReferenceToRawData^);
    end;
    else
      raise Exception.CreateFmt('TValue of type %d cannot be converted to TOmniValue',
        [GetEnumName(TypeInfo(TTypeKind), Ord(value.Kind))]);
  end;
end; { TOmniValue.SetAsTValue }
{$ENDIF OTL_ERTTI}

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

procedure TOmniValue._AddRef;
begin
  if ovType in [ovtInterface, ovtExtended, ovtString, ovtVariant, ovtWideString] then
    ovIntf._AddRef;
end; { TOmniValue._AddRef }

procedure TOmniValue._Release;
begin
  if ovType in [ovtInterface, ovtExtended, ovtString, ovtVariant, ovtWideString] then
    ovIntf._Release;
end; { TOmniValue._Release }

procedure TOmniValue._ReleaseAndClear;
begin
  if ovType in [ovtInterface, ovtExtended, ovtString, ovtVariant, ovtWideString] then begin
    ovIntf._Release;
    RawZero;
  end;
end; { TOmniValue._ReleaseAndClear }

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

{$IFDEF OTL_ERTTI}
class operator TOmniValue.Implicit(const a: TValue): TOmniValue;
begin
  Result.AsTValue := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): TValue;
begin
  Result := a.AsTValue;
end; { TOmniValue.Implicit }
{$ENDIF OTL_ERTTI}

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

{ TOmniExecutable }

procedure TOmniExecutable.CheckKind(kind: TOmniExecutableKind);
begin
  if oeKind <> kind then
    raise Exception.CreateFmt('TOmniExecutable: Wrong kind of executable %s, expected %s',
      [GetEnumName(TypeInfo(TOmniExecutableKind), Ord(oeKind)),
       GetEnumName(TypeInfo(TOmniExecutableKind), Ord(kind))]);
end; { TOmniExecutable.CheckKind }

procedure TOmniExecutable.Clear;
begin
  oeKind := oekNull;
end; { TOmniExecutable.IsNull }

class operator TOmniExecutable.Explicit(const a: TProcedure): TOmniExecutable;
begin
  Result.Proc := a;
end; { TOmniExecutable.Explicit }

class operator TOmniExecutable.Explicit(const a: TMethod): TOmniExecutable;
begin
  Result.Method := a;
end; { TOmniExecutable.Explicit }

class operator TOmniExecutable.Explicit(const a: TOmniExecutable): TMethod;
begin
  Result := a.Method;
end; { TOmniExecutable.Explicit }

class operator TOmniExecutable.Explicit(const a: TOmniExecutable): TProcedure;
begin
  Result := a.Proc;
end; { TOmniExecutable.Explicit }

function TOmniExecutable.IsNull: boolean;
begin
  Result := (oeKind = oekNull);
end; { TOmniExecutable.IsNull }

class operator TOmniExecutable.Implicit(const a: TProcedure): TOmniExecutable;
begin
  Result.Proc := a;
end; { TOmniExecutable.Implicit }

class operator TOmniExecutable.Implicit(const a: TMethod): TOmniExecutable;
begin
  Result.Method := a;
end; { TOmniExecutable.Implicit }

class operator TOmniExecutable.Implicit(const a: TOmniExecutable): TProcedure;
begin
  Result := a.Proc;
end; { TOmniExecutable.Implicit }

class operator TOmniExecutable.Implicit(const a: TOmniExecutable): TMethod;
begin
  Result := a.Method;
end; { TOmniExecutable.Implicit }

function TOmniExecutable.GetMethod: TMethod;
begin
  CheckKind(oekMethod);
  Result := oeMethod;
end; { TOmniExecutable.Method }

function TOmniExecutable.GetProc: TProcedure;
begin
  CheckKind(oekProcedure);
  Result := oeProcedure;
end; { TOmniExecutable.Proc }

procedure TOmniExecutable.SetMethod(const value: TMethod);
begin
  oeKind := oekMethod;
  oeMethod := value;
end; { TOmniExecutable.SetMethod }

procedure TOmniExecutable.SetProc(const value: TProcedure);
begin
  oeKind := oekProcedure;
  oeProcedure := value;
end; { TOmniExecutable.SetProc }

{$IFDEF OTL_Anonymous}
class procedure TOmniExecutable.AnonCopy(var Dest; const Source);
var
  P: Pointer;
begin
  P:= Pointer(Dest);
  if Pointer(Source) <> nil then
    IInterface(Source)._AddRef;
  Pointer(Dest):= Pointer(Source);
  if P <> nil then
    IInterface(P)._Release;
end; { TOmniExecutable.AnonCopy }

class operator TOmniExecutable.Explicit(const a: TOmniExecutable): TProc;
begin
  Result := a.Delegate;
end; { TOmniExecutable.Explicit }

class operator TOmniExecutable.Explicit(const a: TProc): TOmniExecutable;
begin
  Result.SetDelegate(a);
end; { TOmniExecutable.Explicit }

class operator TOmniExecutable.Implicit(const a: TProc): TOmniExecutable;
begin
  Result.SetDelegate(a);
end; { TOmniExecutable.Implicit }

class operator TOmniExecutable.Implicit(const a: TOmniExecutable): TProc;
begin
  Result := a.Delegate;
end; { TOmniExecutable.Implicit }

function TOmniExecutable.GetDelegate: TProc;
begin
  CheckKind(oekDelegate);
  Result := oeDelegate;
end; { TOmniExecutable.GetDelegate }

procedure TOmniExecutable.SetDelegate(const source);
begin
  oeKind := oekDelegate;
  AnonCopy(oeDelegate, source);
end; { TOmniExecutable.SetDelegate }

{$ENDIF OTL_Anonymous}

{ TOmniMessageID }

class operator TOmniMessageID.Implicit(const a: integer): TOmniMessageID;
begin
  Result.omidMessageType := mitInteger;
  Result.omidInteger := a;
end; { TOmniMessageID.Implicit }

class operator TOmniMessageID.Implicit(const a: pointer): TOmniMessageID;
begin
  Result.omidMessageType := mitPointer;
  Result.omidPointer := a;
end; { TOmniMessageID.Implicit }

class operator TOmniMessageID.Implicit(const a: string): TOmniMessageID;
begin
  Result.omidMessageType := mitString;
  Result.omidString := a;
  UniqueString(Result.omidString);
end; { TOmniMessageID.Implicit }

class operator TOmniMessageID.Implicit(const a: TOmniMessageID): integer;
begin
  Assert(a.omidMessageType = mitInteger);
  Result := a.omidInteger;
end; { TOmniMessageID.Implicit }

class operator TOmniMessageID.Implicit(const a: TOmniMessageID): string;
begin
  Assert(a.omidMessageType = mitString);
  Result := a.omidString;
end; { TOmniMessageID.Implicit }

class operator TOmniMessageID.Implicit(const a: TOmniMessageID): pointer;
begin
  Assert(a.omidMessageType = mitPointer);
  Result := a.omidPointer;
end; { TOmniMessageID.Implicit }         

initialization
  Assert(SizeOf(TObject) = SizeOf(cardinal)); //in VarToObj
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(SizeOf(pointer) = 4);
  GEnvironment := TOmniEnvironment.Create;
  {$IFDEF OTL_Generics}
  FillChar(TOmniValue_DataSize, SizeOf(TOmniValue_DataSize), 0);
  TOmniValue_DataSize[tkInteger] := SizeOf(integer);
  TOmniValue_DataSize[tkClass]   := SizeOf(TObject);
  TOmniValue_DataSize[tkMethod]  := SizeOf(TMethod);
  TOmniValue_DataSize[tkInt64]   := SizeOf(int64);
  {$ENDIF OTL_Generics}
  {$IFDEF OTL_HasTkPointer}
  TOmniValue_DataSize[tkPointer] := SizeOf(pointer);
  {$ENDIF OTL_HasTkPointer}
end.
