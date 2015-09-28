///<summary>Stuff common to the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2015, Primoz Gabrijelcic
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
///   Contributors      : GJ, Lee_Nover, scarre, Sean B. Durkin
///
///   Creation date     : 2008-06-12
///   Last modification : 2015-09-28
///   Version           : 1.38
///</para><para>
///   History:
///     1.38: 2015-09-28
///       - [Sean] Introduced non-Windows compatibility.
///     1.37b: 2015-08-30
///       - Fixed record type handling in FromArray<T> and ToArray<T>.
///     1.37a: 2015-04-17
///       - Added vtWideChar and vtPWideChar handling to TOmniValue.Create and .CreateNamed.
///     1.37: 2015-02-09
///       - Added writer for TOmniExecutable.Delegate.
///     1.36: 2015-02-03
///       - Type of TOmniValue data is now an external type - TOmniValueDataType. That
///         way causes less internal errors in the compiler.
///       - Added TOmniValue.DataType property.
///     1.35: 2014-09-23
///       - Implemented TOmniValueContainer.AssignNamed.
///     1.34: 2014-01-13
///       - Implemented TOmniValue.HasArrayItem.
///     1.33: 2014-01-10
///       - TOmniValue can 'own' a TObject (object gets destroyed when a TOmniValue goes
///         out of scope). Supporting properties: IsOwnedObject, AsOwnedObject,
///         OwnsObject.
///     1.32b: 2013-10-17
///       - Fixed exception format string in TOmniValue.SetAsTValue.
///     1.32a: 2013-10-14
///       - Removed XE5 compilation warnings.
///     1.32: 2013-10-13
///       - ToObject<T> is working in D2010 again (thx to [Tomasso Ercole]).
///     1.31a: 2013-10-07
///       - Compiles with D2009 and D2010 again.
///     1.31: 2013-05-06
///       - AnsiString type is supported in TOmniValue.
///       - Implemented FromArray<T> and ToArray<T>.
///       - CastAs<T> renamed to CastTo<T>.
///       - AsRecord<T> renamed to ToRecord<T>.
///       - CastFrom<T> and CastTo<T> work for byte- and word-sized integer types.
///     1.30: 2013-04-29
///       - GetAsXXX family renamed to CastToXXX.
///       - TryGetAsXXX familiy renamed to TryCastToXXX.
///     1.29: 2013-04-26
///       - Added TOmniValue.TryGetAsXXX family of functions.
///       - Added TOmniValue.GetAsXXXDef family of functions.
///     1.28a: 2013-02-27
///       - Fixed TOmniValue._AddRef and _Release when 'nil' interface was stored in
///         the TOmniValue.
///     1.28: 2012-07-30
///       - Implemented TOmniValue.IsRecord.
///     1.27: 2012-05-18
///       - Added property CountPhysical to the IOmniAffinity.
///     1.26c: 2012-02-28
///       - Fixed object and interface casting in TOmniValue.CreateNamed.
///     1.26b: 2012-02-25
///       - TOmniValue.CreateNamed was casting pointer to integer.
///     1.26a: 2012-02-03
///       - TOmniValueContainer.Insert did not update internal 'count' field.
///         Big thanks to [andi] for bug report and fix.
///     1.26: 2012-01-19
///       - Added TOmniValueObj, class wrapper for TOmniValue.
///     1.25g: 2011-12-20
///       - TOmniValue.AsInteger, AsString and AsWideString now work if the TOmniValue
///         contains a Variant of the appropriate type.
///     1.25f: 2011-12-18
///       - Fixed various TOmniInterfaceDictionary bugs (big tnx to Zarko Gajic).
///         - Clear properly clears interface refence before destroying the bucket.
///         - Resize properly clears interface reference in old bucket after copy.
///         - Resize preserves the internal item count.
///         - Resize releases old buckets.
///     1.25e: 2011-12-09
///       - Removed compilation hint "Private symbol 'GetAsRecord' declared but never used".
///     1.25d: 2011-12-02
///       - Removed compilation hint "Private symbol 'GetAsArrayItem' declared but never used".
///     1.25c: 2011-11-29
///       - Reference count handling in TOmniValue was ignoring array and record wrappers.
///     1.25b: 2011-11-18
///       - Overloaded property getters in TOmniValue are not inlined on 2009/
///         2010 because of the buggy compiler.
///     1.25a: 2011-11-15
///       - Some inlining removed because it would not work reliably.
///     1.25: 2011-11-08
///       - Less casting in TOmniValue.Create.
///       - TOmniValue can store records by using FromRecord<T> and AsRecord<T>.
///       - Added a class which can wrap any record - TOmniRecordWrapper<T>.
///       - Added an interface which can wrap any object and destroy it when the
///         interface goes out of scope - IOmniAutoDestroyObject.
///     1.24: 2011-11-05
///       - TOmniValue.Create now internally creates TOmniValueContainer to store values.
///         Variant arrays are no longer used. IsArray tests it TOmniValue contains an
///         array and AsArray returns this internal container. Default indexed property
///         still accesses individual elements in this container. See demo
///         50_OmniValueArray for an example.
///       - TOmniValue.Create creates internal TOmniValueContainer containing named items.
///         See demo 50_OmniValueArray for an example.
///     1.23: 2011-09-06
///       - Implemented IOmniCounter.Take.
///     1.22: 2011-08-31
///       - [Lee_Nover] SetThreadName implementation moved into a separate unit with debug
///         info disabled. That way, debugger doesn't stop on SetThreadName while 
///         single-stepping in another thread.
///     1.21: 2011-08-28
///       - TOmniValue can natively store exception objects (AsException, IsException).
///     1.20: 2011-07-14
///       - Removed EXIT_EXCEPTION error code.
///     1.19a: 2010-12-12
///       - Define implicit TOmniValue<->TDateTime conversion operators only for Delphi XE
///         and higher.
///     1.19: 2010-12-03
///       - [scarre] Added TDateTime support to TOmniValue.
///     1.18a: 2010-09-21
///       - IOmniWaitableValue fully compatible with TOmniWaitableValue.
///     1.18: 2010-09-20
///       - Declared interface IOmniWaitableValue and added function CreateWaitableValue.
///       - Implemented TOmniMessageID.AsString.
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

unit OtlCommon;

{$I OtlOptions.inc}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes,
  Variants,
  TypInfo,
  SyncObjs,
{$IFDEF MSWINDOWS}
  Windows,
  DSiWin32,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
{$IFDEF OTL_ERTTI}
  RTTI,
{$ENDIF OTL_ERTTI}
{$IFDEF OTL_Generics}
  Generics.Collections,
{$ENDIF OTL_Generics}
  SysUtils;

const
  // reserved exit statuses
  EXIT_OK                        = 0;
  EXIT_INTERNAL                  = integer($80000000);
  EXIT_THREADPOOL_QUEUE_TOO_LONG = EXIT_INTERNAL + 0;
  EXIT_THREADPOOL_STALE_TASK     = EXIT_INTERNAL + 1;
  EXIT_THREADPOOL_CANCELLED      = EXIT_INTERNAL + 2;
  EXIT_THREADPOOL_INTERNAL_ERROR = EXIT_INTERNAL + 3;

type
{$IF CompilerVersion < 23} //pre-XE2
  NativeInt = integer;
  PNativeInt = PInteger;
{$IFEND}

  //:TOmniValue conversion exception.
  EOmniValueConv = class(Exception);

  TOmniValueContainer = class;
  IOmniAutoDestroyObject = interface;


  TOmniValueDataType = (ovtNull,
           {ovData} ovtBoolean, ovtInteger, ovtDouble, ovtObject, ovtPointer, ovtDateTime, ovtException,
           {ovIntf} ovtExtended, ovtString, ovtInterface, ovtVariant, ovtArray, ovtRecord, ovtOwnedObject
{$IFDEF MSWINDOWS}
           , ovtWideString, ovtAnsiString
{$ENDIF});

  TOmniValue = packed record // 13 bytes in 32-bit, 17 bytes in 64-bits
  private
    ovData: int64;
    ovIntf: IInterface;
    ovType: TOmniValueDataType;
    function  CastToBoolean: boolean; inline;
    function  CastToCardinal: cardinal; inline;
    function  CastToDouble: Double;
    function  CastToDateTime: TDateTime;
    function  CastToException: Exception;
    function  CastToExtended: Extended;
    function  CastToInt64: int64; inline;
    function  CastToInteger: integer; inline;
    function  CastToInterface: IInterface; inline;
    function  CastToObject: TObject; overload; inline;
    function  CastToPointer: pointer;
    function  CastToRecord: IOmniAutoDestroyObject; inline;
    function  CastToString: string;
    function  CastToVariant: Variant;
    function  GetAsArray: TOmniValueContainer; inline;
    function  GetAsArrayItem(idx: integer): TOmniValue; overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    function  GetAsArrayItem(const name: string): TOmniValue; overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    {$IF CompilerVersion >= 19}//D2007 has problems understanding this overload
    function  GetAsArrayItem(const param: TOmniValue): TOmniValue; overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    //GetAsArrayItemOV is used in D2007 instead
    {$IFEND}
    function  GetAsArrayItemOV(const param: TOmniValue): TOmniValue; overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    procedure SetAsArray(value: TOmniValueContainer); inline;
    procedure SetAsArrayItem(idx: integer; const value: TOmniValue); overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    procedure SetAsArrayItem(const name: string; const value: TOmniValue); overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    {$IF CompilerVersion >= 19}//D2007 has problems understanding this overload
    procedure SetAsArrayItem(const param, value: TOmniValue); overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    //SetAsArrayItemOV is used in D2007 instead
    {$IFEND}
    procedure SetAsArrayItemOV(const param, value: TOmniValue); overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    procedure SetAsBoolean(const value: boolean); inline;
    procedure SetAsCardinal(const value: cardinal); inline;
    procedure SetAsDouble(value: Double); inline;
    procedure SetAsDateTime(value: TDateTime); inline;
    procedure SetAsException(value: Exception);
    procedure SetAsExtended(value: Extended);
    procedure SetAsInt64(const value: int64); inline;
    procedure SetAsInteger(const value: integer); inline;
    procedure SetAsInterface(const value: IInterface); //don't inline, something is broken in codegen (XE)
    procedure SetAsObject(const value: TObject); inline;
    procedure SetAsOwnedObject(const value: TObject); inline;
    procedure SetAsPointer(const value: pointer); inline;
    procedure SetAsRecord(const intf: IOmniAutoDestroyObject); inline;
    procedure SetAsString(const value: string);
    procedure SetAsVariant(const value: Variant);
    procedure SetOwnsObject(const value: boolean);
    {$REGION 'Documentation'}
    ///  <summary>Most of the code in this method never executes. It is just here so that
    ///  stupid compilation hints such as "Private symbol 'GetAsArrayItem' declared but
    ///  never used" are not shown.</summary>
    {$ENDREGION}
    class procedure _RemoveWarnings; inline; static;
    procedure ClearIntf; inline;
  {$IFDEF MSWINDOWS}
    function  CastToAnsiString: AnsiString; inline;
    function  CastToWideString: WideString;
    procedure SetAsAnsiString(const value: AnsiString);
    procedure SetAsWideString(const value: WideString);
  {$ENDIF}
  {$IFDEF OTL_ERTTI}
  private
    function  GetAsTValue: TValue;
    function  GetArrayFromTValue(const value: TValue): TOmniValueContainer;
    function  GetTValueFromArray(const a: TOmniValueContainer): TValue;
    procedure SetAsTValue(const value: TValue);
  {$ENDIF}
  public
    constructor Create(const values: array of const);
    constructor CreateNamed(const values: array of const; const cppDupConWorkaround: boolean = false);
    procedure _AddRef; inline;
    procedure _Release; inline;
    procedure _ReleaseAndClear; inline;
    function  CastToBooleanDef(defValue: boolean): boolean; inline;
    function  CastToCardinalDef(defValue: cardinal): cardinal; inline;
    function  CastToDoubleDef(defValue: Double): Double; inline;
    function  CastToDateTimeDef(defValue: TDateTime): TDateTime; inline;
    function  CastToExceptionDef(defValue: Exception): Exception; inline;
    function  CastToExtendedDef(defValue: Extended): Extended; inline;
    function  CastToInt64Def(defValue: int64): int64; inline;
    function  CastToIntegerDef(defValue: integer): integer; inline;
    function  CastToInterfaceDef(const defValue: IInterface): IInterface; inline;
    function  CastToObjectDef(defValue: TObject): TObject; inline;
    function  CastToPointerDef(defValue: pointer): pointer; inline;
    function  CastToStringDef(const defValue: string): string; inline;
    function  CastToVariantDef(defValue: Variant): Variant; inline;
    procedure Clear; inline;
    function  HasArrayItem(idx: integer): boolean; overload; inline;
    function  HasArrayItem(const name: string): boolean; overload; inline;
    function  HasArrayItem(const param: TOmniValue): boolean; overload; inline;
    function  IsArray: boolean; inline;
    function  IsBoolean: boolean; inline;
    function  IsEmpty: boolean; inline;
    function  IsException: boolean; inline;
    function  IsFloating: boolean; inline;
    function  IsDateTime: boolean; inline;
    function  IsInteger: boolean; inline;
    function  IsInterface: boolean; inline;
    function  IsInterfacedType: boolean; inline;
    function  IsObject: boolean; inline;
    function  IsOwnedObject: boolean; inline;
    function  IsPointer: boolean; inline;
    function  IsRecord: boolean; inline;
    function  IsString: boolean; inline;
    function  IsVariant: boolean; inline;
    class function Null: TOmniValue; static;
    function  RawData: PInt64; inline;
    procedure RawZero; inline;
    function  TryCastToBoolean(var value: boolean): boolean; inline;
    function  TryCastToCardinal(var value: cardinal): boolean; inline;
    function  TryCastToDouble(var value: Double): boolean;
    function  TryCastToDateTime(var value: TDateTime): boolean;
    function  TryCastToException(var value: Exception): boolean;
    function  TryCastToExtended(var value: Extended): boolean;
    function  TryCastToInt64(var value: int64): boolean; inline;
    function  TryCastToInteger(var value: integer): boolean; inline;
    function  TryCastToInterface(var value: IInterface): boolean; inline;
    function  TryCastToObject(var value: TObject): boolean; inline;
    function  TryCastToPointer(var value: pointer): boolean;
    function  TryCastToString(var value: string): boolean;
    function  TryCastToVariant(var value: Variant): boolean;
    class operator Equal(const a: TOmniValue; i: integer): boolean; inline;
    class operator Equal(const a: TOmniValue; const s: string): boolean; inline;
    class operator Implicit(const a: boolean): TOmniValue; inline;
    class operator Implicit(const a: Double): TOmniValue; inline;
    class operator Implicit(const a: Extended): TOmniValue; inline;
    class operator Implicit(const a: integer): TOmniValue; inline;
    class operator Implicit(const a: int64): TOmniValue; inline;
    class operator Implicit(const a: pointer): TOmniValue; inline;
    class operator Implicit(const a: string): TOmniValue; inline;
    class operator Implicit(const a: IInterface): TOmniValue; //don't inline, something is broken in codegen (XE)
    class operator Implicit(const a: TObject): TOmniValue; inline;
    class operator Implicit(const a: Exception): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): int64; inline;
    class operator Implicit(const a: TOmniValue): TObject; inline;
    class operator Implicit(const a: TOmniValue): Double; inline;
    class operator Implicit(const a: TOmniValue): Exception; inline;
    class operator Implicit(const a: TOmniValue): Extended; inline;
    class operator Implicit(const a: TOmniValue): string; inline;
    class operator Implicit(const a: TOmniValue): integer; inline;
    class operator Implicit(const a: TOmniValue): pointer; inline;
    class operator Implicit(const a: TOmniValue): boolean; inline;
    class operator Implicit(const a: TOmniValue): IInterface; inline;
    class operator Implicit(const a: Variant): TOmniValue; inline;
  {$IFDEF OTL_TOmniValueImplicitDateTime}
    class operator Implicit(const a: TDateTime): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): TDateTime; inline;
  {$ENDIF OTL_TOmniValueImplicitDateTime}
    property AsArray: TOmniValueContainer read GetAsArray;
    property AsArrayItem[idx: integer]: TOmniValue read GetAsArrayItem write SetAsArrayItem; default;
    property AsArrayItem[const name: string]: TOmniValue read GetAsArrayItem write SetAsArrayItem; default;
    {$IF CompilerVersion >= 19}//D2007 has problems understanding this overload
    property AsArrayItem[const param: TOmniValue]: TOmniValue read GetAsArrayItem write SetAsArrayItem; default;
    {$IFEND}
    property AsArrayItemOV[const param: TOmniValue]: TOmniValue read GetAsArrayItemOV write SetAsArrayItemOV; 
    property AsBoolean: boolean read CastToBoolean write SetAsBoolean;
    property AsCardinal: cardinal read CastToCardinal write SetAsCardinal;
    property AsDouble: Double read CastToDouble write SetAsDouble;
    property AsDateTime: TDateTime read CastToDateTime write SetAsDateTime;
    property AsException: Exception read CastToException write SetAsException;
    property AsExtended: Extended read CastToExtended write SetAsExtended;
    property AsInt64: int64 read CastToInt64 write SetAsInt64;
    property AsInteger: integer read CastToInteger write SetAsInteger;
    property AsInterface: IInterface read CastToInterface write SetAsInterface;
    property AsObject: TObject read CastToObject write SetAsObject;
    property AsOwnedObject: TObject read CastToObject write SetAsOwnedObject;
    property AsPointer: pointer read CastToPointer write SetAsPointer;
    property AsString: string read CastToString write SetAsString;
    property AsVariant: Variant read CastToVariant write SetAsVariant;
    property DataType: TOmniValueDataType read ovType;
    property OwnsObject: boolean read IsOwnedObject write SetOwnsObject;
  {$IFDEF MSWINDOWS}
    function  CastToAnsiStringDef(const defValue: AnsiString): AnsiString; inline;
    function  CastToWideStringDef(defValue: WideString): WideString; inline;
    function  IsAnsiString: boolean; inline;
    function  IsWideString: boolean; inline;
    function  TryCastToAnsiString(var value: AnsiString): boolean;
    function  TryCastToWideString(var value: WideString): boolean;
    {$IFDEF Unicode}
    class operator Implicit(const a: TOmniValue): AnsiString; inline;
    class operator Implicit(const a: AnsiString): TOmniValue; inline;
    {$ENDIF Unicode}
    class operator Implicit(const a: TOmniValue): WideString; inline;
    class operator Implicit(const a: WideString): TOmniValue; inline;
    property AsAnsiString: AnsiString read CastToAnsiString write SetAsAnsiString;
    property AsWideString: WideString read CastToWideString write SetAsWideString;
  {$ENDIF}
  {$IFDEF OTL_Generics}
    class function CastFrom<T>(const value: T): TOmniValue; static;
    function  CastTo<T>: T;
    class function FromRecord<T: record>(const value: T): TOmniValue; static;
    function  ToRecord<T>: T;
  {$IFDEF OTL_HasArrayOfT}
    class function FromArray<T>(const values: TArray<T>): TOmniValue; static;
    function  ToArray<T>: TArray<T>;
  {$ENDIF OTL_HasArrayOfT}
  {$IF CompilerVersion > 20}
    function  CastToObject<T: class>: T; overload;
    function  ToObject<T: class>: T;
  {$IFEND}
  {$ENDIF OTL_Generics}
  {$IFDEF OTL_ERTTI}
    class operator Implicit(const a: TValue): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): TValue; inline;
    property AsTValue: TValue read GetAsTValue write SetAsTValue;
  {$ENDIF OTL_ERTTI}
  end; { TOmniValue }

  ///  <summary>TOmniValue wrapper - for when you need to treat TOmniValue as an object.</summary>
  TOmniValueObj = class
  strict private
    FValue: TOmniValue;
  public
    constructor Create(const value: TOmniValue);
    property Value: TOmniValue read FValue;
  end; { TOmniValueObj }

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
  TOmniValueEnumerator = class abstract
    function  GetCurrent: TOmniValue; virtual; abstract;
    function  MoveNext: boolean; virtual; abstract;
    property Current: TOmniValue read GetCurrent;
  end; { TOmniValueEnumerator }

  IOmniWaitableValue = interface ['{46EB21E0-B5E8-47DA-8E34-E4DE04C4D8D9}']
  {$IFDEF MSWINDOWS}
    function  GetHandle: THandle;
  {$ENDIF}
    function  GetEvent: TEvent;
    function  GetValue: TOmniValue;
  //
    procedure Reset;
    procedure Signal; overload;
    procedure Signal(const data: TOmniValue); overload;
    function  WaitFor(maxWait_ms: cardinal = INFINITE): boolean;
  {$IFDEF MSWINDOWS}
    property Handle: THandle read GetHandle;
  {$ENDIF}
    property Event: TEvent read GetEvent;
    property Value: TOmniValue read GetValue;
  end; { IOmniWaitableValue }

  TOmniWaitableValue = class( TInterfacedObject, IOmniWaitableValue)
  strict private
    FEvent: TEvent;
    FValue: TOmniValue;
  protected
  {$IFDEF MSWINDOWS}
    function  GetHandle: THandle;
  {$ENDIF}
    function  GetEvent: TEvent;
    function  GetValue: TOmniValue;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Reset; inline;
    procedure Signal; overload; inline;
    procedure Signal(const data: TOmniValue); overload; inline;
    function  WaitFor(maxWait_ms: cardinal = INFINITE): boolean; inline;
  {$IFDEF MSWINDOWS}
    property Handle: THandle read GetHandle;
  {$ENDIF}
    property Event: TEvent read GetEvent;
    property Value: TOmniValue read GetValue;
  end; { TOmniWaitableValue }

  TOmniValueContainer = class
  strict private
    ovcCanModify: boolean;
    ovcCount    : integer;
    ovcNames    : array of string;
    ovcValues   : array of TOmniValue;
  strict protected
    function  AddParam(const paramName: string): integer;
    procedure Clear;
    function  GetItem(paramIdx: integer): TOmniValue; overload;
    function  GetItem(const paramName: string): TOmniValue; overload;
    function  GetItem(const param: TOmniValue): TOmniValue; overload;
    procedure SetItem(idx: integer; const value: TOmniValue); overload; {$IF CompilerVersion >= 22}inline;{$IFEND}
    procedure SetItem(const name: string; const value: TOmniValue); overload;
    procedure SetItem(const param, value: TOmniValue); overload;
    procedure Grow(requiredIdx: integer = -1);
  public
    constructor Create;
    procedure Add(const paramValue: TOmniValue; paramName: string = '');
    procedure Assign(const parameters: array of TOmniValue);
    procedure AssignNamed(const parameters: array of TOmniValue);
    function  ByName(const paramName: string): TOmniValue; overload;
    function  ByName(const paramName: string; const defValue: TOmniValue): TOmniValue; overload;
    function  Count: integer;
    function  Exists(const paramName: string): boolean;
    function  IndexOf(const paramName: string): integer;
    procedure Insert(paramIdx: integer; const value: TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    property Item[paramIdx: integer]: TOmniValue read GetItem write SetItem; default;
    property Item[const paramName: string]: TOmniValue read GetItem write SetItem; default;
    property Item[const param: TOmniValue]: TOmniValue read GetItem write SetItem; default;
  end; { TOmniValueContainer }

  //:Thread-safe counter
  IOmniCounter = interface ['{3A73CCF3-EDC5-484F-8459-532B8C715E3C}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  //
    function  Increment: integer;
    function  Decrement: integer;
    function  Take(count: integer): integer; overload;
    function  Take(count: integer; var taken: integer): boolean; overload;
    property Value: integer read GetValue write SetValue;
  end; { IOmniCounter }

  TOmniCounter = record
  strict private
    {$IFNDEF MSWINDOWS}[Volatile]{$ENDIF}
    ocCounter: IOmniCounter;
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  public
    procedure Initialize;
    function  Increment: integer;
    function  Decrement: integer;
    function  Take(count: integer): integer; overload;
    function  Take(count: integer; var taken: integer): boolean; overload;
    property Value: integer read GetValue write SetValue;
  end; { TOmniCounter }

{$IFDEF OTL_Generics}
  TOmniInterfaceDictionaryPair = TPair<int64, IInterface>;
{$ELSE}
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
{$ENDIF OTL_Generics}

  IOmniInterfaceDictionary = interface ['{619FCCF3-E810-4DCF-B902-1EF1A5A72DB5}']
{$IFDEF OTL_Generics}
    function  GetEnumerator: TDictionary<int64, IInterface>.TPairEnumerator;
{$ELSE}
    function  GetEnumerator: IOmniInterfaceDictionaryEnumerator;
{$ENDIF OTL_Generics}
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
    function  GetCountPhysical: integer;
    procedure SetAsString(const value: string);
    procedure SetCount(const value: integer);
  {$IFDEF MSWINDOWS}
    function  GetMask: DWORD;
    procedure SetMask(const value: DWORD);
  {$ENDIF}
  //
    property AsString: string read GetAsString write SetAsString;
    property Count: integer read GetCount write SetCount;
    property CountPhysical: integer read GetCountPhysical;
  {$IFDEF MSWINDOWS}
    property Mask: DWORD read GetMask write SetMask;
  {$ENDIF}
  end; { IOmniAffinity }

{$IFDEF MSWINDOWS}
  TOmniProcessMemoryCounters = TProcessMemoryCounters;
{$ENDIF}

{$IFDEF MSWINDOWS}
  TOmniProcessTimes = record
    CreationTime: TDateTime;
    UserTime    : int64;
    KernelTime  : int64;
  end; { TOmniProcessTimes }
{$ENDIF}

  TOmniProcessPriorityClass = (pcIdle, pcBelowNormal, pcNormal, pcAboveNormal, pcHigh,
    pcRealtime);

  IOmniProcessEnvironment = interface ['{98D6BDA3-840B-4E19-B01D-633E6A239FE9}']
    function  GetAffinity: IOmniAffinity;
    function  GetPriorityClass: TOmniProcessPriorityClass;
  {$IFDEF MSWINDOWS}
    function  GetMemory: TOmniProcessMemoryCounters;
    function  GetTimes: TOmniProcessTimes;
  {$ENDIF}
  //
    property Affinity: IOmniAffinity read GetAffinity;
    property PriorityClass: TOmniProcessPriorityClass read GetPriorityClass;
  {$IFDEF MSWINDOWS}
    property Memory: TOmniProcessMemoryCounters read GetMemory;
    property Times: TOmniProcessTimes read GetTimes;
  {$ENDIF}
  end; { IOmniProcessEnvironment }

  IOmniSystemEnvironment = interface ['{9BE1EFE3-4ABB-4C2F-B2A4-B014D0949FEC}']
    function GetAffinity: IOmniAffinity;
  //
    property Affinity: IOmniAffinity read GetAffinity;
  end; { IOmniSystemEnvironment }

  {$IF CompilerVersion < 19}//D2007 //TODO
  TThreadID = LongWord;
  {$IFEND}

  IOmniThreadEnvironment = interface ['{5C11FEC7-9FBE-423F-B30E-543C8240E3A3}']
    function  GetAffinity: IOmniAffinity;
    function  GetID: TThreadId;
  //
    property Affinity: IOmniAffinity read GetAffinity;
    property ID: TThreadId read GetID;
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
  {$IFDEF OTL_Anonymous}
  strict private
    oeDelegate: TProc;
    procedure SetAnonDelegate(const value: TProc); inline;
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
    property Delegate: TProc read GetDelegate write SetAnonDelegate;
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
    function AsString: string;
    property MessageType: TOmniMessageIDType read omidMessageType;
  end; { TOmniMessageID }

{$IFDEF OTL_Generics}
  TOmniRecordWrapper<T> = class
  strict private
    FValue: T;
  public
    constructor Create(const value: T);
    function  GetRecord: T;
    procedure SetRecord(const value: T);
    property Value: T read GetRecord write SetRecord;
  end; { TOmniRecordWrapper<T> }
{$ENDIF OTL_Generics}

  IOmniAutoDestroyObject = interface ['{37DE60D3-C53D-4D13-B87C-C70BDC76A530}']
    function GetValue: TObject;
    function Detach: TObject;
    //
    property Value: TObject read GetValue;
  end; { IOmniAutoDestroyObject }

  TOmniAlignedInt32 = record
  strict private
    FData: int64;
    FAddr: PInteger;
    function  GetValue: integer; inline;
    procedure SetValue(value: integer); inline;
  public
    procedure Initialize; inline;
    function  Add(value: integer): integer; inline;
    function  Addr: PInteger; inline;
    function  CAS(oldValue, newValue: integer): boolean;
    function  Decrement: integer; overload; inline;
    function  Decrement(value: integer): integer; overload; inline;
    function  Increment: integer; overload; inline;
    function  Increment(value: integer): integer; overload; inline;
    function  Subtract(value: integer): integer; inline;
    class operator Add(const ai: TOmniAlignedInt32; i: integer): cardinal; inline;
    class operator Equal(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator GreaterThan(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator GreaterThanOrEqual(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator Implicit(const ai: TOmniAlignedInt32): integer; inline;
    class operator Implicit(const ai: TOmniAlignedInt32): cardinal; inline;
    class operator Implicit(const ai: TOmniAlignedInt32): PInteger; inline;
    class operator LessThan(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator LessThanOrEqual(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator NotEqual(const ai: TOmniAlignedInt32; i: integer): boolean; inline;
    class operator Subtract(ai: TOmniAlignedInt32; i: integer): cardinal; inline;
    property Value: integer read GetValue write SetValue;
  end; { TOmniAlignedInt32 }

  TOmniAlignedInt64 = record
  strict private
    FData: packed record
      DataLo, DataHi: int64;
    end;
    FAddr: PInt64;
    function  GetValue: int64; inline;
    procedure SetValue(value: int64); inline;
  public
    procedure Initialize; inline;
    function  Add(value: int64): int64; inline;
    function  Addr: PInt64; inline;
    function  CAS(oldValue, newValue: int64): boolean;
    function  Decrement: int64; overload; inline;
    function  Decrement(value: int64): int64; overload; inline;
    function  Increment: int64; overload; inline;
    function  Increment(value: int64): int64; overload; inline;
    function  Subtract(value: int64): int64; inline;
    property Value: int64 read GetValue write SetValue;
  end; { TOmniAlignedInt64 }

{$IFNDEF MSWINDOWS}
  TObjectList = class(TObjectList<TObject>)
  end;
{$ENDIF}

  // TInterlockedHelper = class helper for TInterlocked doenst seem to be working.
  TInterlockedEx = class
  public
    class function CompareExchange(var Target: NativeInt; Value: NativeInt; Comparand: NativeInt): NativeInt; static; inline;
    class function Add(var Target: NativeInt; Increment: NativeInt): NativeInt; overload; static; inline;
  end;

  function  CreateCounter(initialValue: integer = 0): IOmniCounter;
  function  CreateInterfaceDictionary: IOmniInterfaceDictionary;
  function  CreateWaitableValue: IOmniWaitableValue;
  function  CreateAutoDestroyObject(obj: TObject): IOmniAutoDestroyObject;

  function  Environment: IOmniEnvironment;
  procedure SetThreadName(const name: string);
  function  VarToObj(const v: Variant): TObject; inline;
  function  NextOid: int64;

var
{$IFDEF OTL_USE_ALIGN}
  [Volatile] OtlUID: int64 = 0;
{$ELSE}
  OtlUID: TOmniAlignedInt64;
{$ENDIF}

  TOmniValue_DataSize: array [TTypeKind] of integer;

implementation

{$IFDEF MSWINDOWS}
uses
  {$IFDEF OTL_StrPasInAnsiStrings}System.AnsiStrings,{$ENDIF}
  GpStringHash;
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  IOmniAnsiStringData = interface ['{DBF5674C-AEFF-4CBD-AEC5-95F7A2FC80FF}']
    function  GetValue: AnsiString;
    procedure SetValue(const value: AnsiString);
    property Value: AnsiString read GetValue write SetValue;
  end; { IOmniStringData }

  TOmniAnsiStringData = class(TInterfacedObject, IOmniAnsiStringData)
  strict private
    osdValue: AnsiString;
  public
    constructor Create(const value: AnsiString);
    function  GetValue: AnsiString;
    procedure SetValue(const value: AnsiString);
    property Value: AnsiString read GetValue write SetValue;
  end; { TOmniAnsiStringData }
{$ENDIF}

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

{$IFDEF MSWINDOWS}
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
{$ENDIF}

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
  {$IFDEF OTL_USE_ALIGN}
    [Volatile] FValue: integer;
  {$ELSE}
    ocValue: TOmniAlignedInt32;
  {$ENDIF}
  protected
    function  GetValue: integer; inline;
    procedure SetValue(const value: integer); inline;
  public
    constructor Create(initialValue: integer);
    function  Decrement: integer; inline;
    function  Increment: integer; inline;
    function  Take(count: integer): integer; overload;
    function  Take(count: integer; var taken: integer): boolean; overload; inline;
    property Value: integer read GetValue write SetValue;
  end; { TOmniCounterImpl }

{$IFNDEF OTL_Generics}
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
{$ENDIF ~OTL_Generics}

  TOmniInterfaceDictionary = class(TInterfacedObject, IOmniInterfaceDictionary)
  {$IFDEF OTL_Generics}
  strict private
    FDictionary: TDictionary<int64, IInterface>;
  {$ELSE}
  strict private
    idBuckets: TBucketArray;
    idCount  : integer;
  strict protected
    function  Find(const key: int64): PPHashItem;
    function  HashOf(const key: int64): integer; inline;
    procedure Resize(size: Cardinal);
  {$ENDIF OTL_Generics}
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const key: int64; const value: IInterface);
    procedure Clear;
    function  Count: integer; inline;
    {$IFDEF OTL_Generics}
    function  GetEnumerator: TDictionary<int64, IInterface>.TPairEnumerator;
    {$ELSE}
    function  GetEnumerator: IOmniInterfaceDictionaryEnumerator;
    {$ENDIF ~OTL_Generics}
    procedure Remove(const key: int64);
    function  ValueOf(const key: int64): IInterface;
  end; { TOmniInterfaceDictionary }

  TOmniAffinityTarget = (atSystem, atProcess, atThread);

  TOmniAffinity = class(TInterfacedObject, IOmniAffinity)
  strict private
    oaTarget: TOmniAffinityTarget;
  protected
    function  GetAsString: string;
    function  GetCount: integer;
    function  GetCountPhysical: integer;
    procedure SetAsString(const value: string);
    procedure SetCount(const value: integer);
  {$IFDEF MSWINDOWS}
    function  GetMask: DWORD;
    procedure SetMask(const value: DWORD);
  {$ENDIF}
  public
    constructor Create(target: TOmniAffinityTarget);
    property AsString: string read GetAsString write SetAsString;
    property Count: integer read GetCount write SetCount;
  {$IFDEF MSWINDOWS}
    property Mask: DWORD read GetMask write SetMask;
  {$ENDIF}
  end; { TOmniAffinity }

  TOmniProcessEnvironment = class(TInterfacedObject, IOmniProcessEnvironment)
  strict private
    opeAffinity: IOmniAffinity;
  protected
    function  GetAffinity: IOmniAffinity;
    function  GetPriorityClass: TOmniProcessPriorityClass;
  {$IFDEF MSWINDOWS}
    function  GetMemory: TOmniProcessMemoryCounters;
    function  GetTimes: TOmniProcessTimes;
  {$ENDIF}
  public
    constructor Create;
    property Affinity: IOmniAffinity read GetAffinity;
    property PriorityClass: TOmniProcessPriorityClass read GetPriorityClass;
  {$IFDEF MSWINDOWS}
    property Memory: TOmniProcessMemoryCounters read GetMemory;
    property Times: TOmniProcessTimes read GetTimes;
  {$ENDIF}
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
    oteThreadID:  TThreadID;
  protected
    function  GetAffinity: IOmniAffinity;
    function  GetID: TThreadID;
  public
    constructor Create;
    property Affinity: IOmniAffinity read GetAffinity;
    property ID: TThreadID read GetID;
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

  TOmniAutoDestroyObject = class(TInterfacedObject, IOmniAutoDestroyObject)
  strict private
    FValue: TObject;
  protected
    function Detach: TObject;
  protected
    function  GetValue: TObject;
    procedure SetValue(const value: TObject);
  public
    constructor Create(obj: TObject);
    destructor  Destroy; override;
    property Value: TObject read FValue write SetValue;
  end; { TOmniAutoDestroyObject }

var
  GEnvironment: IOmniEnvironment;

{ exports }

function CreateAutoDestroyObject(obj: TObject): IOmniAutoDestroyObject;
begin
  Result := TOmniAutoDestroyObject.Create(obj);
end; { CreateAutoDestroyObject }

function CreateCounter(initialValue: integer): IOmniCounter;
begin
  Result := TOmniCounterImpl.Create(initialValue);
end; { CreateCounter }

function CreateInterfaceDictionary: IOmniInterfaceDictionary;
begin
  Result := TOmniInterfaceDictionary.Create;
end; { CreateInterfaceDictionary }

function CreateWaitableValue: IOmniWaitableValue;
begin
  Result := TOmniWaitableValue.Create;
end; { CreateWaitableValue }

function Environment: IOmniEnvironment;
begin
  Result := GEnvironment;
end; { Environment }

{$IFDEF MSWINDOWS}
procedure SetThreadName(const name: string);
// TODO 1 -oPrimoz Gabrijelcic : This should only exist in OtlCommon.Utils or not at all
// if SetThreadNameForDebugging exists in D2009.
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
{$ELSE}

procedure SetThreadName( const name: string);
begin
end;
{$ENDIF}

function VarToObj(const v: Variant): TObject;
begin
  Result := TObject({$IFDEF Unicode}NativeUInt{$ELSE}cardinal{$ENDIF}(v));
end; { VarToObj }

{ globals }

{$IFDEF MSWINDOWS}
function StrPasA(const Str: PAnsiChar): AnsiString;
begin
  Result := {$IFDEF OTL_StrPasInAnsiStrings}System.AnsiStrings.{$ENDIF}StrPas(Str);
end; { StrPasA }
{$ENDIF}

{$IFDEF OTL_Generics}
{ TOmniRecordWrapper }

constructor TOmniRecordWrapper<T>.Create(const value: T);
begin
  inherited Create;
  SetRecord(value);
end; { TOmniRecordWrapper<T>.Create }

function TOmniRecordWrapper<T>.GetRecord: T;
begin
  Result := FValue;
end; { TOmniRecordWrapper<T>.GetRecord }

procedure TOmniRecordWrapper<T>.SetRecord(const value: T);
begin
  FValue := value;
end; { TOmniRecordWrapper<T>.SetRecord }
{$ENDIF OTL_Generics}

{ TOmniAutoDestroyObject }

constructor TOmniAutoDestroyObject.Create(obj: TObject);
begin
  inherited Create;
  FValue := obj;
end; { TOmniAutoDestroyObject.Create }

destructor TOmniAutoDestroyObject.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end; { TOmniAutoDestroyObject.Destroy }

function TOmniAutoDestroyObject.GetValue: TObject;
begin
  Result := FValue;
end; { TOmniAutoDestroyObject.GetValue }

procedure TOmniAutoDestroyObject.SetValue(const value: TObject);
begin
  FValue := value;
end; { TOmniAutoDestroyObject.SetValue }

function TOmniAutoDestroyObject.Detach: TObject;
begin
  Result := FValue;
  FValue := nil
end; { TOmniAutoDestroyObject.Detach }

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
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Locked');
  if paramName = '' then
      idxParam := -1
    else
      idxParam := IndexOf( paramName);
  if idxParam < 0 then
    idxParam := AddParam( paramName);
  ovcValues[ idxParam] := paramValue;
end; { TOmniValueContainer.Add }

function TOmniValueContainer.AddParam(const paramName: string): integer;
begin
  Result := ovcCount;
  Inc(ovcCount);
  if Result > High(ovcValues) then
    Grow;
  ovcNames[Result] := paramName;
end; { TOmniValueContainer.AddParam }

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

procedure TOmniValueContainer.AssignNamed(const parameters: array of TOmniValue);
var
  i: integer;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer.AssignNamed: Already locked');
  if Odd(Length(parameters)) then
    raise Exception.Create('TOmniValueContainer.AssignNamed: Not a proper number of parameters');
  Clear;
  Grow(Length(parameters) div 2 - 1);

  i := Low(parameters);
  while i < High(parameters) do begin
    Add(parameters[i+1], parameters[i]);
    Inc(i, 2);
  end;
end; { TOmniValueContainer.AssignNamed }

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
  Assert(paramIdx < ovcCount);
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
  if paramIdx >= ovcCount then
    ovcCount := paramIdx + 1;
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

procedure TOmniValueContainer.SetItem(idx: integer; const value: TOmniValue);
begin
  if IsLocked then
    raise Exception.Create('TOmniValueContainer.SetItem: Container is locked');
  Insert(idx, value);
end; { TOmniValueContainer.SetItem }

procedure TOmniValueContainer.SetItem(const name: string; const value: TOmniValue);
var
  idx: integer;
begin
  if IsLocked then
    raise Exception.Create('TOmniValueContainer.SetItem: Container is locked');
  idx := IndexOf(name);
  if idx < 0 then
    idx := AddParam(name);
  SetItem(idx, value);
end; { TOmniValueContainer.SetItem }

procedure TOmniValueContainer.SetItem(const param, value: TOmniValue);
begin
  if param.IsInteger then
    SetItem(param.AsInteger, value)
  else if param.IsString then
    SetItem(param.AsString, value)
  else
    raise Exception.Create('TOmniValueContainer.SetItem: Container can only be indexed by integer or string.');
end; { TOmniValueContainer.SetItem }

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
{$IFDEF MSWINDOWS}
var
  countIntf: IOmniCounter;
begin
  Assert(cardinal(@ocCounter) mod SizeOf(ocCounter) = 0,
    Format('TOmniCS.Initialize: ocsSync is not %d-aligned!', [SizeOf(ocCounter)]));
  if not assigned(ocCounter) then
  begin
    countIntf := CreateCounter;
    {$IFDEF CPUX64}
    if InterlockedCompareExchange64(PInt64(@ocCounter)^, int64(countIntf), 0) = 0 then
    {$ELSE}
    if InterlockedCompareExchange(PInteger(@ocCounter)^, integer(countIntf), 0) = 0 then
    {$ENDIF ~CPUX64}
      pointer(countIntf) := nil;
  end;

{$ELSE}
var
  Newbie: IOmniCounter;
begin
  if assigned(ocCounter) then exit;
  Newbie := CreateCounter;
  if AtomicCmpExchange(pointer(ocCounter), pointer(Newbie), nil) = nil then
    // Clear Newbie without decrementing the reference count.
    PPointer(@Newbie)^ := nil
{$ENDIF}
end; { TOmniCounter.Initialize }

procedure TOmniCounter.SetValue(const value: integer);
begin
  Initialize;
  ocCounter.SetValue(value);
end; { TOmniCounter.SetValue }

function TOmniCounter.Take(count: integer): integer;
begin
  Result := ocCounter.Take(count);
end; { TOmniCounter.Take }

function TOmniCounter.Take(count: integer; var taken: integer): boolean;
begin
  Result := ocCounter.Take(count, taken);
end; { TOmniCounter.Take }

{ TOmniCounterImpl }

constructor TOmniCounterImpl.Create( initialValue: integer);
begin
{$IFDEF OTL_USE_ALIGN}
  FValue := initialValue;
{$ELSE}
  ocValue.Initialize;
  ocValue.Value := initialValue;
{$ENDIF}
end; { TOmniCounterImpl.Create }

function TOmniCounterImpl.GetValue: integer;
begin
{$IFDEF OTL_USE_ALIGN}
  Result := FValue;
{$ELSE}
  Result := ocValue.Value
{$ENDIF}
end; { TOmniCounterImpl.GetValue }

function TOmniCounterImpl.Decrement: integer;
begin
{$IFDEF OTL_USE_ALIGN}
  Result := TInterlocked.Decrement(FValue);
{$ELSE}
  Result := ocValue.Decrement;
{$ENDIF} { TOmniCounterImpl.Decrement }
end;

function TOmniCounterImpl.Increment: integer;
begin
{$IFDEF OTL_USE_ALIGN}
  Result := TInterlocked.Increment(FValue);
{$ELSE}
  Result := ocValue.Increment;
{$ENDIF}
end; { TOmniCounterImpl.Increment }

procedure TOmniCounterImpl.SetValue(const Value: integer);
begin
{$IFDEF OTL_USE_ALIGN}
  FValue := Value
{$ELSE}
  ocValue.Value := value;
{$ENDIF}
end; { TOmniCounterImpl.SetValue }

function TOmniCounterImpl.Take(count: integer): integer;
{$IFDEF OTL_USE_ALIGN}
var
  current : integer;
  newValue: integer;
  request : integer;
begin
  request := count;
  while request > 0 do begin
    current := FValue;
    if current <= 0 then
      break;
    newValue := current - request;
    if newValue < 0 then
      newValue := 0;
    if TInterlocked.CompareExchange(FValue, newValue, current) = current then begin
      Dec(request, current - newValue);
      current := newValue;
    end;
  end;
  Result := count - request;
{$ELSE}

var
  current : integer;
  newValue: integer;
begin
  repeat
    current := Value;
    if current <= 0 then begin
      Result := 0;
      Exit;
    end;
    newValue := current - count;
    if newValue < 0 then
      newValue := 0;
    if ocValue.CAS(current, newValue) then begin
      Result := current - newValue;
      Exit;
    end;
  until false;
{$ENDIF}

end; { TOmniCounterImpl.Take }

function TOmniCounterImpl.Take(count: integer; var taken: integer): boolean;
begin
  taken  := Take(count);
  Result := taken > 0
end; { TOmniCounterImpl.Take }

{$IFNDEF OTL_Generics}
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
{$ENDIF ~OTL_Generics}

{ TOmniInterfaceDictionary }

constructor TOmniInterfaceDictionary.Create;
begin
  inherited Create;
  {$IFDEF OTL_Generics}
  FDictionary := TDictionary<int64, IInterface>.Create(100);
  {$ELSE}
  Resize(1);
  {$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.Create }

destructor TOmniInterfaceDictionary.Destroy;
begin
  {$IFDEF OTL_Generics}
  FreeAndNil(FDictionary);
  {$ELSE}
  Clear;
  {$ENDIF ~OTL_Generics}
  inherited;
end; { TOmniInterfaceDictionary.Destroy }

procedure TOmniInterfaceDictionary.Add(const key: int64; const value: IInterface);
{$IFNDEF OTL_Generics}
var
  bucket: PHashItem;
  hash  : integer;
{$ENDIF ~OTL_Generics}

begin
{$IFDEF OTL_Generics}
  FDictionary.Add(key, value);
{$ELSE}
  hash := HashOf(key);
  New(bucket);
  bucket^.Key := key;
  bucket^.Value := value;
  bucket^.Next := idBuckets[hash];
  idBuckets[hash] := bucket;
  Inc(idCount);
  if idCount > (1.5 * Length(idBuckets)) then
    Resize(idCount * 2);
{$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.Add }

procedure TOmniInterfaceDictionary.Clear;
{$IFNDEF OTL_Generics}
var
  bucket : PHashItem;
  iBucket: integer;
  next   : PHashItem;
{$ENDIF ~OTL_Generics}

begin
{$IFDEF OTL_Generics}
  FDictionary.Clear;
{$ELSE}
  for iBucket := 0 to Length(idBuckets) - 1 do begin
    bucket := idBuckets[iBucket];
    while bucket <> nil do begin
      next := bucket^.Next;
      bucket^.Value := nil;
      Dispose(bucket);
      bucket := next;
    end;
    idBuckets[iBucket] := nil;
  end;
  idCount := 0;
{$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.Clear }

function TOmniInterfaceDictionary.Count: integer;
begin
  {$IFDEF OTL_Generics}
  Result := FDictionary.Count;;
  {$ELSE}
  Result := idCount;
  {$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.Count }

{$IFNDEF OTL_Generics}
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
{$ENDIF ~OTL_Generics}

function TOmniInterfaceDictionary.GetEnumerator: {$IFDEF OTL_Generics}TDictionary<int64, IInterface>.TPairEnumerator{$ELSE}IOmniInterfaceDictionaryEnumerator{$ENDIF OTL_Generics};
begin
  {$IFDEF OTL_Generics}
  Result := FDictionary.GetEnumerator;
  {$ELSE}
  Result := TOmniInterfaceDictionaryEnumerator.Create(@idBuckets);
  {$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.GetEnumerator }

{$IFNDEF OTL_Generics}
function TOmniInterfaceDictionary.HashOf(const key: int64): integer;
begin
  Result := key mod Length(idBuckets);
end; { TOmniInterfaceDictionary.HashOf }
{$ENDIF ~OTL_Generics}

procedure TOmniInterfaceDictionary.Remove(const key: int64);
{$IFNDEF OTL_Generics}
var
  bucket    : PHashItem;
  bucketHead: PPHashItem;
{$ENDIF ~OTL_Generics}
begin
{$IFDEF OTL_Generics}
  if FDictionary.ContainsKey(key) then
    FDictionary.Remove(key);
{$ELSE}
  bucketHead := Find(key);
  bucket := bucketHead^;
  if assigned(bucket) then begin
    bucketHead^ := bucket^.Next;
    Dispose(bucket);
    Dec(idCount);
  end;
{$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.Remove }

{$IFNDEF OTL_Generics}
procedure TOmniInterfaceDictionary.Resize(size: Cardinal);
var
  bucket    : PHashItem;
  iBucket   : integer;
  next      : PHashItem;
  oldBuckets: TBucketArray;
  oldSize   : integer;
begin
  if Cardinal(Length(idBuckets)) >= size then
    Exit;
  oldSize := Count;
  oldBuckets := idBuckets;
  idBuckets := nil;
  idCount := 0;
  SetLength(idBuckets, GetGoodHashSize(size));
  for iBucket := 0 to High(oldBuckets) do begin
    bucket := oldBuckets[iBucket];
    while assigned(bucket) do begin
       Add(bucket.Key, bucket.Value);
       bucket.Value := nil;
       next := bucket.Next;
       Dispose(bucket);
       bucket := next;
    end;
  end;
  Assert(oldSize = Count);
end; { TOmniInterfaceDictionary.Resize }
{$ENDIF ~OTL_Generics}

function TOmniInterfaceDictionary.ValueOf(const key: int64): IInterface;
{$IFNDEF OTL_Generics}
var
  bucketHead: PHashItem;
{$ENDIF ~OTL_Generics}
begin
{$IFDEF OTL_Generics}
  if not FDictionary.TryGetValue(key, Result) then
    Result := nil;
{$ELSE}
  bucketHead := Find(key)^;
  if bucketHead <> nil then
    Result := bucketHead^.Value
  else
    Result := nil;
{$ENDIF ~OTL_Generics}
end; { TOmniInterfaceDictionary.ValueOf }

{ TOmniValue }

constructor TOmniValue.Create(const values: array of const);
var
  i  : integer;
  ovc: TOmniValueContainer;
begin
  ovc := TOmniValueContainer.Create;
  for i := Low(values) to High(values) do begin
    with values[i] do begin
      case VType of
        vtInteger:       ovc.Add(VInteger);
        vtBoolean:       ovc.Add(VBoolean);
        vtExtended:      ovc.Add(VExtended^);
        vtPointer:       ovc.Add(VPointer);
        vtCurrency:      ovc.Add(VCurrency^);
        vtVariant:       ovc.Add(VVariant^);
        vtObject:        ovc.Add(VObject);
        vtInterface:     ovc.Add(IInterface(VInterface));
        vtInt64:         ovc.Add(VInt64^);

      {$IFDEF UNICODE}
        vtUnicodeString: ovc.Add(string(VUnicodeString));
      {$ENDIF UNICODE}
      {$IFNDEF NEXTGEN}
        vtChar:          ovc.Add(string(VChar));
        vtString:        ovc.Add(string(VString^));
        vtPChar:         ovc.Add(string(StrPasA(VPChar)));
      {$ENDIF NEXTGEN}
      {$IFDEF MSWINDOWS}
        vtAnsiString:    ovc.Add(AnsiString(VAnsiString));
        vtWideString:    ovc.Add(WideString(VWideString));
      {$ENDIF MSWINDOWS}
      else
        raise Exception.Create ('TOmniValue.Create: invalid data type')
      end; //case
    end; //with
  end; //for i
  SetAsArray(ovc);
end; { TOmniValue.Create }

constructor TOmniValue.CreateNamed(const values: array of const;
  const cppDupConWorkaround: boolean);
var
  i   : integer;
  name: string;
  ovc : TOmniValueContainer;
begin
  ovc := TOmniValueContainer.Create;
  Assert(not Odd(Low(values)));
  name := '';
  for i := Low(values) to High(values) do begin
    with values[i] do begin
      if not Odd(i) then
        case VType of
          vtVariant:       name := string(VVariant^);
        {$IFDEF UNICODE}
          vtUnicodeString: name := string(VUnicodeString);
        {$ENDIF UNICODE}
        {$IFNDEF NEXTGEN}
          vtChar:          name := string(VChar);
          vtString:        name := string(VString^);
          vtPChar:         name := string(StrPasA(VPChar));
        {$ENDIF NEXTGEN}
        {$IFDEF MSWINDOWS}
          vtAnsiString:    name := string(VAnsiString);
          vtWideString:    name := WideString(VWideString);
        {$ENDIF MSWINDOWS}
        else
          raise Exception.Create ('TOmniValue.CreateNamed: invalid name type')
        end //case
      else
        case VType of
          vtInteger:       ovc.Add(VInteger, name);
          vtBoolean:       ovc.Add(VBoolean, name);
          vtExtended:      ovc.Add(VExtended^, name);
          vtPointer:       ovc.Add(VPointer, name);
          vtCurrency:      ovc.Add(VCurrency^, name);
          vtVariant:       ovc.Add(VVariant^, name);
          vtObject:        ovc.Add(VObject, name);
          vtInterface:     ovc.Add(IInterface(VInterface), name);
          vtInt64:         ovc.Add(VInt64^, name);
        {$IFDEF UNICODE}
          vtUnicodeString: ovc.Add(string(VUnicodeString), name);
        {$ENDIF UNICODE}
        {$IFNDEF NEXTGEN}
          vtChar:          ovc.Add(string(VChar), name);
          vtString:        ovc.Add(string(VString^), name);
          vtPChar:         ovc.Add(string(StrPasA(VPChar)), name);
        {$ENDIF NEXTGEN}
        {$IFDEF MSWINDOWS}
          vtAnsiString:    ovc.Add(AnsiString(VAnsiString), name);
          vtWideString:    ovc.Add(WideString(VWideString), name);
        {$ENDIF MSWINDOWS}
        else
          raise Exception.Create ('TOmniValue.CreateNamed: invalid data type')
        end; //case
    end; //with
  end; //for i
  SetAsArray(ovc);
end; { TOmniValue.CreateNamed }

{$IFDEF OTL_Generics}
{$IFDEF OTL_HasArrayOfT}
function TOmniValue.ToArray<T>: TArray<T>;
var
  iItem: integer;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  SetLength(Result, TOmniValueContainer(ovData).Count);
  for iItem := 0 to TOmniValueContainer(ovData).Count - 1 do
    Result[iItem] := TOmniValueContainer(ovData)[iItem].CastTo<T>;
end; { TOmniValue.ToArray }
{$ENDIF OTL_HasArrayOfT}

function TOmniValue.ToRecord<T>: T;
begin
  Result := TOmniRecordWrapper<T>(CastToRecord.Value).Value;
end; { TOmniValue.ToRecord }

function TOmniValue.CastTo<T>: T;
var
  ds      : integer;
  maxValue: uint64;
  ti      : PTypeInfo;
begin
  ds := 0;
  ti := System.TypeInfo(T);
  if assigned(ti) then
    if (ti = System.TypeInfo(byte)) or (ti = System.TypeInfo(shortint)) then
      ds := 1
    else if (ti = System.TypeInfo(word)) or (ti = System.TypeInfo(smallint)) then
      ds := 2
    else
      ds := TOmniValue_DataSize[ti^.Kind];
  if ds = 0 then begin // complicated stuff
    if ti.Kind = tkRecord then
      Result := TOmniRecordWrapper<T>(CastToRecord.Value).Value
    else
      {$IFDEF OTL_ERTTI}
      Result := AsTValue.AsType<T>
      {$ELSE}
      raise Exception.Create('Only casting to simple types is supported in Delphi 2009')
      {$ENDIF OTL_ERTTI}
  end
  else begin // simple types
    if ds < 8 then begin
      maxValue := uint64($FF) SHL ((ds-1) * 8);
      if ovData > maxValue then
        raise EOmniValueConv.CreateFmt('Value %d is too big to fit into %s', [ovData, ti^.Name]);
    end;
    Move(ovData, Result, ds);
  end;
end; { TOmniValue.CastTo }

class function TOmniValue.CastFrom<T>(const value: T): TOmniValue;
var
  data: int64;
  ds  : integer;
  ti  : PTypeInfo;
begin
  ds := 0;
  ti := System.TypeInfo(T);
  if assigned(ti) then begin
    if (ti = System.TypeInfo(byte)) or (ti = System.TypeInfo(shortint)) then
      ds := 1
    else if (ti = System.TypeInfo(word)) or (ti = System.TypeInfo(smallint)) then
      ds := 2
    else
      ds := TOmniValue_DataSize[ti^.Kind];
  end;
  if ds = 0 then begin // complicated stuff
    if ti^.Kind = tkRecord then
      Result.SetAsRecord(CreateAutoDestroyObject(TOmniRecordWrapper<T>.Create(value)))
    else
      {$IFDEF OTL_ERTTI}
      Result.AsTValue := TValue.From<T>(value)
      {$ELSE}
      raise Exception.Create('Only casting from simple types is supported in Delphi 2009')
      {$ENDIF OTL_ERTTI}
  end
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

{$IFDEF OTL_HasArrayOfT}
class function TOmniValue.FromArray<T>(const values: TArray<T>): TOmniValue;
var
  ovc  : TOmniValueContainer;
  value: T;
begin
  ovc := TOmniValueContainer.Create;
  for value in values do
    ovc.Add(TOmniValue.CastFrom<T>(value));
  Result.SetAsArray(ovc);
end; { TOmniValue.FromArray }
{$ENDIF OTL_HasArrayOfT}

class function TOmniValue.FromRecord<T>(const value: T): TOmniValue;
begin
  Result.SetAsRecord(CreateAutoDestroyObject(TOmniRecordWrapper<T>.Create(value)));
end; { TOmniValue.FromRecord<T> }

{$IF CompilerVersion > 20}
function TOmniValue.ToObject<T>: T;
begin
  {$IF CompilerVersion <= 21}
  Result := T(AsObject as TClass(T));
  {$ELSE}
  Result := AsObject as T;
  {$IFEND}
end; { TOmniValue.ToObject<T> }
{$IFEND}

{$IF CompilerVersion > 20}
function TOmniValue.CastToObject<T>: T;
begin
  Result := T(AsObject);
 end; { TOmniValue.CastToObject<T> }
{$IFEND}
{$ENDIF OTL_Generics}

procedure TOmniValue.Clear;
begin
  ovData := 0;
  ovIntf := nil;
  ovType := ovtNull;
end; { TOmniValue.Clear }

procedure TOmniValue.ClearIntf;
begin
  if pointer(ovIntf) <> nil then
    ovIntf := nil;
end; { TOmniValue.ClearIntf }

function TOmniValue.GetAsArray: TOmniValueContainer;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := TOmniValueContainer(ovData);
end; { TOmniValue.GetAsArray }

function TOmniValue.GetAsArrayItem(const name: string): TOmniValue;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := TOmniValueContainer(ovData)[name];
end; { TOmniValue.GetAsArrayItem }

{$IF CompilerVersion >= 19}//D2007 has problems understanding this overload
function TOmniValue.GetAsArrayItem(const param: TOmniValue): TOmniValue;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := TOmniValueContainer(ovData)[param];
end; { TOmniValue.GetAsArrayItem }
{$IFEND}

function TOmniValue.GetAsArrayItemOV(const param: TOmniValue): TOmniValue;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := TOmniValueContainer(ovData)[param];
end; { TOmniValue.GetAsArrayItemOV }

function TOmniValue.HasArrayItem(idx: integer): boolean;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := (idx >= 0) and (idx < TOmniValueContainer(ovData).Count);
end; { TOmniValue.HasArrayItem }

function TOmniValue.HasArrayItem(const name: string): boolean;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := (TOmniValueContainer(ovData).IndexOf(name) >= 0);
end; { TOmniValue.HasArrayItem }

function TOmniValue.HasArrayItem(const param: TOmniValue): boolean;
begin
  if param.IsInteger then
    Result := HasArrayItem(param.AsInteger)
  else if param.IsString then
    Result := HasArrayItem(param.AsString)
  else
    raise Exception.Create('TOmniValue contains neither an integer, string nor array');
end; { TOmniValue.HasArrayItem }

function TOmniValue.GetAsArrayItem(idx: integer): TOmniValue;
begin
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  Result := TOmniValueContainer(ovData)[idx];
end; { TOmniValue.GetAsArrayItem }

{$IFDEF MSWINDOWS}
function TOmniValue.CastToAnsiString: AnsiString;
begin
  if not TryCastToAnsiString(Result) then
    raise Exception.Create('TOmniValue cannot be converted to AnsiString');
end; { TOmniValue.CastToAnsiString }

function TOmniValue.CastToAnsiStringDef(const defValue: AnsiString): AnsiString;
begin
  if not TryCastToAnsiString(Result) then
    Result := defValue;
end; { TOmniValue.CastToAnsiStringDef }
{$ENDIF}

function TOmniValue.CastToBoolean: boolean;
begin
  if not TryCastToBoolean(Result) then
    raise Exception.Create('TOmniValue cannot be converted to boolean');
end; { TOmniValue.CastToBoolean }

function TOmniValue.CastToBooleanDef(defValue: boolean): boolean;
begin
  if not TryCastToBoolean(Result) then
    Result := defValue;
end; { TOmniValue.CastToBooleanDef }

function TOmniValue.CastToCardinal: cardinal;
begin
  Result := AsInt64;
end; { TOmniValue.CastToCardinal }

function TOmniValue.CastToCardinalDef(defValue: cardinal): cardinal;
begin
  if not TryCastToCardinal(Result) then
    Result := defValue;
end; { TOmniValue.CastToCardinalDef }

function TOmniValue.CastToDateTime: TDateTime;
begin
  if not TryCastToDateTime(Result) then
    raise Exception.Create('TOmniValue cannot be converted to TDateTime');
end; { TOmniValue.CastToDateTime }

function TOmniValue.CastToDateTimeDef(defValue: TDateTime): TDateTime;
begin
  if not TryCastToDateTime(Result) then
    Result := defValue;
end; { TOmniValue.CastToDateTimeDef }

function TOmniValue.CastToDouble: Double;
begin
  if not TryCastToDouble(Result) then
    raise Exception.Create('TOmniValue cannot be converted to double');
end; { TOmniValue.CastToDouble }

function TOmniValue.CastToDoubleDef(defValue: Double): Double;
begin
  if not TryCastToDouble(Result) then
    Result := defValue;
end; { TOmniValue.CastToDoubleDef }

function TOmniValue.CastToException: Exception;
begin
  if not TryCastToException(Result) then
    raise Exception.Create('TOmniValue cannot be converted to exception');
end; { TOmniValue.CastToException }

function TOmniValue.CastToExceptionDef(defValue: Exception): Exception;
begin
  if not TryCastToException(Result) then
    Result := defValue;
end; { TOmniValue.CastToExceptionDef }

function TOmniValue.CastToExtended: Extended;
begin
  if not TryCastToExtended(Result) then
    raise Exception.Create('TOmniValue cannot be converted to extended');
end; { TOmniValue.CastToExtended }

function TOmniValue.CastToExtendedDef(defValue: Extended): Extended;
begin
  if not TryCastToExtended(Result) then
    Result := defValue;
end; { TOmniValue.CastToExtendedDef }

function TOmniValue.CastToInt64: int64;
begin
  if not TryCastToInt64(Result) then
    raise Exception.Create('TOmniValue cannot be converted to int64');
end; { TOmniValue.CastToInt64 }

function TOmniValue.CastToInt64Def(defValue: int64): int64;
begin
  if not TryCastToInt64(Result) then
    Result := defValue;
end; { TOmniValue.CastToInt64Def }

function TOmniValue.CastToInteger: integer;
begin
  Result := AsInt64;
end; { TOmniValue.CastToInteger }

function TOmniValue.CastToIntegerDef(defValue: integer): integer;
begin
  if not TryCastToInteger(Result) then
    Result := defValue;
end; { TOmniValue.CastToIntegerDef }

function TOmniValue.CastToInterface: IInterface;
begin
  if not TryCastToInterface(Result) then
    raise Exception.Create('TOmniValue cannot be converted to interface');
end; { TOmniValue.CastToInterface }

function TOmniValue.CastToInterfaceDef(const defValue: IInterface): IInterface;
begin
  if not TryCastToInterface(Result) then
    Result := defValue;
end; { TOmniValue.CastToInterfaceDef }

function TOmniValue.CastToObject: TObject;
begin
  if not TryCastToObject(Result) then
    raise Exception.Create('TOmniValue cannot be converted to object');
end; { TOmniValue.CastToObject }

function TOmniValue.CastToObjectDef(defValue: TObject): TObject;
begin
  if not TryCastToObject(Result) then
    Result := defValue;
end; { TOmniValue.CastToObjectDef }

function TOmniValue.CastToPointer: pointer;
begin
  if not TryCastToPointer(Result) then
    raise Exception.Create('TOmniValue cannot be converted to pointer');
end; { TOmniValue.CastToPointer }

function TOmniValue.CastToPointerDef(defValue: pointer): pointer;
begin
  if not TryCastToPointer(Result) then
    Result := defValue;
end; { TOmniValue.CastToPointerDef }

function TOmniValue.CastToRecord: IOmniAutoDestroyObject;
begin
  case ovType of
    ovtRecord: Result := IOmniAutoDestroyObject(ovIntf);
    else raise Exception.Create('TOmniValue cannot be converted to record');
  end;
end; { TOmniValue.CastToRecord }

function TOmniValue.CastToString: string;
begin
  if not TryCastToString(Result) then
    raise Exception.Create('TOmniValue cannot be converted to string');
end; { TOmniValue.CastToString }

function TOmniValue.CastToStringDef(const defValue: string): string;
begin
  if not TryCastToString(Result) then
    Result := defValue;
end; { TOmniValue.CastToStringDef }

{$IFDEF OTL_ERTTI}
function TOmniValue.GetArrayFromTValue(const value: TValue): TOmniValueContainer;
var
  ov: TOmniValue;
  idxItem: Integer;
begin
  Result := TOmniValueContainer.Create;
  for idxItem := 0 to value.GetArrayLength-1 do
  begin
    ov.AsTValue := value.GetArrayElement(idxItem);
    Result.Add(ov);
  end;
end; { TOmniValue.GetArrayFromTValue }

function TOmniValue.GetTValueFromArray(const a: TOmniValueContainer): TValue;
var
  vItem: TValue;
  arrItems: TArray<TValue>;
  idxItem: Integer;
  typInfo: PTypeInfo;
begin
  if a.Count = 0 then
    Exit(TValue.Empty);

  SetLength(arrItems, a.Count);
  for idxItem := 0 to a.Count-1 do
  begin
    vItem := a[idxItem].AsTValue;
    arrItems[idxItem] := vItem;
  end;

  case a[0].ovType of
    ovtBoolean: typInfo := TypeInfo(TArray<Boolean>);
    ovtInteger: typInfo := TypeInfo(TArray<Integer>);
    ovtDouble: typInfo := TypeInfo(TArray<Double>);
    ovtObject: typInfo := TypeInfo(TArray<TObject>);
    ovtPointer: typInfo := TypeInfo(TArray<Pointer>);
    ovtDateTime: typInfo := TypeInfo(TArray<TDateTime>);
    ovtException: typInfo := TypeInfo(TArray<Exception>);
    ovtExtended: typInfo := TypeInfo(TArray<Extended>);
    ovtString: typInfo := TypeInfo(TArray<string>);
    ovtInterface: typInfo := TypeInfo(TArray<IInterface>);
    ovtVariant: typInfo := TypeInfo(TArray<Variant>);
  {$IFDEF MSWINDOWS}
    ovtAnsiString: typInfo := TypeInfo(TArray<AnsiString>);
    ovtWideString: typInfo := TypeInfo(TArray<WideString>);
  {$ENDIF MSWINDOWS}
  else
    typInfo := TypeInfo(TArray<Pointer>);
  end;

  Result := TValue.FromArray(typInfo, arrItems);
end; { TOmniValue.GetTValueFromArray }

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
  {$IFDEF MSWINDOWS}
    ovtAnsiString:
      Result := string(AsAnsiString);
    ovtWideString:
      Result := AsWideString;
  {$ENDIF}
    ovtString:
      Result := AsString;
    ovtObject:
      Result := AsObject;
    ovtOwnedObject:
      Result := AsOwnedObject;
    ovtException:
      Result := AsException;
    ovtInterface:
      Result := TValue.From<IInterface>(AsInterface);
    ovtVariant:
      Result := TValue.FromVariant(AsVariant);
    ovtPointer:
      Result := AsPointer;
    ovtArray:
      Result := GetTValueFromArray(AsArray);
  end;
end; { TOmniValue.GetAsTValue }
{$ENDIF OTL_ERRTI}

function TOmniValue.CastToVariant: Variant;
begin
  if not TryCastToVariant(Result) then
    raise Exception.Create('TOmniValue cannot be converted to variant');
end; { TOmniValue.CastToVariant }

function TOmniValue.CastToVariantDef(defValue: Variant): Variant;
begin
  if not TryCastToVariant(Result) then
    Result := defValue;
end; { TOmniValue.CastToVariantDef }

{$IFDEF MSWINDOWS}
function TOmniValue.CastToWideString: WideString;
begin
  if not TryCastToWideString(Result) then
    raise Exception.Create('TOmniValue cannot be converted to WideString');
end; { TOmniValue.CastToWideString }

function TOmniValue.CastToWideStringDef(defValue: WideString): WideString;
begin
  if not TryCastToWideString(Result) then
    Result := defValue;
end; { TOmniValue.CastToWideStringDef }

function TOmniValue.IsAnsiString: boolean;
begin
  Result := (ovType = ovtAnsiString);
end; { TOmniValue.IsAnsiString }
{$ENDIF}

function TOmniValue.IsArray: boolean;
begin
  Result := (ovType = ovtArray);
end; { TOmniValue.IsArray }

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

function TOmniValue.IsDateTime: boolean;
begin
  Result := (ovType = ovtDateTime);
end; { TOmniValue.IsDateTime }

function TOmniValue.IsException: boolean;
begin
  Result := (ovType = ovtException);
end; { TOmniValue.IsException }

function TOmniValue.IsInteger: boolean;
begin
  Result := (ovType = ovtInteger);
end; { TOmniValue.IsInteger }

function TOmniValue.IsInterface: boolean;
begin
  Result := (ovType = ovtInterface);
end; { TOmniValue.IsInterface }

function TOmniValue.IsInterfacedType: boolean;
begin
  Result := ovType in [ovtInterface, ovtExtended, ovtString, ovtVariant, ovtArray, ovtRecord
                       {$IFDEF MSWINDOWS}, ovtWideString, ovtAnsiString {$ENDIF}];
end; { TOmniValue.IsInterfacedType }

function TOmniValue.IsObject: boolean;
begin
  Result := (ovType = ovtObject);
end; { TOmniValue.IsObject }

function TOmniValue.IsOwnedObject: boolean;
begin
  Result := (ovType = ovtOwnedObject);
end; { TOmniValue.IsOwnedObject }

function TOmniValue.IsPointer: boolean;
begin
  Result := (ovType = ovtPointer);
end; { TOmniValue.IsPointer }

function TOmniValue.IsRecord: boolean;
begin
  Result := (ovType = ovtRecord);
end; { TOmniValue.IsRecord }

function TOmniValue.IsString: boolean;
begin
  Result := (ovType = ovtString);
end; { TOmniValue.IsString }

function TOmniValue.IsVariant: boolean;
begin
  Result := (ovType = ovtVariant);
end; { TOmniValue.IsVariant }

{$IFDEF MSWINDOWS}
function TOmniValue.IsWideString: boolean;
begin
  Result := (ovType = ovtWideString);
end; { TOmniValue.IsWideString }
{$ENDIF}

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

class procedure TOmniValue._RemoveWarnings;
var
  a   : integer;
  intf: IOmniAutoDestroyObject;
  ov  : TOmniValue;
begin
  a := 0;
  if a = (a + 1) then begin
    ov := ov.GetAsArrayItem('');
    ov := ov.GetAsArrayItemOV(ov);
    ov.SetAsArrayItem('', 0);
    ov.SetAsArrayItemOV(ov, 0);
    intf := ov.CastToRecord;
    ov.SetAsRecord(intf);
    {$IF CompilerVersion >= 19}
    ov := ov.GetAsArrayItem(ov);
    ov.SetAsArrayItem(ov, 0);
    {$IFEND}
  end;
end; { TOmniValue._RemoveWarnings }

{$IFDEF MSWINDOWS}
procedure TOmniValue.SetAsAnsiString(const value: AnsiString);
begin
  ovIntf := TOmniAnsiStringData.Create(value);
  ovType := ovtAnsiString;
end; { TOmniValue.SetAsAnsiString }
{$ENDIF}

procedure TOmniValue.SetAsArray(value: TOmniValueContainer);
begin
  ovType := ovtArray;
  ovIntf := CreateAutoDestroyObject(value);
  ovData := int64(value);
end; { TOmniValue.SetAsArray }

procedure TOmniValue.SetAsArrayItem(idx: integer; const value: TOmniValue);
begin
  if IsEmpty then
    SetAsArray(TOmniValueContainer.Create);
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  TOmniValueContainer(ovData)[idx] := value;
end; { TOmniValue.SetAsArrayItem }

procedure TOmniValue.SetAsArrayItem(const name: string; const value: TOmniValue);
begin
  if IsEmpty then
    SetAsArray(TOmniValueContainer.Create);
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  TOmniValueContainer(ovData)[name] := value;
end; { TOmniValue.SetAsArrayItem }

{$IF CompilerVersion >= 19}//D2007 has problems understanding this overload
procedure TOmniValue.SetAsArrayItem(const param, value: TOmniValue);
begin
  if IsEmpty then
    SetAsArray(TOmniValueContainer.Create);
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  TOmniValueContainer(ovData)[param] := value;
end; { TOmniValue.SetAsArrayItem }
{$IFEND}

procedure TOmniValue.SetAsArrayItemOV(const param, value: TOmniValue);
begin
  if IsEmpty then
    SetAsArray(TOmniValueContainer.Create);
  if not IsArray then
    raise Exception.Create('TOmniValue does not contain an array');
  TOmniValueContainer(ovData)[param] := value;
end; { TOmniValue.SetAsArrayItemOV }

{ TOmniValue.SetAsArrayItem }

procedure TOmniValue.SetAsBoolean(const value: boolean);
begin
  ClearIntf;
  PByte(@ovData)^ := Ord(value);
  ovType := ovtBoolean;
end; { TOmniValue.SetAsBoolean }

procedure TOmniValue.SetAsCardinal(const value: cardinal);
begin
  AsInt64 := value;
end; { TOmniValue.SetAsCardinal }

procedure TOmniValue.SetAsDouble(value: Double);
begin
  ClearIntf;
  PDouble(@ovData)^ := value;
  ovType := ovtDouble;
end; { TOmniValue.SetAsDouble }

procedure TOmniValue.SetAsDateTime(value: TDateTime);
begin
  ClearIntf;
  PDouble(@ovData)^ := value;
  ovType := ovtDateTime;
end; { TOmniValue.SetAsDateTime }

procedure TOmniValue.SetAsException(value: Exception);
begin
  ClearIntf;
  PInt64(@ovData)^ := int64(value);
  ovType := ovtException
end; { TOmniValue.SetAsException }

procedure TOmniValue.SetAsExtended(value: Extended);
begin
  ovIntf := TOmniExtendedData.Create(value);
  ovType := ovtExtended;
end; { TOmniValue.SetAsExtended }

procedure TOmniValue.SetAsInt64(const value: int64);
begin
  ClearIntf;
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
  ClearIntf;
  PInt64(@ovData)^ := int64(value);
  ovType := ovtObject;
end; { TOmniValue.SetAsObject }

procedure TOmniValue.SetAsOwnedObject(const value: TObject);
begin
  ovType := ovtOwnedObject;
  ovIntf := CreateAutoDestroyObject(value);
  ovData := int64(value);
end;

procedure TOmniValue.SetAsPointer(const value: pointer);
begin
  ClearIntf;
  PInt64(@ovData)^ := int64(value);
  ovType := ovtPointer;
end; { TOmniValue.SetAsPointer }

procedure TOmniValue.SetAsRecord(const intf: IOmniAutoDestroyObject);
begin
  ovIntf := intf;
  ovType := ovtRecord;
end; { TOmniValue.SetAsRecord }

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
      Assert(SizeOf(pointer) <= SizeOf(int64));
      AsPointer := pointer(value.GetReferenceToRawData^);
    end;
    tkArray,
    tkDynArray:
      SetAsArray(GetArrayFromTValue(value));
    else
      raise Exception.CreateFmt('TValue of type %s cannot be converted to TOmniValue',
        [GetEnumName(TypeInfo(TTypeKind), Ord(value.Kind))]);
  end;
end; { TOmniValue.SetAsTValue }
{$ENDIF OTL_ERTTI}

procedure TOmniValue.SetAsVariant(const value: Variant);
begin
  ovIntf := TOmniVariantData.Create(value);
  ovType := ovtVariant;
end; { TOmniValue.SetAsVariant }

{$IFDEF MSWINDOWS}
procedure TOmniValue.SetAsWideString(const value: WideString);
begin
  ovIntf := TOmniWideStringData.Create(value);
  ovType := ovtWideString;
end; { TOmniValue.SetAsWideString }
{$ENDIF}

procedure TOmniValue.SetOwnsObject(const value: boolean);
var
  obj: TObject;
begin
  if value then begin
    if not IsObject then
      raise Exception.Create('TOmniValue does not contain an object');
    SetAsOwnedObject(TObject(ovData));
  end
  else begin
    if not IsOwnedObject then
      raise Exception.Create('TOmniValue does not contain an owned object');
    obj := (ovIntf as IOmniAutoDestroyObject).Detach;
    SetAsObject(obj);
  end;
end; { TOmniValue.SetOwnsObject }

{$IFDEF MSWINDOWS}
function TOmniValue.TryCastToAnsiString(var value: AnsiString): boolean;
begin
  Result := true;
  case ovType of
    ovtNull:       value := '';
    ovtBoolean:    value := AnsiString(BoolToStr(AsBoolean, true));
    ovtInteger:    value := AnsiString(IntToStr(ovData));
    ovtDouble,
    ovtDateTime,
    ovtExtended:   value := AnsiString(FloatToStr(AsExtended));
    ovtAnsiString: value := (ovIntf as IOmniAnsiStringData).Value;
    ovtString:     value := AnsiString((ovIntf as IOmniStringData).Value);
    ovtWideString: value := AnsiString((ovIntf as IOmniWideStringData).Value);
    ovtVariant:    value := AnsiString(AsVariant);
    else Result := false;
  end;
end; { TOmniValue.TryCastToAnsiString }
{$ENDIF}

function TOmniValue.TryCastToBoolean(var value: boolean): boolean;
begin
  if ovType <> ovtBoolean then
    Result := false
  else begin
    value := PByte(@ovData)^ <> 0;
    Result := true;
  end;
end; { TOmniValue.TryCastToBoolean }

function TOmniValue.TryCastToCardinal(var value: cardinal): boolean;
var
  val64: int64;
begin
  Result := TryCastToInt64(val64);
  if Result then
    value := val64;
end; { TOmniValue.TryCastToCardinal }

function TOmniValue.TryCastToDateTime(var value: TDateTime): boolean;
begin
  Result := true;
  case ovType of
    ovtDouble,
    ovtDateTime: value := PDouble(@ovData)^;
    ovtExtended: value := (ovIntf as IOmniExtendedData).Value;
    ovtNull:     value := 0;
    else Result := false;
  end;
end; { TOmniValue.TryCastToDateTime }

function TOmniValue.TryCastToDouble(var value: Double): boolean;
begin
  Result := true;
  case ovType of
    ovtInteger,
    ovtNull:     value := AsInt64;
    ovtDouble,
    ovtDateTime: value := PDouble(@ovData)^;
    ovtExtended: value := (ovIntf as IOmniExtendedData).Value;
    else Result := false;
  end;
end; { TOmniValue.TryCastToDouble }

function TOmniValue.TryCastToException(var value: Exception): boolean;
begin
  Result := true;
  if IsException or
     (IsObject and AsObject.InheritsFrom(Exception))
  then
    value := Exception(ovData)
  else if IsEmpty then
    value := nil
  else
    Result := false;
end; { TOmniValue.TryCastToException }

function TOmniValue.TryCastToExtended(var value: Extended): boolean;
begin
  Result := true;
  case ovType of
    ovtInteger,
    ovtNull:     value := AsInt64;
    ovtDouble,
    ovtDateTime: value := PDouble(@ovData)^;
    ovtExtended: value  := (ovIntf as IOmniExtendedData).Value;
    else Result := false;
  end;
end; { TOmniValue.TryCastToExtended }

function TOmniValue.TryCastToInt64(var value: int64): boolean;
begin
  Result := true;
  case ovType of
    ovtInteger: value := ovData;
    ovtNull:    value := 0;
    ovtVariant: value := integer(AsVariant);
    else Result := false;
  end;
end; { TOmniValue.TryGetAsInt64 }

function TOmniValue.TryCastToInteger(var value: integer): boolean;
var
  val64: int64;
begin
  Result := TryCastToInt64(val64);
  if Result then
    value := val64;
end; { TOmniValue.TryCastToInteger }

function TOmniValue.TryCastToInterface(var value: IInterface): boolean;
begin
  Result := true;
  case ovType of
    ovtInterface: value := ovIntf;
    ovtNull:      value := nil;
    else Result := false;
  end;
end; { TOmniValue.TryCastToInterface }

function TOmniValue.TryCastToObject(var value: TObject): boolean;
begin
  Result := true;
  case ovType of
    ovtObject,
    ovtException:   value := TObject(ovData);
    ovtOwnedObject: value := (ovIntf as IOmniAutoDestroyObject).Value;
    ovtNull:        value := nil;
    else Result := false;
  end;
end; { TOmniValue.TryCastToObject }

function TOmniValue.TryCastToPointer(var value: pointer): boolean;
begin
  Result := true;
  case ovType of
    ovtPointer,
    ovtObject,
    ovtException:   value := pointer(ovData);
    ovtOwnedObject: value := pointer((ovIntf as IOmniAutoDestroyObject).Value);
    ovtNull:        value := nil;
    else Result := false;
  end;
end; { TOmniValue.TryCastToPointer }

function TOmniValue.TryCastToString(var value: string): boolean;
begin
  Result := true;
  case ovType of
    ovtNull:       value := '';
    ovtBoolean:    value := BoolToStr(AsBoolean, true);
    ovtInteger:    value := IntToStr(ovData);
    ovtDouble,
    ovtDateTime,
    ovtExtended:   value := FloatToStr(AsExtended);
  {$IFDEF MSWINDOWS}
    ovtAnsiString: value := string((ovIntf as IOmniAnsiStringData).Value);
    ovtWideString: value := (ovIntf as IOmniWideStringData).Value;
  {$ENDIF}
    ovtString:     value := (ovIntf as IOmniStringData).Value;
    ovtVariant:    value := string(AsVariant);
    else Result := false;
  end;
end; { TOmniValue.TryCastToString }

function TOmniValue.TryCastToVariant(var value: Variant): boolean;
begin
  Result := true;
  case ovType of
    ovtVariant: value := (ovIntf as IOmniVariantData).Value;
    ovtNull:    value := Variants.Null;
    else Result := false;
  end;
end; { TOmniValue.TryCastToVariant }

{$IFDEF MSWINDOWS}
function TOmniValue.TryCastToWideString(var value: WideString): boolean;
var
  str: string;
begin
  Result := true;
  case ovType of
    ovtWideString: value := (ovIntf as IOmniWideStringData).Value;
    ovtVariant:    value := WideString(AsVariant);
    else begin
      Result := TryCastToString(str);
      if Result then
        value := str;
    end;
  end;
end; { TOmniValue.TryCastToWideString }
{$ENDIF}

procedure TOmniValue._AddRef;
begin
  if IsInterfacedType and assigned(ovIntf) then
    ovIntf._AddRef;
end; { TOmniValue._AddRef }

procedure TOmniValue._Release;
begin
  if IsInterfacedType and assigned(ovIntf) then
    ovIntf._Release;
end; { TOmniValue._Release }

procedure TOmniValue._ReleaseAndClear;
begin
  if IsInterfacedType then begin
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

{$IFDEF MSWINDOWS}
{$IFDEF UNICODE}
class operator TOmniValue.Implicit(const a: AnsiString): TOmniValue;
begin
  Result.AsAnsiString := a;
end; { TOmniValue.Implicit }
{$ENDIF UNICODE}
{$ENDIF}

class operator TOmniValue.Implicit(const a: boolean): TOmniValue;
begin
  Result.AsBoolean := a;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: Double): TOmniValue;
begin
  Result.AsDouble := a;
end; { TOmniValue.Implicit }

{$IFDEF OTL_TOmniValueImplicitDateTime}
class operator TOmniValue.Implicit(const a: TDateTime): TOmniValue;
begin
  Result.AsDateTime := a;
end; { TOmniValue.Implicit }
{$ENDIF OTL_TOmniValueImplicitDateTime}

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

class operator TOmniValue.Implicit(const a: Exception): TOmniValue;
begin
  Result.AsException := a;
end; { TOmniValue.Implicit }

{$IFDEF MSWINDOWS}
{$IFDEF UNICODE}
class operator TOmniValue.Implicit(const a: TOmniValue): AnsiString;
begin
  Result := a.AsAnsiString;
end; { TOmniValue.Implicit }
{$ENDIF UNICODE}

class operator TOmniValue.Implicit(const a: TOmniValue): WideString;
begin
  Result := a.AsWideString;
end; { TOmniValue.Implicit }
{$ENDIF}

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

{$IFDEF OTL_TOmniValueImplicitDateTime}
class operator TOmniValue.Implicit(const a: TOmniValue): TDateTime;
begin
  Result := a.AsDateTime;
end; { TOmniValue.Implicit }
{$ENDIF OTL_TOmniValueImplicitDateTime}

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

class operator TOmniValue.Implicit(const a: TOmniValue): Exception;
begin
  Result := a.AsException;
end; { TOmniValue.Implicit }

class operator TOmniValue.Implicit(const a: TOmniValue): string;
begin
  Result := a.AsString;
end; { TOmniValue.Implicit }

{$IFDEF MSWINDOWS}
class operator TOmniValue.Implicit(const a: WideString): TOmniValue;
begin
  Result.AsWideString := a;
end; { TOmniValue.Implicit }
{$ENDIF}

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

{ TOmniValueObj }

constructor TOmniValueObj.Create(const value: TOmniValue);
begin
  inherited Create;
  FValue := value;
end; { TOmniValueObj.Create }

{ TOmniWaitableValue }

constructor TOmniWaitableValue.Create;
begin
  FEvent := TEvent.Create(false);
  FValue := TOmniValue.Null;
end; { TOmniWaitableValue.Create }

destructor TOmniWaitableValue.Destroy;
begin
  FreeAndNil( FEvent);
  inherited
end; { TOmniWaitableValue.Destroy }

{$IFDEF MSWINDOWS}
function TOmniWaitableValue.GetHandle: THandle;
begin
  Result := FEvent.Handle
end; { TOmniWaitableValue.GetHandle }
{$ENDIF}

function TOmniWaitableValue.GetEvent: TEvent;
begin
  Result := FEvent
end; { TOmniWaitableValue.GetEvent }

function TOmniWaitableValue.GetValue: TOmniValue;
begin
  Result := FValue;
end; { TOmniWaitableValue.GetValue }

procedure TOmniWaitableValue.Reset;
begin
  FEvent.ResetEvent
end; { TOmniWaitableValue.Reset }

procedure TOmniWaitableValue.Signal;
begin
  FEvent.SetEvent
end; { TOmniWaitableValue.Signal }

procedure TOmniWaitableValue.Signal(const data: TOmniValue);
begin
  FValue := data;
  Signal;
end; { TOmniWaitableValue.Signal }

function TOmniWaitableValue.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := FEvent.WaitFor(maxWait_ms) = wrSignaled;
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

{ TOmniAnsiStringData }

{$IFDEF MSWINDOWS}
constructor TOmniAnsiStringData.Create(const value: AnsiString);
begin
  inherited Create;
  osdValue := value;
end; { TOmniAnsiStringData.Create }

function TOmniAnsiStringData.GetValue: AnsiString;
begin
  Result := osdValue;
end; { TOmniAnsiStringData.GetValue }

procedure TOmniAnsiStringData.SetValue(const value: AnsiString);
begin
  osdValue := value;
end; { TOmniAnsiStringData.SetValue }

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
{$ENDIF}

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
{$IFNDEF MSWINDOWS}
var
  i: integer;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
// TODO 1 -oPrimoz Gabrijelcic : Port this to non-Windows
  Result := DSiAffinityMaskToString(Mask);
{$ELSE}
  for i := 1 to System.CPUCount do
    Result := result + 'P'
{$ENDIF !MSWINDOWS}
end; { TOmniAffinity.GetAsString }

function TOmniAffinity.GetCount: integer;
{$IFDEF MSWINDOWS}
var
  affMask: DWORD;
begin
// TODO 1 -oPrimoz Gabrijelcic : Did System.CPUCount exist in D2009?
  Result := 0;
  affMask := Mask;
  while affMask <> 0 do begin
    if Odd(affMask) then
      Inc(Result);
    affMask := affMask SHR 1;
  end;
{$ELSE}
begin
  Result := System.CPUCount;
{$ENDIF}
end;

function TOmniAffinity.GetCountPhysical: integer;
{$IFDEF MSWINDOWS}
var
  info: TSystemLogicalProcessorInformationArr;
  item: TSystemLogicalProcessorInformation;
  mask: DWORD;
begin
  if not DSiGetLogicalProcessorInfo(info) then
    Result := GetCount // running on pre-XP SP3 OS, just get the best approximation
  else begin
    Result := 0;
    mask := GetMask;
    for item in info do begin
      if (item.Relationship = RelationProcessorCore) and
         ((item.ProcessorMask AND mask) <> 0) then
      begin
        mask := mask AND NOT item.ProcessorMask;
        Inc(Result);
      end;
    end;
  end;
end;

{$ELSE}
begin
  Result := System.CPUCount;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
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
{$ENDIF}

procedure TOmniAffinity.SetAsString(const value: string);
begin
{$IFDEF MSWINDOWS}
  case oaTarget of
    atSystem:
      raise Exception.Create('TOmniAffinity.SetMask: Cannot modify system affinity mask.');
    atProcess:
      DSiSetProcessAffinity(value);
    atThread:
      DSiSetThreadAffinity(value);
  end;
{$ENDIF}
end; { TOmniAffinity.SetAsString }

procedure TOmniAffinity.SetCount(const value: integer);
{$IFDEF MSWINDOWS}
var
  affMask: string;
  numCore: integer;
  pCore  : integer;
  sysMask: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
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
{$ENDIF}
end; { TOmniAffinity.SetCount }

{$IFDEF MSWINDOWS}
procedure TOmniAffinity.SetMask(const value: DWORD);
begin
  AsString := DSiAffinityMaskToString(value);
end; { TOmniAffinity.SetMask }
{$ENDIF}

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

{$IFDEF MSWINDOWS}
function TOmniProcessEnvironment.GetMemory: TOmniProcessMemoryCounters;
begin
  if not DSiGetProcessMemory(Result) then
    FillChar(Result, SizeOf(Result), 0);
end; { TOmniProcessEnvironment.GetMemory }
{$ENDIF}

function TOmniProcessEnvironment.GetPriorityClass: TOmniProcessPriorityClass;
{$IFDEF MSWINDOWS}
var
  priority: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
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
{$ELSE}
  Result := pcNormal;
{$ENDIF}
end; { TOmniProcessEnvironment.GetPriorityClass }

{$IFDEF MSWINDOWS}
function TOmniProcessEnvironment.GetTimes: TOmniProcessTimes;
begin
  if not DSiGetProcessTimes(Result.CreationTime, Result.UserTime, Result.KernelTime) then
    FillChar(Result, SizeOf(Result), 0);
end; { TOmniProcessEnvironment.GetTimes }
{$ENDIF}

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

function TOmniThreadEnvironment.GetID: TThreadId;
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

procedure TOmniExecutable.SetAnonDelegate(const value: TProc);
begin
  oeDelegate := value;
  oeKind := oekDelegate;
end; { TOmniExecutable.SetAnonDelegate }

procedure TOmniExecutable.SetDelegate(const source);
begin
  oeKind := oekDelegate;
  AnonCopy(oeDelegate, source);
end; { TOmniExecutable.SetDelegate }

{$ENDIF OTL_Anonymous}

{ TOmniMessageID }

function TOmniMessageID.AsString: string;
begin
  case MessageType of
    mitInteger: Result := IntToStr(omidInteger);
    mitString:  Result := omidString;
    mitPointer: Result := Format('%p', [omidPointer]);
    else        raise Exception.CreateFmt('TOmniMessageID.AsString: Unexpected message type %d', [Ord(MessageType)]);
  end;
end; { TOmniMessageID.AsString }

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

function NextOid: int64;
begin
  {$IFDEF OTL_USE_ALIGN}
  Result := TInterlocked.Increment(OtlUID);
  {$ELSE}
  Result := OtlUID.Increment;
  {$ENDIF}
end; { NextOid }

{ TOmniAlignedInt32 }

procedure TOmniAlignedInt32.Initialize;
begin
  FAddr := PInteger((NativeInt(@FData) + 3) AND NOT 3);
end; { TOmniAlignedInt32.Initialize }

function TOmniAlignedInt32.Add(value: integer): integer;
begin
  {$IFDEF MSWINDOWS}
  Result := InterlockedExchangeAdd(Addr^, value);
  {$ELSE}
  Result := TInterlocked.Add(Addr^, value);
  {$ENDIF}
end; { TOmniAlignedInt32.Add }

function TOmniAlignedInt32.Addr: PInteger;
begin
  Initialize;
  Result := FAddr;
end; { TOmniAlignedInt32.Addr }

function TOmniAlignedInt32.CAS(oldValue, newValue: integer): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := InterlockedCompareExchange(Addr^, newValue, oldValue) = oldValue;
  {$ELSE}
  Result := TInterlocked.CompareExchange(Addr^, newValue, OldValue) = oldValue;
  {$ENDIF}
end; { TOmniAlignedInt32.CAS }

function TOmniAlignedInt32.Decrement: integer;
begin
  {$IFDEF MSWINDOWS}
  Result := InterlockedDecrement(Addr^);
  {$ELSE}
  Result := TInterlocked.Decrement(Addr^);
  {$ENDIF}
end; { TOmniAlignedInt32.Decrement }

function TOmniAlignedInt32.Decrement(value: integer): integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Subtract(value) - value;
  {$ELSE}
  Result := TInterlocked.Add(Addr^, -value);
  {$ENDIF}
end; { TOmniAlignedInt32.Decrement }

function TOmniAlignedInt32.GetValue: integer;
begin
  Result := FAddr^;
end; { TOmniAlignedInt32.GetValue }

function TOmniAlignedInt32.Increment: integer;
begin
  {$IFDEF MSWINDOWS}
  Result := InterlockedIncrement(Addr^);
  {$ELSE}
  Result := TInterlocked.Increment(Addr^);
  {$ENDIF}
end; { TOmniAlignedInt32.Increment }

function TOmniAlignedInt32.Increment(value: integer): integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Add(value) + value;
  {$ELSE}
  Result := TInterlocked.Add(Addr^, value);
  {$ENDIF}
end; { TOmniAlignedInt32.Increment }

procedure TOmniAlignedInt32.SetValue(value: integer);
begin
  Addr^ := value;
end; { TOmniAlignedInt32.SetValue }

function TOmniAlignedInt32.Subtract(value: integer): integer;
begin
  {$IFDEF MSWINDOWS}
  Result := InterlockedExchangeAdd(Addr^, -value);
  {$ELSE}
  Result := TInterlocked.Add(Addr^, -value);
  {$ENDIF}
end; { TOmniAlignedInt32.Subtract }

class operator TOmniAlignedInt32.Add(const ai: TOmniAlignedInt32; i: integer): cardinal;
begin
  Result := cardinal(int64(ai.Value) + i);
end; { TOmniAlignedInt32.Add }

class operator TOmniAlignedInt32.Equal(const ai: TOmniAlignedInt32; i: integer): boolean;
begin
  Result := (ai.Value = i);
end; { TOmniAlignedInt32.Equal }

class operator TOmniAlignedInt32.GreaterThan(const ai: TOmniAlignedInt32; i: integer): boolean;
begin
  Result := (ai.Value > i);
end; { TOmniAlignedInt32.GreaterThan }

class operator TOmniAlignedInt32.GreaterThanOrEqual(const ai: TOmniAlignedInt32; i: integer):
  boolean;
begin
  Result := (ai.Value >= i);
end; { TOmniAlignedInt32.GreaterThanOrEqual }

class operator TOmniAlignedInt32.Implicit(const ai: TOmniAlignedInt32): PInteger;
begin
  Result := ai.Addr;
end; { TOmniAlignedInt32.Implicit }

class operator TOmniAlignedInt32.Implicit(const ai: TOmniAlignedInt32): cardinal;
begin
  Result := ai.Value;
end; { TOmniAlignedInt32.Implicit }

class operator TOmniAlignedInt32.Implicit(const ai: TOmniAlignedInt32): integer;
begin
  Result := integer(ai.Value);
end; { TOmniAlignedInt32.Implicit }

class operator TOmniAlignedInt32.LessThan(const ai: TOmniAlignedInt32; i: integer): boolean;
begin
  Result := (ai.Value < i);
end; { TOmniAlignedInt32.LessThan }

class operator TOmniAlignedInt32.LessThanOrEqual(const ai: TOmniAlignedInt32; i: integer):
  boolean;
begin
  Result := (ai.Value <= i);
end; { TOmniAlignedInt32.LessThanOrEqual }

class operator TOmniAlignedInt32.NotEqual(const ai: TOmniAlignedInt32; i: integer): boolean;
begin
  Result := (ai.Value <> i);
end; { TOmniAlignedInt32.NotEqual }

class operator TOmniAlignedInt32.Subtract(ai: TOmniAlignedInt32; i: integer): cardinal;
begin
  Result := cardinal(int64(ai.Value) - i);
end; { TOmniAlignedInt32.Subtract }

{ TOmniAlignedInt64 }

procedure TOmniAlignedInt64.Initialize;
begin
  Assert(SizeOf(pointer) = SizeOf(NativeInt));
  FAddr := PInt64((NativeInt(@FData) + 7) AND NOT 7);
end; { TOmniAlignedInt64.Initialize }

function TOmniAlignedInt64.Add(value: int64): int64;
begin
  {$IFDEF MSWINDOWS}
  Result := DSiInterlockedExchangeAdd64(Addr^, value);
  {$ELSE}
  Result := TInterlocked.Add(Addr^, value);
  {$ENDIF}
end; { TOmniAlignedInt64.Add }

function TOmniAlignedInt64.Addr: PInt64;
begin
  Initialize;
  Result := FAddr;
end; { TOmniAlignedInt64.Addr }

function TOmniAlignedInt64.CAS(oldValue, newValue: int64): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := DSiInterlockedCompareExchange64(Addr, newValue, oldValue) = oldValue;
  {$ELSE}
  Result := TInterlocked.CompareExchange(Addr^, newValue, OldValue) = oldValue;
  {$ENDIF}
end; { TOmniAlignedInt64.CAS }

function TOmniAlignedInt64.Decrement: int64;
begin
  {$IFDEF MSWINDOWS}
  Result := DSiInterlockedDecrement64(Addr^);
  {$ELSE}
  Result := TInterlocked.Decrement(Addr^)
  {$ENDIF}
end; { TOmniAlignedInt64.Decrement }

function TOmniAlignedInt64.Decrement(value: int64): int64;
begin
  {$IFDEF MSWINDOWS}
  Result := Subtract(value) - value;
  {$ELSE}
  Result := TInterlocked.Add(Addr^, -value);
  {$ENDIF}
end; { TOmniAlignedInt64.Decrement }

function TOmniAlignedInt64.GetValue: int64;
begin
  Result := Addr^;
end; { TOmniAlignedInt64.GetValue }

function TOmniAlignedInt64.Increment: int64;
begin
  {$IFDEF MSWINDOWS}
  Result := DSiInterlockedIncrement64(Addr^);
  {$ELSE}
  Result := TInterlocked.Increment(Addr^);
  {$ENDIF}
end; { TOmniAlignedInt64.Increment }

function TOmniAlignedInt64.Increment(value: int64): int64;
begin
  {$IFDEF MSWINDOWS}
  Result := Add(value) + value;
  {$ELSE}
  Result := TInterlocked.Add(Addr^, value);
  {$ENDIF}
end; { TOmniAlignedInt64.Increment }

procedure TOmniAlignedInt64.SetValue(value: int64);
begin
  Addr^ := value;
end; { TOmniAlignedInt64.SetValue }

function TOmniAlignedInt64.Subtract(value: int64): int64;
begin
  {$IFDEF MSWINDOWS}
  Result := DSiInterlockedExchangeAdd64(Addr^, -value);
  {$ELSE}
  Result := TInterlocked.Add(Addr^, -value);
  {$ENDIF}
end; { TOmniAlignedInt64.Subtract }

{ TInterlockedEx }

class function TInterlockedEx.CompareExchange(var Target: NativeInt; Value: NativeInt; Comparand: NativeInt): NativeInt;
begin
  {$IFDEF CPUX64}
    Result := TInterlocked.CompareExchange(Int64  (Target), Int64  (Value), Int64  (Comparand));
  {$ELSE}
    {$IF CompilerVersion < 19}//D2007
    Result := InterlockedCompareExchange(Target, Value, Comparand);
    {$ELSE}
    Result := TInterlocked.CompareExchange(Integer(Target), Integer(Value), Integer(Comparand));
    {$IFEND}
  {$ENDIF}
end; { TInterlockedEx.CompareExchange }

class function TInterlockedEx.Add(var Target: NativeInt; Increment: NativeInt): NativeInt;
begin
  {$IFDEF CPUX64}
    Result := TInterlocked.Add(Int64  (Target), Int64  (Increment));
  {$ELSE}
    {$IF CompilerVersion < 19}//D2007
    Result := InterlockedExchangeAdd(Target, Increment);
    {$ELSE}
    Result := TInterlocked.Add(Integer(Target), Integer(Increment));
    {$IFEND}
  {$ENDIF}
end; { TInterlockedEx.Add }


initialization
  Assert(SizeOf(TObject) = {$IFDEF CPUX64}SizeOf(NativeUInt){$ELSE}SizeOf(cardinal){$ENDIF}); //in VarToObj
  GEnvironment := TOmniEnvironment.Create;
  FillChar(TOmniValue_DataSize, SizeOf(TOmniValue_DataSize), 0);
  TOmniValue_DataSize[tkInteger] := SizeOf(integer);
  TOmniValue_DataSize[tkClass]   := SizeOf(TObject);
  TOmniValue_DataSize[tkMethod]  := SizeOf(TMethod);
  TOmniValue_DataSize[tkInt64]   := SizeOf(int64);
  {$IFDEF OTL_HasTkPointer}
  TOmniValue_DataSize[tkPointer] := SizeOf(pointer);
  {$ENDIF OTL_HasTkPointer}
  TOmniValue._RemoveWarnings;
end.
