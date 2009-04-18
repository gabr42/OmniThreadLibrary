///<summary>Stuff common for the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2009, Primoz Gabrijelcic
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
///   Last modification : 2009-04-18
///   Version           : 1.04
///</para><para>
///   History:
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
             ovtObject, ovtInterface, ovtVariant, ovtWideString);
    function  GetAsBoolean: boolean; inline;
    function  GetAsCardinal: cardinal; inline;
    function  GetAsDouble: Double;
    function  GetAsExtended: Extended;
    function  GetAsInt64: int64; inline;
    function  GetAsInteger: integer; inline;
    function  GetAsInterface: IInterface; inline;
    function  GetAsObject: TObject; inline;
    function  GetAsString: string;
    function  GetAsVariant: Variant;
    function  GetAsVariantArr(idx: integer): Variant;
    function  GetAsWideString: WideString;
    procedure SetAsBoolean(const value: boolean); inline;
    procedure SetAsCardinal(const value: cardinal); inline;
    procedure SetAsDouble(value: Double); inline;
    procedure SetAsExtended(value: Extended);
    procedure SetAsInt64(const value: int64); inline;
    procedure SetAsInteger(const value: integer); inline;
    procedure SetAsInterface(const value: IInterface); inline;
    procedure SetAsObject(const value: TObject); inline;
    procedure SetAsString(const value: string);
    procedure SetAsVariant(const value: Variant);
    procedure SetAsWideString(const value: WideString);
  public
    procedure Clear; inline;
    function  IsBoolean: boolean; inline;
    function  IsEmpty: boolean; inline;
    function  IsFloating: boolean; inline;
    function  IsInterface: boolean; inline;
    function  IsObject: boolean; inline;
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
    class operator Implicit(const a: Extended): TOmniValue;
    class operator Implicit(const a: integer): TOmniValue; inline;
    class operator Implicit(const a: int64): TOmniValue; inline;
    class operator Implicit(const a: string): TOmniValue;
    class operator Implicit(const a: IInterface): TOmniValue;
    class operator Implicit(const a: TObject): TOmniValue; inline;
    class operator Implicit(const a: TOmniValue): int64; inline;
    class operator Implicit(const a: TOmniValue): TObject; inline;
    class operator Implicit(const a: TOmniValue): Double; inline;
    class operator Implicit(const a: TOmniValue): Extended; inline;
    class operator Implicit(const a: TOmniValue): string; inline;
    class operator Implicit(const a: TOmniValue): integer; inline;
    class operator Implicit(const a: TOmniValue): WideString; inline;
    class operator Implicit(const a: TOmniValue): boolean; inline;
    class operator Implicit(const a: TOmniValue): IInterface;
    class operator Implicit(const a: WideString): TOmniValue;
    class operator Implicit(const a: Variant): TOmniValue; inline;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsCardinal: cardinal read GetAsCardinal write SetAsCardinal;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInt64: int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property AsObject: TObject read GetAsObject write SetAsObject;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsVariantArr[idx: integer]: Variant read GetAsVariantArr; default;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end; { TOmniValue }

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
  end; { Implicit }

  TOmniValueContainer = class
  strict private
    ovcCanModify: boolean;
    ovcNames    : TStringList;
    ovcValues   : array of TOmniValue;
  strict protected
    procedure Clear;
    procedure Grow;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const paramValue: TOmniValue; paramName: string = '');
    procedure Assign(const parameters: array of TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    function ParamByIdx(paramIdx: integer): TOmniValue;
    function ParamByName(const paramName: string): TOmniValue;
  end; { TOmniValueContainer }

  IOmniMonitorParams = interface
    function  GetLParam: integer;
    function  GetMessage: cardinal;
    function  GetWindow: THandle;
    function  GetWParam: integer;
  //
    property Window: THandle read GetWindow;
    property Msg: cardinal read GetMessage;
    property WParam: integer read GetWParam;
    property LParam: integer read GetLParam;
  end; { IOmniMonitorParams }

  IOmniMonitorSupport = interface ['{6D5F1191-9E4A-4DD5-99D8-694C95B0DE90}']
    function  GetMonitor: IOmniMonitorParams;
  //
    procedure Notify; overload;
    procedure Notify(obj: TObject); overload; 
    procedure RemoveMonitor;
    procedure SetMonitor(const monitor: IOmniMonitorParams);
    property Monitor: IOmniMonitorParams read GetMonitor;
  end; { IOmniMonitorSupport }

  IOmniCounter = interface ['{3A73CCF3-EDC5-484F-8459-532B8C715E3C}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  //
    function  Increment: integer;
    function  Decrement: integer;
    property Value: integer read GetValue write SetValue;
  end; { IOmniCounter }

  TInterfaceDictionaryPair = class
  strict private
    idpKey  : int64;
    idpValue: IInterface;
  protected
    procedure SetKeyValue(const key: int64; const value: IInterface);
  public
    property Key: int64 read idpKey;
    property Value: IInterface read idpValue;
  end; { TInterfaceDictionaryPair }

  IInterfaceDictionaryEnumerator = interface
    function  GetCurrent: TInterfaceDictionaryPair;
    function  MoveNext: boolean;
    property Current: TInterfaceDictionaryPair read GetCurrent;
  end; { IInterfaceDictionaryEnumerator }

  IInterfaceDictionary = interface ['{619FCCF3-E810-4DCF-B902-1EF1A5A72DB5}']
    function  GetEnumerator: IInterfaceDictionaryEnumerator;
  //
    procedure Add(const key: int64; const value: IInterface);
    procedure Clear;
    function  Count: integer; 
    procedure Remove(const key: int64);
    function  ValueOf(const key: int64): IInterface;
  end; { IInterfaceHash }

  function CreateCounter(initialValue: integer = 0): IOmniCounter;

  function CreateOmniMonitorParams(window: THandle; msg: cardinal;
    wParam, lParam: integer): IOmniMonitorParams;
  function CreateOmniMonitorSupport: IOmniMonitorSupport;

  function CreateInterfaceDictionary: IInterfaceDictionary;

  procedure SetThreadName(const name: string);

  function VarToObj(const v: Variant): TObject; inline;

var
  OtlUID: TGp8AlignedInt64;

implementation

uses
  DSiWin32,
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

  // TODO 1 -oPrimoz Gabrijelcic : This one goes out?
  TOmniMonitorParams = class(TInterfacedObject, IOmniMonitorParams)
  strict private
    ompLParam : integer;
    ompMessage: cardinal;
    ompWindow : THandle;
    ompWParam : integer;
  protected
    function  GetLParam: integer;
    function  GetMessage: cardinal;
    function  GetWindow: THandle;
    function  GetWParam: integer;
  public
    constructor Create(window: THandle; msg: cardinal; wParam, lParam: integer);
    destructor Destroy; override;
    property LParam: integer read GetLParam;
    property Msg: cardinal read GetMessage;
    property Window: THandle read GetWindow;
    property WParam: integer read GetWParam;
  end; { TOmniMonitorParams }

  TOmniMonitorSupport = class(TInterfacedObject, IOmniMonitorSupport)
  strict private
    omsMonitor: IOmniMonitorParams;
  protected
    function  GetMonitor: IOmniMonitorParams;
  public
    procedure Notify; overload;
    procedure Notify(obj: TObject); overload;
    procedure RemoveMonitor;
    procedure SetMonitor(const monitor: IOmniMonitorParams);
    property Monitor: IOmniMonitorParams read GetMonitor;
  end; { TOmniMonitorSupport }

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next : PHashItem;
    Key  : int64;
    Value: IInterface;
  end; { THashItem }

  TBucketArray = array of PHashItem;
  PBucketArray = ^TBucketArray;

  TInterfaceDictionaryEnumerator = class(TInterfacedObject, IInterfaceDictionaryEnumerator)
  strict private
    ideBuckets  : PBucketArray;
    ideBucketIdx: integer;
    ideCurrent  : PHashItem;
    ideItem     : PHashItem;
    idePair     : TInterfaceDictionaryPair;
  public
    constructor Create(buckets: PBucketArray);
    destructor  Destroy; override;
    function  GetCurrent: TInterfaceDictionaryPair;
    function  MoveNext: boolean;
    property Current: TInterfaceDictionaryPair read GetCurrent;
  end; { IInterfaceDictionaryEnumerator }

  TInterfaceDictionary = class(TInterfacedObject, IInterfaceDictionary)
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
    function  GetEnumerator: IInterfaceDictionaryEnumerator;
    procedure Remove(const key: int64);
    function  ValueOf(const key: int64): IInterface;
  end; { TInterfaceDictionary }

{ exports }

function CreateCounter(initialValue: integer): IOmniCounter;
begin
  Result := TOmniCounter.Create(initialValue);
end; { CreateCounter }

function CreateOmniMonitorParams(window: THandle; msg: cardinal;
  wParam, lParam: integer): IOmniMonitorParams;
begin
  Result := TOmniMonitorParams.Create(window, msg, wParam, lParam);
end; { CreateOmniMonitorParams }

function CreateOmniMonitorSupport: IOmniMonitorSupport;
begin
  Result := TOmniMonitorSupport.Create;
end; { CreateOmniMonitorSupport }

function CreateInterfaceDictionary: IInterfaceDictionary;
begin
  Result := TInterfaceDictionary.Create;
end; { CreateInterfaceDictionary }

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
  ansiName := AnsiString(name);
  threadNameInfo.FType := $1000;
  threadNameInfo.FName := PAnsiChar(ansiName);
  threadNameInfo.FThreadID := $FFFFFFFF;
  threadNameInfo.FFlags := 0;
  try
    RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
  except {ignore} end;
end; { SetThreadName }

function VarToObj(const v: Variant): TObject;
begin
  Result := TObject(cardinal(v));
end; { VarToObj }

{ TOmniValueContainer }

constructor TOmniValueContainer.Create;
begin
  inherited Create;
  ovcNames := TStringList.Create;
  ovcCanModify := true;
end; { TOmniValueContainer.Create }

destructor TOmniValueContainer.Destroy;
begin
  FreeAndNil(ovcNames);
  inherited Destroy;
end; { TOmniValueContainer.Destroy }

procedure TOmniValueContainer.Add(const paramValue: TOmniValue; paramName: string = '');
var
  idxParam: integer;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Locked');
  if paramName = '' then
    paramName := IntToStr(ovcNames.Count);
  idxParam := ovcNames.IndexOf(paramName); 
  if idxParam < 0 then begin
    idxParam := ovcNames.Add(paramName);
    if ovcNames.Count > Length(ovcValues) then
      Grow;
  end;
  ovcValues[idxParam] := paramValue;
end; { TOmniValueContainer.Add }

procedure TOmniValueContainer.Assign(const parameters: array of TOmniValue);
var
  value: TOmniValue;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  Clear;
  SetLength(ovcValues, Length(parameters));
  for value in parameters do
    Add(value);
end; { TOmniValueContainer.Assign }

procedure TOmniValueContainer.Clear;
begin
  SetLength(ovcValues, 0);
  ovcNames.Clear;
end; { TOmniValueContainer.Clear }

procedure TOmniValueContainer.Grow;
var
  iValue   : integer;
  tmpValues: array of TOmniValue;
begin
  SetLength(tmpValues, Length(ovcValues));
  for iValue := 0 to High(ovcValues) - 1 do
    tmpValues[iValue] := ovcValues[iValue];
  SetLength(ovcValues, 2*Length(ovcValues)+1);
  for iValue := 0 to High(tmpValues) - 1 do
    ovcValues[iValue] := tmpValues[iValue];
end; { TOmniValueContainer.Grow }

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
  Result := ovcValues[ovcNames.IndexOf(paramName)];
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

{ TOmniMonitorSupport }

function TOmniMonitorSupport.GetMonitor: IOmniMonitorParams;
begin
  Result := omsMonitor;
end; { TOmniMonitorSupport.GetMonitor }

procedure TOmniMonitorSupport.Notify;
var
  params: IOmniMonitorParams;
begin
  params := GetMonitor;
  if assigned(params) then
    Win32Check(PostMessage(params.Window, params.Msg, params.WParam, params.LParam));
end; { TOmniMonitorSupport.Notify }

procedure TOmniMonitorSupport.Notify(obj: TObject);
var
  params: IOmniMonitorParams;
begin
  params := GetMonitor;
  if not (assigned(params) and
          PostMessage(params.Window, params.Msg, params.WParam, LParam(obj)))
  then
    FreeAndNil(obj);
end; { TOmniMonitorSupport.Notify }

procedure TOmniMonitorSupport.RemoveMonitor;
begin
  omsMonitor := nil;
end; { TOmniMonitorSupport.RemoveMonitor }

procedure TOmniMonitorSupport.SetMonitor(const monitor: IOmniMonitorParams);
begin
  omsMonitor := monitor;
end; { TOmniMonitorSupport.SetMonitor }

constructor TOmniMonitorParams.Create(window: THandle; msg: cardinal; wParam, lParam:
  integer);
begin
  ompMessage := msg;
  ompLParam := lParam;
  ompWParam := wParam;
  ompWindow := window;
end; { TOmniMonitorParams.Create }

destructor TOmniMonitorParams.Destroy;
begin
  inherited Destroy;
end;

function TOmniMonitorParams.GetLParam: integer;
begin
  Result := ompLParam;
end; { TOmniMonitorParams.GetLParam }

function TOmniMonitorParams.GetMessage: cardinal;
begin
  Result := ompMessage;
end; { TOmniMonitorParams.GetMessage }

function TOmniMonitorParams.GetWindow: THandle;
begin
  Result := ompWindow;
end; { TOmniMonitorParams.GetWindow }

function TOmniMonitorParams.GetWParam: integer;
begin
  Result := ompWParam;
end; { TOmniMonitorParams.GetWParam }

{ TInterfaceDictionaryPair }

procedure TInterfaceDictionaryPair.SetKeyValue(const key: int64; const value: IInterface);
begin
  idpKey := key;
  idpValue := value;
end; { TInterfaceDictionaryPair.SetKeyValue }

{ TInterfaceDictionaryEnumerator }

constructor TInterfaceDictionaryEnumerator.Create(buckets: PBucketArray);
begin
  ideBuckets := buckets;
  ideBucketIdx := Low(ideBuckets^);
  ideItem := nil;
  idePair := TInterfaceDictionaryPair.Create;
end; { TInterfaceDictionaryEnumerator.Create }

destructor TInterfaceDictionaryEnumerator.Destroy;
begin
  FreeAndNil(idePair);
  inherited Destroy;
end; { TInterfaceDictionaryEnumerator.Destroy }

function TInterfaceDictionaryEnumerator.GetCurrent: TInterfaceDictionaryPair;
begin
  idePair.SetKeyValue(ideCurrent^.Key, ideCurrent^.Value);
  Result := idePair;
end; { TInterfaceDictionaryEnumerator.GetCurrent }

function TInterfaceDictionaryEnumerator.MoveNext: boolean;
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
end; { TInterfaceDictionaryEnumerator.MoveNext }

{ TInterfaceHash }

constructor TInterfaceDictionary.Create;
begin
  inherited Create;
  Resize(1);
end; { TInterfaceDictionary.Create }

destructor TInterfaceDictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end; { TInterfaceDictionary.Destroy }

procedure TInterfaceDictionary.Add(const key: int64; const value: IInterface);
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
end; { TInterfaceDictionary.Add }

procedure TInterfaceDictionary.Clear;
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
end; { TInterfaceDictionary.Clear }

function TInterfaceDictionary.Count: integer;
begin
  Result := idCount;
end; { TInterfaceDictionary.Count }

function TInterfaceDictionary.Find(const key: int64): PPHashItem;
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
end; { TInterfaceDictionary.Find }

function TInterfaceDictionary.GetEnumerator: IInterfaceDictionaryEnumerator;
begin
  Result := TInterfaceDictionaryEnumerator.Create(@idBuckets);
end; { TInterfaceDictionary.GetEnumerator }

function TInterfaceDictionary.HashOf(const key: int64): integer;
begin
  Result := key mod Length(idBuckets);
end; { TInterfaceDictionary.HashOf }

procedure TInterfaceDictionary.Remove(const key: int64);
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
end; { TInterfaceDictionary.Remove }

procedure TInterfaceDictionary.Resize(size: Cardinal);
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
end; { TInterfaceDictionary.Resize }

function TInterfaceDictionary.ValueOf(const key: int64): IInterface;
var
  bucketHead: PHashItem;
begin
  bucketHead := Find(key)^;
  if bucketHead <> nil then
    Result := bucketHead^.Value
  else
    Result := nil;
end; { TInterfaceDictionary.ValueOf }

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
    Exception.Create('TOmniValue cannot be converted to boolean');
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
  if ovType <> ovtInteger then
    Exception.Create('TOmniValue cannot be converted to int64');
  Result := ovData;
end; { TOmniValue.GetAsInt64 }

function TOmniValue.GetAsInteger: integer;
begin
  Result := AsInt64;
end; { TOmniValue.GetAsInteger }

function TOmniValue.GetAsInterface: IInterface;
begin
  if ovType <> ovtInterface then
    Exception.Create('TOmniValue cannot be converted to interface');
  Result := ovIntf;
end; { TOmniValue.GetAsInterface }

function TOmniValue.GetAsObject: TObject;
begin
  if ovType <> ovtObject then
    Exception.Create('TOmniValue cannot be converted to object');
  Result := TObject(RawData^);
end; { TOmniValue.GetAsObject }

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
  if ovType <> ovtVariant then
    Exception.Create('TOmniValue cannot be converted to variant');
  Result := (ovIntf as IOmniVariantData).Value;
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

function TOmniValue.IsInterface: boolean;
begin
  Result := (ovType = ovtInterface);
end; { TOmniValue.IsInterface }

function TOmniValue.IsObject: boolean;
begin
  Result := (ovType = ovtObject);
end; { TOmniValue.IsObject }

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

initialization
  Assert(SizeOf(TObject) = SizeOf(cardinal));
  Assert(SizeOf(pointer) = SizeOf(cardinal));
  Assert(SizeOf(pointer) = 4);
end.
