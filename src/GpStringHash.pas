(*:Preallocated hasher.
   @author Primoz Gabrijelcic
   @desc <pre>
Copyright (c) 2017, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Tested with Delphi 2007. Should work with older versions, too.

   Author            : Primoz Gabrijelcic
   Creation date     : 2005-02-24
   Last modification : 2017-09-05
   Version           : 2.0a
</pre>*)(*
   History:
     2.0a: 2017-09-05
       - Fixed integer() casting in TGpStringObjectHash.
     2.0: 2017-03-31
       - Fixed pointer operations in 64-bit code.
     1.11a: 2015-10-04
       - Removed dependency on DSiWin32.
     1.11: 2012-02-06
       - TGpStringObjectHash.Find returns 'nil' in 'value' parameter if key is not found.
     1.10c: 2011-12-20
       - Fixed HashOf operation in 64-bit code.
     1.10b: 2011-10-25
       - Fixed bug in TGpStringInterfaceHash.Find implementation.
     1.10a: 2011-02-27
       - Fixed bugs in TGpStringInterfaceHash implementation.
     1.10: 2011-02-09
       - Implemented TGpStringInterfaceHash, a string-indexed hash of interfaces.
     1.09: 2010-09-27
       - Faster TGpStringDictionary enumerator, enumerates items in insertion order.
     1.08: 2010-09-25
       - Always define enumerators, they can be used explicitly in pre-2005 Delphi's
         (see ForEach method in each class for example).
       - Defined ForEach method in each enumerator-supporting class.
       - TGpStringHash enumerator returns (string, integer) pairs.
       - TGpStringObjectHash enumerator returns (string, TObject) pairs.
       - TGpStringDictionary enumerator returns (string, cardinal, int64) triplets.
     1.07: 2010-08-18
       - Use better hashing function written by Paul Hsieh (Delphi translation by Davy
         Landman).
     1.06c: 2010-07-18
       - HashOf hashes complete string in Unicode Delphis.
     1.06b: 2010-06-28
       - [lkessler] In TGpStringHash.Add, hash must be calculated *after* Grow is called.
     1.06a: 2010-04-09
       - Unicode fixes in TGpStringTable.
     1.06: 2010-03-04
       - String hashing function made public.
     1.05b: 2010-01-26
       - Fixed a bug in TGpStringTable.SetValue.
     1.05: 2008-11-09
       - Fixed a bug in TGpStringTable.Grow and TGpStringDictionary.Grow which caused
         random memory overwrites.
     1.05: 2008-11-08
       - Added function Count to TGpStringTable, TGpStringDictionary and TGpStringHash.
     1.04: 2008-10-20
       - Added TGpStringTable string storage.
       - Added TGpStringDictionary - a hash that uses TGpStringTable for string storage.
     1.03: 2008-10-04
       - Added support for growing.
     1.02: 2007-12-06
       - Much enhanced TGpStringHash.
       - TGpStringObjectHash class added.
     1.01: 2006-04-13
       - Added simplified constructor.
*)

unit GpStringHash;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17} //Delphi 2005 or newer
    {$DEFINE GpStringHash_Inline}
  {$IFEND}
{$ENDIF}

type
{$IFDEF ConditionalExpressions}
{$IF CompilerVersion <= 20} //D2009 or older
  NativeInt  = integer;
  NativeUInt = cardinal;
{$IFEND}
{$ENDIF}    

  ///<summary>Internal hash item representation.</summary>
  PGpHashItem = ^TGpHashItem;
  TGpHashItem = record
    Next : cardinal;
    Key  : string;
    Value: int64;
  end; { TGpHashItem }

  ///<summary>External (string, integer) hash item representation.</summary>
  TGpStringHashKV = class
  private
    kvKey  : string;
    kvValue: int64;
  public
    property Key: string read kvKey write kvKey;
    property Value: int64 read kvValue write kvValue;
  end; { TGpStringHashKV }

  TGpStringHash = class;

  TGpStringHashEnumerator = class
  private
    sheHash : TGpStringHash;
    sheIndex: cardinal;
    sheKV   : TGpStringHashKV;
  public
    constructor Create(stringHash: TGpStringHash);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringHashKV;          {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpStringHashKV read GetCurrent;
  end; { TGpStringHashEnumerator }

  TGpStringHashEnumMethod = procedure(item: TGpStringHashKV) of object;

  ///<summary>String-indexed hash of int64 items.</summary>
  ///<since>2005-02-24</since>
  TGpStringHash = class
  private
    shBuckets   : array of cardinal;
    shCanGrow   : boolean;
    shFirstEmpty: cardinal;
    shItems     : array of TGpHashItem;
    shNumBuckets: cardinal;
    shSize      : cardinal;
  protected
    function  FindBucket(const key: string): cardinal;
    function  GetHashItem(idxHashItem: cardinal): PGpHashItem;     {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  GetItems(const key: string): int64;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Grow;
    procedure SetItems(const key: string; const value: int64);     {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    property  HashItems[idxItem: cardinal]: PGpHashItem read GetHashItem;
  public
    constructor Create; overload;
    constructor Create(numItems: cardinal; canGrow: boolean = false); overload;
    constructor Create(numBuckets, numItems: cardinal; canGrow: boolean = false); overload;
    destructor  Destroy; override;
    procedure Add(const key: string; value: int64);
    function  Count: integer;                                      {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Find(const key: string; var value: int64): boolean;
    procedure ForEach(enumerator: TGpStringHashEnumMethod);
    function  GetEnumerator: TGpStringHashEnumerator;              {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  HasKey(const key: string): boolean;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Update(const key: string; value: int64);
    function  ValueOf(const key: string): int64;
    property Items[const key: string]: int64 read GetItems write SetItems; default;
  end; { TGpStringHash }

  ///<summary>External (string, object) hash item representation.</summary>
  TGpStringObjectHashKV = class
  private
    kvKey  : string;
    kvValue: TObject;
  public
    property Key: string read kvKey write kvKey;
    property Value: TObject read kvValue write kvValue;
  end; { TGpStringObjectHashKV }

  TGpStringObjectHashEnumerator = class
  private
    soheKV              : TGpStringObjectHashKV;
    soheStringEnumerator: TGpStringHashEnumerator;
  public
    constructor Create(stringHash: TGpStringHash);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringObjectHashKV;    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpStringObjectHashKV read GetCurrent;
  end; { TGpStringObjectHashEnumerator }

  TGpStringObjectHashEnumMethod = procedure(item: TGpStringObjectHashKV) of object;

  ///<summary>String-indexed hash of objects.
  ///    Redirects all operations to the internal TGpStringHash.</summary>
  ///<since>2007-12-06</since>
  TGpStringObjectHash = class
  private
    sohHash       : TGpStringHash;
    sohOwnsObjects: boolean;
  protected
    procedure FreeObject(item: TGpStringObjectHashKV);
    function  GetObjects(const key: string): TObject;
    procedure SetObjects(const key: string; const value: TObject);  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
  public
    constructor Create(numItems: cardinal; ownsObjects: boolean = true; canGrow: boolean = false); overload;
    constructor Create(numBuckets, numItems: cardinal; ownsObjects: boolean = true; canGrow: boolean = false); overload;
    destructor  Destroy; override;
    procedure Add(const key: string; value: TObject);               {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Count: integer;                                       {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Find(const key: string; var value: TObject): boolean; {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure ForEach(enumerator: TGpStringObjectHashEnumMethod);   
    function  GetEnumerator: TGpStringObjectHashEnumerator;         {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  HasKey(const key: string): boolean;                   {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Update(const key: string; value: TObject);
    function  ValueOf(const key: string): TObject;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    property Objects[const key: string]: TObject read GetObjects write SetObjects; default;
  end; { TGpStringObjectHash }

  ///<summary>External (string, Interface) hash item representation.</summary>
  TGpStringInterfaceHashKV = class
  private
    kvKey  : string;
    kvValue: IInterface;
  public
    property Key: string read kvKey write kvKey;
    property Value: IInterface read kvValue write kvValue;
  end; { TGpStringInterfaceHashKV }

  TGpStringInterfaceHashEnumerator = class
  private
    siheKV              : TGpStringInterfaceHashKV;
    siheStringEnumerator: TGpStringHashEnumerator;
  public
    constructor Create(stringHash: TGpStringHash);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringInterfaceHashKV; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpStringInterfaceHashKV read GetCurrent;
  end; { TGpStringObjectHashEnumerator }

  TGpStringInterfaceHashEnumMethod = procedure(item: TGpStringInterfaceHashKV) of object;

  ///<summary>String-indexed hash of interfaces.
  ///    Redirects all operations to the internal TGpStringHash.</summary>
  ///<since>2011-02-09</since>
  TGpStringInterfaceHash = class
  private
    sihHash: TGpStringHash;
  protected
    function  GetInterfaces(const key: string): IInterface;
    procedure ReleaseInterface(item: TGpStringInterfaceHashKV);
    procedure SetInterfaces(const key: string; const value: IInterface);
  public
    constructor Create(numItems: cardinal; canGrow: boolean = false); overload;
    constructor Create(numBuckets, numItems: cardinal; canGrow: boolean = false); overload;
    destructor  Destroy; override;
    procedure Add(const key: string; const value: IInterface);         {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Count: integer;                                          {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Find(const key: string; var value: IInterface): boolean; {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure ForEach(enumerator: TGpStringInterfaceHashEnumMethod);
    function  GetEnumerator: TGpStringInterfaceHashEnumerator;         {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  HasKey(const key: string): boolean;                      {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Update(const key: string; const value: IInterface);
    function  ValueOf(const key: string): IInterface;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    property Interfaces[const key: string]: IInterface read GetInterfaces write SetInterfaces; default;
  end; { TGpStringInterfaceHash }

  ///<summary>External (string, int64) hash table item representation.</summary>
  TGpStringTableKV = class
  private
    kvKey  : string;
    kvValue: int64;                           
  public
    property Key: string read kvKey write kvKey;
    property Value: int64 read kvValue write kvValue;
  end; { TGpStringTableKV }

  TGpStringTableEnumerator = class
  private
    steCurrent: TGpStringTableKV;
    steTable  : PByte;
    steTail   : pointer;
  public
    constructor Create(pTable, pTail: pointer);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringTableKV;
    function  MoveNext: boolean;
    property Current: TGpStringTableKV read GetCurrent;
  end; { TGpStringTableEnumerator }

  TGpStringTableEnumMethod = procedure(item: TGpStringTableKV) of object;

  ///<summary>A growable table of strings that doesn't support a Delete operation.
  ///  When string is inserted, its position (relative to the table start) is never
  ///  changed and can be used as a index. TGpStringDictionary uses this class for the
  ///  underlying data structure.
  ///<para>Data layout: [string length:32][string:N][value:8].</para>
  ///</summary>
  ///<since>2008-10-20</since>
  TGpStringTable = class
  private
    stCanGrow : boolean;
    stData    : pointer;
    stDataSize: cardinal;
    stDataTail: PByte;
  protected
    procedure CheckPointer(pData: pointer; dataSize: cardinal);
    function  GetKey(index: cardinal): string;
    function  GetValue(index: cardinal): int64;
    procedure Grow(requiredSize: cardinal);
    procedure SetValue(index: cardinal; const value: int64);
  public
    constructor Create(initialSize: cardinal; canGrow: boolean = true);
    destructor  Destroy; override;
    function  Add(const key: string; value: int64): cardinal;
    procedure ForEach(enumerator: TGpStringTableEnumMethod);
    procedure Get(index: cardinal; var key: string; var value: int64);
    function  GetEnumerator: TGpStringTableEnumerator;        {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    property Key[index: cardinal]: string read GetKey;
    property Value[index: cardinal]: int64 read GetValue write SetValue;
  end; { TGpStringTable }

  ///<summary>External (string, int64) hash table item representation.</summary>
  TGpStringDictionaryKV = class
  private
    kvIndex: cardinal;
    kvKey  : string;
    kvValue: int64;
  public
    property Index: Cardinal read kvIndex write kvIndex;
    property Key: string read kvKey write kvKey;
    property Value: int64 read kvValue write kvValue;
  end; { TGpStringTableKV }

  TBucketArr = array of cardinal;
  PBucketArr = ^TBucketArr;

  PGpTableHashItem = ^TGpTableHashItem;
  TGpTableHashItem = record
    Index: cardinal;
    Chain: cardinal;
    Next : cardinal;
  end; { TGpHashItem }

  TGpTableHashItemArr = array of TGpTableHashItem;
  PGpTableHashItemArr = ^TGpTableHashItemArr;

  TGpStringDictionaryEnumerator = class
  private
    steBucket     : PGpTableHashItem;
    steBucketIdx  : integer;
    steCurrent    : TGpStringDictionaryKV;
    steFirstBucket: cardinal;
    steItems      : PGpTableHashItemArr;
    steStringTable: TGpStringTable;
  public
    constructor Create(pItems: PGpTableHashItemArr; firstBucket: cardinal; stringTable:
      TGpStringTable);
    destructor  Destroy; override;
    function  GetCurrent: TGpStringDictionaryKV;
    function  MoveNext: boolean;
    property Current: TGpStringDictionaryKV read GetCurrent;
  end; { TGpStringDictionaryEnumerator }

  TGpStringDictionaryEnumMethod = procedure(item: TGpStringDictionaryKV) of object;

  ///<summary>A dictionary of <string, cardinal, int64> triplets.</summary>
  ///<since>2008-10-20</since>
  TGpStringDictionary = class
  private
    sdBuckets    : TBucketArr;
    sdCanGrow    : boolean;
    sdFirstEmpty : cardinal;
    sdItems      : TGpTableHashItemArr;
    sdNumBuckets : cardinal;
    sdSize       : cardinal;
    sdStringTable: TGpStringTable;
    sdTail       : PGpTableHashItem;
    sdUsedBuckets: cardinal;
  protected
    function  FindBucket(const key: string): cardinal;
    function  GetHashItem(idxHashItem: cardinal): PGpTableHashItem;{$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  GetItems(const key: string): int64;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Grow;
    procedure LinkAtTail(bucket: PGpTableHashItem; bucketIdx: cardinal);
    procedure SetItems(const key: string; const value: int64);     {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    property  HashItems[idxItem: cardinal]: PGpTableHashItem read GetHashItem;
  public
    constructor Create(initialArraySize: cardinal);
    destructor  Destroy; override;
    function  Add(const key: string; value: int64): cardinal;
    function  Count: integer;                                      {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  Find(const key: string; var index: cardinal; var value: int64): boolean;
    procedure ForEach(enumerator: TGpStringDictionaryEnumMethod);
    procedure Get(index: cardinal; var key: string; var value: int64);
    function  GetEnumerator: TGpStringDictionaryEnumerator;        {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    function  HasKey(const key: string): boolean;                  {$IFDEF GpStringHash_Inline}inline;{$ENDIF GpStringHash_Inline}
    procedure Update(const key: string; value: int64);
    function  ValueOf(const key: string): int64;
    property Items[const key: string]: int64 read GetItems write SetItems; default;
  end; { TGpStringDictionary }

function GetGoodHashSize(dataSize: cardinal): cardinal;
function HashOf(const key: string): cardinal; overload;
function HashOf(data: pointer; dataLength: integer): cardinal; overload;

implementation

uses
  SysUtils;

const
  //List of good hash table sizes, taken from
  //http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
  CGpGoodHashSizes: array [5..30] of cardinal = (53, 97, 193, 389, 769, 1543, 3079, 6151,
    12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469,
    12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);
    
{$R-,Q-}

function GetGoodHashSize(dataSize: cardinal): cardinal;
var
  iHashSize: integer;
  upper    : cardinal;
begin
  upper := 1 SHL Low(CGpGoodHashSizes);
  for iHashSize := Low(CGpGoodHashSizes) to High(CGpGoodHashSizes) do begin
    Result := CGpGoodHashSizes[iHashSize];
    if dataSize <= upper then
      Exit;
    upper := 2*upper;
  end;
  raise Exception.CreateFmt('GetGoodHashSize: Only data sizes up to %d are supported',
    [upper div 2]);
end; { GetGoodHashSize }

function HashOf(data: pointer; dataLength: integer): cardinal;
// Pascal translation of the SuperFastHash function by Paul Hsieh
// more info: http://www.azillionmonkeys.com/qed/hash.html
// Translation by: Davy Landman
// No warranties, but have fun :)
var
  TempPart: longword;
  RemainingBytes: integer;
begin
  if not Assigned(data) or (dataLength <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Result := dataLength;
  RemainingBytes := dataLength and 3;
  dataLength := dataLength shr 2; // div 4, so var name is not correct anymore..
  // main loop
  while dataLength > 0 do
  begin
    inc(Result, PWord(data)^);
    TempPart := (PWord(Pointer(NativeUInt(data)+2))^ shl 11) xor Result;
    Result := (Result shl 16) xor TempPart;
    data := Pointer(NativeUInt(data) + 4);
    inc(Result, Result shr 11);
    dec(dataLength);
  end;
  // end case
  if RemainingBytes = 3 then
  begin
    inc(Result, PWord(data)^);
    Result := Result xor (Result shl 16);
    Result := Result xor (PByte(Pointer(NativeUInt(data)+2))^ shl 18);
    inc(Result, Result shr 11);
  end
  else if RemainingBytes = 2 then
  begin
    inc(Result, PWord(data)^);
    Result := Result xor (Result shl 11);
    inc(Result, Result shr 17);
  end
  else if RemainingBytes = 1 then
  begin
    inc(Result, PByte(data)^);
    Result := Result xor (Result shl 10);
    inc(Result, Result shr 1);
  end;
  // avalance
  Result := Result xor (Result shl 3);
  inc(Result, Result shr 5);
  Result := Result xor (Result shl 4);
  inc(Result, Result shr 17);
  Result := Result xor (Result shl 25);
  inc(Result, Result shr 6);
end; { HashOf }

function HashOf(const key: string): cardinal;
begin
  if Length(key) = 0 then
    Result := 0
  else
    Result := HashOf(@key[1], Length(key) * SizeOf(Char));
end; { HashOf }

{ TGpStringHashEnumerator }

constructor TGpStringHashEnumerator.Create(stringHash: TGpStringHash);
begin
  inherited Create;
  sheIndex := 0;
  sheHash := stringHash;
  sheKV := TGpStringHashKV.Create;
end; { TGpStringHashEnumerator.Create }

destructor TGpStringHashEnumerator.Destroy;
begin
  FreeAndNil(sheKV);
  inherited;
end; { TGpStringHashEnumerator.Destroy }

function TGpStringHashEnumerator.GetCurrent: TGpStringHashKV;
begin
  sheKV.Key := sheHash.shItems[sheIndex].Key;
  sheKV.Value := sheHash.shItems[sheIndex].Value;
  Result := sheKV;
end; { TGpStringHashEnumerator.GetCurrent }

function TGpStringHashEnumerator.MoveNext: boolean;
begin
  Result := sheIndex < (sheHash.shFirstEmpty - 1);
  if Result then
    Inc(sheIndex);
end; { TGpStringHashEnumerator.MoveNext }

{ TGpStringHash }

constructor TGpStringHash.Create(numItems: cardinal; canGrow: boolean);
begin
  Create(GetGoodHashSize(numItems), numItems, canGrow);
end; { TGpStringHash.Create }

constructor TGpStringHash.Create(numBuckets, numItems: cardinal; canGrow: boolean);
begin
  inherited Create;
  SetLength(shBuckets, numBuckets);
  SetLength(shItems, numItems + 1);
  shItems[0].Value := -1; // sentinel for the ValueOf operation
  shSize := numItems;
  shNumBuckets := numBuckets;
  shFirstEmpty := 1;
  shCanGrow := canGrow;
end; { TGpStringHash.Create }

constructor TGpStringHash.Create;
begin
  raise Exception.Create('Don''t use this constructor');
end; { TGpStringHash.Create }

destructor TGpStringHash.Destroy;
begin
  SetLength(shItems, 0);
  SetLength(shBuckets, 0);
  inherited Destroy;
end; { TGpStringHash.Destroy }

procedure TGpStringHash.Add(const key: string; value: int64);
var
  bucket: PGpHashItem;
  hash  : cardinal;
begin
  if shFirstEmpty > shSize then
    if shCanGrow then
      Grow
    else
      raise Exception.Create('TGpStringHash.Add: Maximum size reached');
  hash := HashOf(key) mod shNumBuckets;
  bucket := @(shItems[shFirstEmpty]); // point to an empty slot in the pre-allocated array
  bucket^.Key := key;
  bucket^.Value := value;
  bucket^.Next := shBuckets[hash];
  shBuckets[hash] := shFirstEmpty;
  Inc(shFirstEmpty);
end; { TGpStringHash.Add }

function TGpStringHash.Count: integer;
begin
  Result := shFirstEmpty - 1;
end; { TGpStringHash.Count }

function TGpStringHash.Find(const key: string; var value: int64): boolean;
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then begin
    value := shItems[bucket].Value;
    Result := true;
  end
  else
    Result := false;
end; { TGpStringHash.Find }

function TGpStringHash.GetEnumerator: TGpStringHashEnumerator;
begin
  Result := TGpStringHashEnumerator.Create(Self);
end; { TGpStringHash.GetEnumerator }

function TGpStringHash.FindBucket(const key: string): cardinal;
begin
  Result := shBuckets[HashOf(key) mod shNumBuckets];
  while (Result <> 0) and (shItems[Result].Key <> key) do
    Result := shItems[Result].Next;
end; { TGpStringHash.FindBucket }

procedure TGpStringHash.ForEach(enumerator: TGpStringHashEnumMethod);
var
  enum: TGpStringHashEnumerator;
begin
  enum := GetEnumerator;
  try
    while enum.MoveNext do
      enumerator(enum.Current);
  finally FreeAndNil(enum); end;
end; { TGpStringHash.ForEach }

function TGpStringHash.GetHashItem(idxHashItem: cardinal): PGpHashItem;
begin
  if idxHashItem > 0 then
    Result := @shItems[idxHashItem]
  else
    Result := nil;
end; { TGpStringHash.GetHashItem }

function TGpStringHash.GetItems(const key: string): int64;
begin
  Result := ValueOf(key);
end; { TGpStringHash.GetItems }

procedure TGpStringHash.Grow;
var
  bucket      : PGpHashItem;
  hash        : cardinal;
  oldBucket   : PGpHashItem;
  oldIndex    : integer;
  shOldBuckets: array of cardinal;
  shOldItems  : array of TGpHashItem;
begin
  SetLength(shOldBuckets, Length(shBuckets));
  Move(shBuckets[0], shOldBuckets[0], Length(shBuckets) * SizeOf(shBuckets[0]));
  SetLength(shOldItems, Length(shItems));
  for oldIndex := 0 to Length(shItems) - 1 do  begin
    shOldItems[oldIndex] := shItems[oldIndex];
    shItems[oldIndex].Key := '';
    shItems[oldIndex].Next := 0;
  end;
  SetLength(shItems, 2*Length(shItems) + 1);
  SetLength(shBuckets, GetGoodHashSize(Length(shItems)));
  FillChar(shBuckets[0], Length(shBuckets) * SizeOf(shBuckets[0]), 0);
  shItems[0].Value := -1; // sentinel for the ValueOf operation
  shSize := Length(shItems) - 1;
  shNumBuckets := Length(shBuckets);
  shFirstEmpty := 1;
  for oldIndex := 1 to Length(shOldItems) - 1 do begin
    oldBucket := @(shOldItems[oldIndex]);
    hash := HashOf(oldBucket.Key) mod shNumBuckets;
    bucket := @(shItems[shFirstEmpty]); // point to an empty slot in the pre-allocated array
    Move(oldBucket^, bucket^, SizeOf(bucket^) - SizeOf(bucket^.Next));
    bucket^.Next := shBuckets[hash];
    shBuckets[hash] := shFirstEmpty;
    Inc(shFirstEmpty);
  end;
  FillChar(shOldItems[0], Length(shOldItems) * SizeOf(shOldItems[0]), 0); //prevent string refcount problems
end; { TGpStringHash.Grow }

function TGpStringHash.HasKey(const key: string): boolean;
begin
  Result := (FindBucket(key) <> 0);
end; { TGpStringHash.HasKey }

procedure TGpStringHash.SetItems(const key: string; const value: int64);
begin
  Update(key, value);
end; { TGpStringHash.SetItems }

procedure TGpStringHash.Update(const key: string; value: int64);
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then
    shItems[bucket].Value := value
  else
    Add(key, value);
end; { TGpStringHash.Update }

function TGpStringHash.ValueOf(const key: string): int64;
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then
    Result := shItems[bucket].Value
  else
    raise Exception.CreateFmt('TGpStringHash.ValueOf: Key %s does not exist', [key]);
end; { TGpStringHash.ValueOf }

{ TGpStringObjectHashEnumerator }

constructor TGpStringObjectHashEnumerator.Create(stringHash: TGpStringHash);
begin
  inherited Create;
  soheStringEnumerator := TGpStringHashEnumerator.Create(stringHash);
  soheKV := TGpStringObjectHashKV.Create;
end; { TGpStringObjectHashEnumerator.Create }

destructor TGpStringObjectHashEnumerator.Destroy;
begin
  FreeAndNil(soheKV);
  FreeAndNil(soheStringEnumerator);
  inherited;
end; { TGpStringObjectHashEnumerator.Destroy }

function TGpStringObjectHashEnumerator.GetCurrent: TGpStringObjectHashKV;
begin
  soheKV.Key := soheStringEnumerator.GetCurrent.Key;
  soheKV.Value := TObject(soheStringEnumerator.GetCurrent.Value);
  Result := soheKV;
end; { TGpStringObjectHashEnumerator.GetCurrent }

function TGpStringObjectHashEnumerator.MoveNext: boolean;
begin
  Result := soheStringEnumerator.MoveNext;
end; { TGpStringObjectHashEnumerator.MoveNext }

{ TGpStringObjectHash }

constructor TGpStringObjectHash.Create(numItems: cardinal; ownsObjects, canGrow: boolean);
begin
  inherited Create;
  sohOwnsObjects := ownsObjects;
  sohHash := TGpStringHash.Create(numItems, canGrow);
end; { TGpStringObjectHash.Create }

constructor TGpStringObjectHash.Create(numBuckets, numItems: cardinal; ownsObjects,
  canGrow: boolean);
begin
  inherited Create;
  sohOwnsObjects := ownsObjects;
  sohHash := TGpStringHash.Create(numBuckets, numItems, canGrow);
end; { TGpStringObjectHash.Create }

destructor TGpStringObjectHash.Destroy;
begin
  if sohOwnsObjects and assigned(sohHash) then
    ForEach(FreeObject);
  FreeAndNil(sohHash);
  inherited;
end; { TGpStringObjectHash.Destroy }

procedure TGpStringObjectHash.Add(const key: string; value: TObject);
begin
  Assert(SizeOf(TObject) <= SizeOf(int64));
  sohHash.Add(key, int64(value));
end; { TGpStringObjectHash.Add }

function TGpStringObjectHash.Count: integer;
begin
  Result := sohHash.Count;
end; { TGpStringObjectHash.Count }

function TGpStringObjectHash.Find(const key: string; var value: TObject): boolean;
var
  intValue: int64;
begin
  Result := sohHash.Find(key, intValue);
  if Result then
    value := TObject(intValue)
  else
    value := nil;
end;  { TGpStringObjectHash.Find }

procedure TGpStringObjectHash.ForEach(enumerator: TGpStringObjectHashEnumMethod);
var
  enum: TGpStringObjectHashEnumerator;
begin
  enum := GetEnumerator;
  try
    while enum.MoveNext do
      enumerator(enum.Current);
  finally FreeAndNil(enum); end;
end; { TGpStringObjectHash.ForEach }

procedure TGpStringObjectHash.FreeObject(item: TGpStringObjectHashKV);
begin
  item.Value.Free;
end; { TGpStringObjectHash.FreeObject }

function TGpStringObjectHash.GetEnumerator: TGpStringObjectHashEnumerator;
begin
  Result := TGpStringObjectHashEnumerator.Create(sohHash);
end; { TGpStringObjectHash.GetEnumerator }

function TGpStringObjectHash.GetObjects(const key: string): TObject;
var
  value: int64;
begin
  if sohHash.Find(key, value) then
    Result := TObject(value)
  else
    Result := nil;
end; { TGpStringObjectHash.GetObjects }

function TGpStringObjectHash.HasKey(const key: string): boolean;
begin
  Result := sohHash.HasKey(key);
end; { TGpStringObjectHash.HasKey }

procedure TGpStringObjectHash.SetObjects(const key: string; const value: TObject);
begin
  Update(key, value);
end; { TGpStringObjectHash.SetObjects }

procedure TGpStringObjectHash.Update(const key: string; value: TObject);
var
  bucket: cardinal;
  item  : PGpHashItem;
begin
  if not sohOwnsObjects then
    sohHash.Update(key, int64(value))
  else begin
    bucket := sohHash.FindBucket(key);
    if bucket > 0 then begin
      item := sohHash.HashItems[bucket];
      if TObject(item.Value) <> value then
        TObject(item.Value).Free;
      item.Value := int64(value);
    end
    else
      sohHash.Add(key, int64(value));
  end;
end; { TGpStringObjectHash.Update }

function TGpStringObjectHash.ValueOf(const key: string): TObject;
begin
  Result := TObject(sohHash.ValueOf(key));
end; { TGpStringObjectHash.ValueOf }

{ TGpStringInterfaceHashEnumerator }

constructor TGpStringInterfaceHashEnumerator.Create(stringHash: TGpStringHash);
begin
  inherited Create;
  siheStringEnumerator := TGpStringHashEnumerator.Create(stringHash);
  siheKV := TGpStringInterfaceHashKV.Create;
end; { TGpStringInterfaceHashEnumerator.Create }

destructor TGpStringInterfaceHashEnumerator.Destroy;
begin
  FreeAndNil(siheKV);
  FreeAndNil(siheStringEnumerator);
  inherited;
end; { TGpStringInterfaceHashEnumerator.Destroy }

function TGpStringInterfaceHashEnumerator.GetCurrent: TGpStringInterfaceHashKV;
begin
  siheKV.Key := siheStringEnumerator.GetCurrent.Key;
  siheKV.Value := IInterface(NativeInt(siheStringEnumerator.GetCurrent.Value));
  Result := siheKV;
end; { TGpStringInterfaceHashEnumerator.GetCurrent }

function TGpStringInterfaceHashEnumerator.MoveNext: boolean;
begin
  Result := siheStringEnumerator.MoveNext;
end; { TGpStringInterfaceHashEnumerator.MoveNext }

{ TGpStringInterfaceHash }

constructor TGpStringInterfaceHash.Create(numItems: cardinal; canGrow: boolean);
begin
  inherited Create;
  sihHash := TGpStringHash.Create(numItems, canGrow);
end; { TGpStringInterfaceHash.Create }

constructor TGpStringInterfaceHash.Create(numBuckets, numItems: cardinal;
  canGrow: boolean);
begin
  inherited Create;
  Assert(SizeOf(IInterface) <= SizeOf(int64));
  sihHash := TGpStringHash.Create(numBuckets, numItems, canGrow);
end; { TGpStringInterfaceHash.Create }

destructor TGpStringInterfaceHash.Destroy;
begin
  if assigned(sihHash) then
    ForEach(ReleaseInterface);
  FreeAndNil(sihHash);
  inherited;
end; { TGpStringInterfaceHash.Destroy }

procedure TGpStringInterfaceHash.Add(const key: string; const value: IInterface);
begin
  sihHash.Add(key, int64(NativeInt(value)));
  value._AddRef;
end; { TGpStringInterfaceHash.Add }

function TGpStringInterfaceHash.Count: integer;
begin
  Result := sihHash.Count;
end; { TGpStringInterfaceHash.Count }

function TGpStringInterfaceHash.Find(const key: string; var value: IInterface): boolean;
var
  intValue: int64;
begin
  Result := sihHash.Find(key, intValue);
  if Result then
    value := IInterface(NativeInt(intValue));
end; { TGpStringInterfaceHash.Find }

procedure TGpStringInterfaceHash.ForEach(enumerator: TGpStringInterfaceHashEnumMethod);
var
  enum: TGpStringInterfaceHashEnumerator;
begin
  enum := GetEnumerator;
  try
    while enum.MoveNext do
      enumerator(enum.Current);
  finally FreeAndNil(enum); end;
end; { TGpStringInterfaceHash.ForEach }

function TGpStringInterfaceHash.GetEnumerator: TGpStringInterfaceHashEnumerator;
begin
  Result := TGpStringInterfaceHashEnumerator.Create(sihHash);
end; { TGpStringInterfaceHash.GetEnumerator }

function TGpStringInterfaceHash.GetInterfaces(const key: string): IInterface;
var
  value: int64;
begin
  if sihHash.Find(key, value) then begin
    NativeInt(Result) := value;
    if assigned(Result) then
      Result._AddRef;
  end
  else
    Result := nil;
end; { TGpStringInterfaceHash.GetInterfaces }

function TGpStringInterfaceHash.HasKey(const key: string): boolean;
begin
  Result := sihHash.HasKey(key);
end; { TGpStringInterfaceHash.HasKey }

procedure TGpStringInterfaceHash.ReleaseInterface(item: TGpStringInterfaceHashKV);
begin
  if assigned(item.Value) then
    item.Value._Release;
end; { TGpStringInterfaceHash.ReleaseInterface }

procedure TGpStringInterfaceHash.SetInterfaces(const key: string; const value:
  IInterface);
begin
  Update(key, value);
end; { TGpStringInterfaceHash.SetInterfaces }

procedure TGpStringInterfaceHash.Update(const key: string; const value: IInterface);
var
  bucket: cardinal;
  item  : PGpHashItem;
begin
  bucket := sihHash.FindBucket(key);
  if bucket > 0 then begin
    item := sihHash.HashItems[bucket];
    IInterface(NativeInt(item.Value))._Release;
    item.Value := int64(NativeInt(value));
    if assigned(value) then
      value._AddRef;
  end
  else
    Add(key, value);
end; { TGpStringInterfaceHash.Update }

function TGpStringInterfaceHash.ValueOf(const key: string): IInterface;
begin
  Result := IInterface(NativeInt(sihHash.ValueOf(key)));
end; { TGpStringInterfaceHash.ValueOf }

{ TGpStringTableEnumerator }

constructor TGpStringTableEnumerator.Create(pTable, pTail: pointer);
begin
  inherited Create;
  steCurrent := TGpStringTableKV.Create;
  steTable := pTable;
  steTail := pTail;
end; { TGpStringTableEnumerator.Create }

destructor TGpStringTableEnumerator.Destroy;
begin
  FreeAndNil(steCurrent);
  inherited;
end; { TGpStringTableEnumerator.Destroy }

function TGpStringTableEnumerator.GetCurrent: TGpStringTableKV;
var
  lenStr: cardinal;
begin
  lenStr := PCardinal(steTable)^;
  SetLength(steCurrent.kvKey, lenStr * SizeOf(char));
  Inc(steTable, SizeOf(cardinal));
  if Length(steCurrent.kvKey) > 0 then begin
    Move(steTable^, steCurrent.kvKey[1], lenStr * SizeOf(char));
    Inc(steTable, lenStr * SizeOf(char));
  end;
  steCurrent.kvValue := PInt64(steTable)^;
  Inc(steTable, SizeOf(int64));
  Result := steCurrent;
end; { TGpStringTableEnumerator.GetCurrent }

function TGpStringTableEnumerator.MoveNext: boolean;
begin
  Result := NativeUInt(steTable) < NativeUInt(steTail);
end; { TGpStringTableEnumerator.MoveNext }

{ TGpStringTable }

constructor TGpStringTable.Create(initialSize: cardinal; canGrow: boolean);
begin
  inherited Create;
  GetMem(stData, initialSize);
  stDataSize := initialSize;
  stDataTail := stData;
  stCanGrow := canGrow;
end; { TGpStringTable.Create }

destructor TGpStringTable.Destroy;
begin
  FreeMem(stData);
  inherited;
end; { TGpStringTable.Destroy }

function TGpStringTable.Add(const key: string; value: int64): cardinal;
var
  requiredSize: cardinal;
begin
  if key = '' then
    raise Exception.Create('TGpStringTable.Add: Cannot store empty key');
  Result := NativeUInt(stDataTail) - NativeUInt(stData);
  requiredSize := Result + SizeOf(cardinal) + cardinal(Length(key) * SizeOf(char)) + SizeOf(int64);
  if requiredSize > stDataSize then
    Grow(requiredSize);
  PCardinal(stDataTail)^ := Length(key);
  Inc(stDataTail, SizeOf(cardinal));
  Move(key[1], stDataTail^, Length(key) * SizeOf(char));
  Inc(stDataTail, Length(key) * SizeOf(char));
  PInt64(stDataTail)^ := value;
  Inc(stDataTail, SizeOf(int64));
end; { TGpStringTable.Add }

procedure TGpStringTable.CheckPointer(pData: pointer; dataSize: cardinal);
begin
  if (NativeUInt(pData) + dataSize - NativeUInt(stData)) > stDataSize then
    raise Exception.Create('TGpStringTable: Invalid index');
end; { TGpStringTable.CheckPointer }

procedure TGpStringTable.ForEach(enumerator: TGpStringTableEnumMethod);
var
  enum: TGpStringTableEnumerator;
begin
  enum := GetEnumerator;
  try
    while enum.MoveNext do
      enumerator(enum.Current);
  finally FreeAndNil(enum); end;
end; { TGpStringTable.ForEach }

procedure TGpStringTable.Grow(requiredSize: cardinal);
var
  pNewData: pointer;
begin
  if not stCanGrow then
    raise Exception.Create('TGpStringTable.Grow: String table size is fixed');
  requiredSize := Round(requiredSize * 1.6);
  GetMem(pNewData, requiredSize);
  Move(stData^, pNewData^, stDataSize);
  stDataSize := requiredSize;
  stDataTail := PByte(NativeUInt(stDataTail) - NativeUInt(stData) + NativeUInt(pNewData));
  FreeMem(stData);
  stData := pNewData;
end; { TGpStringTable.Grow }

procedure TGpStringTable.Get(index: cardinal; var key: string; var value: int64);
var
  lenStr: cardinal;
  pData : PByte;
begin 
  pData := pointer(NativeUInt(stData) + index);
  CheckPointer(pData, SizeOf(cardinal));
  lenStr := PCardinal(pData)^;
  Inc(pData, SizeOf(cardinal));
  CheckPointer(pData, lenStr * SizeOf(char));
  SetLength(key, lenStr);
  if lenStr > 0 then begin
    Move(pData^, key[1], lenStr * SizeOf(char));
    Inc(pData, lenStr * SizeOf(char));
  end;
  CheckPointer(pData, SizeOf(int64));
  value := PInt64(pData)^;
end; { TGpStringTable.Get }

function TGpStringTable.GetEnumerator: TGpStringTableEnumerator;
begin
  Result := TGpStringTableEnumerator.Create(stData, stDataTail);
end; { TGpStringTable.GetEnumerator }

function TGpStringTable.GetKey(index: cardinal): string;
var
  lenStr: cardinal;
  pData : PByte;
begin 
  pData := pointer(NativeUInt(stData) + index);
  CheckPointer(pData, SizeOf(cardinal));
  lenStr := PCardinal(pData)^;
  Inc(pData, SizeOf(cardinal));
  CheckPointer(pData, lenStr * SizeOf(char));
  SetLength(Result, lenStr);
  if lenStr > 0 then
    Move(pData^, Result[1], lenStr * SizeOf(char));
end; { TGpStringTable.GetKey }

function TGpStringTable.GetValue(index: cardinal): int64;
var
  lenStr: cardinal;
  pData : PByte;
begin
  pData := pointer(NativeUInt(stData) + index);
  CheckPointer(pData, SizeOf(cardinal));
  lenStr := PCardinal(pData)^;
  Inc(pData, SizeOf(cardinal));
  CheckPointer(pData, lenStr * SizeOf(char));
  Inc(pData, lenStr * SizeOf(char));
  CheckPointer(pData, SizeOf(int64));
  Result := PInt64(pData)^;
end; { TGpStringTable.GetValue }

procedure TGpStringTable.SetValue(index: cardinal; const value: int64);
var
  lenStr: cardinal;
  pData: PByte;
begin
  pData := pointer(NativeUInt(stData) + index);
  CheckPointer(pData, SizeOf(cardinal));
  lenStr := PCardinal(pData)^;
  Inc(pData, SizeOf(cardinal));
  CheckPointer(pData, lenStr * SizeOf(char));
  Inc(pData, lenStr * SizeOf(char));
  CheckPointer(pData, SizeOf(int64));
  PInt64(pData)^ := value;
end; { TGpStringTable.SetValue }

{ TGpStringDictionaryEnumerator }

constructor TGpStringDictionaryEnumerator.Create(pItems: PGpTableHashItemArr;
  firstBucket: cardinal; stringTable: TGpStringTable);
begin
  inherited Create;
  steCurrent := TGpStringDictionaryKV.Create;
  steItems := pItems;
  steFirstBucket := firstBucket;
  steStringTable := stringTable;
  steBucketIdx := -1;
//  steMaxBucket := High(steBuckets^);
end; { TGpStringDictionaryEnumerator.Create }

destructor TGpStringDictionaryEnumerator.Destroy;
begin
  FreeAndNil(steCurrent);
  inherited;
end; { TGpStringDictionaryEnumerator.Destroy }

function TGpStringDictionaryEnumerator.GetCurrent: TGpStringDictionaryKV;
var
  key   : string;
  value : int64;
begin
  steStringTable.Get(steBucket^.Index, key, value);
  steCurrent.Index := steBucket^.Index;
  steCurrent.Key := key;
  steCurrent.Value := value;
  Result := steCurrent;
end; { TGpStringDictionaryEnumerator.GetCurrent }

function TGpStringDictionaryEnumerator.MoveNext: boolean;
begin
  Result := (steFirstBucket <> 0);
  if Result then begin
    steBucket := @(steItems^[steFirstBucket]);
    steFirstBucket := steBucket^.Next;
  end;
end; { TGpStringDictionaryEnumerator.MoveNext }

{ TGpStringDictionary }

constructor TGpStringDictionary.Create(initialArraySize: cardinal);
begin
  inherited Create;
  sdStringTable := TGpStringTable.Create(initialArraySize);
  sdNumBuckets := GetGoodHashSize(initialArraySize);
  SetLength(sdBuckets, sdNumBuckets);
  SetLength(sdItems, initialArraySize + 1);
  sdSize := initialArraySize;
  sdFirstEmpty := 1;
  sdCanGrow := true;
end; { TGpStringDictionary.Create }

destructor TGpStringDictionary.Destroy;
begin
  SetLength(sdItems, 0);
  SetLength(sdBuckets, 0);
  FreeAndNil(sdStringTable);
  inherited Destroy;
end; { TGpStringDictionary.Destroy }

function TGpStringDictionary.Add(const key: string; value: int64): cardinal;
var
  bucket: PGpTableHashItem;
  hash  : cardinal;
begin
  hash := HashOf(key) mod sdNumBuckets;
  if sdFirstEmpty > sdSize then
    if sdCanGrow then
      Grow
    else
      raise Exception.Create('TGpStringDictionary.Add: Maximum size reached');
  bucket := @(sdItems[sdFirstEmpty]); // point to an empty slot in the pre-allocated array
  bucket^.Index := sdStringTable.Add(key, value);
  bucket^.Chain := sdBuckets[hash];
  bucket^.Next := 0;
  LinkAtTail(bucket, sdFirstEmpty);
  sdBuckets[hash] := sdFirstEmpty;
  Inc(sdFirstEmpty);
  Result := bucket^.index;
end; { TGpStringDictionary.Add }

function TGpStringDictionary.Count: integer;
begin
  Result := sdFirstEmpty - 1;
end; { TGpStringDictionary.Count }

function TGpStringDictionary.Find(const key: string; var index: cardinal; var value:
  int64): boolean;
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then begin
    index := sdItems[bucket].Index;
    value := sdStringTable.Value[index];
    Result := true;
  end
  else
    Result := false;
end; { TGpStringDictionary.Find }

function TGpStringDictionary.FindBucket(const key: string): cardinal;
begin
  Result := sdBuckets[HashOf(key) mod sdNumBuckets];
   while (Result <> 0) and (sdStringTable.Key[sdItems[Result].Index] <> key) do
    Result := sdItems[Result].Chain;
end; { TGpStringDictionary.FindBucket }

procedure TGpStringDictionary.ForEach(enumerator: TGpStringDictionaryEnumMethod);
var
  enum: TGpStringDictionaryEnumerator;
begin
  enum := GetEnumerator;
  try
    while enum.MoveNext do
      enumerator(enum.Current);
  finally FreeAndNil(enum); end;
end; { TGpStringDictionary.ForEach }

procedure TGpStringDictionary.Get(index: cardinal; var key: string; var value: int64);
begin
  sdStringTable.Get(index, key, value);
end; { TGpStringDictionary.Get }

function TGpStringDictionary.GetEnumerator: TGpStringDictionaryEnumerator;
begin
  Result := TGpStringDictionaryEnumerator.Create(@sdItems, sdUsedBuckets, sdStringTable);
end; { TGpStringDictionary.GetEnumerator }

function TGpStringDictionary.GetHashItem(idxHashItem: cardinal): PGpTableHashItem;
begin
  if idxHashItem > 0 then
    Result := @sdItems[idxHashItem]
  else
    Result := nil;
end; { TGpStringDictionary.GetHashItem }

function TGpStringDictionary.GetItems(const key: string): int64;
begin
  Result := ValueOf(key);
end; { TGpStringDictionary.GetItems }

procedure TGpStringDictionary.Grow;
var
  bucket      : PGpTableHashItem;
  hash        : cardinal;
  oldBucket   : PGpTableHashItem;
  oldIndex    : integer;
  shOldBuckets: array of cardinal;
  shOldItems  : array of TGpTableHashItem;
begin
  SetLength(shOldBuckets, Length(sdBuckets));
  Move(sdBuckets[0], shOldBuckets[0], Length(sdBuckets) * SizeOf(sdBuckets[0]));
  SetLength(shOldItems, Length(sdItems));
  for oldIndex := 0 to Length(sdItems) - 1 do  begin
    shOldItems[oldIndex] := sdItems[oldIndex];
    sdItems[oldIndex].Chain := 0;
    sdItems[oldIndex].Next := 0;
  end;
  SetLength(sdItems, 2*Length(sdItems) + 1);
  SetLength(sdBuckets, GetGoodHashSize(Length(sdItems)));
  FillChar(sdBuckets[0], Length(sdBuckets) * SizeOf(sdBuckets[0]), 0);
  sdSize := Length(sdItems) - 1;
  sdNumBuckets := Length(sdBuckets);
  sdFirstEmpty := 1;
  sdUsedBuckets := 0;
  sdTail := nil;
  for oldIndex := 1 to Length(shOldItems) - 1 do begin
    oldBucket := @(shOldItems[oldIndex]);
    hash := HashOf(sdStringTable.Key[oldBucket.index]) mod sdNumBuckets;
    bucket := @(sdItems[sdFirstEmpty]); // point to an empty slot in the pre-allocated array
    Move(oldBucket^, bucket^, SizeOf(bucket^) - SizeOf(bucket^.Chain));
    bucket^.Chain := sdBuckets[hash];
    sdBuckets[hash] := sdFirstEmpty;
    LinkAtTail(bucket, sdFirstEmpty);
    Inc(sdFirstEmpty);
  end;
  FillChar(shOldItems[0], Length(shOldItems) * SizeOf(shOldItems[0]), 0); //prevent string refcount problems
end; { TGpStringDictionary.Grow }

function TGpStringDictionary.HasKey(const key: string): boolean;
begin
  Result := (FindBucket(key) <> 0);
end; { TGpStringDictionary.HasKey }

procedure TGpStringDictionary.LinkAtTail(bucket: PGpTableHashItem; bucketIdx: cardinal);
begin
  if assigned(sdTail) then begin
    sdTail^.Next := bucketIdx;
    sdTail := bucket;
  end
  else begin
    sdUsedBuckets := bucketIdx;
    sdTail := @(sdItems[bucketIdx]);
  end;
end; { TGpStringDictionary.LinkAtTail }

procedure TGpStringDictionary.SetItems(const key: string; const value: int64);
begin
  Update(key, value);
end; { TGpStringDictionary.SetItems }

procedure TGpStringDictionary.Update(const key: string; value: int64);
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then
    sdStringTable.Value[sdItems[bucket].Index] := value
  else
    Add(key, value);
end; { TGpStringDictionary.Update }

function TGpStringDictionary.ValueOf(const key: string): int64;
var
  bucket: integer;
begin
  bucket := FindBucket(key);
  if bucket > 0 then
    Result := sdStringTable.Value[sdItems[bucket].Index]
  else
    raise Exception.CreateFmt('TGpStringDictionary.ValueOf: Key %s does not exist', [key]);
end; { TGpStringDictionary.ValueOf }

end.

