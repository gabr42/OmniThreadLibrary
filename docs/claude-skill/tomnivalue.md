# OTL reference: TOmniValue

`TOmniValue` variant record: data access, safe conversion, type tests, generics, arrays, records, ownership, `TOmniValueObj`.

## Contents

- Data Access Properties
- Safe Conversion (no exception on failure)
- Conversion with Default
- Type Testing
- Clearing
- Generics Support (D2010+)
- Array Access
- Record Handling (D2009+)
- Object Ownership
- Debugging
- TOmniValueObj


`TOmniValue` (in `OtlCommon`) is OTL's central data type — a smart record similar to Variant/TValue but faster. Stores:
- Simple values: `byte`, `integer`, `int64`, `cardinal`, `boolean`, `char`, `double`, `extended`
- Strings: `AnsiString`, `string` (Unicode), `WideString`
- `Variant`, `TDateTime`
- Objects (`TObject`), interfaces (`IInterface`), pointers, exceptions
- Records (D2009+), arrays of `TOmniValue`

**Two floating-point types:** `double` stored directly (faster); `extended` wrapped in `TOmniExtendedData` (slower). Use `double` when possible.

### Data Access Properties

```pascal
property AsAnsiString: AnsiString;
property AsBoolean: boolean;
property AsCardinal: cardinal;
property AsDouble: Double;
property AsDateTime: TDateTime;      // Delphi XE+ only
property AsException: Exception;
property AsExtended: Extended;
property AsInt64: int64;
property AsInteger: integer;
property AsInterface: IInterface;
property AsObject: TObject;
property AsOwnedObject: TObject;
property AsPointer: pointer;
property AsString: string;
property AsUInt64: uint64;           // [3.07.11]
property AsVariant: Variant;
property AsWideString: WideString;
property AsTValue: TValue;           // Delphi 2010+ only
```

Getters attempt cross-type conversion (e.g. integer→string). Raise exception if conversion impossible.

[3.09]: `CastTo<TOmniValue>` now works (it used to raise "TOmniValue cannot be converted to record"), which makes `Parallel.ForEach<TOmniValue>` usable; objects can be stored through `AsTValue`, so `Parallel.ForEach<T>` can iterate enumerables that return objects (for example `TListView.Items`). `LogValue` output was improved in 3.08 (do not depend on its exact format).

### Safe Conversion (no exception on failure)

```pascal
function TryCastToAnsiString(var value: AnsiString): boolean;
function TryCastToBoolean(var value: boolean): boolean;
function TryCastToCardinal(var value: cardinal): boolean;
function TryCastToDouble(var value: Double): boolean;
function TryCastToDateTime(var value: TDateTime): boolean;
function TryCastToException(var value: Exception): boolean;
function TryCastToExtended(var value: Extended): boolean;
function TryCastToInt64(var value: int64): boolean;
function TryCastToInteger(var value: integer): boolean;
function TryCastToInterface(var value: IInterface): boolean;
function TryCastToObject(var value: TObject): boolean;
function TryCastToPointer(var value: pointer): boolean;
function TryCastToString(var value: string): boolean;
function TryCastToUInt64(var value: uint64): boolean;   // [3.07.11]
function TryCastToVariant(var value: Variant): boolean;
function TryCastToWideString(var value: WideString): boolean;
```

### Conversion with Default

```pascal
function CastToAnsiStringDef(const defValue: AnsiString): AnsiString;
function CastToBooleanDef(defValue: boolean): boolean;
function CastToCardinalDef(defValue: cardinal): cardinal;
function CastToDoubleDef(defValue: Double): Double;
function CastToDateTimeDef(defValue: TDateTime): TDateTime;
function CastToExceptionDef(defValue: Exception): Exception;
function CastToExtendedDef(defValue: Extended): Extended;
function CastToInt64Def(defValue: int64): int64;
function CastToIntegerDef(defValue: integer): integer;
function CastToInterfaceDef(const defValue: IInterface): IInterface;
function CastToObjectDef(defValue: TObject): TObject;
function CastToPointerDef(defValue: pointer): pointer;
function CastToStringDef(const defValue: string): string;
function CastToVariantDef(defValue: Variant): Variant;
function CastToWideStringDef(defValue: WideString): WideString;
```

### Type Testing

```pascal
function IsAnsiString: boolean;   function IsArray: boolean;
function IsBoolean: boolean;      function IsEmpty: boolean;
function IsException: boolean;    function IsFloating: boolean;
function IsDateTime: boolean;     function IsInteger: boolean;
function IsInterface: boolean;    function IsInterfacedType: boolean;
function IsObject: boolean;       function IsOwnedObject: boolean;
function IsPointer: boolean;      function IsRecord: boolean;
function IsString: boolean;       function IsVariant: boolean;
function IsWideString: boolean;

type TOmniValueDataType = (ovtNull, ovtBoolean, ovtInteger, ovtDouble, ovtObject,
  ovtPointer, ovtDateTime, ovtException, ovtExtended, ovtString, ovtInterface,
  ovtVariant, ovtWideString, ovtArray, ovtRecord, ovtAnsiString, ovtOwnedObject);

property DataType: TOmniValueDataType;
```

### Clearing

```pascal
procedure Clear;
class function Null: TOmniValue; static;
```

`Clear` is slightly faster than assigning `TOmniValue.Null`.

### Generics Support (D2010+)

```pascal
class function CastFrom<T>(const value: T): TOmniValue; static;
function  CastTo<T>: T;
function  CastToObject<T: class>: T;    // hard cast, no type check
function  ToObject<T: class>: T;        // safe cast (as T)
class function Wrap<T>(const value: T): TOmniValue; static;    // [3.06]
function  Unwrap<T>: T;                                         // [3.06]
```

`Wrap`/`Unwrap` are especially useful for storing `TMethod` data (event handlers) in a `TOmniValue`.

### Array Access

```pascal
constructor Create(const values: array of const);           // integer-indexed
constructor CreateNamed(const values: array of const);      // alternating name/value pairs

property AsArray: TOmniValueContainer;
property AsArrayItem[idx: integer]: TOmniValue; default;
property AsArrayItem[const name: string]: TOmniValue; default;

function HasArrayItem(idx: integer): boolean; overload;
function HasArrayItem(const name: string): boolean; overload;

// D2010+ only:
class function FromArray<T>(const values: TArray<T>): TOmniValue; overload; static;
class function FromArray<T>(const values: array of T): TOmniValue; overload; static;  // [3.07.8]
function ToArray<T>: TArray<T>;
```

C++Builder (`BCB` defined) [3.09]: cannot express overloaded properties in the generated header, so the string-indexed `AsArrayItem` is named `AsArrayItemByName`; the `TOmniValue`-indexed one is `AsArrayItemOV` in all compilers. These names do not exist in Delphi.

`TOmniValueContainer` (the array behind `AsArray` and `task.Param`): `Item[idx]` / `Item[name]` / `Item[TOmniValue]`, `ByName`, `Exists`, `IndexOf`, `Add`, `Insert`, `Assign`, `AssignNamed`, `Lock`; `Name[idx]: string` [3.07.11] returns the name for an integer index (empty if unnamed). With `BCB` [3.09] the `Item` overloads are `ItemByName` and `ItemOV`.

Writing to a non-existent index grows the array automatically.

### Record Handling (D2009+)

```pascal
class function FromRecord<T: record>(const value: T): TOmniValue; static;
function ToRecord<T>: T;
```

**Caveat:** Record storage is slow (wraps in `TOmniRecordWrapper`). Use sparingly.

### Object Ownership

```pascal
property AsOwnedObject: TObject;
function IsOwnedObject: boolean;
property OwnsObject: boolean;
```

When an object-owning `TOmniValue` goes out of scope, it automatically destroys the owned object.

### Debugging

```pascal
function LogValue: string;  // [3.07.6] returns "type: value" string
```

### TOmniValueObj

Object wrapper for storing `TOmniValue` in containers that require objects:

```pascal
TOmniValueObj = class
  constructor Create(const value: TOmniValue);
  property Value: TOmniValue read FValue;
end;
```
