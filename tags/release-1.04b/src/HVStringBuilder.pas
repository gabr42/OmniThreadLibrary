unit HVStringBuilder;

// Written by Hallvard Vassbotn, September 2003

// Enable these at your own risk ;)
{.$DEFINE SUPPORT_APPENDFORMAT}    // AppendFormat is not .NET compatible
{$DEFINE STRINGBUILDEREXTENSIONS} // Class helpers may not be the right tool for this... ;)
{.$DEFINE TOBJECT_TOSTRING}        // HVObjectToString is experimental

interface

{$IFDEF CLR}
uses
  System.Text;
type
  StringBuilder = System.Text.StringBuilder;

{$IFDEF STRINGBUILDEREXTENSIONS}
type
  StringBuilderHelper = class helper for System.Text.StringBuilder
  public
    procedure FromString(const Value: string);
    procedure Clear;
    procedure Reverse;
    procedure UpperCase;
    procedure LowerCase;
  end;
{$ENDIF STRINGBUILDEREXTENSIONS}

{$ELSE NOT CLR}

uses
  SysUtils,
  HVStringData;

type
  Decimal = Currency;
  StringBuilder = class
  private
    FStringValue: string;
    FStringData: TStringData;
    FUseStringData: boolean;
    function GetStringData: TStringData;
    function GetStringValue: string;
    property StringData: TStringData read GetStringData;
    property StringValue: string read GetStringValue;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetChars(Index: Integer): Char;
    procedure SetChars(Index: Integer; Value: Char);
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
    function GetMaxCapacity: Integer;
    procedure NeedStringValue;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    constructor Create(Capacity: Integer; MaxCapacity: Integer); overload;
    constructor Create(const Value: String); overload;
    constructor Create(const Value: String; Capacity: Integer); overload;
    constructor Create(const Value: String; StartIndex: Integer; Length: Integer; Capacity: Integer); overload;
    destructor Destroy; override;
    function Append(Value: Boolean): StringBuilder; overload;
    function Append(Value: Char): StringBuilder; overload;
    function Append(Value: Char; RepeatCount: Integer): StringBuilder; overload;
    function Append(Value: PChar): StringBuilder; overload;
    function Append(Value: PChar; StartIndex: Integer; CharCount: Integer): StringBuilder; overload;
//    function Append(Value: Decimal): StringBuilder; overload; // Ambiguous overloaded call to 'Append'
    function Append(Value: Double): StringBuilder; overload;
    function Append(Value: Integer): StringBuilder; overload;
    function Append(Value: Int64): StringBuilder; overload;
    function Append(const Value: String): StringBuilder; overload;
    function Append(const Value: String; StartIndex: Integer; Count: Integer): StringBuilder; overload;
    function Append(Value: Cardinal): StringBuilder; overload;
    function Append(Value: UInt64): StringBuilder; overload;
{$IFDEF TOBJECT_TOSTRING}
    function Append(Value: TObject): StringBuilder; overload;
{$ENDIF}
{$IFDEF SUPPORT_APPENDFORMAT}
//    function AppendFormat(provider: IFormatProvider; const AFormat: String; const args: array of const): StringBuilder; overload;
    function AppendFormat(const FormatSettings: TFormatSettings; const AFormat: String; const Args: array of const): StringBuilder; overload;
    function AppendFormat(const AFormat: String; const Args: array of const): StringBuilder; overload;
{$IFDEF TOBJECT_TOSTRING}
    function AppendFormat2(const AFormat: String; const Args: array of TObject): StringBuilder; overload;
{$ENDIF}
{$ENDIF}
    function EnsureCapacity(Capacity: Integer): Integer;
    function Equals(aStringBuilder: StringBuilder): Boolean; {$IF CompilerVersion >= 21}reintroduce;{$IFEND}
    function Insert(Index: Integer; Value: Boolean): StringBuilder; overload;
    function Insert(Index: Integer; Value: Char): StringBuilder; overload;
    function Insert(Index: Integer; Value: PChar): StringBuilder; overload;
    function Insert(Index: Integer; Value: PChar; StartIndex: Integer; CharCount: Integer): StringBuilder; overload;
//    function Insert(Index: Integer; Value: Decimal): StringBuilder; overload; Ambiguous overloaded call to 'Append'
    function Insert(Index: Integer; Value: Double): StringBuilder; overload;
    function Insert(Index: Integer; Value: Integer): StringBuilder; overload;
    function Insert(Index: Integer; Value: Int64): StringBuilder; overload;
    function Insert(Index: Integer; const Value: String): StringBuilder; overload;
    function Insert(Index: Integer; const Value: String; Count: Integer): StringBuilder; overload;
    function Insert(Index: Integer; Value: Cardinal): StringBuilder; overload;
    function Insert(Index: Integer; Value: UInt64): StringBuilder; overload;
{$IFDEF TOBJECT_TOSTRING}
    function Insert(Index: Integer; Value: TObject): StringBuilder; overload;
{$ENDIF}
    function Remove(StartIndex: Integer; Length: Integer): StringBuilder;
    function Replace(OldChar: Char; NewChar: Char): StringBuilder; overload;
    function Replace(OldChar: Char; NewChar: Char; StartIndex: Integer; Count: Integer): StringBuilder; overload;
    function Replace(const OldValue: String; NewValue: String): StringBuilder; overload;
    function Replace(const OldValue: String; NewValue: String; StartIndex: Integer; Count: Integer): StringBuilder; overload;
    function ToString: String; {$IF CompilerVersion >= 21}reintroduce;{$IFEND} overload;
    function ToString(StartIndex: Integer; Length: Integer): String; {$IF CompilerVersion >= 21}reintroduce;{$IFEND} overload;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Chars[Index: Integer]: Char read GetChars write SetChars; default;
    property Length: Integer read GetLength write SetLength;
    property MaxCapacity: Integer read GetMaxCapacity;
{$IFDEF STRINGBUILDEREXTENSIONS}
  public
    procedure FromString(const Value: string);
    procedure Clear;
    procedure Reverse;
    procedure UpperCase;
    procedure LowerCase;
{$ENDIF}
  end;
  ArgumentOutOfRangeException = class(Exception);

{$ENDIF NOT CLR}

implementation

{$IFDEF CLR}

{$IFDEF STRINGBUILDEREXTENSIONS}
procedure StringBuilderHelper.Clear;
begin
  Length := 0;
end;

procedure StringBuilderHelper.Reverse;
var
  CharArray: array of char;
begin
  CharArray := ToString.ToCharArray;
  System.Array.Reverse(CharArray);
  FromString(System.String.Create(CharArray));
end;

procedure StringBuilderHelper.UpperCase;
begin
  FromString(ToString.ToUpper);
end;

procedure StringBuilderHelper.LowerCase;
begin
  FromString(ToString.ToLower);
end;

procedure StringBuilderHelper.FromString(const Value: string);
begin
  Clear;
  Append(Value);
end;
{$ENDIF STRINGBUILDEREXTENSIONS}

{$ELSE}

uses StrUtils
{$IFDEF TOBJECT_TOSTRING},HVObjectToString{$ENDIF}
  ;

procedure ArgumentError;
begin
  raise ArgumentOutOfRangeException.Create('Argument out of range');
end;

{ StringBuilder }

constructor StringBuilder.Create;
begin
  inherited Create;
  FStringData := TStringData.Create;
end;

destructor StringBuilder.Destroy;
begin
  FStringData.Free;
  inherited Destroy;
end;

constructor StringBuilder.Create(Capacity, MaxCapacity: Integer);
begin
  Create;
end;

constructor StringBuilder.Create(Capacity: Integer);
begin
  Create;
end;

constructor StringBuilder.Create(const Value: String; StartIndex, Length,
  Capacity: Integer);
begin
  Create;
  Append(Value, StartIndex, Length);
end;

constructor StringBuilder.Create(const Value: String; Capacity: Integer);
begin
  Create;
  Append(Value);
end;

constructor StringBuilder.Create(const Value: String);
begin
  Create;
  Append(Value);
end;

function StringBuilder.Append(Value: Integer): StringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function StringBuilder.Append(Value: Int64): StringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function StringBuilder.Append(Value: Cardinal): StringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function StringBuilder.Append(Value: UInt64): StringBuilder;
begin
  Result := Append(IntToStr(Value));
end;

function StringBuilder.Append(const Value: String): StringBuilder;
begin
  StringData.Append(Value);
  Result := Self;
end;

function StringBuilder.Append(const Value: String; StartIndex,
  Count: Integer): StringBuilder;
begin
  Result := Append(Copy(Value, StartIndex+1, Count));
end;

function StringBuilder.Append(Value: Char): StringBuilder;
begin
  StringData.Append(Value);
  Result := Self;
end;

function StringBuilder.Append(Value: Char;
  RepeatCount: Integer): StringBuilder;
begin
  StringData.Append(StringOfChar(Value, RepeatCount));
  Result := Self;
end;

function StringBuilder.Append(Value: Boolean): StringBuilder;
begin
  Result := Append(BoolToStr(Value, True));
end;

function StringBuilder.Append(Value: Double): StringBuilder;
begin
  Result := Append(FloatToStr(Value));
end;

{function StringBuilder.Append(Value: Decimal): StringBuilder;
begin
  Result := Append(CurrToStr(Value));
end;}

function StringBuilder.Append(Value: PChar): StringBuilder;
begin
  Result := Append(string(Value));
end;

function StringBuilder.Append(Value: PChar; StartIndex,
  CharCount: Integer): StringBuilder;
begin
  Result := Append(Copy(string(Value), StartIndex+1, CharCount));
end;

{$IFDEF TOBJECT_TOSTRING}
function StringBuilder.Append(Value: TObject): StringBuilder;
begin
  Result := Append(ObjectToString(Value));
end;
{$ENDIF}

{$IFDEF SUPPORT_APPENDFORMAT}
function StringBuilder.AppendFormat(const AFormat: String;
  const Args: array of const): StringBuilder;
begin
  Result := Append(Format(AFormat, Args));
end;

{$IFDEF TOBJECT_TOSTRING}
function StringBuilder.AppendFormat2(const AFormat: String;
  const Args: array of TObject): StringBuilder;
var
  S: array of string;
  C: array of TVarRec;
  i: integer;
begin
  System.SetLength(S, High(Args)+1);
  System.SetLength(C, High(Args)+1);
  for i := Low(Args) to High(Args) do
  begin
    S[i] := ObjectToString(Args[i]);
    C[i].VAnsiString := Pointer(S[i]);
    C[i].VType := vtAnsiString;
  end;
  Result := Append(Format(AFormat, C));
end;
{$ENDIF}

function StringBuilder.AppendFormat(const FormatSettings: TFormatSettings;
  const AFormat: String; const Args: array of const): StringBuilder;
begin
  Result := Append(Format(AFormat, Args, FormatSettings));
end;
{$ENDIF}

function StringBuilder.EnsureCapacity(Capacity: Integer): Integer;
begin
  // Do nothing, explicit capacity not supported
  Result := Length;
end;

function StringBuilder.Equals(aStringBuilder: StringBuilder): Boolean;
begin
  Result := Assigned(aStringBuilder) and (Self.StringValue = aStringBuilder.StringValue);
end;

function StringBuilder.GetCapacity: Integer;
begin
  Result := Length;
end;

function StringBuilder.GetChars(Index: Integer): Char;
begin
  NeedStringValue;
  Result := FStringValue[Index+1];
end;

function StringBuilder.GetLength: Integer;
begin
  if FUseStringData
  then Result := FStringData.Length
  else Result := System.Length(FStringValue);
end;

function StringBuilder.GetMaxCapacity: Integer;
begin
  Result := MaxInt; // dynamic MaxCapacity not supported
end;

function StringBuilder.Insert(Index: Integer; Value: PChar): StringBuilder;
begin
  Result := Insert(Index, string(Value));
end;

{function StringBuilder.Insert(Index: Integer;
  Value: Decimal): StringBuilder;
begin
  Result := Insert(Index, CurrToStr(Value));
end;}

function StringBuilder.Insert(Index: Integer; Value: PChar; StartIndex,
  CharCount: Integer): StringBuilder;
begin
  Result := Insert(Index, Copy(string(Value), StartIndex+1, CharCount));
end;

function StringBuilder.Insert(Index: Integer; const Value: String;
  Count: Integer): StringBuilder;
begin
  Result := Insert(Index, DupeString(Value, Count));
end;

function StringBuilder.Insert(Index: Integer;
  const Value: String): StringBuilder;
begin
  NeedStringValue;
  System.Insert(Value, FStringValue, Index+1);
  Result := Self;
end;

function StringBuilder.Insert(Index: Integer;
  Value: UInt64): StringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function StringBuilder.Insert(Index: Integer;
  Value: Cardinal): StringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function StringBuilder.Insert(Index, Value: Integer): StringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

function StringBuilder.Insert(Index: Integer;
  Value: Boolean): StringBuilder;
begin
  Result := Insert(Index, BoolToStr(Value, True));
end;

function StringBuilder.Insert(Index: Integer;
  Value: Double): StringBuilder;
begin
  Result := Insert(Index, FloatToStr(Value));
end;

function StringBuilder.Insert(Index: Integer; Value: Int64): StringBuilder;
begin
  Result := Insert(Index, IntToStr(Value));
end;

{$IFDEF TOBJECT_TOSTRING}
function StringBuilder.Insert(Index: Integer; Value: TObject): StringBuilder; 
begin
  Result := Insert(Index, ObjectToString(Value));
end;
{$ENDIF}

function StringBuilder.Insert(Index: Integer; Value: Char): StringBuilder;
begin
  NeedStringValue;
  System.Insert(Value, FStringValue, Index+1); // no _LStrInsert overload for char?
  Result := Self;
end;

function StringBuilder.Remove(StartIndex, Length: Integer): StringBuilder;
begin
  NeedStringValue;
  System.Delete(FStringValue, StartIndex+1, Length);
  Result := Self;
end;

function StringBuilder.Replace(OldChar, NewChar: Char; StartIndex,
  Count: Integer): StringBuilder;
var
  i : integer;
  P : PChar;
begin
  if (StartIndex < 0) or (Count < 0) or (StartIndex + Count > Length) then
    ArgumentError;
  NeedStringValue;
  UniqueString(FStringValue);
  P := PChar(Pointer(FStringValue));
  for i := StartIndex to StartIndex+Count do
    if P[i] = OldChar then
      P[i]  := NewChar;
  Result := Self;
end;

function StringBuilder.Replace(OldChar, NewChar: Char): StringBuilder;
begin
  Result := Replace(OldChar, NewChar, 0, Length);
end;

function StringBuilder.Replace(const OldValue: String; NewValue: String;
  StartIndex, Count: Integer): StringBuilder;
begin
  Inc(StartIndex);
  if (StartIndex <= 0) or (Count < 0) or (StartIndex + Count - 1 > Length) then
    ArgumentError;
  NeedStringValue;
  // Note: This is slow
  FStringValue := Copy(FStringValue, 1, StartIndex-1) +
                  StringReplace(Copy(FStringValue, StartIndex, Count), OldValue, NewValue, [rfReplaceAll]) +
                  Copy(FStringValue, StartIndex+Count, MaxInt);
  Result := Self;
end;

function StringBuilder.Replace(const OldValue: String;
  NewValue: String): StringBuilder;
begin
  NeedStringValue;
  // Note: StringReplace is slow
  FStringValue := StringReplace(FStringValue, OldValue, NewValue, [rfReplaceAll]);
  Result := Self;
end;

procedure StringBuilder.SetCapacity(Value: Integer);
begin
  // Do nothing - capacity is auto adjusted in StringData, string does not support capacity
end;

procedure StringBuilder.SetChars(Index: Integer; Value: Char);
begin
  NeedStringValue;
  FStringValue[Index+1] := Value;
end;

procedure StringBuilder.SetLength(Value: Integer);
begin
  NeedStringValue;
  System.SetLength(FStringValue, Length);
end;

function StringBuilder.ToString(StartIndex, Length: Integer): String;
begin
  Result := Copy(StringValue, StartIndex, Length);
end;

function StringBuilder.ToString: String;
begin
  Result := StringValue;
end;

function StringBuilder.GetStringData: TStringData;
begin
  if not FUseStringData then
  begin
    FUseStringData := True;
    FStringData.Append(FStringValue);
    FStringValue := '';
  end;
  Result := FStringData;
end;

procedure StringBuilder.NeedStringValue;
begin
  if FUseStringData then
  begin
    FUseStringData := False;
    FStringValue := FStringData.Data;
    FStringData.Clear;
  end;
end;

function StringBuilder.GetStringValue: string;
begin
  NeedStringValue;
  Result := FStringValue;
end;

{$IFDEF STRINGBUILDEREXTENSIONS}
procedure StringBuilder.Clear;
begin
  if FUseStringData
  then FStringData.Clear
  else FStringValue := '';
end;

procedure StringBuilder.Reverse;
begin
  NeedStringValue;
  FStringValue := StrUtils.ReverseString(FStringValue);
end;

procedure StringBuilder.LowerCase;
begin
  NeedStringValue;
  FStringValue := AnsiLowerCase(FStringValue);
end;

procedure StringBuilder.UpperCase;
begin
  NeedStringValue;
  FStringValue := AnsiUpperCase(FStringValue);
end;

procedure StringBuilder.FromString(const Value: string);
begin
  Clear;
  Append(Value);
end;
{$ENDIF STRINGBUILDEREXTENSIONS}

{$ENDIF NOT CLR}

end.

