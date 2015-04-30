unit HVStringData;
// Written by Hallvard Vassbotn,
// simplified and version of StringData in WOS (WebBroker on Steroids)
// extended with Append(Char)

interface

type
  PInteger = ^Integer;
  PStringRec = ^TStringRec;
  TStringRec = packed record
    case Length: Integer of
       1 : (Chars: array[0..3] of Char);
       5 : (StringP: Pointer); // String
  end;
const
  MaxBlockCount = (4096 div SizeOf(TStringRec)) - 1;
type
  PStringsBlock = ^TStringsBlock;
  TStringsBlock = packed record
    Block: array[0..MaxBlockCount-1] of TStringRec;
    Next: PStringsBlock;
  end;

  // TStringdata is a simplied class optimized for appending strings
  TStringData = class(TObject)
  private
    FFirstBlock: PStringsBlock;
    FCurrentBlock: PStringsBlock;
    FWriteIndex: Integer;
    FLength: Integer;
    procedure AllocBlock;
    function AppendToCurrentSlot(const Value: Char): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const Value: string); overload;
    procedure Append(const Value: Char); overload;
    procedure Clear;
    function Data: string;
    property Length: Integer read FLength;
  end;

implementation

uses
  SysUtils;

{$IF CompilerVersion < 23} //pre-XE2
type
  NativeInt = integer;
{$IFEND}

{ TStringData }

constructor TStringData.Create;
begin
  inherited Create;
  FWriteIndex := MaxBlockCount;
end;

destructor TStringData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringData.AllocBlock;
var
  NewBlock: PStringsBlock;
begin
  GetMem(NewBlock, SizeOf(NewBlock^));
  FillChar(NewBlock^, SizeOf(NewBlock^), 0);
  if not Assigned(FFirstBlock) then
    FFirstBlock := NewBlock
  else
    FCurrentBlock^.Next := NewBlock;
  FCurrentBlock := NewBlock;
  FWriteIndex := 0;
end;

procedure TStringData.Append(const Value: string);
var
  ThisLength: integer;
begin
  if Value <> '' then
  begin
    // Faster than: ThisLength := System.Length(Value);
    ThisLength := PInteger(NativeInt(Value) - SizeOf(Integer))^;
    if ThisLength = 1 then
      Append(Value[1])
    else
    begin
      if not (FWriteIndex < MaxBlockCount) then
        AllocBlock;
      Inc(FLength, ThisLength);
      with FCurrentBlock^.Block[FWriteIndex] do
      begin
        Length := ThisLength;
        if ThisLength > SizeOf(Chars)
        then String(StringP) := Value
        else Move(Pointer(Value)^, Chars, ThisLength);
      end;
      Inc(FWriteIndex);
    end;
  end;
end;

function TStringData.AppendToCurrentSlot(const Value: Char): boolean;
var
  CurrStringRec: PStringRec;
begin
  Result := Assigned(FCurrentBlock) and (FWriteIndex > 0);
  if Result then
  begin
    CurrStringRec := @FCurrentBlock^.Block[FWriteIndex-1];
    Result := CurrStringRec.Length < SizeOf(CurrStringRec.Chars);
    if Result then
    begin
      CurrStringRec.Chars[CurrStringRec.Length] := Value;
      Inc(CurrStringRec.Length);
      Inc(FLength, 1);
    end;
  end;
end;

procedure TStringData.Append(const Value: Char);
begin
  if not AppendToCurrentSlot(Value) then
  begin
    if not (FWriteIndex < MaxBlockCount) then
      AllocBlock;
    Inc(FLength, 1);
    with FCurrentBlock^.Block[FWriteIndex] do
    begin
      Length := 1;
      Chars[0] := Value;
    end;
    Inc(FWriteIndex);
  end;
end;

procedure TStringData.Clear;
var
  ThisBlock: PStringsBlock;
  NextBlock: PStringsBlock;
  ThisStringRec: PStringRec;
  ThisLength: integer;
  i: integer;
begin
  ThisBlock := FFirstBlock;
  while ThisBlock <> nil do
  begin
    NextBlock := ThisBlock^.Next;
    ThisStringRec := @ThisBlock^.Block[0];
    for i := 0 to MaxBlockCount-1 do
    begin
      ThisLength := ThisStringRec.Length;
      if ThisLength = 0 then Break;
      if ThisLength > SizeOf(ThisStringRec.Chars) then
        String(ThisStringRec.StringP) := '';  // Finalize the string
      Inc(ThisStringRec);
    end;
    FreeMem(ThisBlock);
    ThisBlock := NextBlock;
  end;
  FFirstBlock := nil;
  FCurrentBlock := nil;
  FLength := 0;
  FWriteIndex := MaxBlockCount;
end;

function TStringData.Data: string;
var
  i: Integer;
  ThisBlock: PStringsBlock;
  ResultP: Pointer;
  ThisStringRec: PStringRec;
  ThisLength: integer;
begin
  SetLength(Result, FLength);
  ResultP := Pointer(Result);
  ThisBlock := FFirstBlock;
  while ThisBlock <> nil do
  begin
    ThisStringRec := @ThisBlock^.Block[0];
    for i := 0 to MaxBlockCount-1 do
    begin
      ThisLength := Abs(ThisStringRec.Length);
      if ThisLength = 0 then Break;
      if ThisLength > SizeOf(ThisStringRec.Chars)
      then Move(ThisStringRec.StringP^, ResultP^, ThisLength)
      else Move(ThisStringRec.Chars, ResultP^, ThisLength);
      Inc(NativeInt(ResultP), ThisLength);
      Inc(ThisStringRec);
    end;
    ThisBlock := ThisBlock^.Next;
  end;
end;

end.

