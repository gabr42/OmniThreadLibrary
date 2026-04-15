unit TestOtlCommon1;

interface

{$I OtlOptions.inc}

uses
  TestFramework,
  OtlCommon;

type
  TestOmniCounter = class(TTestCase)
  published
    procedure TestInitialValue;
    procedure TestIncrement;
    procedure TestDecrement;
    procedure TestTakeCount;
    procedure TestTakeReturnsZeroWhenExhausted;
    procedure TestTakeBooleanOverload;
    procedure TestValueProperty;
  end;

  TestOmniWaitableValue = class(TTestCase)
  published
    procedure TestCreateDefault;
    procedure TestSignalWithValue;
    procedure TestWaitForReturnsTrue;
    procedure TestResetClears;
    procedure TestSignalWithoutValue;
    procedure TestWaitForTimeout;
  end;

  TestOmniIntegerSet = class(TTestCase)
  private
    FChangeFired: boolean;
    procedure HandleChange(const intSet: IOmniIntegerSet);
  published
    procedure TestAddContainsRemove;
    procedure TestCountAndIsEmpty;
    procedure TestClear;
    procedure TestAsMaskRoundTrip;
    {$IFDEF OTL_HasArrayOfT}
    procedure TestAsArrayRoundTrip;
    {$ENDIF}
    procedure TestOnChangeFires;
  end;

{$IFDEF OTL_HasArrayOfT}
  TestOmniValueWrap = class(TTestCase)
  published
    procedure TestWrapUnwrapRecord;
    procedure TestFromRecordToRecord;
    procedure TestFromArrayToArray;
    procedure TestCastToInteger;
    procedure TestCastToString;
    procedure TestCastToBoolean;
    procedure TestCastToInt64;
  end;
{$ENDIF}

  TestOmniValueOwned = class(TTestCase)
  published
    procedure TestAsOwnedObject;
    procedure TestOwnsObjectProperty;
    procedure TestOwnedObjectFreedOnClear;
  end;

  TestOmniValueContainer = class(TTestCase)
  published
    procedure TestCountAndAdd;
    procedure TestAccessByIndex;
    procedure TestAccessByName;
    procedure TestExists;
    procedure TestClear;
    procedure TestLock;
  end;

implementation

uses
  SysUtils,
  Classes;

type
  TTestRecord = record
    X: integer;
    Y: integer;
  end;

{ TestOmniCounter }

procedure TestOmniCounter.TestInitialValue;
var
  counter: IOmniCounter;
begin
  counter := CreateCounter(10);
  CheckEquals(10, counter.Value);
end;

procedure TestOmniCounter.TestIncrement;
var
  counter: IOmniCounter;
begin
  counter := CreateCounter(0);
  CheckEquals(1, counter.Increment);
  CheckEquals(2, counter.Increment);
  CheckEquals(2, counter.Value);
end;

procedure TestOmniCounter.TestDecrement;
var
  counter: IOmniCounter;
begin
  counter := CreateCounter(5);
  CheckEquals(4, counter.Decrement);
  CheckEquals(3, counter.Decrement);
  CheckEquals(3, counter.Value);
end;

procedure TestOmniCounter.TestTakeCount;
var
  counter: IOmniCounter;
  taken  : integer;
begin
  counter := CreateCounter(10);
  taken := counter.Take(3);
  CheckEquals(3, taken);
  CheckEquals(7, counter.Value);
end;

procedure TestOmniCounter.TestTakeReturnsZeroWhenExhausted;
var
  counter: IOmniCounter;
  taken  : integer;
begin
  counter := CreateCounter(2);
  taken := counter.Take(5);
  CheckEquals(2, taken);
  CheckEquals(0, counter.Value);
end;

procedure TestOmniCounter.TestTakeBooleanOverload;
var
  counter: IOmniCounter;
  taken  : integer;
begin
  counter := CreateCounter(3);
  CheckTrue(counter.Take(2, taken));
  CheckEquals(2, taken);
  CheckTrue(counter.Take(5, taken));
  CheckEquals(1, taken);
  CheckFalse(counter.Take(1, taken));
end;

procedure TestOmniCounter.TestValueProperty;
var
  counter: IOmniCounter;
begin
  counter := CreateCounter(0);
  counter.Value := 42;
  CheckEquals(42, counter.Value);
  counter.Value := 0;
  CheckEquals(0, counter.Value);
end;

{ TestOmniWaitableValue }

procedure TestOmniWaitableValue.TestCreateDefault;
var
  ov: TOmniValue;
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  ov := wv.Value;
  CheckTrue(ov.IsEmpty);
end;

procedure TestOmniWaitableValue.TestSignalWithValue;
var
  ov: TOmniValue;
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  wv.Signal(42);
  ov := wv.Value;
  CheckEquals(42, ov.AsInteger);
end;

procedure TestOmniWaitableValue.TestWaitForReturnsTrue;
var
  ov: TOmniValue;
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  wv.Signal(100);
  CheckTrue(wv.WaitFor(0));
  ov := wv.Value;
  CheckEquals(100, ov.AsInteger);
end;

procedure TestOmniWaitableValue.TestResetClears;
var
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  wv.Signal(42);
  CheckTrue(wv.WaitFor(0));
  wv.Reset;
  CheckFalse(wv.WaitFor(0));
end;

procedure TestOmniWaitableValue.TestSignalWithoutValue;
var
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  wv.Signal;
  CheckTrue(wv.WaitFor(0));
end;

procedure TestOmniWaitableValue.TestWaitForTimeout;
var
  wv: IOmniWaitableValue;
begin
  wv := CreateWaitableValue;
  CheckFalse(wv.WaitFor(10));
end;

{ TestOmniIntegerSet }

procedure TestOmniIntegerSet.HandleChange(const intSet: IOmniIntegerSet);
begin
  FChangeFired := true;
end;

procedure TestOmniIntegerSet.TestAddContainsRemove;
var
  s: IOmniIntegerSet;
begin
  s := TOmniIntegerSet.Create;
  CheckFalse(s.Add(5));
  CheckFalse(s.Add(10));
  CheckTrue(s.Add(5));
  CheckTrue(s.Contains(5));
  CheckTrue(s.Contains(10));
  CheckFalse(s.Contains(7));
  CheckTrue(s.Remove(5));
  CheckFalse(s.Contains(5));
  CheckFalse(s.Remove(5));
end;

procedure TestOmniIntegerSet.TestCountAndIsEmpty;
var
  s: IOmniIntegerSet;
begin
  s := TOmniIntegerSet.Create;
  CheckTrue(s.IsEmpty);
  CheckEquals(0, s.Count);
  s.Add(1);
  s.Add(2);
  CheckFalse(s.IsEmpty);
  CheckEquals(2, s.Count);
end;

procedure TestOmniIntegerSet.TestClear;
var
  s: IOmniIntegerSet;
begin
  s := TOmniIntegerSet.Create;
  s.Add(1);
  s.Add(2);
  s.Add(3);
  s.Clear;
  CheckTrue(s.IsEmpty);
  CheckEquals(0, s.Count);
end;

procedure TestOmniIntegerSet.TestAsMaskRoundTrip;
var
  mask: uint64;
  s   : IOmniIntegerSet;
  s2  : IOmniIntegerSet;
begin
  s := TOmniIntegerSet.Create;
  s.Add(0);
  s.Add(3);
  s.Add(5);
  mask := s.AsMask;
  Check(uint64(41) = mask);

  s2 := TOmniIntegerSet.Create;
  s2.AsMask := mask;
  CheckTrue(s2.Contains(0));
  CheckTrue(s2.Contains(3));
  CheckTrue(s2.Contains(5));
  CheckFalse(s2.Contains(1));
end;

{$IFDEF OTL_HasArrayOfT}
procedure TestOmniIntegerSet.TestAsArrayRoundTrip;
var
  arr: TArray<integer>;
  s  : IOmniIntegerSet;
  s2 : IOmniIntegerSet;
begin
  s := TOmniIntegerSet.Create;
  s.Add(10);
  s.Add(20);
  s.Add(30);
  arr := s.AsArray;
  CheckEquals(3, Length(arr));

  s2 := TOmniIntegerSet.Create;
  s2.AsArray := arr;
  CheckTrue(s2.Contains(10));
  CheckTrue(s2.Contains(20));
  CheckTrue(s2.Contains(30));
  CheckEquals(3, s2.Count);
end;
{$ENDIF}

procedure TestOmniIntegerSet.TestOnChangeFires;
var
  s: IOmniIntegerSet;
begin
  FChangeFired := false;
  s := TOmniIntegerSet.Create;
  s.OnChange := HandleChange;
  s.Add(1);
  CheckTrue(FChangeFired);
end;

{ TestOmniValueWrap }

{$IFDEF OTL_HasArrayOfT}
procedure TestOmniValueWrap.TestWrapUnwrapRecord;
var
  rec : TTestRecord;
  rec2: TTestRecord;
  v   : TOmniValue;
begin
  rec.X := 10;
  rec.Y := 20;
  v := TOmniValue.Wrap<TTestRecord>(rec);
  rec2 := v.Unwrap<TTestRecord>;
  CheckEquals(10, rec2.X);
  CheckEquals(20, rec2.Y);
end;

procedure TestOmniValueWrap.TestFromRecordToRecord;
var
  rec : TTestRecord;
  rec2: TTestRecord;
  v   : TOmniValue;
begin
  rec.X := 42;
  rec.Y := 99;
  v := TOmniValue.FromRecord<TTestRecord>(rec);
  CheckTrue(v.IsRecord);
  rec2 := v.ToRecord<TTestRecord>;
  CheckEquals(42, rec2.X);
  CheckEquals(99, rec2.Y);
end;

procedure TestOmniValueWrap.TestFromArrayToArray;
var
  arr : TArray<integer>;
  arr2: TArray<integer>;
  v   : TOmniValue;
begin
  arr := TArray<integer>.Create(1, 2, 3, 4, 5);
  v := TOmniValue.FromArray<integer>(arr);
  CheckTrue(v.IsArray);
  arr2 := v.ToArray<integer>;
  CheckEquals(5, Length(arr2));
  CheckEquals(1, arr2[0]);
  CheckEquals(5, arr2[4]);
end;

procedure TestOmniValueWrap.TestCastToInteger;
var
  v: TOmniValue;
begin
  v := 42;
  CheckEquals(42, v.CastTo<integer>);
end;

procedure TestOmniValueWrap.TestCastToString;
var
  v: TOmniValue;
begin
  v := 'hello';
  CheckEquals('hello', v.CastTo<string>);
end;

procedure TestOmniValueWrap.TestCastToBoolean;
var
  v: TOmniValue;
begin
  v := true;
  CheckEquals(true, v.CastTo<boolean>);
end;

procedure TestOmniValueWrap.TestCastToInt64;
var
  v: TOmniValue;
begin
  v := int64(123456789012345);
  CheckEquals(int64(123456789012345), v.CastTo<int64>);
end;
{$ENDIF}

{ TestOmniValueOwned }

procedure TestOmniValueOwned.TestAsOwnedObject;
var
  obj: TStringList;
  v  : TOmniValue;
begin
  obj := TStringList.Create;
  v.AsOwnedObject := obj;
  CheckTrue(v.IsOwnedObject);
  CheckFalse(v.IsObject);
  CheckSame(obj, v.AsObject);
end;

procedure TestOmniValueOwned.TestOwnsObjectProperty;
var
  obj: TStringList;
  v  : TOmniValue;
begin
  obj := TStringList.Create;
  v.AsObject := obj;
  CheckFalse(v.IsOwnedObject);
  v.OwnsObject := true;
  CheckTrue(v.IsOwnedObject);
  v.Clear;
end;

procedure TestOmniValueOwned.TestOwnedObjectFreedOnClear;
var
  sl: TStringList;
  v : TOmniValue;
begin
  sl := TStringList.Create;
  v.AsOwnedObject := sl;
  CheckTrue(v.IsOwnedObject);
  v.Clear;
  CheckTrue(v.IsEmpty);
end;

{ TestOmniValueContainer }

procedure TestOmniValueContainer.TestCountAndAdd;
var
  c: TOmniValueContainer;
begin
  c := TOmniValueContainer.Create;
  try
    CheckEquals(0, c.Count);
    c.Add(1);
    c.Add(2);
    c.Add(3);
    CheckEquals(3, c.Count);
  finally c.Free; end;
end;

procedure TestOmniValueContainer.TestAccessByIndex;
var
  c : TOmniValueContainer;
  ov: TOmniValue;
begin
  c := TOmniValueContainer.Create;
  try
    c.Add(10);
    c.Add(20);
    c.Add(30);
    ov := c[0]; CheckEquals(10, ov.AsInteger);
    ov := c[1]; CheckEquals(20, ov.AsInteger);
    ov := c[2]; CheckEquals(30, ov.AsInteger);
  finally c.Free; end;
end;

procedure TestOmniValueContainer.TestAccessByName;
var
  c : TOmniValueContainer;
  ov: TOmniValue;
begin
  c := TOmniValueContainer.Create;
  try
    c.Add(42, 'answer');
    c.Add('hello', 'greeting');
    ov := c.ByName('answer');   CheckEquals(42, ov.AsInteger);
    ov := c.ByName('greeting'); CheckEquals('hello', ov.AsString);
  finally c.Free; end;
end;

procedure TestOmniValueContainer.TestExists;
var
  c: TOmniValueContainer;
begin
  c := TOmniValueContainer.Create;
  try
    c.Add(1, 'first');
    CheckTrue(c.Exists('first'));
    CheckFalse(c.Exists('second'));
  finally c.Free; end;
end;

procedure TestOmniValueContainer.TestClear;
var
  c : TOmniValueContainer;
  ov: TOmniValue;
begin
  c := TOmniValueContainer.Create;
  try
    c.Assign([1, 2, 3]);
    CheckEquals(3, c.Count);
    c.Assign([10]);
    CheckEquals(1, c.Count);
    ov := c[0]; CheckEquals(10, ov.AsInteger);
  finally c.Free; end;
end;

procedure TestOmniValueContainer.TestLock;
var
  c: TOmniValueContainer;
begin
  c := TOmniValueContainer.Create;
  try
    CheckFalse(c.IsLocked);
    c.Lock;
    CheckTrue(c.IsLocked);
  finally c.Free; end;
end;

initialization
  RegisterTest(TestOmniCounter.Suite);
  RegisterTest(TestOmniWaitableValue.Suite);
  RegisterTest(TestOmniIntegerSet.Suite);
  {$IFDEF OTL_HasArrayOfT}
  RegisterTest(TestOmniValueWrap.Suite);
  {$ENDIF}
  RegisterTest(TestOmniValueOwned.Suite);
  RegisterTest(TestOmniValueContainer.Suite);
end.
