unit TestInterlocked;

interface

uses
  TestFramework,
  OtlSync,
  OtlCommon;

type
  // Tests for interlocked and aligned operations (single-threaded only)
  TInterlockedSTTest = class(TTestCase)
  published
    procedure TestAligned32;
    procedure TestAligned32Oper;
    procedure TestAligned64;
    procedure TestInterlockedEx;
  end;

implementation

uses
  SyncObjs;

{ TInterlockedSTTest }

procedure TInterlockedSTTest.TestAligned32;
var
  r: packed record
    filler: byte;
    int: TOmniAlignedInt32;
  end;
begin
  CheckTrue((NativeUInt(r.int.Addr) mod 4) = 0, 'alignment');
  r.int.Value := 42;

  CheckEquals(42,    r.int.Value);
  CheckEquals(42+17, r.int.Add(17));
  CheckEquals(42+17, r.int.Value);
  CheckEquals(42,    r.int.Subtract(17));
  CheckEquals(42,    r.int.Value);
  CheckEquals(43,    r.int.Increment);
  CheckEquals(43,    r.int.Value);
  CheckEquals(42,    r.int.Decrement);
  CheckEquals(42,    r.int.Value);
  CheckEquals(42+17, r.int.Increment(17));
  CheckEquals(42+17, r.int.Value);
  CheckEquals(42,    r.int.Decrement(17));
  CheckEquals(42,    r.int.Value);
  CheckTrue(r.int.CAS(42, 17));
  CheckEquals(r.int.Value, 17);
  CheckFalse(r.int.CAS(42, 17));
  CheckEquals(r.int.Value, 17);

  r.int.Value := $7FFFFFFF;
  CheckEquals($7FFFFFFF,  r.int.Value);
  r.int.Value := -$7FFFFFFF;
  CheckEquals(-$7FFFFFFF, r.int.Value);
end;


procedure TInterlockedSTTest.TestAligned32Oper;
var
  r: packed record
    filler: word;
    int: TOmniAlignedInt32;
  end;
begin
  CheckTrue((NativeUInt(r.int.Addr) mod 4) = 0, 'alignment');
  r.int.Value := 42;

  CheckEquals(42, r.int.Value, 'value');
  CheckFalse(r.int > 42,  'greater');
  CheckTrue(r.int >= 42,  'greater or equal');
  CheckFalse(r.int < 42,  'less');
  CheckTrue(r.int <= 42,  'less or equal');
  CheckFalse(r.int <> 42, 'not equal');
  CheckTrue(r.int = 42,   'equal');
  CheckEquals(42+17, r.int + 17,  'add');
  CheckEquals(42,    r.int.Value, 'value after add');
  CheckEquals(42-17, r.int - 17,  'subtract');
  CheckEquals(42,    r.int.Value, 'value after subtract');
end;

procedure TInterlockedSTTest.TestAligned64;
var
  r: packed record
    filler: integer;
    int: TOmniAlignedInt64;
  end;
begin
  CheckTrue((NativeUInt(r.int.Addr) mod 8) = 0, 'alignment');
  r.int.Value := 42;

  CheckEquals(42,    r.int.Value);
  CheckEquals(42+17, r.int.Add(17));
  CheckEquals(42+17, r.int.Value);
  CheckEquals(42,    r.int.Subtract(17));
  CheckEquals(42,    r.int.Value);
  CheckEquals(43,    r.int.Increment);
  CheckEquals(43,    r.int.Value);
  CheckEquals(42,    r.int.Decrement);
  CheckEquals(42,    r.int.Value);
  CheckEquals(42+17, r.int.Increment(17));
  CheckEquals(42+17, r.int.Value);
  CheckEquals(42,    r.int.Decrement(17));
  CheckEquals(42,    r.int.Value);
  CheckTrue(r.int.CAS(42, 17));
  CheckEquals(r.int.Value, 17);
  CheckFalse(r.int.CAS(42, 17));
  CheckEquals(r.int.Value, 17);

  r.int.Value := $7FFFFFFFFFFFFFFF;
  CheckEquals($7FFFFFFFFFFFFFFF,  r.int.Value);
  r.int.Value := -$7FFFFFFFFFFFFFFF;
  CheckEquals(-$7FFFFFFFFFFFFFFF, r.int.Value);
end;

procedure TInterlockedSTTest.TestInterlockedEx;
var
  ni: NativeInt;
begin
  ni := 42;
  CheckEquals(42+17, TInterlockedEx.Add(ni, 17));
  CheckEquals(42+17, ni);
  CheckEquals(42,    TInterlockedEx.Add(ni, -17));
  CheckEquals(42,    ni);

  ni := 42;
  CheckEquals(42,    TInterlockedEx.CompareExchange(ni, 17, 42));
  CheckEquals(17,    ni);
  CheckEquals(17,    TInterlockedEx.CompareExchange(ni, 42, 43));
  CheckEquals(17,    ni);

  ni := 17;
  CheckTrue(TInterlockedEx.CAS(17, 42, ni));
  CheckEquals(42, ni);
  CheckFalse(TInterlockedEx.CAS(17, 43, ni));
  CheckEquals(42, ni);

  ni := 17;
  CheckTrue(TInterlockedEx.CAS(pointer(17), pointer(42), ni));
  CheckEquals(42, ni);
  CheckFalse(TInterlockedEx.CAS(pointer(17), pointer(43), ni));
  CheckEquals(42, ni);
end;

initialization
  RegisterTest(TInterlockedSTTest.Suite);
end.
