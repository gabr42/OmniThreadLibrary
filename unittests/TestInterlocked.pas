unit TestInterlocked;

interface

uses
  DUnitX.TestFramework,
  OtlSync,
  OtlCommon;

type
  // Tests for interlocked and aligned operations (single-threaded only)
  [TestFixture]
  TInterlockedSTTest = class
  public
    [Test]
    procedure TestAligned32;
    [Test]
    procedure TestAligned32Oper;
    [Test]
    procedure TestAligned64;
    [Test]
    procedure TestInterlockedEx;
  end;

implementation

uses
  System.SyncObjs;

{ TInterlockedSTTest }

procedure TInterlockedSTTest.TestAligned32;
var
  r: packed record
    filler: byte;
    int: TOmniAlignedInt32;
  end;
begin
  Assert.IsTrue((NativeUInt(r.int.Addr) mod 4) = 0, 'alignment');
  r.int.Value := 42;

  Assert.AreEqual<integer>(42,    r.int.Value);
  Assert.AreEqual<integer>(42+17, r.int.Add(17));
  Assert.AreEqual<integer>(42+17, r.int.Value);
  Assert.AreEqual<integer>(42,    r.int.Subtract(17));
  Assert.AreEqual<integer>(42,    r.int.Value);
  Assert.AreEqual<integer>(43,    r.int.Increment);
  Assert.AreEqual<integer>(43,    r.int.Value);
  Assert.AreEqual<integer>(42,    r.int.Decrement);
  Assert.AreEqual<integer>(42,    r.int.Value);
  Assert.AreEqual<integer>(42+17, r.int.Increment(17));
  Assert.AreEqual<integer>(42+17, r.int.Value);
  Assert.AreEqual<integer>(42,    r.int.Decrement(17));
  Assert.AreEqual<integer>(42,    r.int.Value);
  Assert.IsTrue(r.int.CAS(42, 17));
  Assert.AreEqual<integer>(r.int.Value, 17);
  Assert.IsFalse(r.int.CAS(42, 17));
  Assert.AreEqual<integer>(r.int.Value, 17);

  r.int.Value := $7FFFFFFF;
  Assert.AreEqual<integer>($7FFFFFFF,  r.int.Value);
  r.int.Value := -$7FFFFFFF;
  Assert.AreEqual<integer>(-$7FFFFFFF, r.int.Value);
end;


procedure TInterlockedSTTest.TestAligned32Oper;
var
  r: packed record
    filler: word;
    int: TOmniAlignedInt32;
  end;
begin
  Assert.IsTrue((NativeUInt(r.int.Addr) mod 4) = 0, 'alignment');
  r.int.Value := 42;

  Assert.AreEqual<integer>(42, r.int.Value, 'value');
  Assert.IsFalse(r.int > 42,  'greater');
  Assert.IsTrue(r.int >= 42,  'greater or equal');
  Assert.IsFalse(r.int < 42,  'less');
  Assert.IsTrue(r.int <= 42,  'less or equal');
  Assert.IsFalse(r.int <> 42, 'not equal');
  Assert.IsTrue(r.int = 42,   'equal');
  Assert.AreEqual<integer>(42+17, r.int + 17,  'add');
  Assert.AreEqual<integer>(42,    r.int.Value, 'value after add');
  Assert.AreEqual<integer>(42-17, r.int - 17,  'subtract');
  Assert.AreEqual<integer>(42,    r.int.Value, 'value after subtract');
end;

procedure TInterlockedSTTest.TestAligned64;
var
  r: packed record
    filler: integer;
    int: TOmniAlignedInt64;
  end;
begin
  Assert.IsTrue((NativeUInt(r.int.Addr) mod 8) = 0, 'alignment');
  r.int.Value := 42;

  Assert.AreEqual<int64>(42,    r.int.Value);
  Assert.AreEqual<int64>(42+17, r.int.Add(17));
  Assert.AreEqual<int64>(42+17, r.int.Value);
  Assert.AreEqual<int64>(42,    r.int.Subtract(17));
  Assert.AreEqual<int64>(42,    r.int.Value);
  Assert.AreEqual<int64>(43,    r.int.Increment);
  Assert.AreEqual<int64>(43,    r.int.Value);
  Assert.AreEqual<int64>(42,    r.int.Decrement);
  Assert.AreEqual<int64>(42,    r.int.Value);
  Assert.AreEqual<int64>(42+17, r.int.Increment(17));
  Assert.AreEqual<int64>(42+17, r.int.Value);
  Assert.AreEqual<int64>(42,    r.int.Decrement(17));
  Assert.AreEqual<int64>(42,    r.int.Value);
  Assert.IsTrue(r.int.CAS(42, 17));
  Assert.AreEqual<int64>(r.int.Value, 17);
  Assert.IsFalse(r.int.CAS(42, 17));
  Assert.AreEqual<int64>(r.int.Value, 17);

  r.int.Value := $7FFFFFFFFFFFFFFF;
  Assert.AreEqual<int64>($7FFFFFFFFFFFFFFF,  r.int.Value);
  r.int.Value := -$7FFFFFFFFFFFFFFF;
  Assert.AreEqual<int64>(-$7FFFFFFFFFFFFFFF, r.int.Value);
end;

procedure TInterlockedSTTest.TestInterlockedEx;
var
  ni: NativeInt;
begin
  ni := 42;
  Assert.AreEqual<NativeInt>(42+17, TInterlockedEx.Add(ni, 17));
  Assert.AreEqual<NativeInt>(42+17, ni);
  Assert.AreEqual<NativeInt>(42,    TInterlockedEx.Add(ni, -17));
  Assert.AreEqual<NativeInt>(42,    ni);

  ni := 42;
  Assert.AreEqual<NativeInt>(42,    TInterlockedEx.CompareExchange(ni, 17, 42));
  Assert.AreEqual<NativeInt>(17,    ni);
  Assert.AreEqual<NativeInt>(17,    TInterlockedEx.CompareExchange(ni, 42, 43));
  Assert.AreEqual<NativeInt>(17,    ni);

  ni := 17;
  Assert.IsTrue(TInterlockedEx.CAS(17, 42, ni));
  Assert.AreEqual<NativeInt>(42, ni);
  Assert.IsFalse(TInterlockedEx.CAS(17, 43, ni));
  Assert.AreEqual<NativeInt>(42, ni);

  ni := 17;
  Assert.IsTrue(TInterlockedEx.CAS(pointer(17), pointer(42), ni));
  Assert.AreEqual<NativeInt>(42, ni);
  Assert.IsFalse(TInterlockedEx.CAS(pointer(17), pointer(43), ni));
  Assert.AreEqual<NativeInt>(42, ni);
end;

end.
