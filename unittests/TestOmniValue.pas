unit TestOmniValue;

interface

uses
  DUnitX.TestFramework, GpStuff, Windows, TypInfo, DSiWin32, Classes, SysUtils, Variants,
  OtlCommon;

{$I OtlOptions.inc}

type
  // Test methods for class TOmniValueContainer

  [TestFixture]
  TestTOmniValueContainer = class
  strict protected
    procedure CheckSimpleType(const ov: TOmniValue; expected: array of boolean);
    procedure CheckWrappedType(const ov: TOmniValue; expected: array of boolean);
  public
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestComposed;
    [Test]
    procedure TestInterface;
    [Test]
    procedure TestSimpleValues;
    [Test]
    procedure TestWrappedValues;
  {$IFDEF OTL_TypeInfoHasTypeData}
    [Test]
    procedure TestCastToInterface_issue_128;
  {$ENDIF OTL_TypeInfoHasTypeData}
  end;

implementation

uses
  TestValue;

type
  ITestInterface = interface ['{D9F4791C-1FD3-46F6-BE8E-DF5C5356402D}']
    function GetValue: int64;
    procedure SetValue(const value: int64);
    property Value: int64 read GetValue write SetValue;
  end;

  TTestInterface = class(TInterfacedObject, ITestInterface)
  strict private
    FValue: int64;
  public
    function GetValue: int64;
    procedure SetValue(const value: int64);
  end;

procedure TestTOmniValueContainer.CheckSimpleType(const ov: TOmniValue; expected: array
  of boolean);
begin
  Assert.AreEqual(4, Length(expected));
  Assert.AreEqual<boolean>(expected[0], ov.IsBoolean);
  Assert.AreEqual<boolean>(expected[1], ov.IsInteger);
  Assert.AreEqual<boolean>(expected[2], ov.IsFloating);
  Assert.AreEqual<boolean>(expected[3], ov.IsDateTime);
end;

procedure TestTOmniValueContainer.CheckWrappedType(const ov: TOmniValue;
  expected: array of boolean);
begin
  Assert.AreEqual(4, Length(expected));
  Assert.AreEqual<boolean>(expected[0], ov.IsObject);
  Assert.AreEqual<boolean>(expected[1], ov.IsString);
  Assert.AreEqual<boolean>(expected[2], ov.IsWideString);
  Assert.AreEqual<boolean>(expected[3], ov.IsVariant);
end;

procedure TestTOmniValueContainer.TearDown;
begin
  Assert.AreEqual(0, GTestValueCount);
end;

{$IFDEF OTL_TypeInfoHasTypeData}
procedure TestTOmniValueContainer.TestCastToInterface_issue_128;
var
  ov: TOmniValue;
  intf: ITestInterface;
begin
  intf := TTestInterface.Create;
  intf.Value := $42000000000017;
  ov := intf;
  intf := nil;
  Assert.AreEqual<int64>($42000000000017, ov.CastTo<ITestInterface>.Value);
end;
{$ENDIF OTL_TypeInfoHasTypeData}

procedure TestTOmniValueContainer.TestComposed;
var
  ov: TOmniValue;
begin
  ov := TOmniValue.Create([17, '42']);
  Assert.IsTrue(ov.IsArray);
  Assert.AreEqual(integer(17), integer(ov[0]));
  Assert.AreEqual('42', string(ov[1]));
  ov := TOmniValue.CreateNamed(['42', 17]);
  Assert.IsTrue(ov.IsArray);
  Assert.AreEqual(integer(17), integer(ov['42']));
end;

procedure TestTOmniValueContainer.TestInterface;
var
  ov: TOmniValue;

  procedure TestInterface;
  var
    intf: ITestValue;
  begin
    intf := TTestValue.Create(42);
    ov.AsInterface := intf;
    Assert.IsTrue(ov.IsInterface); CheckSimpleType(ov, [false, false, false, false]); CheckWrappedType(ov, [false, false, false, false]);
    Assert.AreEqual(42, (ov.AsInterface as ITestValue).Value);
  end;

begin
  TestInterface;
  ov := true; Assert.IsFalse(ov.IsInterface);
  TestInterface;
  ov := 17; Assert.IsFalse(ov.IsInterface);
  TestInterface;
  ov := 17.42; Assert.IsFalse(ov.IsInterface);
  TestInterface;
  ov := '17'; Assert.IsFalse(ov.IsInterface);
  TestInterface;
  ov := TTestValue.Create(17); Assert.IsFalse(ov.IsInterface); ov.AsObject.Free;
  TestInterface;
  ov := TOmniValue.Create([17, '42']); Assert.IsFalse(ov.IsInterface);
  TestInterface;
  ov := TOmniValue.CreateNamed(['42', 17]); Assert.IsFalse(ov.IsInterface);
  TestInterface;
end;

procedure TestTOmniValueContainer.TestSimpleValues;
var
  ov: TOmniValue;
begin
  ov := true;
  Assert.AreEqual<boolean>(true, ov.AsBoolean);
  CheckSimpleType(ov, [true, false, false, false]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov := false;
  Assert.AreEqual<boolean>(false, ov.AsBoolean);
  CheckSimpleType(ov, [true, false, false, false]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov := 0;
  Assert.AreEqual(0, ov.AsInteger);
  CheckSimpleType(ov, [false, true, false, false]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov := 42;
  Assert.AreEqual(42, ov.AsInteger);
  CheckSimpleType(ov, [false, true, false, false]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov := 3.14;
  Assert.AreEqual(3.14, ov.AsExtended, 0);
  CheckSimpleType(ov, [false, false, true, false]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov.AsDateTime := EncodeDate(2011,12,19) + EncodeTime(19,53,42,17);
  Assert.AreEqual(
    FormatDateTime('yyyy-mm-ddThh:nn:ss.zzz', EncodeDate(2011,12,19) + EncodeTime(19,53,42,17)),
    FormatDateTime('yyyy-mm-ddThh:nn:ss.zzz', ov.AsDateTime));
  CheckSimpleType(ov, [false, false, false, true]); CheckWrappedType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
end;

procedure TestTOmniValueContainer.TestWrappedValues;
var
  ov: TOmniValue;
  v : Variant;
begin
  ov := TTestValue.Create(42);
  Assert.AreEqual('TTestValue', ov.AsObject.ClassName);
  CheckWrappedType(ov, [true, false, false, false]); CheckSimpleType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov.AsObject.Free;
  ov := '42';
  Assert.AreEqual('42', ov.AsString);
  CheckWrappedType(ov, [false, true, false, false]); CheckSimpleType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  ov.AsWideString := '17';
  Assert.AreEqual('17', string(ov.AsWideString));
  CheckWrappedType(ov, [false, false, true, false]); CheckSimpleType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
  v := 127;
  ov := v;
  Assert.AreEqual(integer(127), integer(ov.AsVariant));
  CheckWrappedType(ov, [false, false, false, true]); CheckSimpleType(ov, [false, false, false, false]); Assert.IsFalse(ov.IsInterface);
end;

{ TTestInterface }

function TTestInterface.GetValue: int64;
begin
  Result := FValue;
end;

procedure TTestInterface.SetValue(const value: int64);
begin
  FValue := value;
end;

end.
