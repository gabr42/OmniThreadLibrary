unit TestOmniInterfaceDictionary;

interface

uses
  DUnitX.TestFramework, GpStuff, Windows, TypInfo, DSiWin32, Classes, SysUtils, Variants,
  OtlCommon;

type
  // Test methods for class IOmniInterfaceDictionary

  [TestFixture]
  TestIOmniInterfaceDictionary = class
  strict private
    FIOmniInterfaceDictionary: IOmniInterfaceDictionary;
  strict protected
    procedure CheckContainsRange(low, high: integer);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestClear;
    [Test]
    procedure TestCount;
    [Test]
    procedure TestEnumerate;
    [Test]
    procedure TestRemove1;
    [Test]
    procedure TestRemove2;
    [Test]
    procedure TestResize;
    [Test]
    procedure TestRetrieve1;
    [Test]
    procedure TestRetrieve2;
    [Test]
    procedure TestRetrieve3;
    [Test]
    procedure TestRetrieve4;
    [Test]
    procedure TestRetrieve5;
  end;

implementation

uses
  GpLists,
  GpStringHash,
  TestValue;

procedure TestIOmniInterfaceDictionary.CheckContainsRange(low, high: integer);
var
  i     : integer;
  keys  : IGpIntegerList;
  pair  : TOmniInterfaceDictionaryPair;
  values: IGpIntegerList;
begin
  Assert.AreEqual(high-low+1, FIOmniInterfaceDictionary.Count);
  keys := TGpIntegerList.CreateInterface;
  values := TGpIntegerList.CreateInterface;
  for pair in FIOmniInterfaceDictionary do begin
    keys.Add(pair.Key);
    values.Add((pair.Value as ITestValue).Value);
  end;
  Assert.AreEqual(high-low+1, keys.Count);
  Assert.AreEqual(high-low+1, values.Count);
  for i := low to high do begin
    Assert.AreEqual(keys[i-low], values[i-low]);
    Assert.IsTrue(keys.Contains(i));
    Assert.IsTrue(values.Contains(i));
  end;
end;

procedure TestIOmniInterfaceDictionary.SetUp;
begin
  FIOmniInterfaceDictionary := CreateInterfaceDictionary;
end;

procedure TestIOmniInterfaceDictionary.TearDown;
begin
  FIOmniInterfaceDictionary := nil;
  Assert.AreEqual(0, GTestValueCount);
end;

procedure TestIOmniInterfaceDictionary.TestClear;
var
  intf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  FIOmniInterfaceDictionary.Clear;
  Assert.AreEqual(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestCount;
var
  intf: ITestValue;
begin
  Assert.AreEqual(0, FIOmniInterfaceDictionary.Count);
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  Assert.AreEqual(1, FIOmniInterfaceDictionary.Count);
  FIOmniInterfaceDictionary.Clear;
  Assert.AreEqual(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestEnumerate;
const
  CNumElements = 3 ;
var
  i   : integer;
  intf: ITestValue;
begin
  for i := 1 to CNumElements do begin
    intf := TTestValue.Create(i);
    FIOmniInterfaceDictionary.Add(i, intf);
  end;
  CheckContainsRange(1, CNumElements);
end;

procedure TestIOmniInterfaceDictionary.TestRemove1;
var
  intf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  FIOmniInterfaceDictionary.Remove(1);
  Assert.IsNull(FIOmniInterfaceDictionary.ValueOf(1));
  Assert.AreEqual(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestRemove2;
var
  intf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  FIOmniInterfaceDictionary.Remove(2);
  Assert.IsNotNull(FIOmniInterfaceDictionary.ValueOf(1));
  Assert.AreEqual(1, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestResize;
var
  i          : integer;
  intf: ITestValue;
  numElements: integer;
begin
  numElements := GetGoodHashSize(1) * 2;
  for i := 1 to numElements do begin
    intf := TTestValue.Create(i);
    FIOmniInterfaceDictionary.Add(i, intf);
  end;
  CheckContainsRange(1, numElements);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve1;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  Assert.IsNotNull(retIntf);
  Assert.AreEqual(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve2;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(2) as ITestValue;
  Assert.IsNull(retIntf);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve3;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  intf := TTestValue.Create(2);
  FIOmniInterfaceDictionary.Add(2, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  Assert.IsNotNull(retIntf);
  Assert.AreEqual(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve4;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(2);
  FIOmniInterfaceDictionary.Add(2, intf);
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  Assert.IsNotNull(retIntf);
  Assert.AreEqual(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve5;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  intf := TTestValue.Create(2);
  FIOmniInterfaceDictionary.Add(1, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  Assert.IsNotNull(retIntf);
  Assert.AreEqual(2, retIntf.Value);
end;

end.
