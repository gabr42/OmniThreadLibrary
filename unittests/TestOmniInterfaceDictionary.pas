unit TestOmniInterfaceDictionary;

interface

uses
  TestFramework, GpStuff, Windows, TypInfo, DSiWin32, Classes, SysUtils, Variants,
  OtlCommon;

type
  // Test methods for class IOmniInterfaceDictionary

  TestIOmniInterfaceDictionary = class(TTestCase)
  strict private
    FIOmniInterfaceDictionary: IOmniInterfaceDictionary;
  strict protected
    procedure CheckContainsRange(low, high: integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
    procedure TestCount;
    procedure TestEnumerate;
    procedure TestRemove1;
    procedure TestRemove2;
    procedure TestResize;
    procedure TestRetrieve1;
    procedure TestRetrieve2;
    procedure TestRetrieve3;
    procedure TestRetrieve4;
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
  CheckEquals(high-low+1, FIOmniInterfaceDictionary.Count);
  keys := TGpIntegerList.CreateInterface;
  values := TGpIntegerList.CreateInterface;
  for pair in FIOmniInterfaceDictionary do begin
    keys.Add(pair.Key);
    values.Add((pair.Value as ITestValue).Value);
  end;
  CheckEquals(high-low+1, keys.Count);
  CheckEquals(high-low+1, values.Count);
  for i := low to high do begin
    CheckEquals(keys[i-low], values[i-low]);
    CheckTrue(keys.Contains(i));
    CheckTrue(values.Contains(i));
  end;
end;

procedure TestIOmniInterfaceDictionary.SetUp;
begin
  FIOmniInterfaceDictionary := CreateInterfaceDictionary;
end;

procedure TestIOmniInterfaceDictionary.TearDown;
begin
  FIOmniInterfaceDictionary := nil;
  CheckEquals(0, GTestValueCount);
end;

procedure TestIOmniInterfaceDictionary.TestClear;
var
  intf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  FIOmniInterfaceDictionary.Clear;
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestCount;
var
  intf: ITestValue;
begin
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  CheckEquals(1, FIOmniInterfaceDictionary.Count);
  FIOmniInterfaceDictionary.Clear;
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
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
  Check(FIOmniInterfaceDictionary.ValueOf(1) = nil);
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestRemove2;
var
  intf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  FIOmniInterfaceDictionary.Remove(2);
  Check(FIOmniInterfaceDictionary.ValueOf(1) <> nil);
  CheckEquals(1, FIOmniInterfaceDictionary.Count);
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
  Check(retIntf <> nil);
  CheckEquals(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve2;
var
  intf   : ITestValue;
  retIntf: ITestValue;
begin
  intf := TTestValue.Create(1);
  FIOmniInterfaceDictionary.Add(1, intf);
  retIntf := FIOmniInterfaceDictionary.ValueOf(2) as ITestValue;
  Check(retIntf = nil);
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
  Check(retIntf <> nil);
  CheckEquals(1, retIntf.Value);
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
  Check(retIntf <> nil);
  CheckEquals(1, retIntf.Value);
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
  Check(retIntf <> nil);
  CheckEquals(2, retIntf.Value);
end;

initialization
  RegisterTest(TestIOmniInterfaceDictionary.Suite);
end.
