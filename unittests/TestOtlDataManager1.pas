unit TestOtlDataManager1;

interface

{$IFDEF Unicode}
uses
  DUnitX.TestFramework, OtlCommon, OtlDataManager, OtlCollections, GpStuff;

type
  TestTOmniDataManager = class
  public
    FOmniDataPackage   : TOmniDataPackage;
    FOmniSourceProvider: TOmniSourceProvider;
    procedure Cleanup; virtual; abstract;
    procedure CheckCapabilities(cap: TOmniSourceProviderCapabilities); virtual; abstract;
    function  GetNext(pkg: TOmniDataPackage; cnt: integer): string;
    procedure Initialize(low, high, step: integer); virtual; abstract;
    procedure Split(low, high, step, fetch1: integer; const result1: string;
      splitok: boolean; fetch2: integer; const result2: string; fetch3: integer;
      const result3: string);
    procedure TestLoop(low, high, step, count: integer);
    procedure TestLoopsImpl;
  end;

  TestIntegerProvider = class(TestTOmniDataManager)
  public
    procedure CheckCapabilities(cap: TOmniSourceProviderCapabilities); override;
    procedure Cleanup; override;
    procedure Initialize(low, high, step: integer); override;
  end;

  [TestFixture]
  TestIntegerDataPackage = class(TestIntegerProvider)
  public
    [Test]
    procedure TestCreation;
  end;

  [TestFixture]
  TestIntegerSourceProvider = class(TestIntegerProvider)
  public
    [Test]
    procedure TestLoops;
    [Test]
    procedure TestSplit;
  end;

  TestOmniValueProvider = class(TestTOmniDataManager)
  private
    FCollection: TOmniBlockingCollection;
  public
    procedure CheckCapabilities(cap: TOmniSourceProviderCapabilities); override;
    procedure Cleanup; override;
    procedure Initialize(low, high, step: integer); override;
  end;

  [TestFixture]
  TestOmniValueDataPackage = class(TestOmniValueProvider)
  public
    [Test]
    procedure TestCreation;
  end;

  [TestFixture]
  TestOmniValueSourceProvider = class(TestOmniValueProvider)
  public
    [Test]
    procedure TestLoops;
    [Test]
    procedure TestSplit;
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  Windows,
  SysUtils;

function TestTOmniDataManager.GetNext(pkg: TOmniDataPackage; cnt: integer): string;
var
  iData: integer;
  value: TOmniValue;
begin
  Result := '';
  for iData := 1 to cnt do begin
    if iData > 1 then
      Result := Result + '/';
    if pkg.GetNext(value) then
      Result := Result + value
    else
      Result := Result + '-';
  end;
end;

procedure TestTOmniDataManager.Split(low, high, step, fetch1: integer; const result1:
  string; splitok: boolean; fetch2: integer; const result2: string; fetch3: integer;
  const result3: string);
var
  pkg2: TOmniDataPackage;
begin
  Initialize(low, high, step);
  try
    Assert.IsTrue(FOmniSourceProvider.GetPackage($FFFF, FOmniDataPackage));
    if fetch1 > 0 then
      Assert.AreEqual(result1, GetNext(FOmniDataPackage, fetch1));
    pkg2 := FOmniSourceProvider.CreateDataPackage;
    try
      Assert.IsTrue(assigned(pkg2));
      if splitOK then begin
        Assert.IsTrue(FOmniDataPackage.Split(pkg2));
        Assert.AreEqual(result2, GetNext(pkg2, fetch2));
      end
      else
        Assert.IsFalse(FOmniDataPackage.Split(pkg2));
    finally pkg2.Free; end;
    Assert.AreEqual(result3, GetNext(FOmniDataPackage, fetch3));
  finally Cleanup; end;
end;

procedure TestTOmniDataManager.TestLoop(low, high, step, count: integer);
var
  dataCount: integer;
  expVal   : integer;
  numPkg   : integer;
  value    : TOmniValue;
begin
  for dataCount := 1 to count + 1 do begin
    Initialize(low, high, step);
    CheckCapabilities(FOmniSourceProvider.GetCapabilities);
    if spcCountable in FOmniSourceProvider.GetCapabilities then
      Assert.AreEqual(count, integer(FOmniSourceProvider.Count));
    expVal := low;
    while FOmniSourceProvider.GetPackage(dataCount, FOmniDataPackage) do begin
      numPkg := 0;
      while FOmniDataPackage.GetNext(value) do begin
        Inc(numPkg);
        Assert.AreEqual(expVal, value.AsInteger);
        expVal := expVal + step;
      end;
      if ((step > 0) and (expVal <= high)) or
         ((step < 0) and (expVal >= high))
      then
        Assert.AreEqual(numPkg, dataCount);
    end;
    if step > 0 then
      Assert.IsTrue(expVal > high)
    else
      Assert.IsTrue(expVal < high);
    Cleanup;
  end;
end;

procedure TestTOmniDataManager.TestLoopsImpl;
var
  step: integer;
begin
  for step := 1 to 5 do begin
    TestLoop(1, 5, step, (5 - 1 + step) div step);
    TestLoop(5, 1, -step, (5 - 1 + step) div step);
  end;
end;

procedure TestIntegerProvider.CheckCapabilities(cap: TOmniSourceProviderCapabilities);
begin
  Assert.IsTrue(cap = [spcCountable, spcFast]);
end;

procedure TestIntegerProvider.Cleanup;
begin
  FOmniDataPackage.Free;
  FOmniDataPackage := nil;
  FOmniSourceProvider.Free;
  FOmniSourceProvider := nil;
end;

procedure TestIntegerProvider.Initialize(low, high, step: integer);
begin
  FOmniSourceProvider := CreateSourceProvider(low, high, step);
  FOmniDataPackage := FOmniSourceProvider.CreateDataPackage;
end;

procedure TestIntegerDataPackage.TestCreation;
var
  value: TOmniValue;
begin
  Initialize(1, 3, 1);
  Assert.IsFalse(FOmniDataPackage.GetNext(value));
  Cleanup;
end;

procedure TestIntegerSourceProvider.TestLoops;
begin
  TestLoopsImpl;
end;

procedure TestIntegerSourceProvider.TestSplit;
begin
  Split(1, 3,  1, 1, '1',     true,  2, '2/-', 2, '3/-');
  Split(1, 3,  1, 0, '',      true,  2, '1/2', 2, '3/-');
  Split(1, 3,  1, 2, '1/2',   true,  2, '3/-', 1, '-');
  Split(1, 3,  1, 3, '1/2/3', false, 0, '',    1, '-');
  Split(1, 10, 2, 1, '1',     true,  2, '3/5', 3, '7/9/-');
end;

procedure TestOmniValueProvider.CheckCapabilities(cap: TOmniSourceProviderCapabilities);
begin
  Assert.IsTrue(cap = [spcDataLimit]);
end;

procedure TestOmniValueProvider.Cleanup;
begin
  FOmniDataPackage.Free;
  FOmniDataPackage := nil;
  FOmniSourceProvider.Free;
  FOmniSourceProvider := nil;
  FCollection.Free;
  FCollection := nil;
end;

procedure TestOmniValueProvider.Initialize(low, high, step: integer);
begin
  FCollection := TOmniBlockingCollection.Create;
  while low*step <= high*step do begin
    FCollection.Add(low);
    low := low + step;
  end;
  FCollection.CompleteAdding;
  FOmniSourceProvider := CreateSourceProvider(FCollection.GetEnumerator);
  FOmniDataPackage := FOmniSourceProvider.CreateDataPackage;
end;

procedure TestOmniValueDataPackage.TestCreation;
var
  value: TOmniValue;
begin
  Initialize(1, 3, 1);
  Assert.IsFalse(FOmniDataPackage.GetNext(value));
  Cleanup;
end;

procedure TestOmniValueSourceProvider.TestLoops;
begin
  TestLoopsImpl;
end;

procedure TestOmniValueSourceProvider.TestSplit;
begin
  Split(1, 3,  1, 1, '1',     true,  2, '2/-', 2, '3/-');
  Split(1, 3,  1, 0, '',      true,  2, '1/-', 3, '2/3/-');
  Split(1, 3,  1, 2, '1/2',   false, 0, '',    2, '3/-');
  Split(1, 3,  1, 3, '1/2/3', false, 0, '',    1, '-');
  Split(1, 10, 2, 1, '1',     true,  2, '3/5', 3, '7/9/-');
end;

{$ENDIF}
end.
