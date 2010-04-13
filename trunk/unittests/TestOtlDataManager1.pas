unit TestOtlDataManager1;

interface

uses
  TestFramework, OtlCommon, OtlDataManager, GpStuff;

type
  TestTTOmniIntegerData = class(TTestCase)
  public
    FOmniDataPackage   : TOmniDataPackage;
    FOmniSourceProvider: TOmniSourceProvider;
    procedure Initialize(low, high, step: integer);
    procedure Cleanup;
  end;

  // Test methods for class TOmniDataPackage
  TestTOmniIntegerDataPackage = class(TestTTOmniIntegerData)
  published
    procedure TestCreation;
    procedure TestSplit;
  end;

  // Test methods for class TOmniSourceProvider
  TestTOmniIntegerSourceProvider = class(TestTTOmniIntegerData)
  strict protected
  public
    procedure TestLoop(low, high, step, count: integer);
  published
    procedure TestLoops;
  end;

implementation

uses
  SysUtils;

procedure TestTTOmniIntegerData.Cleanup;
begin
  FOmniDataPackage.Free;
  FOmniDataPackage := nil;
  FOmniSourceProvider.Free;
  FOmniSourceProvider := nil;
end;

procedure TestTTOmniIntegerData.Initialize(low, high, step: integer);
begin
  FOmniSourceProvider := CreateSourceProvider(low, high, step);
  FOmniDataPackage := FOmniSourceProvider.CreateDataPackage;
end;

procedure TestTOmniIntegerDataPackage.TestCreation;
var
  value: TOmniValue;
begin
  Initialize(1, 3, 1);
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;
end;

procedure TestTOmniIntegerDataPackage.TestSplit;
var
  pkg2 : TOmniDataPackage;
  value: TOmniValue;
begin
  Initialize(1, 3, 1);
  CheckTrue(FOmniSourceProvider.GetPackage(FOmniSourceProvider.Count, FOmniDataPackage));
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(1, value.AsInteger);
  pkg2 := FOmniSourceProvider.CreateDataPackage;
  Check(assigned(pkg2));
  CheckTrue(FOmniDataPackage.Split(pkg2));
  CheckTrue(pkg2.GetNext(value));
  CheckEquals(2, value.AsInteger);
  CheckFalse(pkg2.GetNext(value));
  pkg2.Free;
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(3, value.AsInteger);
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;

  Initialize(1, 3, 1);
  CheckTrue(FOmniSourceProvider.GetPackage(FOmniSourceProvider.Count, FOmniDataPackage));
  pkg2 := FOmniSourceProvider.CreateDataPackage;
  Check(assigned(pkg2));
  CheckTrue(FOmniDataPackage.Split(pkg2));
  CheckTrue(pkg2.GetNext(value));
  CheckEquals(1, value.AsInteger);
  CheckFalse(pkg2.GetNext(value));
  pkg2.Free;
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(2, value.AsInteger);
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(3, value.AsInteger);
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;

  Initialize(1, 3, 1);
  CheckTrue(FOmniSourceProvider.GetPackage(FOmniSourceProvider.Count, FOmniDataPackage));
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(1, value.AsInteger);
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(2, value.AsInteger);
  pkg2 := FOmniSourceProvider.CreateDataPackage;
  Check(assigned(pkg2));
  CheckTrue(FOmniDataPackage.Split(pkg2));
  CheckTrue(pkg2.GetNext(value));
  CheckEquals(3, value.AsInteger);
  CheckFalse(pkg2.GetNext(value));
  pkg2.Free;
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;

  Initialize(1, 3, 1);
  CheckTrue(FOmniSourceProvider.GetPackage(FOmniSourceProvider.Count, FOmniDataPackage));
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(1, value.AsInteger);
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(2, value.AsInteger);
  CheckTrue(FOmniDataPackage.GetNext(value));
  CheckEquals(3, value.AsInteger);
  pkg2 := FOmniSourceProvider.CreateDataPackage;
  Check(assigned(pkg2));
  CheckFalse(FOmniDataPackage.Split(pkg2));
  pkg2.Free;
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;
end;

procedure TestTOmniIntegerSourceProvider.TestLoop(low, high, step, count: integer);
var
  dataCount: integer;
  expVal   : integer;
  numPkg   : integer;
  value    : TOmniValue;
begin
  for dataCount := 1 to count + 1 do begin
    Initialize(low, high, step);
    Check(spcCountable in FOmniSourceProvider.GetCapabilities);
    CheckEquals(count, FOmniSourceProvider.Count);
    expVal := low;
    while FOmniSourceProvider.GetPackage(dataCount, FOmniDataPackage) do begin
      numPkg := 0;
      while FOmniDataPackage.GetNext(value) do begin
        Inc(numPkg);
        CheckEquals(expVal, value);
        expVal := expVal + step;
      end;
      if ((step > 0) and (expVal <= high)) or
         ((step < 0) and (expVal >= high))
      then
        CheckEquals(numPkg, dataCount);
    end;
    if step > 0 then
      Check(expVal > high)
    else
      Check(expVal < high);
    Cleanup;
  end;
end;

procedure TestTOmniIntegerSourceProvider.TestLoops;
var
  step: integer;
begin
  for step := 1 to 5 do begin
    TestLoop(1, 5, step, (5 - 1 + step) div step);
    TestLoop(5, 1, -step, (5 - 1 + step) div step);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOmniIntegerDataPackage.Suite);
  RegisterTest(TestTOmniIntegerSourceProvider.Suite);
end.

