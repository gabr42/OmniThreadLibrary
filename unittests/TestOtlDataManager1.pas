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
  protected
    function GetNext(pkg: TOmniDataPackage; cnt: integer): string;
  published
    procedure TestCreation;
    procedure TestSplit;
  end;

  // Test methods for class TOmniSourceProvider
  TestTOmniIntegerSourceProvider = class(TestTTOmniIntegerData)
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

function TestTOmniIntegerDataPackage.GetNext(pkg: TOmniDataPackage; cnt: integer): string;
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

procedure TestTOmniIntegerDataPackage.TestCreation;
var
  value: TOmniValue;
begin
  Initialize(1, 3, 1);
  CheckFalse(FOmniDataPackage.GetNext(value));
  Cleanup;
end;

procedure TestTOmniIntegerDataPackage.TestSplit;

  procedure Split(low, high, step, fetch1: integer; const result1: string;
    splitok: boolean; fetch2: integer; const result2: string; fetch3: integer;
    const result3: string);
  var
    pkg2 : TOmniDataPackage;
    value: TOmniValue;
  begin
    Initialize(low, high, step);
    try
      CheckTrue(FOmniSourceProvider.GetPackage(FOmniSourceProvider.Count, FOmniDataPackage));
      if fetch1 > 0 then
        CheckEquals(result1, GetNext(FOmniDataPackage, fetch1));
      pkg2 := FOmniSourceProvider.CreateDataPackage;
      try
        Check(assigned(pkg2));
        if splitOK then begin
          CheckTrue(FOmniDataPackage.Split(pkg2));
          CheckEquals(result2, GetNext(pkg2, fetch2));
        end
        else
          CheckFalse(FOmniDataPackage.Split(pkg2));
      finally pkg2.Free; end;
      CheckEquals(result3, GetNext(FOmniDataPackage, fetch3));
    finally Cleanup; end;
  end;

begin
  Split(1, 3,  1, 1, '1',     true,  2, '2/-', 2, '3/-');
  Split(1, 3,  1, 0, '',      true,  2, '1/2', 2, '3/-');
  Split(1, 3,  1, 2, '1/2',   true,  2, '3/-', 1, '-');
  Split(1, 3,  1, 3, '1/2/3', false, 0, '',    1, '-');
  Split(1, 10, 2, 1, '1',     true,  2, '3/5', 3, '7/9/-');
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

