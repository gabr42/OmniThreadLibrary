unit StressTestBlockingCollection1;

interface

{$IFDEF Unicode}
uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils,
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync;

type
  // Test methods for class IOmniBlockingCollection
  TestIOmniBlockingCollection = class(TTestCase)
  private
  published
    procedure TestCompleteAdding;
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  OtlParallel,
  Classes;

type
  TMemLeakCheckObj=class(TInterfacedObject)
    constructor Create;
    destructor Destroy; override;
  end;

var
  vMemLeakCheckObjCount: integer = 0;

procedure TestIOmniBlockingCollection.TestCompleteAdding;
var
  coll     : IOmniBlockingCollection;
  iTest    : integer;
  lastAdded: integer;
  lastRead : TOmniValue;
begin
  for iTest := 1 to 1000 do begin
    coll := TOmniBlockingCollection.Create;
    lastAdded := -1;
    lastRead := -2;
    Parallel.Join([
      procedure
      var
        i: integer;
      begin
        for i := 1 to 100000 do begin
          if not coll.TryAdd(i) then
            break;
          lastAdded := i;
        end;
      end,

      procedure
      begin
        Sleep(1);
        coll.CompleteAdding;
      end,

      procedure
      begin
        while coll.TryTake(lastRead, INFINITE) do
          ;
      end
    ]).Execute;
    if (lastAdded > 0) and (lastRead.AsInteger > 0) and (lastAdded <> lastRead.AsInteger) then
      break; //for iTest
  end;
  CheckEquals(lastAdded, lastRead.AsInteger);
end;

{ TMemLeakCheckObj }

constructor TMemLeakCheckObj.Create;
begin
  InterlockedIncrement(vMemLeakCheckObjCount);
  inherited;
end;

destructor TMemLeakCheckObj.Destroy;
begin
  inherited;
  InterlockedDecrement(vMemLeakCheckObjCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestIOmniBlockingCollection.Suite);
{$ENDIF}
end.

