unit TestBlockingCollection1;

interface

{$I OtlOptions.inc}

{$IFDEF Unicode}
uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils,
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync;

type
  // Test methods for class IOmniBlockingCollection
  TestIOmniBlockingCollection = class(TTestCase)
  private
    procedure FillOmniValueWithOwnedObject(VAR lValue:TOmniValue);
  published
    procedure TestCompleteAdding;
    procedure TestOwnedObjectleak;
    procedure TestOmniValueObjectleak;
    procedure TestInterfaceLeak;
    procedure TestTryTakeEmpty;
    {$IFDEF OTL_HasSystemThreading}
    procedure TestTryTakeWithTimeout;
    {$ENDIF}
    procedure TestCountAndIsEmpty;
    procedure TestIsCompletedAndIsFinalized;
    procedure TestGetEnumerator;
    procedure TestNext;
    {$IFDEF OTL_GoodGenerics}
    procedure TestFromArrayToArray;
    procedure TestToArrayWordMax;
    procedure TestToArraySmallintNegative;
    procedure TestToArrayInt64;
    procedure TestToArrayCardinalMax;
    procedure TestToArrayObjectAtHighAddress;
    procedure TestToArrayTooBig;
    procedure TestAddRange;
    {$ENDIF}
    {$IFDEF OTL_HasSystemThreading}
    procedure TestMultiConsumerTryTake;
    {$ENDIF}
  end;
{$ENDIF}

implementation

{$IFDEF Unicode}
uses
  Classes,
  {$IFDEF OTL_HasSystemThreading}
  Threading,
  {$ENDIF}
  OtlParallel;

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
  lastAdded: integer;
  lastRead : TOmniValue;
begin
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

procedure TestIOmniBlockingCollection.TestInterfaceLeak;
const cTestSize=10;
VAR i:integer;
    lCollection:IOmniBlockingCollection;
    lValue:TOmniValue;
begin
  lCollection := TOmniBlockingCollection.Create;
  vMemLeakCheckObjCount := 0;
  for i := 1 to cTestSize do begin
    lValue.AsInterface := TMemLeakCheckObj.Create;
    lCollection.Add(lValue);
  end;
  lValue.Clear;
  CheckEquals(cTestSize, vMemLeakCheckObjCount);
  for i := 1 to cTestSize do
    lCollection.Take(lValue);
  lCollection := nil;

  CheckEquals(1, vMemLeakCheckObjCount);
  lValue.Clear; // drop the last interface in the queue
  CheckEquals(0, vMemLeakCheckObjCount);
end;

//Using a separate routine to set the AsOwnedObject property is required because
//the compiler generates code that keeps the last created object alive (refcount) until the
//routine is actually finished
procedure TestIOmniBlockingCollection.FillOmniValueWithOwnedObject(var lValue: TOmniValue);
begin
  lValue.AsOwnedObject := TMemLeakCheckObj.Create;
end;

procedure TestIOmniBlockingCollection.TestOmniValueObjectleak;
VAR lValue:TOmniValue;
begin
  vMemLeakCheckObjCount := 0;
  FillOmniValueWithOwnedObject(lValue);
  CheckEquals(1, vMemLeakCheckObjCount);
  lValue.Clear; // one would expect the owned object to be destroyed here, but it does NOT

  CheckEquals(0, vMemLeakCheckObjCount); // this test Fails
end;

procedure TestIOmniBlockingCollection.TestOwnedObjectleak;
const
  cTestSize = 10;
var
  i          : integer;
  lCollection: IOmniBlockingCollection;
  lValue     : TOmniValue;

begin
  lCollection := TOmniBlockingCollection.Create;
  vMemLeakCheckObjCount := 0;
  for i := 1 to cTestSize do begin
    FillOmniValueWithOwnedObject(lValue);
    lCollection.Add(lValue);
  end;
  lValue.Clear;
  CheckEquals(cTestSize, vMemLeakCheckObjCount);

  for i := 1 to cTestSize do
    lCollection.Take(lValue);
  lCollection := nil;

  CheckEquals(1, vMemLeakCheckObjCount);

  // drop the last owned object in the queue
  lValue.Clear; // drop the last owned object in the queue

  // this test fails for some strange reason, obviously the lValue is not
  // released until the end of the routine eventhough it is actually cleared
  CheckEquals(0, vMemLeakCheckObjCount);
end;

procedure TestIOmniBlockingCollection.TestTryTakeEmpty;
var
  coll : IOmniBlockingCollection;
  value: TOmniValue;
begin
  coll := TOmniBlockingCollection.Create;
  CheckFalse(coll.TryTake(value, 0));
end;

{$IFDEF OTL_HasSystemThreading}
procedure TestIOmniBlockingCollection.TestTryTakeWithTimeout;
var
  coll : IOmniBlockingCollection;
  value: TOmniValue;
begin
  coll := TOmniBlockingCollection.Create;

  // Start a thread that adds a value after a short delay
  System.Threading.TTask.Run(
    procedure
    begin
      Sleep(100);
      coll.Add(42);
    end);

  // TryTake should block and then succeed
  CheckTrue(coll.TryTake(value, 10000));
  CheckEquals(42, value.AsInteger);
end;
{$ENDIF}

procedure TestIOmniBlockingCollection.TestCountAndIsEmpty;
var
  coll : IOmniBlockingCollection;
  value: TOmniValue;
begin
  coll := TOmniBlockingCollection.Create;
  CheckTrue(coll.IsEmpty);
  CheckEquals(0, coll.Count);

  coll.Add(1);
  coll.Add(2);
  CheckFalse(coll.IsEmpty);
  CheckEquals(2, coll.Count);

  coll.Take(value);
  CheckEquals(1, coll.Count);

  coll.Take(value);
  CheckTrue(coll.IsEmpty);
end;

procedure TestIOmniBlockingCollection.TestIsCompletedAndIsFinalized;
var
  coll : IOmniBlockingCollection;
  value: TOmniValue;
begin
  coll := TOmniBlockingCollection.Create;
  CheckFalse(coll.IsCompleted);
  CheckFalse(coll.IsFinalized);

  coll.Add(1);
  coll.CompleteAdding;
  CheckTrue(coll.IsCompleted);
  CheckFalse(coll.IsFinalized);

  coll.Take(value);
  // After draining all items from a completed collection, it should be finalized
  CheckTrue(coll.IsFinalized);
end;

procedure TestIOmniBlockingCollection.TestGetEnumerator;
var
  coll : IOmniBlockingCollection;
  enum : IOmniValueEnumerator;
  sum  : integer;
  count: integer;
begin
  coll := TOmniBlockingCollection.Create;
  coll.Add(10);
  coll.Add(20);
  coll.Add(30);
  coll.CompleteAdding;

  sum := 0;
  count := 0;
  enum := coll.GetEnumerator;
  while enum.MoveNext do begin
    sum := sum + enum.Current.AsInteger;
    Inc(count);
  end;
  CheckEquals(3, count);
  CheckEquals(60, sum);
end;

procedure TestIOmniBlockingCollection.TestNext;
var
  coll: IOmniBlockingCollection;
begin
  coll := TOmniBlockingCollection.Create;
  coll.Add(100);
  coll.Add(200);
  coll.CompleteAdding;

  CheckEquals(100, coll.Next.AsInteger);
  CheckEquals(200, coll.Next.AsInteger);
end;

{$IFDEF OTL_GoodGenerics}
procedure TestIOmniBlockingCollection.TestToArrayWordMax;
var
  coll  : IOmniBlockingCollection;
  result: TArray<word>;
begin
  coll := TOmniBlockingCollection.FromRange<word>([0, 65535]);
  coll.CompleteAdding;
  result := TOmniBlockingCollection.ToArray<word>(coll);
  CheckEquals(2, Length(result));
  CheckTrue(result[0] = 0);
  CheckTrue(result[1] = 65535);
end;

procedure TestIOmniBlockingCollection.TestToArraySmallintNegative;
var
  coll  : IOmniBlockingCollection;
  result: TArray<smallint>;
begin
  coll := TOmniBlockingCollection.FromRange<smallint>([-1, -32768, 32767]);
  coll.CompleteAdding;
  result := TOmniBlockingCollection.ToArray<smallint>(coll);
  CheckEquals(3, Length(result));
  CheckTrue(result[0] = -1);
  CheckTrue(result[1] = -32768);
  CheckTrue(result[2] = 32767);
end;

procedure TestIOmniBlockingCollection.TestToArrayInt64;
var
  coll  : IOmniBlockingCollection;
  result: TArray<int64>;
begin
  coll := TOmniBlockingCollection.FromRange<int64>([0, 5, -1, Low(int64), High(int64)]);
  coll.CompleteAdding;
  result := TOmniBlockingCollection.ToArray<int64>(coll);
  CheckEquals(5, Length(result));
  CheckTrue(result[0] = 0);
  CheckTrue(result[1] = 5);
  CheckTrue(result[2] = -1);
  CheckTrue(result[3] = Low(int64));
  CheckTrue(result[4] = High(int64));
end;

procedure TestIOmniBlockingCollection.TestToArrayCardinalMax;
var
  coll  : IOmniBlockingCollection;
  ov    : TOmniValue;
  result: TArray<cardinal>;
begin
  coll := TOmniBlockingCollection.Create;
  ov.AsCardinal := $FFFFFFFF; // stored as int64; AddRange<cardinal> would store it as integer
  coll.Add(ov);
  coll.CompleteAdding;
  result := TOmniBlockingCollection.ToArray<cardinal>(coll);
  CheckEquals(1, Length(result));
  CheckTrue(result[0] = $FFFFFFFF);
end;

procedure TestIOmniBlockingCollection.TestToArrayObjectAtHighAddress;
var
  coll  : IOmniBlockingCollection;
  obj   : TObject;
  result: TArray<TObject>;
begin
  // A large-address-aware 32-bit process can allocate objects above $FF000000.
  // The object is never dereferenced, so a fake address is enough.
  obj := TObject(NativeUInt($FF15C760));
  coll := TOmniBlockingCollection.FromRange<TObject>([obj]);
  coll.CompleteAdding;
  result := TOmniBlockingCollection.ToArray<TObject>(coll);
  CheckEquals(1, Length(result));
  CheckTrue(result[0] = obj);
end;

procedure TestIOmniBlockingCollection.TestToArrayTooBig;
var
  coll  : IOmniBlockingCollection;
  raised: boolean;
begin
  coll := TOmniBlockingCollection.Create;
  coll.Add(65536);
  coll.CompleteAdding;
  raised := false;
  try
    TOmniBlockingCollection.ToArray<word>(coll);
  except
    on E: EOmniValueConv do
      raised := true;
  end;
  CheckTrue(raised, 'EOmniValueConv was not raised');
end;

procedure TestIOmniBlockingCollection.TestFromArrayToArray;
var
  arr   : TArray<integer>;
  coll  : IOmniBlockingCollection;
  result: TArray<integer>;
begin
  arr := TArray<integer>.Create(1, 2, 3, 4, 5);
  coll := TOmniBlockingCollection.FromArray<integer>(arr);
  coll.CompleteAdding; // required before ToArray, which enumerates via Take(INFINITE)
  result := TOmniBlockingCollection.ToArray<integer>(coll);
  CheckEquals(5, Length(result));
  CheckEquals(1, result[0]);
  CheckEquals(5, result[4]);
end;

procedure TestIOmniBlockingCollection.TestAddRange;
var
  collObj: TOmniBlockingCollection;
  coll   : IOmniBlockingCollection;
  value  : TOmniValue;
begin
  collObj := TOmniBlockingCollection.Create;
  coll := collObj;
  collObj.AddRange<integer>([10, 20, 30, 40]);
  CheckEquals(4, coll.Count);

  coll.Take(value);
  CheckEquals(10, value.AsInteger);
  coll.Take(value);
  CheckEquals(20, value.AsInteger);
end;
{$ENDIF}

{$IFDEF OTL_HasSystemThreading}
procedure TestIOmniBlockingCollection.TestMultiConsumerTryTake;
// Regression: two threads calling TryTake concurrently on the same collection
const
  CIterations = 200;
  CCount      = 500;
var
  coll         : IOmniBlockingCollection;
  iter         : integer;
  producer     : ITask;
  t1           : ITask;
  t2           : ITask;
  totalReceived: integer;
begin
  for iter := 1 to CIterations do begin
    coll := TOmniBlockingCollection.Create;
    totalReceived := 0;

    // Producer: feed items one at a time (concurrent with consumers)
    producer := TTask.Run(
      procedure
      var
        i: integer;
      begin
        for i := 1 to CCount do
          coll.TryAdd(i);
        coll.CompleteAdding;
      end);

    // Two consumers draining concurrently with TryTake(0)
    t1 := TTask.Run(
      procedure
      var value: TOmniValue;
      begin
        while not coll.IsFinalized do
          if coll.TryTake(value, 0) then
            InterlockedIncrement(totalReceived);
      end);

    t2 := TTask.Run(
      procedure
      var value: TOmniValue;
      begin
        while not coll.IsFinalized do
          if coll.TryTake(value, 0) then
            InterlockedIncrement(totalReceived);
      end);

    producer.Wait(5000);
    t1.Wait(5000);
    t2.Wait(5000);
    CheckEquals(CCount, totalReceived,
      Format('Iteration %d: expected %d, got %d', [iter, CCount, totalReceived]));
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF Unicode}
  RegisterTest(TestIOmniBlockingCollection.Suite);
{$ENDIF}
end.
