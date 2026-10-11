unit TestParallelChannelSelect1;

interface

{$I OtlOptions.inc}

{$IFDEF Unicode}
{$IFDEF OTL_GoodGenerics}
uses
  TestFramework, Windows, SysUtils, Classes, SyncObjs;

type
  TestParallelChannel = class(TTestCase)
  published
    procedure TestSendReceive;
    procedure TestForInIteration;
    procedure TestCloseEmpty;
    procedure TestCloseAfterSends;
    procedure TestTrySendCapacity;
    procedure TestTryReceiveTimeout;
  end;

  TestParallelSelect = class(TTestCase)
  published
    procedure TestSelectMultiplex;
    procedure TestSelectDefaultCase;
    procedure TestSelectAllClosed;
    procedure TestSelectTimeout;
  end;

  TestParallelMerge = class(TTestCase)
  published
    procedure TestMergeFanIn;
    procedure TestMergeAllClosed;
  end;

  TestParallelRace = class(TTestCase)
  published
    procedure TestRaceFirstWins;
    procedure TestRaceTimeoutRaises;
    procedure TestTryRaceTimeoutReturnsFalse;
    procedure TestTryRaceAllClosedReturnsFalse;
  end;

  // Thread-safety tests: several threads use the same channels at the same time.
  TestParallelChannelMT = class(TTestCase)
  strict private
    procedure RaceRound(sendFirst: boolean);
    procedure RunMPMC(capacity: integer);
  published
    procedure TestMPMCCapacity1;
    procedure TestMPMCCapacity16;
    procedure TestMPMCUnbounded;
    procedure TestCloseWakesBlockedReceivers;
    procedure TestCloseWithBlockedSenders;
    procedure TestCloseReleasesBlockedSenders;
    procedure TestCloseReleasesBlockedTrySend;
    procedure TestSelectConcurrentWaiters;
    procedure TestMergeConcurrent;
    procedure TestRaceConcurrentCallers;
  end;
{$ENDIF OTL_GoodGenerics}
{$ENDIF Unicode}

implementation

{$IFDEF Unicode}
{$IFDEF OTL_GoodGenerics}
uses
  Generics.Collections,
  OtlCommon,
  OtlCollections,
  OtlParallel;

{ TestParallelChannel }

procedure TestParallelChannel.TestSendReceive;
var
  ch: IOmniChannel<integer>;
  v : integer;
  i : integer;
begin
  ch := Parallel.Channel<integer>(8);
  for i := 1 to 5 do
    ch.Sender.Send(i * 10);
  for i := 1 to 5 do begin
    v := ch.Receiver.Receive;
    CheckEquals(i * 10, v, Format('value at index %d', [i]));
  end;
end;

procedure TestParallelChannel.TestForInIteration;
var
  ch  : IOmniChannel<integer>;
  i   : integer;
  sum : integer;
  v   : integer;
begin
  // Drain a closed channel via while-TryReceive(INFINITE). GetEnumerator is on
  // the concrete class only (TOmniChannelReceiver<T>), not the interface, so
  // a for-in loop over IOmniChannelReceiver<T> is not supported here.
  ch := Parallel.Channel<integer>(16);
  for i := 1 to 10 do
    ch.Sender.Send(i);
  ch.Close;

  sum := 0;
  while ch.Receiver.TryReceive(v, INFINITE) do
    Inc(sum, v);
  CheckEquals(55, sum, 'sum of 1..10');
end;

procedure TestParallelChannel.TestCloseEmpty;
var
  ch  : IOmniChannel<integer>;
  v   : integer;
  ok  : boolean;
  raised: boolean;
begin
  ch := Parallel.Channel<integer>(4);
  ch.Close;
  CheckTrue(ch.Receiver.IsClosed, 'IsClosed after Close');
  CheckTrue(ch.Receiver.IsEmpty, 'IsEmpty after Close');

  // TryReceive on closed empty channel returns false, no exception.
  ok := ch.Receiver.TryReceive(v, 100);
  CheckFalse(ok, 'TryReceive on closed empty channel');

  // Receive on closed empty channel raises ECollectionCompleted.
  raised := false;
  try
    v := ch.Receiver.Receive;
  except
    on E: ECollectionCompleted do
      raised := true;
  end;
  CheckTrue(raised, 'Receive on closed empty channel raises ECollectionCompleted');
end;

procedure TestParallelChannel.TestCloseAfterSends;
var
  ch  : IOmniChannel<integer>;
  v   : integer;
  i   : integer;
  sum : integer;
begin
  // Drain semantics: values sent before Close must still be receivable.
  ch := Parallel.Channel<integer>(8);
  for i := 1 to 5 do
    ch.Sender.Send(i);
  ch.Close;

  CheckTrue(ch.Receiver.IsClosed, 'IsClosed after Close');
  CheckFalse(ch.Receiver.IsEmpty, 'channel still has buffered items');

  sum := 0;
  while ch.Receiver.TryReceive(v, 0) do
    Inc(sum, v);
  CheckEquals(15, sum, 'all buffered values received');
  CheckTrue(ch.Receiver.IsEmpty, 'empty after drain');
end;

procedure TestParallelChannel.TestTrySendCapacity;
var
  ch : IOmniChannel<integer>;
  ok : boolean;
  i  : integer;
begin
  // capacity = 2; first two TrySend(0) must succeed, third must fail.
  ch := Parallel.Channel<integer>(2);
  for i := 1 to 2 do begin
    ok := ch.Sender.TrySend(i, 0);
    CheckTrue(ok, Format('TrySend %d should succeed', [i]));
  end;
  ok := ch.Sender.TrySend(99, 50);
  CheckFalse(ok, 'TrySend on full channel should time out');
end;

procedure TestParallelChannel.TestTryReceiveTimeout;
var
  ch : IOmniChannel<integer>;
  v  : integer;
  ok : boolean;
  start_ms: int64;
  elapsed_ms: int64;
begin
  ch := Parallel.Channel<integer>(8);
  start_ms := GetTickCount;
  ok := ch.Receiver.TryReceive(v, 200);
  elapsed_ms := int64(GetTickCount) - start_ms;
  CheckFalse(ok, 'TryReceive with timeout should fail when empty');
  CheckTrue(elapsed_ms >= 150, Format('elapsed=%d_ms (expected >=150)', [elapsed_ms]));
  CheckTrue(elapsed_ms < 1000, Format('elapsed=%d_ms (expected <1000)', [elapsed_ms]));
end;

{ TestParallelSelect }

procedure TestParallelSelect.TestSelectMultiplex;
var
  ch1   : IOmniChannel<integer>;
  ch2   : IOmniChannel<integer>;
  sel   : IOmniSelect;
  cases : array[0..1] of IOmniSelectCase;
  hits1 : integer;
  hits2 : integer;
  res   : TOmniSelectResult;
begin
  ch1 := Parallel.Channel<integer>(8);
  ch2 := Parallel.Channel<integer>(8);
  hits1 := 0;
  hits2 := 0;

  cases[0] := SelectCase.Receive<integer>(ch1.Receiver,
    procedure(v: integer) begin Inc(hits1, v) end);
  cases[1] := SelectCase.Receive<integer>(ch2.Receiver,
    procedure(v: integer) begin Inc(hits2, v) end);

  ch1.Sender.Send(10);
  ch2.Sender.Send(20);
  ch1.Sender.Send(30);

  sel := Parallel.Select(cases);

  res := sel.Wait(1000); CheckEquals(integer(srHandled), integer(res), 'wait 1');
  res := sel.Wait(1000); CheckEquals(integer(srHandled), integer(res), 'wait 2');
  res := sel.Wait(1000); CheckEquals(integer(srHandled), integer(res), 'wait 3');

  CheckEquals(40, hits1, 'hits1 = 10+30');
  CheckEquals(20, hits2, 'hits2 = 20');
end;

procedure TestParallelSelect.TestSelectDefaultCase;
var
  ch         : IOmniChannel<integer>;
  cases      : array[0..1] of IOmniSelectCase;
  defaultHit : boolean;
  recvHit    : boolean;
  res        : TOmniSelectResult;
  sel        : IOmniSelect;
begin
  ch := Parallel.Channel<integer>(8);
  defaultHit := false;
  recvHit := false;

  cases[0] := SelectCase.Receive<integer>(ch.Receiver,
    procedure(v: integer) begin recvHit := true end);
  cases[1] := SelectCase.Default(
    procedure begin defaultHit := true end);

  // Channel is empty - default case must fire.
  sel := Parallel.Select(cases);
  res := sel.Wait(1000);
  CheckEquals(integer(srDefault), integer(res), 'default fires when nothing ready');
  CheckTrue(defaultHit, 'default handler invoked');
  CheckFalse(recvHit, 'receive handler not invoked');
end;

procedure TestParallelSelect.TestSelectAllClosed;
var
  ch1   : IOmniChannel<integer>;
  ch2   : IOmniChannel<integer>;
  cases : array[0..1] of IOmniSelectCase;
  sel   : IOmniSelect;
  res   : TOmniSelectResult;
begin
  ch1 := Parallel.Channel<integer>(4);
  ch2 := Parallel.Channel<integer>(4);
  ch1.Close;
  ch2.Close;

  cases[0] := SelectCase.Receive<integer>(ch1.Receiver, procedure(v: integer) begin end);
  cases[1] := SelectCase.Receive<integer>(ch2.Receiver, procedure(v: integer) begin end);

  sel := Parallel.Select(cases);
  res := sel.Wait(1000);
  CheckEquals(integer(srAllClosed), integer(res), 'srAllClosed expected');
end;

procedure TestParallelSelect.TestSelectTimeout;
var
  ch    : IOmniChannel<integer>;
  cases : array[0..0] of IOmniSelectCase;
  sel   : IOmniSelect;
  res   : TOmniSelectResult;
  start_ms  : int64;
  elapsed_ms: int64;
begin
  ch := Parallel.Channel<integer>(4);
  cases[0] := SelectCase.Receive<integer>(ch.Receiver,
    procedure(v: integer) begin end);

  sel := Parallel.Select(cases);
  start_ms := GetTickCount;
  res := sel.Wait(200);
  elapsed_ms := int64(GetTickCount) - start_ms;

  CheckEquals(integer(srTimeout), integer(res), 'srTimeout expected');
  CheckTrue(elapsed_ms >= 150, Format('elapsed=%d_ms (expected >=150)', [elapsed_ms]));
  CheckTrue(elapsed_ms < 2000, Format('elapsed=%d_ms (expected <2000)', [elapsed_ms]));
end;

{ TestParallelMerge }

procedure TestParallelMerge.TestMergeFanIn;
var
  ch1     : IOmniChannel<integer>;
  ch2     : IOmniChannel<integer>;
  ch3     : IOmniChannel<integer>;
  receivers: array[0..2] of IOmniChannelReceiver<integer>;
  merged  : IOmniChannelReceiver<integer>;
  v       : integer;
  sum     : integer;
  count   : integer;
  ok      : boolean;
  start_ms: int64;
begin
  ch1 := Parallel.Channel<integer>(16);
  ch2 := Parallel.Channel<integer>(16);
  ch3 := Parallel.Channel<integer>(16);
  ch1.Sender.Send(1); ch1.Sender.Send(2); ch1.Close;
  ch2.Sender.Send(10); ch2.Sender.Send(20); ch2.Close;
  ch3.Sender.Send(100); ch3.Close;

  receivers[0] := ch1.Receiver;
  receivers[1] := ch2.Receiver;
  receivers[2] := ch3.Receiver;
  merged := Parallel.Merge<integer>(receivers, 16);

  sum := 0;
  count := 0;
  start_ms := GetTickCount;
  while int64(GetTickCount) - start_ms < 5000 do begin
    ok := merged.TryReceive(v, 200);
    if ok then begin
      Inc(sum, v);
      Inc(count);
    end
    else if merged.IsClosed and merged.IsEmpty then
      break;
  end;

  CheckEquals(5, count, 'merged item count');
  CheckEquals(133, sum, 'merged sum');
end;

procedure TestParallelMerge.TestMergeAllClosed;
var
  ch1     : IOmniChannel<integer>;
  ch2     : IOmniChannel<integer>;
  receivers: array[0..1] of IOmniChannelReceiver<integer>;
  merged  : IOmniChannelReceiver<integer>;
  v       : integer;
  ok      : boolean;
  start_ms: int64;
begin
  // Two empty closed channels; merge must close its output without producing items.
  ch1 := Parallel.Channel<integer>(4);
  ch2 := Parallel.Channel<integer>(4);
  ch1.Close;
  ch2.Close;

  receivers[0] := ch1.Receiver;
  receivers[1] := ch2.Receiver;
  merged := Parallel.Merge<integer>(receivers, 4);

  start_ms := GetTickCount;
  while int64(GetTickCount) - start_ms < 2000 do begin
    if merged.IsClosed and merged.IsEmpty then
      break;
    Sleep(20);
  end;

  CheckTrue(merged.IsClosed, 'merged closed');
  CheckTrue(merged.IsEmpty, 'merged empty');
  ok := merged.TryReceive(v, 0);
  CheckFalse(ok, 'TryReceive on closed empty merged channel');
end;

{ TestParallelRace }

procedure TestParallelRace.TestRaceFirstWins;
var
  ch1     : IOmniChannel<integer>;
  ch2     : IOmniChannel<integer>;
  receivers: array[0..1] of IOmniChannelReceiver<integer>;
  v       : integer;
begin
  ch1 := Parallel.Channel<integer>(4);
  ch2 := Parallel.Channel<integer>(4);
  ch2.Sender.Send(99);
  // ch1 stays empty.

  receivers[0] := ch1.Receiver;
  receivers[1] := ch2.Receiver;
  v := Parallel.Race<integer>(receivers, 1000);
  CheckEquals(99, v, 'race winner is ch2 value');
end;

procedure TestParallelRace.TestRaceTimeoutRaises;
var
  ch1     : IOmniChannel<integer>;
  ch2     : IOmniChannel<integer>;
  receivers: array[0..1] of IOmniChannelReceiver<integer>;
  v       : integer;
  raised  : boolean;
begin
  ch1 := Parallel.Channel<integer>(4);
  ch2 := Parallel.Channel<integer>(4);
  receivers[0] := ch1.Receiver;
  receivers[1] := ch2.Receiver;

  raised := false;
  try
    v := Parallel.Race<integer>(receivers, 200);
  except
    on E: ESelectTimeout do
      raised := true;
  end;
  CheckTrue(raised, 'ESelectTimeout raised on timeout');
end;

procedure TestParallelRace.TestTryRaceTimeoutReturnsFalse;
var
  ch1     : IOmniChannel<integer>;
  receivers: array[0..0] of IOmniChannelReceiver<integer>;
  v       : integer;
  ok      : boolean;
begin
  ch1 := Parallel.Channel<integer>(4);
  receivers[0] := ch1.Receiver;

  ok := Parallel.TryRace<integer>(receivers, v, 200);
  CheckFalse(ok, 'TryRace returns false on timeout');
end;

procedure TestParallelRace.TestTryRaceAllClosedReturnsFalse;
var
  ch1     : IOmniChannel<integer>;
  ch2     : IOmniChannel<integer>;
  receivers: array[0..1] of IOmniChannelReceiver<integer>;
  v       : integer;
  ok      : boolean;
begin
  ch1 := Parallel.Channel<integer>(4);
  ch2 := Parallel.Channel<integer>(4);
  ch1.Close;
  ch2.Close;
  receivers[0] := ch1.Receiver;
  receivers[1] := ch2.Receiver;

  ok := Parallel.TryRace<integer>(receivers, v, 1000);
  CheckFalse(ok, 'TryRace returns false when all channels closed');
end;

{ multi-threaded test infrastructure }

const
  CMTJoinTimeout_ms = 60000; // a worker that does not finish in this time is reported as deadlocked
  CMTBlockedCheck_ms = 200;  // time given to workers to reach their blocking call
  CMTRaceWait_ms     = 10000;
  CMTSelectWait_ms   = 30000;

type
  // Test thread. Checks (DUnit asserts) must only be done in the main thread, so a worker
  // just stores the exception that escaped its body and the test reports it after the join.
  TMTWorker = class(TThread)
  strict private
    FBody   : TProc<TMTWorker>;
    FDone   : TEvent;
    FError  : string;
    FIndex  : integer;
    FItems  : TList<integer>;
    FOutcome: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(index: integer; const body: TProc<TMTWorker>);
    destructor  Destroy; override;
    function  IsDone: boolean;
    function  WaitDone(timeout_ms: cardinal): boolean;
    property Error  : string read FError;
    property Index  : integer read FIndex;
    property Items  : TList<integer> read FItems;
    property Outcome: integer read FOutcome write FOutcome;
  end; { TMTWorker }

  TMTWorkerGroup = class
  strict private
    FName   : string;
    FWorkers: TList<TMTWorker>;
  public
    constructor Create(const name: string);
    destructor  Destroy; override;
    function  AnyDone: boolean;
    function  AllDone: boolean;
    procedure CheckExactlyOnce(expectedCount: integer);
    procedure CheckNoErrors;
    procedure CheckSourceOrder(sourceCount, itemsPerSource: integer);
    procedure Start(count: integer; const body: TProc<TMTWorker>);
    procedure WaitAll(timeout_ms: cardinal = CMTJoinTimeout_ms);
    function  Worker(idx: integer): TMTWorker;
  end; { TMTWorkerGroup }

{ TMTWorker }

constructor TMTWorker.Create(index: integer; const body: TProc<TMTWorker>);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FIndex := index;
  FBody := body;
  FItems := TList<integer>.Create;
  FDone := TEvent.Create(nil, true, false, '');
end; { TMTWorker.Create }

destructor TMTWorker.Destroy;
begin
  inherited Destroy; // waits for the thread to finish
  FreeAndNil(FItems);
  FreeAndNil(FDone);
end; { TMTWorker.Destroy }

procedure TMTWorker.Execute;
begin
  try
    try
      FBody(Self);
    except
      on E: Exception do
        FError := E.ClassName + ': ' + E.Message;
    end;
  finally FDone.SetEvent; end;
end; { TMTWorker.Execute }

function TMTWorker.IsDone: boolean;
begin
  Result := WaitDone(0);
end; { TMTWorker.IsDone }

function TMTWorker.WaitDone(timeout_ms: cardinal): boolean;
begin
  Result := FDone.WaitFor(timeout_ms) = wrSignaled;
end; { TMTWorker.WaitDone }

{ TMTWorkerGroup }

constructor TMTWorkerGroup.Create(const name: string);
begin
  inherited Create;
  FName := name;
  FWorkers := TList<TMTWorker>.Create;
end; { TMTWorkerGroup.Create }

destructor TMTWorkerGroup.Destroy;
var
  worker: TMTWorker;
begin
  // A worker that is still blocked (the test failed) is abandoned: destroying a running
  // thread would wait for it forever.
  for worker in FWorkers do
    if worker.IsDone then
      worker.Free;
  FreeAndNil(FWorkers);
  inherited;
end; { TMTWorkerGroup.Destroy }

function TMTWorkerGroup.AllDone: boolean;
var
  worker: TMTWorker;
begin
  Result := true;
  for worker in FWorkers do
    if not worker.IsDone then
      Exit(false);
end; { TMTWorkerGroup.AllDone }

function TMTWorkerGroup.AnyDone: boolean;
var
  worker: TMTWorker;
begin
  Result := false;
  for worker in FWorkers do
    if worker.IsDone then
      Exit(true);
end; { TMTWorkerGroup.AnyDone }

// Every value in 0..expectedCount-1 must have been collected by exactly one worker.
procedure TMTWorkerGroup.CheckExactlyOnce(expectedCount: integer);
var
  seen  : array of boolean;
  total : integer;
  v     : integer;
  worker: TMTWorker;
begin
  SetLength(seen, expectedCount);
  total := 0;
  for worker in FWorkers do
    for v in worker.Items do begin
      if (v < 0) or (v >= expectedCount) then
        raise ETestFailure.CreateFmt('%s: worker %d received unexpected value %d',
          [FName, worker.Index, v]);
      if seen[v] then
        raise ETestFailure.CreateFmt('%s: value %d was received more than once',
          [FName, v]);
      seen[v] := true;
      Inc(total);
    end;
  if total <> expectedCount then
    raise ETestFailure.CreateFmt('%s: %d values received, %d expected (values were lost)',
      [FName, total, expectedCount]);
end; { TMTWorkerGroup.CheckExactlyOnce }

procedure TMTWorkerGroup.CheckNoErrors;
var
  worker: TMTWorker;
begin
  for worker in FWorkers do
    if worker.Error <> '' then
      raise ETestFailure.CreateFmt('%s: worker %d failed: %s',
        [FName, worker.Index, worker.Error]);
end; { TMTWorkerGroup.CheckNoErrors }

// Values are encoded as source * itemsPerSource + sequence. A single worker must see the values
// of each source in the order in which the source sent them.
procedure TMTWorkerGroup.CheckSourceOrder(sourceCount, itemsPerSource: integer);
var
  last  : array of integer;
  seq   : integer;
  src   : integer;
  v     : integer;
  worker: TMTWorker;
begin
  SetLength(last, sourceCount);
  for worker in FWorkers do begin
    for src := 0 to sourceCount - 1 do
      last[src] := -1;
    for v in worker.Items do begin
      src := v div itemsPerSource;
      seq := v mod itemsPerSource;
      if seq <= last[src] then
        raise ETestFailure.CreateFmt('%s: worker %d received value %d of source %d after value %d',
          [FName, worker.Index, seq, src, last[src]]);
      last[src] := seq;
    end;
  end;
end; { TMTWorkerGroup.CheckSourceOrder }

procedure TMTWorkerGroup.Start(count: integer; const body: TProc<TMTWorker>);
var
  i     : integer;
  worker: TMTWorker;
begin
  for i := 1 to count do begin
    worker := TMTWorker.Create(FWorkers.Count, body);
    FWorkers.Add(worker);
    worker.Start;
  end;
end; { TMTWorkerGroup.Start }

procedure TMTWorkerGroup.WaitAll(timeout_ms: cardinal);
var
  worker: TMTWorker;
begin
  for worker in FWorkers do
    if not worker.WaitDone(timeout_ms) then
      raise ETestFailure.CreateFmt('%s: worker %d did not finish in %d ms (deadlock?)',
        [FName, worker.Index, timeout_ms]);
end; { TMTWorkerGroup.WaitAll }

function TMTWorkerGroup.Worker(idx: integer): TMTWorker;
begin
  Result := FWorkers[idx];
end; { TMTWorkerGroup.Worker }

// Parameters are passed by value so that every closure captures its own list.
function MakeCollector(const list: TList<integer>): TProc<integer>;
begin
  Result :=
    procedure (value: integer)
    begin
      list.Add(value);
    end;
end; { MakeCollector }

{ TestParallelChannelMT }

// Several producers and consumers use one channel at the same time while a fifth thread
// keeps reading the status properties. Every value must arrive exactly once, values of one
// producer must arrive in order, and nobody may deadlock.
procedure TestParallelChannelMT.RunMPMC(capacity: integer);
const
  CConsumers = 4;
  CItems     = 5000;
  CProducers = 4;
var
  ch          : IOmniChannel<integer>;
  consumerBody: TProc<TMTWorker>;
  consumers   : TMTWorkerGroup;
  poller      : TMTWorkerGroup;
  pollerBody  : TProc<TMTWorker>;
  producerBody: TProc<TMTWorker>;
  producers   : TMTWorkerGroup;
  stopPoller  : TEvent;
begin
  ch := Parallel.Channel<integer>(capacity);
  stopPoller := TEvent.Create(nil, true, false, '');
  producers := TMTWorkerGroup.Create('producers');
  consumers := TMTWorkerGroup.Create('consumers');
  poller := TMTWorkerGroup.Create('poller');
  try
    producerBody :=
      procedure (w: TMTWorker)
      var
        n: integer;
      begin
        for n := 0 to CItems - 1 do
          ch.Sender.Send(w.Index * CItems + n);
      end;

    consumerBody :=
      procedure (w: TMTWorker)
      var
        v: integer;
      begin
        while ch.Receiver.TryReceive(v, INFINITE) do
          w.Items.Add(v);
      end;

    pollerBody :=
      procedure (w: TMTWorker)
      var
        closed: boolean;
        count : integer;
      begin
        while stopPoller.WaitFor(0) <> wrSignaled do begin
          // Count is approximate: the underlying blocking collection increments its counter
          // after the enqueue and decrements it after the dequeue, so it can be off by the
          // number of threads that are in the middle of an operation.
          count := ch.Receiver.Count;
          if (count < -CProducers) or ((capacity > 0) and (count > capacity + CConsumers)) then
            raise Exception.CreateFmt('TestParallelChannelMT.RunMPMC: Count = %d', [count]);
          closed := ch.Receiver.IsClosed;
          if ch.Receiver.IsEmpty and closed and ch.Sender.IsClosed then
            Sleep(10)
          else
            Sleep(1);
        end;
      end;

    poller.Start(1, pollerBody);
    consumers.Start(CConsumers, consumerBody);
    producers.Start(CProducers, producerBody);

    producers.WaitAll;
    producers.CheckNoErrors;
    ch.Close;
    consumers.WaitAll;
    consumers.CheckNoErrors;
    stopPoller.SetEvent;
    poller.WaitAll;
    poller.CheckNoErrors;

    consumers.CheckExactlyOnce(CProducers * CItems);
    consumers.CheckSourceOrder(CProducers, CItems);
    CheckEquals(0, ch.Receiver.Count, 'Count after all values were received');
    CheckTrue(ch.Receiver.IsEmpty, 'IsEmpty after all values were received');
  finally
    ch.Close;
    stopPoller.SetEvent;
    FreeAndNil(producers);
    FreeAndNil(consumers);
    if poller.AllDone then
      FreeAndNil(stopPoller);
    FreeAndNil(poller);
  end;
end; { TestParallelChannelMT.RunMPMC }

procedure TestParallelChannelMT.TestMPMCCapacity1;
begin
  RunMPMC(1); // maximum contention: every Send waits for a Receive
end; { TestParallelChannelMT.TestMPMCCapacity1 }

procedure TestParallelChannelMT.TestMPMCCapacity16;
begin
  RunMPMC(16);
end; { TestParallelChannelMT.TestMPMCCapacity16 }

procedure TestParallelChannelMT.TestMPMCUnbounded;
begin
  RunMPMC(0);
end; { TestParallelChannelMT.TestMPMCUnbounded }

// Close must wake up every thread that waits for data in Receive or TryReceive(INFINITE).
procedure TestParallelChannelMT.TestCloseWakesBlockedReceivers;
const
  CReceivers = 4;
var
  body : TProc<TMTWorker>;
  ch   : IOmniChannel<integer>;
  group: TMTWorkerGroup;
  i    : integer;
begin
  ch := Parallel.Channel<integer>(4);
  group := TMTWorkerGroup.Create('receivers');
  try
    body :=
      procedure (w: TMTWorker)
      var
        v: integer;
      begin
        if (w.Index mod 2) = 0 then begin
          try
            v := ch.Receiver.Receive;
          except
            on ECollectionCompleted do
              w.Outcome := 1;
          end;
        end
        else if not ch.Receiver.TryReceive(v, INFINITE) then
          w.Outcome := 1;
      end;

    group.Start(CReceivers, body);
    Sleep(CMTBlockedCheck_ms);
    CheckFalse(group.AnyDone, 'receivers must be blocked on the empty channel');

    ch.Close;
    group.WaitAll(10000);
    group.CheckNoErrors;
    for i := 0 to CReceivers - 1 do
      CheckEquals(1, group.Worker(i).Outcome,
        Format('receiver %d: Receive must raise ECollectionCompleted, TryReceive must return false', [i]));
  finally
    ch.Close;
    FreeAndNil(group);
  end;
end; { TestParallelChannelMT.TestCloseWakesBlockedReceivers }

// Senders blocked on a full channel when it is closed: once the consumer makes room,
// they must find out that the channel is closed (ECollectionCompleted) and must not add
// their value.
procedure TestParallelChannelMT.TestCloseWithBlockedSenders;
const
  CCapacity = 2;
var
  body   : TProc<TMTWorker>;
  ch     : IOmniChannel<integer>;
  drained: integer;
  group  : TMTWorkerGroup;
  i      : integer;
  v      : integer;
begin
  ch := Parallel.Channel<integer>(CCapacity);
  group := TMTWorkerGroup.Create('senders');
  try
    ch.Sender.Send(-1);
    ch.Sender.Send(-2);

    body :=
      procedure (w: TMTWorker)
      begin
        try
          ch.Sender.Send(100 + w.Index);
        except
          on ECollectionCompleted do
            w.Outcome := 1;
        end;
      end;

    group.Start(CCapacity, body);
    Sleep(CMTBlockedCheck_ms);
    CheckFalse(group.AnyDone, 'senders must be blocked on the full channel');

    ch.Close;
    drained := 0;
    while ch.Receiver.TryReceive(v, 0) do begin
      CheckTrue(v < 0, Format('only the values sent before Close may be received (got %d)', [v]));
      Inc(drained);
    end;
    CheckEquals(CCapacity, drained, 'values sent before Close');

    group.WaitAll(10000);
    group.CheckNoErrors;
    for i := 0 to CCapacity - 1 do
      CheckEquals(1, group.Worker(i).Outcome,
        Format('sender %d must get ECollectionCompleted', [i]));
  finally
    ch.Close;
    FreeAndNil(group);
  end;
end; { TestParallelChannelMT.TestCloseWithBlockedSenders }

// Senders blocked on a full channel must also be released by Close alone, without anybody
// receiving from the channel (the book says that sending to a closed channel raises
// ECollectionCompleted).
procedure TestParallelChannelMT.TestCloseReleasesBlockedSenders;
const
  CCapacity = 2;
  CSenders  = 3;
var
  body : TProc<TMTWorker>;
  ch   : IOmniChannel<integer>;
  group: TMTWorkerGroup;
  i    : integer;
  v    : integer;
begin
  ch := Parallel.Channel<integer>(CCapacity);
  group := TMTWorkerGroup.Create('senders');
  try
    ch.Sender.Send(-1);
    ch.Sender.Send(-2);

    body :=
      procedure (w: TMTWorker)
      begin
        try
          ch.Sender.Send(100 + w.Index);
        except
          on ECollectionCompleted do
            w.Outcome := 1;
        end;
      end;

    group.Start(CSenders, body);
    Sleep(CMTBlockedCheck_ms);
    CheckFalse(group.AnyDone, 'senders must be blocked on the full channel');

    ch.Close;
    group.WaitAll(5000);
    group.CheckNoErrors;
    for i := 0 to CSenders - 1 do
      CheckEquals(1, group.Worker(i).Outcome,
        Format('sender %d must get ECollectionCompleted', [i]));
  finally
    ch.Close;
    // release senders that are still blocked, otherwise their threads are abandoned
    while ch.Receiver.TryReceive(v, 0) do
      ;
    FreeAndNil(group);
  end;
end; { TestParallelChannelMT.TestCloseReleasesBlockedSenders }

// TrySend with a long timeout on a full channel must return False as soon as the channel
// is closed; it must not wait for the timeout.
procedure TestParallelChannelMT.TestCloseReleasesBlockedTrySend;
const
  CSenders = 2;
var
  body   : TProc<TMTWorker>;
  ch     : IOmniChannel<integer>;
  group  : TMTWorkerGroup;
  i      : integer;
  start_ms: int64;
  v      : integer;
begin
  ch := Parallel.Channel<integer>(1);
  group := TMTWorkerGroup.Create('senders');
  try
    ch.Sender.Send(-1);

    body :=
      procedure (w: TMTWorker)
      begin
        if not ch.Sender.TrySend(100 + w.Index, 30000) then
          w.Outcome := 1;
      end;

    group.Start(CSenders, body);
    Sleep(CMTBlockedCheck_ms);
    CheckFalse(group.AnyDone, 'TrySend must be waiting for free capacity');

    start_ms := GetTickCount;
    ch.Close;
    group.WaitAll(5000);
    CheckTrue(int64(GetTickCount) - start_ms < 5000, 'TrySend must not wait for its timeout');
    group.CheckNoErrors;
    for i := 0 to CSenders - 1 do
      CheckEquals(1, group.Worker(i).Outcome, Format('TrySend %d must return false', [i]));
    // the value sent before Close is still there, nothing was added after it
    CheckTrue(ch.Receiver.TryReceive(v, 0), 'value sent before Close');
    CheckEquals(-1, v, 'value sent before Close');
    CheckFalse(ch.Receiver.TryReceive(v, 0), 'nothing was added after Close');
  finally
    ch.Close;
    FreeAndNil(group);
  end;
end; { TestParallelChannelMT.TestCloseReleasesBlockedTrySend }

// Every consumer thread has its own Select (IOmniSelect is not meant to be shared between
// threads) over the same channels, so the consumers compete for the values.
procedure TestParallelChannelMT.TestSelectConcurrentWaiters;
const
  CChannels  = 3;
  CConsumers = 3;
  CItems     = 3000;
var
  channels    : TArray<IOmniChannel<integer>>;
  consumerBody: TProc<TMTWorker>;
  consumers   : TMTWorkerGroup;
  i           : integer;
  producerBody: TProc<TMTWorker>;
  producers   : TMTWorkerGroup;
begin
  SetLength(channels, CChannels);
  for i := 0 to CChannels - 1 do
    channels[i] := Parallel.Channel<integer>(8);
  producers := TMTWorkerGroup.Create('producers');
  consumers := TMTWorkerGroup.Create('consumers');
  try
    producerBody :=
      procedure (w: TMTWorker)
      var
        n: integer;
      begin
        for n := 0 to CItems - 1 do
          channels[w.Index].Sender.Send(w.Index * CItems + n);
        channels[w.Index].Close;
      end;

    consumerBody :=
      procedure (w: TMTWorker)
      var
        cases: TArray<IOmniSelectCase>;
        k    : integer;
        res  : TOmniSelectResult;
        sel  : IOmniSelect;
      begin
        SetLength(cases, CChannels);
        for k := 0 to CChannels - 1 do
          cases[k] := SelectCase.Receive<integer>(channels[k].Receiver, MakeCollector(w.Items));
        sel := Parallel.Select(cases);
        repeat
          res := sel.Wait(CMTSelectWait_ms);
        until res <> srHandled;
        if res <> srAllClosed then
          raise Exception.CreateFmt('TestParallelChannelMT.TestSelectConcurrentWaiters: ' +
            'Wait returned %d instead of srAllClosed', [Ord(res)]);
      end;

    consumers.Start(CConsumers, consumerBody);
    producers.Start(CChannels, producerBody);

    producers.WaitAll;
    producers.CheckNoErrors;
    consumers.WaitAll;
    consumers.CheckNoErrors;

    consumers.CheckExactlyOnce(CChannels * CItems);
    consumers.CheckSourceOrder(CChannels, CItems);
  finally
    for i := 0 to CChannels - 1 do
      channels[i].Close;
    FreeAndNil(producers);
    FreeAndNil(consumers);
  end;
end; { TestParallelChannelMT.TestSelectConcurrentWaiters }

// Merge forwards from a background thread while several producers feed the inputs and
// several consumers read the output. The small capacities force both sides to wait.
procedure TestParallelChannelMT.TestMergeConcurrent;
const
  CConsumers = 3;
  CItems     = 2500;
  CProducers = 4;
var
  channels    : TArray<IOmniChannel<integer>>;
  consumerBody: TProc<TMTWorker>;
  consumers   : TMTWorkerGroup;
  i           : integer;
  merged      : IOmniChannelReceiver<integer>;
  producerBody: TProc<TMTWorker>;
  producers   : TMTWorkerGroup;
  receivers   : TArray<IOmniChannelReceiver<integer>>;
begin
  SetLength(channels, CProducers);
  SetLength(receivers, CProducers);
  for i := 0 to CProducers - 1 do begin
    channels[i] := Parallel.Channel<integer>(4);
    receivers[i] := channels[i].Receiver;
  end;
  producers := TMTWorkerGroup.Create('producers');
  consumers := TMTWorkerGroup.Create('consumers');
  try
    merged := Parallel.Merge<integer>(receivers, 8);

    producerBody :=
      procedure (w: TMTWorker)
      var
        n: integer;
      begin
        for n := 0 to CItems - 1 do
          channels[w.Index].Sender.Send(w.Index * CItems + n);
        channels[w.Index].Close;
      end;

    consumerBody :=
      procedure (w: TMTWorker)
      var
        v: integer;
      begin
        while merged.TryReceive(v, INFINITE) do
          w.Items.Add(v);
      end;

    consumers.Start(CConsumers, consumerBody);
    producers.Start(CProducers, producerBody);

    producers.WaitAll;
    producers.CheckNoErrors;
    consumers.WaitAll; // the merged channel is closed when all inputs are closed and drained
    consumers.CheckNoErrors;

    consumers.CheckExactlyOnce(CProducers * CItems);
    consumers.CheckSourceOrder(CProducers, CItems);
    CheckTrue(merged.IsClosed, 'merged channel must be closed');
  finally
    for i := 0 to CProducers - 1 do
      channels[i].Close;
    FreeAndNil(producers);
    FreeAndNil(consumers);
  end;
end; { TestParallelChannelMT.TestMergeConcurrent }

// Four racers wait on the same two channels. Four values are sent; each racer must get
// exactly one of them and none may be lost or delivered twice.
procedure TestParallelChannelMT.RaceRound(sendFirst: boolean);
const
  CRacers = 4;

  procedure SendValues(const chA, chB: IOmniChannel<integer>);
  begin
    chA.Sender.Send(0);
    chA.Sender.Send(1);
    chB.Sender.Send(2);
    chB.Sender.Send(3);
  end; { SendValues }

var
  body : TProc<TMTWorker>;
  chA  : IOmniChannel<integer>;
  chB  : IOmniChannel<integer>;
  group: TMTWorkerGroup;
  i    : integer;
begin
  chA := Parallel.Channel<integer>(4);
  chB := Parallel.Channel<integer>(4);
  group := TMTWorkerGroup.Create('racers');
  try
    body :=
      procedure (w: TMTWorker)
      var
        receivers: array [0..1] of IOmniChannelReceiver<integer>;
        v        : integer;
      begin
        receivers[0] := chA.Receiver;
        receivers[1] := chB.Receiver;
        if Parallel.TryRace<integer>(receivers, v, CMTRaceWait_ms) then begin
          w.Items.Add(v);
          w.Outcome := 1;
        end;
      end;

    if sendFirst then
      SendValues(chA, chB);
    group.Start(CRacers, body);
    if not sendFirst then begin
      Sleep(10);
      SendValues(chA, chB);
    end;

    group.WaitAll(CMTRaceWait_ms + 5000);
    group.CheckNoErrors;
    for i := 0 to CRacers - 1 do
      CheckEquals(1, group.Worker(i).Outcome, Format('racer %d must get a value', [i]));
    group.CheckExactlyOnce(CRacers);
    CheckTrue(chA.Receiver.IsEmpty, 'channel A must be empty');
    CheckTrue(chB.Receiver.IsEmpty, 'channel B must be empty');
  finally
    chA.Close;
    chB.Close;
    FreeAndNil(group);
  end;
end; { TestParallelChannelMT.RaceRound }

procedure TestParallelChannelMT.TestRaceConcurrentCallers;
var
  round: integer;
begin
  for round := 1 to 40 do
    RaceRound(Odd(round));
end; { TestParallelChannelMT.TestRaceConcurrentCallers }
{$ENDIF OTL_GoodGenerics}
{$ENDIF Unicode}

initialization
{$IFDEF Unicode}
{$IFDEF OTL_GoodGenerics}
  RegisterTest(TestParallelChannel.Suite);
  RegisterTest(TestParallelSelect.Suite);
  RegisterTest(TestParallelMerge.Suite);
  RegisterTest(TestParallelRace.Suite);
  RegisterTest(TestParallelChannelMT.Suite);
{$ENDIF OTL_GoodGenerics}
{$ENDIF Unicode}
end.
