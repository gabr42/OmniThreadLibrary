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
{$ENDIF OTL_GoodGenerics}
{$ENDIF Unicode}

initialization
{$IFDEF Unicode}
{$IFDEF OTL_GoodGenerics}
  RegisterTest(TestParallelChannel.Suite);
  RegisterTest(TestParallelSelect.Suite);
  RegisterTest(TestParallelMerge.Suite);
  RegisterTest(TestParallelRace.Suite);
{$ENDIF OTL_GoodGenerics}
{$ENDIF Unicode}
end.
