program DeThrottle;

{ IOmniBlockingCollection.DeThrottle and IOmniPipeline.DeThrottle (issue #61):
  switch throttling off at run time and release writers that are blocked on a full collection.
  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlCollections,
  OtlParallel;

var
  failed: boolean = false;

procedure Check(condition: boolean; const msg: string);
begin
  if condition then
    Writeln('PASS: ', msg)
  else begin
    Writeln('FAIL: ', msg);
    failed := true;
  end;
end;

procedure TestCollection;
var
  coll   : IOmniBlockingCollection;
  added  : integer;
  writer : TThread;
  t0     : cardinal;
begin
  coll := TOmniBlockingCollection.Create;
  coll.SetThrottling(2, 1);
  Check(coll.IsThrottling, 'IsThrottling after SetThrottling');
  added := 0;
  writer := TThread.CreateAnonymousThread(
    procedure
    var i: integer;
    begin
      for i := 1 to 10 do begin
        coll.Add(i);
        AtomicIncrement(added);
      end;
    end);
  writer.FreeOnTerminate := false;
  writer.Start;
  Sleep(300);
  Check(added = 2, 'writer is blocked by throttling (added=' + IntToStr(added) + ')');
  coll.DeThrottle;
  Check(not coll.IsThrottling, 'IsThrottling after DeThrottle');
  t0 := GetTickCount;
  while (not writer.Finished) and (GetTickCount - t0 < 5000) do
    Sleep(10);
  Check(writer.Finished and (added = 10), 'writer released by DeThrottle (added=' + IntToStr(added) + ')');
  writer.Free;
  coll.DeThrottle; // calling it again is harmless
  Check(coll.Count = 10, 'all items are in the collection');
end;

procedure TestPipeline;
var
  pipe   : IOmniPipeline;
  i      : integer;
  value  : TOmniValue;
  count  : integer;
  t0     : cardinal;
begin
  pipe := Parallel.Pipeline
    .Throttle(5)
    .Stage(
      procedure (const input, output: IOmniBlockingCollection)
      var v: TOmniValue;
      begin
        for v in input do
          output.Add(v);
      end)
    .Run;
  for i := 1 to 200 do
    pipe.Input.Add(i);
  pipe.Input.CompleteAdding;
  Sleep(300);
  Check(not pipe.WaitFor(100), 'pipeline is stuck on a full output queue without a consumer');
  pipe.DeThrottle;
  Check(pipe.WaitFor(5000), 'pipeline finishes after DeThrottle');
  count := 0;
  while pipe.Output.TryTake(value) do
    Inc(count);
  Check(count = 200, 'all items arrived: ' + IntToStr(count));
end;

begin
  TestCollection;
  TestPipeline;
  if failed then Halt(1);
end.
