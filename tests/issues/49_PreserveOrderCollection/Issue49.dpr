program Issue49;

{ Repro for GitHub issue #49: Parallel.ForEach over a TOmniBlockingCollection with
  PreserveOrder and Into raised an 'Abstract Error'.

  Usage: Issue49 [order|plain|nointo|prefilled]
    order     - PreserveOrder.NoWait.Into(...)      (the case from the issue; default)
    plain     - NoWait.Into(...)                     (control, no ordering)
    nointo    - NoWait.Execute(...)                  (control, no output collection)
    prefilled - Execute(...) over a pre-filled collection (control)

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OtlCommon,
  OtlCollections,
  OtlParallel;

const
  CNumItems = 100;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  count    : integer;
  i        : integer;
  inQueue  : IOmniBlockingCollection;
  loop     : IOmniParallelLoop<TOmniValue>;
  mode     : string;
  outQueue : IOmniBlockingCollection;
  startTick: cardinal;
  sum      : integer;
  value    : TOmniValue;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  mode := ParamStr(1);
  if mode = '' then
    mode := 'order';
  sum := 0;
  startTick := GetTickCount;
  TThread.CreateAnonymousThread(procedure begin Sleep(15000); Fail(mode + ': hang'); end).Start;
  try
    inQueue := TOmniBlockingCollection.Create;
    outQueue := TOmniBlockingCollection.Create;
    loop := Parallel.ForEach<TOmniValue>(inQueue);
    if SameText(mode, 'prefilled') then begin
      for i := 1 to CNumItems do
        inQueue.Add(i);
      inQueue.CompleteAdding;
      loop.Execute(procedure (const value: TOmniValue) begin TInterlocked.Increment(count); end);
      if count <> CNumItems then
        Fail(Format('processed %d of %d', [count, CNumItems]));
    end
    else begin
      if SameText(mode, 'nointo') then
        loop.NoWait.Execute(
          procedure (const value: TOmniValue)
          begin
            TInterlocked.Increment(count);
          end)
      else if SameText(mode, 'plain') then
        loop.NoWait.Into(outQueue).Execute(
          procedure (const value: TOmniValue; var res: TOmniValue)
          begin
            res := value.AsInteger * 2;
          end)
      else
        loop.PreserveOrder.NoWait.Into(outQueue).Execute(
          procedure (const value: TOmniValue; var res: TOmniValue)
          begin
            res := value.AsInteger * 2;
          end);
      for i := 1 to CNumItems do
        inQueue.Add(i);
      inQueue.CompleteAdding;
      if SameText(mode, 'nointo') then begin
        Sleep(3000);
        if count <> CNumItems then
          Fail(Format('processed %d of %d items (input left: %d)', [count, CNumItems, inQueue.Count]));
      end
      else begin
        for i := 1 to CNumItems do begin
          if not outQueue.TryTake(value, 5000) then
            Fail(Format('output ended after %d items (output completed=%s, input left=%d)',
              [i-1, BoolToStr(outQueue.IsCompleted, true), inQueue.Count]));
          if SameText(mode, 'plain') then
            Inc(sum, value.AsInteger) // order is not guaranteed, only check the total
          else if value.AsInteger <> i * 2 then
            Fail(Format('item %d is %d, expected %d', [i, value.AsInteger, i*2]));
        end;
        if SameText(mode, 'plain') and (sum <> CNumItems * (CNumItems + 1)) then
          Fail(Format('sum is %d, expected %d', [sum, CNumItems * (CNumItems + 1)]));
        // NOTE: Ending the program immediately after the last item was taken hangs the
        // process at exit (loop destructor waits for the worker tasks while the RTL is
        // finalizing). That is a separate problem from #49; let the loop finish first.
        while (not outQueue.IsCompleted) and (GetTickCount < startTick + 10000) do
          Sleep(10);
        Sleep(200);
      end;
    end;
  except
    on E: Exception do
      Fail(mode + ': ' + E.ClassName + ': ' + E.Message);
  end;
  Writeln('PASS: ', mode);
  Flush(System.Output);
end.
