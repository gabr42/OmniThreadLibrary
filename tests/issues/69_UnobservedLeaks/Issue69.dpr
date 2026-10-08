program Issue69;

{ Repro for GitHub issues #69, #78, #152, #194, #197 (and #200): memory is not released for tasks
  that are started with Unobserved / internally monitored.

  Classic OTL releases such tasks from the message window of the thread that started them. That
  works for a thread that processes messages (the GUI thread) but not for
    - a thread that does not pump messages (console main thread, a tight loop in a button click),
    - worker threads (nested Parallel.Async / Pipeline / For started from inside a task).

  Cases (each runs the scenario for a while and measures the growth of the private bytes of the
  process; "pumped" cases process the main thread's messages as a GUI application does):
    ForLoopPumped     - repeated Parallel.For in the main thread; messages pumped between runs
    ForLoopUnpumped   - the same without pumping messages (documented 'message loop required')
    NestedAsync       - issue #78: Async started from Async started from Async, main pumps
    NestedPipeline    - issue #69: pipelines started from Parallel.For tasks, main pumps
    NestedAsyncTask   - issue #152: Async started from a task, main pumps

  Exit code: 0 = pass (memory growth below the limit), 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  Winapi.PsAPI,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlParallel;

const
  CWatchdog_ms  = 120000;
  CLimit_MB     = 60;   // allowed growth
  CRepeats      = 300;
  CRepeatsX     = 1500;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

function PrivateMB: integer;
var
  mem: TProcessMemoryCountersEx;
begin
  FillChar(mem, SizeOf(mem), 0);
  mem.cb := SizeOf(mem);
  GetProcessMemoryInfo(GetCurrentProcess, @mem, SizeOf(mem));
  Result := mem.PrivateUsage div (1024*1024);
end; { PrivateMB }

procedure PumpMessages;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end; { PumpMessages }

procedure PumpFor(time_ms: integer);
var
  start: cardinal;
begin
  start := GetTickCount;
  while GetTickCount - start < cardinal(time_ms) do begin
    PumpMessages;
    Sleep(5);
  end;
end; { PumpFor }

procedure NestedAsyncOnce;
begin
  Parallel.Async(
    procedure
    begin
      Parallel.Async(
        procedure
        begin
          Parallel.Async(procedure begin end);
        end);
    end);
end; { NestedAsyncOnce }

procedure PipelineFromTask;
var
  pipe: IOmniPipeline;
begin
  pipe := Parallel.Pipeline
    .Stage(procedure (const input: TOmniValue; var output: TOmniValue) begin end)
    .Run;
  pipe.Cancel;
  pipe.WaitFor(100000);
  pipe := nil;
end; { PipelineFromTask }

var
  caseName: string;
  i       : integer;
  start   : integer;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  caseName := ParamStr(1);
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(CWatchdog_ms);
      Fail(caseName + ': hang');
    end).Start;
  try
    // warm up so that one-time allocations are not counted
    Parallel.For(1, 10).Execute(procedure (value: integer) begin end);
    PumpFor(200);
    start := PrivateMB;
    if SameText(caseName, 'ForLoopPumpedLong') then
      for i := 1 to CRepeatsX do begin
        Parallel.For(1, 10).Execute(procedure (value: integer) begin end);
        PumpMessages;
      end
    else if SameText(caseName, 'ForLoopPumped') then
      for i := 1 to CRepeats do begin
        Parallel.For(1, 10).Execute(procedure (value: integer) begin end);
        PumpMessages;
      end
    else if SameText(caseName, 'ForLoopUnpumped') then
      for i := 1 to CRepeats do
        Parallel.For(1, 10).Execute(procedure (value: integer) begin end)
    else if SameText(caseName, 'NestedAsync') then
      for i := 1 to CRepeats do begin
        NestedAsyncOnce;
        PumpFor(5);
      end
    else if SameText(caseName, 'NestedPipeline') then begin
      // Every core runs a pipeline at a time, so memory use legitimately peaks; measure the
      // steady state, not the peak: two warm-up rounds, then the rounds that are counted.
      for i := 1 to 2 do begin
        Parallel.&For(1, 100).Execute(procedure (value: integer) begin PipelineFromTask; end);
        PumpFor(200);
      end;
      start := PrivateMB;
      for i := 1 to 10 do begin
        Parallel.&For(1, 100).Execute(procedure (value: integer) begin PipelineFromTask; end);
        PumpFor(200);
      end;
    end
    else if SameText(caseName, 'NestedAsyncTask') then
      for i := 1 to CRepeats do begin
        Parallel.Async(
          procedure
          begin
            Parallel.Async(procedure begin end);
          end);
        PumpFor(5);
      end
    else
      Fail('unknown case ' + caseName);
    PumpFor(1500); // let everything finish and be released
    if PrivateMB - start > CLimit_MB then
      Fail(Format('%s: private bytes grew from %d MB to %d MB (limit: +%d MB)',
        [caseName, start, PrivateMB, CLimit_MB]));
    Writeln(Format('PASS: %s (private bytes %d MB -> %d MB)', [caseName, start, PrivateMB]));
  except
    on E: Exception do
      Fail(caseName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  Flush(System.Output);
end.
