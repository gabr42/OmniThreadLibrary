program Issue154;

{ Repro for GitHub issue #154: Parallel.* constructs raise
  'Task can be only monitored with a single monitor' when the task config
  passed to them uses MonitorWith.

  TOmniTaskConfig.MonitorWith attaches the user's TOmniEventMonitor to the task
  (ApplyConfig), then the Parallel.* code calls task.Unobserved, which attaches an
  internal monitor as well. TOmniTaskControl.SetMonitor rejects the second window.

  Usage: Issue154 <Async|AsyncTerminated|Join|For|ForEach|Future|Pipeline>  (one case per process,
  because a failing case can leave OTL in a state that hangs later cases).
  Known limitation (not run by run.bat): Async and AsyncTerminated are expected to fail.
  Parallel.Async installs an OnTerminated closure, which only the internal monitor
  dispatches, so it cannot be combined with a user's monitor.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlCollections,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor,
  OtlParallel;

const
  CWatchdog_ms = 10000;

var
  monitor: TOmniEventMonitor;
  caseName: string;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', caseName, ' - ', msg);
  Flush(Output);
  ExitProcess(1);
end; { Fail }

procedure StartWatchdog;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(CWatchdog_ms);
      Fail('hang (no result in ' + IntToStr(CWatchdog_ms) + ' ms)');
    end).Start;
end; { StartWatchdog }

function NewConfig: IOmniTaskConfig;
begin
  Result := Parallel.TaskConfig.MonitorWith(monitor);
end; { NewConfig }

procedure PumpUntil(const flag: PBoolean; timeout_ms: integer);
var
  msg  : TMsg;
  start: cardinal;
begin
  start := GetTickCount;
  while (not flag^) and (GetTickCount - start < cardinal(timeout_ms)) do begin
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    Sleep(10);
  end;
end; { PumpUntil }

procedure RunCase;
var
  terminated: boolean;
  future: IOmniFuture<integer>;
  pipe  : IOmniPipeline;
begin
  if SameText(caseName, 'Async') then begin
    Parallel.Async(procedure begin end, NewConfig);
    Sleep(200);
  end
  else if SameText(caseName, 'AsyncTerminated') then begin
    // the user's monitor must still dispatch the config's OnTerminated handler
    terminated := false;
    Parallel.Async(procedure begin end,
      NewConfig.OnTerminated(
        procedure (const task: IOmniTaskControl)
        begin
          terminated := true;
        end));
    PumpUntil(@terminated, 5000);
    if not terminated then
      raise Exception.Create('OnTerminated handler was not called');
  end
  else if SameText(caseName, 'Join') then
    Parallel.Join([procedure begin end, procedure begin end]).
      NumTasks(2).TaskConfig(NewConfig).Execute
  else if SameText(caseName, 'For') then
    Parallel.&For(1, 10).TaskConfig(NewConfig).
      Execute(procedure (value: integer) begin end)
  else if SameText(caseName, 'ForEach') then
    Parallel.ForEach(1, 10).TaskConfig(NewConfig).
      Execute(procedure (const value: integer) begin end)
  else if SameText(caseName, 'Future') then begin
    future := Parallel.Future<integer>(function: integer begin Result := 42; end, NewConfig);
    if future.Value <> 42 then
      raise Exception.Create('Wrong future value');
  end
  else if SameText(caseName, 'Pipeline') then begin
    pipe := Parallel.Pipeline.Stage(
      procedure (const input, output: IOmniBlockingCollection)
      begin
      end, NewConfig).Run;
    pipe.WaitFor(5000);
  end
  else
    raise Exception.CreateFmt('Unknown case "%s"', [caseName]);
end; { RunCase }

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  caseName := ParamStr(1);
  StartWatchdog;
  monitor := TOmniEventMonitor.Create(nil);
  try
    try
      RunCase;
      Writeln('PASS: ', caseName);
    except
      on E: Exception do
        Fail(E.ClassName + ': ' + E.Message);
    end;
  finally FreeAndNil(monitor); end;
end.
