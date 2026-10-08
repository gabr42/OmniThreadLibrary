program Issue213;

{ Repro for GitHub issues #213 and #24: DSiAllocateHWnd fails (here: the process runs out of
  window handles) while OTL creates its internal task monitor.

  #24:  TOmniEventMonitor.Destroy accessed the not yet created monitored-task dictionary, so
        the original error was replaced by an access violation.
  #213: Parallel.ForEach hung forever: the exception was raised after the loop had created its
        stop counter but before any worker was started, and the loop destructor waits for it.

  The test uses up the process's window quota (10,000 USER objects), then runs the case
  while no window can be created.

  Usage: Issue213 <Unobserved|ForEach|For>
  Expected: an exception about the failed window allocation reaches the caller (EOSError or
  similar), not an access violation, and nothing hangs.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlParallel;

const
  CWatchdog_ms = 20000;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

procedure NoOp(const task: IOmniTask);
begin
end; { NoOp }

var
  caseName: string;
  gotError: boolean;
  i       : integer;
  windows : array of HWND;
  wnd     : HWND;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  caseName := ParamStr(1);
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(CWatchdog_ms);
      Fail(caseName + ': hang (no result in ' + IntToStr(CWatchdog_ms) + ' ms)');
    end).Start;

  // No warm-up on purpose: OTL's shared internal monitor window must not exist yet,
  // so that creating it is what fails.
  // Use up the window quota.
  SetLength(windows, 0);
  repeat
    wnd := CreateWindowEx(0, 'STATIC', nil, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, HInstance, nil);
    if wnd <> 0 then begin
      SetLength(windows, Length(windows) + 1);
      windows[High(windows)] := wnd;
    end;
  until wnd = 0;
  Writeln('created ', Length(windows), ' windows before the quota was exhausted');
  // Leave room for a few handles needed by the runtime itself (thread, event, console).
  gotError := false;
  try
    if SameText(caseName, 'Unobserved') then
      CreateTask(NoOp, 'NoOp').Unobserved.Run
    else if SameText(caseName, 'ForEach') then
      Parallel.ForEach(1, 100).NoWait.Execute(procedure (const value: integer) begin end)
    else if SameText(caseName, 'For') then
      Parallel.&For(1, 100).NoWait.Execute(procedure (value: integer) begin end)
    else
      Fail('unknown case ' + caseName);
  except
    on E: EAccessViolation do
      Fail(caseName + ': ' + E.ClassName + ': ' + E.Message + ' (the original error was lost)');
    on E: Exception do begin
      gotError := true;
      Writeln(caseName, ': got the expected failure: ', E.ClassName, ': ', E.Message);
    end;
  end;
  for i := 0 to High(windows) do
    DestroyWindow(windows[i]);
  if not gotError then
    Fail(caseName + ': no error although no window could be created');
  Writeln('PASS: ', caseName);
  Flush(System.Output);
end.
