program Issue60;

{ Repro for GitHub issue #60: scheduling a task monitored with the same TOmniEventMonitor as its
  thread pool raised an access violation in TOmniEventMonitor.WndProc (reported for 64-bit).

  The test pumps messages for a while and checks that the monitor reports the pool events and
  the task termination without any exception.

  Usage: Issue60 [basic|replace]
    basic   - one pool, several tasks
    replace - like the original report, a new pool replaces the previous one (which still has
              a running task) every time a task is started

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool,
  OtlEventMonitor;

type
  TSleepWorker = class(TOmniWorker)
  public
    function Initialize: boolean; override;
  end;

  TCounters = class
  public
    Completed : integer;
    Created   : integer;
    Terminated: integer;
    procedure OnCreated(const pool: IOmniThreadPool; threadID: integer);
    procedure OnCompleted(const pool: IOmniThreadPool; taskID: int64);
    procedure OnTerminated(const task: IOmniTaskControl);
  end;

function TSleepWorker.Initialize: boolean;
begin
  Sleep(50);
  Result := true;
  Task.Terminate;
end; { TSleepWorker.Initialize }

procedure TCounters.OnCreated(const pool: IOmniThreadPool; threadID: integer);
begin
  Inc(Created);
end; { TCounters.OnCreated }

procedure TCounters.OnCompleted(const pool: IOmniThreadPool; taskID: int64);
begin
  Inc(Completed);
end; { TCounters.OnCompleted }

procedure TCounters.OnTerminated(const task: IOmniTaskControl);
begin
  Inc(Terminated);
end; { TCounters.OnTerminated }

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

procedure Worker(const task: IOmniTask);
begin
  Sleep(50);
end; { Worker }

const
  CNumTasks = 10;

var
  counters: TCounters;
  i       : integer;
  monitor : TOmniEventMonitor;
  msg     : TMsg;
  pool    : IOmniThreadPool;
  start   : cardinal;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  TThread.CreateAnonymousThread(procedure begin Sleep(20000); Fail('hang'); end).Start;
  counters := TCounters.Create;
  monitor := TOmniEventMonitor.Create(nil);
  try
    try
      monitor.OnPoolThreadCreated := counters.OnCreated;
      monitor.OnPoolWorkItemCompleted := counters.OnCompleted;
      monitor.OnTaskTerminated := counters.OnTerminated;
      if SameText(ParamStr(1), 'replace') then begin
        for i := 1 to CNumTasks do begin
          pool := CreateThreadPool('Issue60 #' + IntToStr(i)).MonitorWith(monitor);
          CreateTask(TSleepWorker.Create() as IOmniWorker, 'Test task ' + IntToStr(i)).MonitorWith(monitor).Schedule(pool);
          Sleep(10);
        end;
      end
      else begin
        pool := CreateThreadPool('Issue60').MonitorWith(monitor);
        for i := 1 to CNumTasks do
          CreateTask(Worker, 'Test task ' + IntToStr(i)).MonitorWith(monitor).Schedule(pool);
      end;
      start := GetTickCount;
      while ((counters.Terminated < CNumTasks) or
             ((counters.Completed < CNumTasks) and not SameText(ParamStr(1), 'replace'))) and
            (GetTickCount - start < 5000) do
      begin
        while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;
        Sleep(10);
      end;
      if counters.Created = 0 then
        Fail('no OnPoolThreadCreated event');
      if (not SameText(ParamStr(1), 'replace')) and (counters.Completed <> CNumTasks) then
        Fail(Format('%d OnPoolWorkItemCompleted events, expected %d', [counters.Completed, CNumTasks]));
      if counters.Terminated <> CNumTasks then
        Fail(Format('%d OnTaskTerminated events, expected %d', [counters.Terminated, CNumTasks]));
    except
      on E: Exception do
        Fail(E.ClassName + ': ' + E.Message);
    end;
    pool := nil;
  finally
    FreeAndNil(monitor);
    FreeAndNil(counters);
  end;
  Writeln('PASS');
  Flush(System.Output);
end.
