program Issue40;

{ Repro for GitHub issue #40: a thread pool stopped scheduling tasks when tasks were scheduled
  while its idle worker threads were being destroyed.

  Each round schedules a batch of tasks and waits for all of them to run; between rounds the
  test waits for about the idle thread timeout (1 s here, the pool's maintenance timer ticks
  once per second), so that new batches arrive just as idle workers are being stopped.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool;

const
  CBatchSize = 200;
  CRounds    = 25;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  GExecuted: integer;

// Unobserved tasks are released by a message window of the thread that created them, so a
// thread that creates tasks must pump messages (as every GUI application does).
procedure PumpMessages;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end; { PumpMessages }

procedure PerformOperations(const task: IOmniTask);
begin
  Sleep(100);
  TInterlocked.Increment(GExecuted);
end; { PerformOperations }

var
  i      : integer;
  pool   : IOmniThreadPool;
  round  : integer;
  start  : cardinal;
  target : integer;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  Randomize;
  pool := CreateThreadPool('Issue40');
  pool.MaxExecuting := 60;
  pool.MaxQueued := 0;
  pool.IdleWorkerThreadTimeout_sec := 1;
  target := 0;
  try
    for round := 1 to CRounds do begin
      for i := 1 to CBatchSize do
        CreateTask(PerformOperations, 'Issue40 task').Unobserved.Schedule(pool);
      Inc(target, CBatchSize);
      start := GetTickCount;
      while (GExecuted < target) and (GetTickCount - start < 15000) do begin
        PumpMessages;
        Sleep(10);
      end;
      if GExecuted < target then
        Fail(Format('round %d: only %d of %d tasks ran in 15 s - the pool stopped scheduling',
          [round, GExecuted - (target - CBatchSize), CBatchSize]));
      // arrive while the idle threads are being destroyed
      start := GetTickCount;
      while GetTickCount - start < cardinal(800 + Random(600)) do begin
        PumpMessages;
        Sleep(10);
      end;
    end;
  except
    on E: Exception do
      Fail(E.ClassName + ': ' + E.Message);
  end;
  pool := nil;
  Writeln('PASS');
  Flush(System.Output);
end.
