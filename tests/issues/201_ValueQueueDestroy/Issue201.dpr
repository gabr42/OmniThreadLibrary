program Issue201;

{ Repro for GitHub issue #201: destroying a TOmniValueQueue that still contains
  items raises an access violation.

  TOmniValueQueue.Destroy does FreeAndNil(FInnerQueue). TQueue<T>.Destroy clears
  the queue and fires OnNotify (cnRemoved) for every remaining item; the handler,
  TOmniValueQueue.CollectionNotifyEvent, reads FInnerQueue.Count, but FInnerQueue
  has already been set to nil by FreeAndNil.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  OtlCommon,
  OtlContainers;

var
  failures: integer = 0;

procedure Check(const name: string; useBusLocking: boolean; numItems: integer);
var
  i    : integer;
  queue: IOmniValueQueue;
begin
  try
    queue := CreateOmniValueQueue(useBusLocking);
    for i := 1 to numItems do
      queue.Enqueue(i);
    queue := nil; // destroys the queue with numItems items still in it
    Writeln('PASS: ', name);
  except
    on E: Exception do begin
      Writeln('FAIL: ', name, ' - ', E.ClassName, ': ', E.Message);
      Inc(failures);
    end;
  end;
end; { Check }

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  Check('critical section queue, empty (control)',   false, 0);
  Check('spinlock queue, empty (control)',           true,  0);
  Check('critical section queue, 3 items left',      false, 3);
  Check('spinlock queue, 3 items left',              true,  3);
  if failures > 0 then
    ExitCode := 1;
end.
