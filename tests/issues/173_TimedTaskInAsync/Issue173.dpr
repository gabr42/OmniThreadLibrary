program Issue173;

{ Repro for GitHub issue #173: timed tasks created from a Parallel.Async task raise
  'Error 1400: invalid window handle' when the application is closed.

  The timed tasks are started from a thread pool thread. Their internal monitor belongs to that
  thread, so when they terminate, the termination message may be posted to a window that does
  not exist anymore.

  Exceptions are detected with a vectored exception handler (Delphi exceptions use the
  exception code $0EEDFADE); any EOSError counts.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlTask,
  OtlThreadPool,
  OtlParallel;

const
  CDelphiException = $0EEDFADE;

var
  GErrors: integer;
  GLastError: integer;

function AddVectoredExceptionHandler(first: ULONG; handler: pointer): pointer; stdcall;
  external 'kernel32.dll';

function Watch(excInfo: PExceptionPointers): LONG; stdcall;
var
  obj: TObject;
begin
  if excInfo.ExceptionRecord.ExceptionCode = CDelphiException then begin
    obj := TObject(excInfo.ExceptionRecord.ExceptionInformation[1]);
    if obj is EOSError then begin
      GLastError := EOSError(obj).ErrorCode;
      AtomicIncrement(GErrors);
    end;
  end;
  Result := 0; // EXCEPTION_CONTINUE_SEARCH
end; { Watch }

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  FTask1: IOmniTimedTask;
  FTask2: IOmniTimedTask;

procedure DoSomething1;
begin
end; { DoSomething1 }

procedure DoSomething2;
begin
end; { DoSomething2 }

procedure StartTimedTasks(const task: IOmniTask);
begin
  FTask1 := Parallel.TimedTask.Every(100).Execute(DoSomething1);
  FTask2 := Parallel.TimedTask.Every(100).Execute(DoSomething2);
  FTask1.Start;
  FTask2.Start;
end; { StartTimedTasks }

procedure PumpFor(time_ms: integer);
var
  msg  : TMsg;
  start: cardinal;
begin
  start := GetTickCount;
  while GetTickCount - start < cardinal(time_ms) do begin
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    Sleep(5);
  end;
end; { PumpFor }

var
  pool: IOmniThreadPool;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  TThread.CreateAnonymousThread(procedure begin Sleep(30000); Fail('hang'); end).Start;
  AddVectoredExceptionHandler(1, @Watch);
  try
    // The thread that starts the timed tasks lives in a private pool, so that the test can
    // end it before the timed tasks, as happens when an application is closed.
    pool := CreateThreadPool('Issue173');
    Parallel.Async(StartTimedTasks, Parallel.TaskConfig.ThreadPool(pool));
    PumpFor(1500);
    // "close the application": the pool threads go away first ...
    pool := nil;
    PumpFor(500);
    // ... then the timed tasks are terminated
    FTask1 := nil;
    FTask2 := nil;
    PumpFor(1500);
  except
    on E: Exception do
      Fail(E.ClassName + ': ' + E.Message);
  end;
  if GErrors > 0 then
    Fail(Format('%d EOSError exception(s), last code %d', [GErrors, GLastError]));
  Writeln('PASS');
  Flush(System.Output);
end.
