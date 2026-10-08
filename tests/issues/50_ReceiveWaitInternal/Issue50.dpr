program Issue50;

{ Repro for GitHub issue #50: task.Comm.ReceiveWait (and Receive) returned OTL's internal
  messages - for example the call of the method passed to IOmniTaskControl.Run(@Method) - to
  the user code, which swallowed them. The method was then never executed.

  The worker waits in Initialize for a user message. The method to run is queued for the
  worker before that message arrives. Expected: the worker only sees its own messages, and
  Execute is called after Initialize has finished.

  Usage: Issue50 <Run|Invoke|Receive>
    Run     - IOmniTaskControl.Run(@TWorker.Execute), worker uses ReceiveWait in Initialize
    Invoke  - like Run, but the method is queued with Invoke after Run
    Receive - like Run, but the worker polls with Receive instead of ReceiveWait

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl;

const
  MSG_CONTINUE = 1;

type
  TWorker = class(TOmniWorker)
  strict private
    FUsePolling: boolean;
  public
    SeenInternalMessage: boolean;
    constructor Create(usePolling: boolean);
    function Initialize: boolean; override;
  published
    procedure Execute;
  end;

var
  GExecuted: boolean;
  GSawInternal: boolean;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

constructor TWorker.Create(usePolling: boolean);
begin
  inherited Create;
  FUsePolling := usePolling;
end; { TWorker.Create }

function TWorker.Initialize: boolean;
var
  found: boolean;
  msg  : TOmniMessage;
begin
  Result := true;
  found := false;
  repeat
    if FUsePolling then begin
      if not Task.Comm.Receive(msg) then begin
        Sleep(10);
        continue;
      end;
    end
    else if not Task.Comm.ReceiveWait(msg, 5000) then
      Fail('timeout waiting for MSG_CONTINUE');
    if msg.MsgID = MSG_CONTINUE then
      found := true
    else
      GSawInternal := true; // an OTL internal message leaked to the user code
  until found;
end; { TWorker.Initialize }

procedure TWorker.Execute;
begin
  GExecuted := true;
end; { TWorker.Execute }

var
  caseName: string;
  start   : cardinal;
  task    : IOmniTaskControl;
  worker  : TWorker;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  caseName := ParamStr(1);
  TThread.CreateAnonymousThread(procedure begin Sleep(30000); Fail(caseName + ': hang'); end).Start;
  worker := TWorker.Create(SameText(caseName, 'Receive'));
  if SameText(caseName, 'Invoke') then begin
    task := CreateTask(worker, 'Issue50').Unobserved.Run;
    task.Invoke(@TWorker.Execute);
  end
  else
    task := CreateTask(worker, 'Issue50').Unobserved.Run(@TWorker.Execute);
  Sleep(300); // the worker is now in Initialize and has the internal message in its queue
  task.Comm.Send(MSG_CONTINUE);
  start := GetTickCount;
  while (not GExecuted) and (GetTickCount - start < 3000) do
    Sleep(10);
  if GSawInternal then
    Fail(caseName + ': the user code received an internal OTL message');
  if not GExecuted then
    Fail(caseName + ': Execute was not called');
  Writeln('PASS: ', caseName);
  Flush(System.Output);
end.
