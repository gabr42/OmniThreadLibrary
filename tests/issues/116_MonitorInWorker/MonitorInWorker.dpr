program MonitorInWorker;

{ Issue #116: a TOmniEventMonitor created inside a TOmniWorker (a non-main owner thread)
  must deliver OnTaskTerminated and OnTaskMessage events in that thread when the
  owner task uses MsgWait (the worker then pumps Windows messages; without it nobody does) -
  the worker's message loop pumps Windows messages.
  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TOwner = class(TOmniWorker)
  strict private
    FMonitor     : TOmniEventMonitor;
    FChild       : IOmniTaskControl;
  public
    Terminated   : integer;
    GotMessage   : integer;
    EventThreadOK: boolean;
    function  Initialize: boolean; override;
    procedure Cleanup; override;
    procedure OnTerminated(const task: IOmniTaskControl);
    procedure OnMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  end;

  TChild = class(TOmniWorker)
  public
    function Initialize: boolean; override;
  end;

function TChild.Initialize: boolean;
begin
  Result := true;
  Task.Comm.Send(7, 'hello');
  Task.Terminate;
end;

function TOwner.Initialize: boolean;
begin
  Result := inherited Initialize;
  FMonitor := TOmniEventMonitor.Create(nil);
  FMonitor.OnTaskTerminated := OnTerminated;
  FMonitor.OnTaskMessage := OnMessage;
  FChild := FMonitor.Monitor(CreateTask(TChild.Create as IOmniWorker, 'child')).Run;
end;

procedure TOwner.Cleanup;
begin
  FChild := nil;
  FreeAndNil(FMonitor);
  inherited;
end;

procedure TOwner.OnTerminated(const task: IOmniTaskControl);
begin
  EventThreadOK := GetCurrentThreadID = FMonitor.ThreadID;
  Inc(Terminated);
end;

procedure TOwner.OnMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  Inc(GotMessage);
end;

var
  owner  : TOwner;
  ctl    : IOmniTaskControl;
  t0     : cardinal;
begin
  owner := TOwner.Create;
  ctl := CreateTask(owner, 'owner').MsgWait.Run;
  t0 := GetTickCount;
  while ((owner.Terminated = 0) or (owner.GotMessage = 0)) and (GetTickCount - t0 < 10000) do
    Sleep(50);
  if (owner.Terminated = 1) and (owner.GotMessage = 1) and owner.EventThreadOK then
    Writeln('PASS: monitor in a worker thread delivered terminated + message events in its owner thread')
  else begin
    Writeln('FAIL: terminated=', owner.Terminated, ' messages=', owner.GotMessage, ' ownerThread=', owner.EventThreadOK);
    Halt(1);
  end;
  ctl.Terminate(5000);
end.
