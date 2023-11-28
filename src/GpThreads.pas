(* A simple re-implementation of a proprietary unit.
*)

unit GpThreads;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes;

type
  TNamedThread = class(TThread)
  private
    FInitialized        : boolean;
    FThreadName         : string;
    FRunning            : boolean;
    FTerminateEvent     : THandle;
    FThreadFinishedEvent: THandle;
  protected
    procedure CleanupThreadData; virtual;
    function  GetThreadFinished: boolean;
    procedure InitializeThreadData; virtual;
    procedure PostExecute; virtual;
    procedure PreExecute; virtual;
    property IsRunning: boolean read FRunning;
  public
    constructor Create(CreateSuspended: boolean; const ThreadName: string);
    destructor  Destroy; override;
    procedure Execute; override;
    procedure ProcessThreadMessages(exitOnTerminate: boolean = false);
    procedure SafeExecute; virtual; abstract;
    procedure Terminate; virtual;
    property Name: string read FThreadName;
    property Terminated;
    property TerminateEvent: THandle read FTerminateEvent;
    property ThreadFinished: Boolean read GetThreadFinished;
    property ThreadFinishedEvent: THandle read FThreadFinishedEvent;
  end; { TNamedThread }

implementation

{ TNamedThread }

constructor TNamedThread.Create(CreateSuspended: boolean; const ThreadName: string);
begin
  FThreadName := ThreadName;
  InitializeThreadData;
  inherited Create(CreateSuspended);
end; { TNamedThread.CreateEx }

destructor TNamedThread.Destroy;
begin
  if FRunning and (not Terminated) then begin
    Terminate;
    WaitFor;
  end;
  CleanupThreadData;
  inherited Destroy;
end; { TNamedThread.Destroy }

procedure TNamedThread.CleanupThreadData;
begin
  CloseHandle(FThreadFinishedEvent);
  CloseHandle(FTerminateEvent);
end; { TNamedThread.CleanupThreadData }

function TNamedThread.GetThreadFinished: boolean;
begin
  Result := WaitForSingleObject(ThreadFinishedEvent, 0) = WAIT_OBJECT_0;
end; { TNamedThread.GetThreadFinished }

procedure TNamedThread.Terminate;
begin
  if not Suspended then // wait for thread to start up
    while not FInitialized do
      Sleep(1);
  inherited Terminate;
  if TerminateEvent <> 0 then
    SetEvent(TerminateEvent);
  FInitialized := true; //handle special case: thread.Create(suspended); thread.Terminate; thread.Resume; thread.Terminate;
end; { TNamedThread.Terminate }

procedure TNamedThread.Execute;
begin
  TThread.NameThreadForDebugging(FThreadName);
  FInitialized := true;
  FRunning := true;
  try
    PreExecute;
    try
      SafeExecute;
    finally PostExecute; end;
  finally
    FRunning := false;
    Terminate; // set Terminated
    SetEvent(ThreadFinishedEvent);
  end;
end; { TNamedThread.Execute }

procedure TNamedThread.InitializeThreadData;
begin
  FTerminateEvent := CreateEvent (nil, true, false, nil);
  FThreadFinishedEvent := CreateEvent (nil, true, false, nil);
end; { TNamedThread.InitializeThreadData }

procedure TNamedThread.PostExecute;
begin
  // intentionally empty
end; { TNamedThread.PostExecute }

procedure TNamedThread.PreExecute;
begin
  // intentionally empty
end; { TNamedThread.PreExecute }

procedure TNamedThread.ProcessThreadMessages(exitOnTerminate: boolean);
var
  msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) and (Msg.Message <> WM_QUIT) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
    if exitOnTerminate and (TerminateEvent <> 0)
      and (WaitForSingleObject(TerminateEvent, 0) <> WAIT_TIMEOUT)
    then
      break; //while
  end;
end; { TNamedThread.ProcessThreadMessages }

end.
