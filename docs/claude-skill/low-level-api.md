# OTL reference: Low-Level API

`CreateTask`, `IOmniTaskControl`, `TOmniWorker`, `IOmniTask`, task groups.

## Contents

- CreateTask
- IOmniTaskControl
- TOmniWorker
- IOmniTask
- Task Groups


### CreateTask

```pascal
function CreateTask(worker: TOmniTaskWorkerDelegate;
  const taskName: string = ''): IOmniTaskControl; overload;
function CreateTask(worker: TOmniWorker;
  const taskName: string = ''): IOmniTaskControl; overload;
// Also overloads for global procedure and object method
```

**Four ways to create a task:**

1. **Global procedure:**
```pascal
procedure RunHelloWorld(const task: IOmniTask);
begin ... end;
CreateTask(RunHelloWorld, 'HelloWorld').Run;
```

2. **Object method:**
```pascal
CreateTask(Self.RunHelloWorld, 'HelloWorld').Run;
```

3. **Anonymous method (D2009+):**
```pascal
CreateTask(
  procedure (const task: IOmniTask)
  begin ... end,
  'HelloWorld').Run;
```

4. **TOmniWorker descendant (recommended for complex tasks):**
```pascal
CreateTask(TMyWorker.Create(), 'Worker').Run;
```

---

### IOmniTaskControl

The owner-side interface for controlling a task. Returned from `CreateTask`.

```pascal
IOmniTaskControl = interface
  function  Alertable: IOmniTaskControl;
  function  CancelWith(const token: IOmniCancellationToken): IOmniTaskControl;
  function  ChainTo(const task: IOmniTaskControl;
    ignoreErrors: boolean = false): IOmniTaskControl;
  function  ClearTimer(timerID: integer): IOmniTaskControl;
  function  DetachException: Exception;
  function  DirectExecute: IOmniTaskControl;      // [3.07.9] run the task code in the calling thread
  function  Enforced(forceExecution: boolean = true): IOmniTaskControl;
  function  GetFatalException: Exception;
  function  GetParam: TOmniValueContainer;
  function  Invoke(const msgMethod: pointer): IOmniTaskControl; overload;
  function  Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload;
  function  Invoke(const msgName: string): IOmniTaskControl; overload;
  function  Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload;
  function  Invoke(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl; overload;
  function  Invoke(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl; overload;
  function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
  function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
  function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
  function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
  function  NUMANode(numaNodeNumber: integer): IOmniTaskControl;
  function  OnMessage(eventDispatcher: TObject): IOmniTaskControl; overload;
  function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
  function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
  function  OnMessage(eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
  function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
  function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskControl; overload;
  function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskControl; overload;
  function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskControl; overload;
  function  ProcessorGroup(procGroupNumber: integer): IOmniTaskControl;
  function  RemoveMonitor: IOmniTaskControl;
  function  Run: IOmniTaskControl;
  function  Schedule(const threadPool: IOmniThreadPool = nil): IOmniTaskControl;
  function  SetMonitor(hWindow: THandle): IOmniTaskControl;
  function  SetParameter(const paramName: string; const paramValue: TOmniValue):
    IOmniTaskControl; overload;
  function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
  function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
  function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
  function  SetQueueSize(numMessages: integer): IOmniTaskControl;
  function  SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TOmniMessageID): IOmniTaskControl; overload;
  procedure SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TProc); overload;
  procedure SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TProc<integer>); overload;
  function  SetUserData(const idxData: TOmniValue;
    const value: TOmniValue): IOmniTaskControl;
  procedure Stop;
  function  Terminate(maxWait_ms: cardinal = INFINITE): boolean;
  function  TerminateWhen(event: THandle): IOmniTaskControl; overload;
  function  TerminateWhen(token: IOmniCancellationToken): IOmniTaskControl; overload;
  function  Unobserved: IOmniTaskControl;
  function  WaitFor(maxWait_ms: cardinal): boolean;
  function  WaitForInit: boolean;
  function  WithCounter(const counter: IOmniCounter): IOmniTaskControl;
  function  WithLock(const lock: TSynchroObject;
    autoDestroyLock: boolean = true): IOmniTaskControl; overload;
  function  WithLock(const lock: IOmniCriticalSection): IOmniTaskControl; overload;
  //
  property CancellationToken: IOmniCancellationToken;
  property Comm: IOmniCommunicationEndpoint;
  property ExitCode: integer;
  property ExitMessage: string;
  property FatalException: Exception;
  property Lock: TSynchroObject;
  property Name: string;
  property Param: TOmniValueContainer;
  property UniqueID: int64;
  property UserData[const idxData: TOmniValue]: TOmniValue;
end;
```

**DirectExecute** [3.07.9]: instead of starting a thread (as `Run` does), runs the task code in the context of the calling thread and returns when it completes. Useful for testing, or where a task designed for the background does not need its own thread.

**MonitorWith** [3.09]: the high-level abstractions (except `Parallel.Async`) now accept `MonitorWith` in their task configuration; the internal monitor no longer conflicts with the user monitor.

**Communication** [3.09]: `Comm.Receive`/`ReceiveWait` no longer return OTL's internal messages (the queued `Run(@Method)`/`Invoke` calls) to user code - they are put back and processed by OTL (before, user code reading these endpoints swallowed them and the methods never ran). `ReceiveWait` could also lose a message (and `SendWait` send it twice) after an empty wake-up; fixed.

**CRITICAL — Owner requirement:** Always store the returned `IOmniTaskControl` in a field that outlives the task. A local variable destroys the task immediately.

```pascal
// WRONG — task is destroyed immediately:
CreateTask(MyWorker).Run;

// CORRECT:
FTaskControl := CreateTask(MyWorker).Run;

// Valid alternatives — task has implicit owner:
CreateTask(MyWorker).MonitorWith(eventMonitor).Run;
CreateTask(MyWorker).Unobserved.Run;
CreateTask(Beep, 'Beep').Schedule;    // thread pool acts as owner
```

**Parameters (set before Run/Schedule):**
```pascal
taskControl.SetParameter('Name', 'Value');           // named
taskControl.SetParameters(['From', 0, 'To', 99]);    // multiple named
taskControl.SetParameter('42');                       // positional
```

**Termination:**
```pascal
procedure Stop;                                           // request stop, don't wait
function Terminate(maxWait_ms: cardinal = INFINITE): boolean;  // request + wait
function WaitFor(maxWait_ms: cardinal): boolean;         // wait for already-stopping task
```

**Reserved exit codes:**
```pascal
EXIT_OK                        = 0;
EXIT_INTERNAL                  = integer($80000000);
EXIT_THREADPOOL_QUEUE_TOO_LONG = EXIT_INTERNAL + 0;
EXIT_THREADPOOL_STALE_TASK     = EXIT_INTERNAL + 1;
EXIT_THREADPOOL_CANCELLED      = EXIT_INTERNAL + 2;
EXIT_THREADPOOL_INTERNAL_ERROR = EXIT_INTERNAL + 3;
```

**Timers (for TOmniWorker tasks):**
```pascal
FTask.SetTimer(1, 150, 'Timer1');             // calls procedure Timer1
FTask.SetTimer(2, 200, @TMyTask.Timer2);      // calls procedure Timer2
FTask.SetTimer(3, 250, MSG_TIMER3);           // dispatches message MSG_TIMER3
FTask.SetTimer(4, 333,
  procedure (timerID: integer) begin ... end);  // anonymous [3.07.3]
FTask.ClearTimer(timerID);
```

**Invoke (execute code in task's thread):**
```pascal
// By method name (runtime check):
FTask.Invoke('MethodName');
FTask.Invoke('MethodName', someValue);

// By method address (compile-time check):
FTask.Invoke(@TMyTask.MethodName);
FTask.Invoke(@TMyTask.MethodName, someValue);

// By anonymous method:
FTask.Invoke(procedure begin ... end);
FTask.Invoke(procedure (const task: IOmniTask) begin ... end);
```

Invoke sends a special message (ID $FFFF) to the task — executed asynchronously, not immediately.

**Thread priority:**
```pascal
type TOTLThreadPriority = (tpIdle, tpLowest, tpBelowNormal, tpNormal, tpAboveNormal, tpHighest);
```

**Message queue size (default 1000):**
```pascal
CreateTask(MyWorker).SetQueueSize(10000).Run;  // must be before Run/Schedule
```

**MsgWait / Alertable:**
```pascal
function MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;  // process Windows msgs
function Alertable: IOmniTaskControl;   // enable MWMO_ALERTABLE for APC processing
```

---

### TOmniWorker

Base class for structured worker tasks. OTL implements the internal wait loop.

```pascal
type
  TOmniWorker = class(TInterfacedObject, IOmniWorker)
  public
    function  Initialize: boolean; virtual;  // called on worker thread; return false = abort
    procedure Cleanup; virtual;              // called after loop exits
    property Task: IOmniTask read GetTask write SetTask;
  protected
    procedure BeforeWait(var timeout_ms: cardinal); virtual;   // before waiting for the next event; may change the timeout
    procedure AfterWait(waitFor: TWaitFor; awaited: TWaitFor.TWaitForResult); virtual;  // wait over, before dispatching
    procedure MessageLoopPayload; virtual;                     // after each processed event
    function  EventInfo(awaited: TWaitFor.TWaitForResult): TOmniWorkerEventInfo;  // [3.07.9] only valid inside AfterWait
  end;

  TOmniWorkerEventInfoType = (etError, etFailed, etIOCompletion, etTimeout,
    etWinMessage, etMessage, etEvent, etTimer, etWaitObject, etTerminate,
    etInternal, etUnknown);

  TOmniWorkerEventInfo = record
    EventType  : TOmniWorkerEventInfoType;
    TimerID    : integer;   // etTimer
    WaitObject : integer;   // etWaitObject: index of the signalled wait object
    CommChannel: integer;   // etMessage: -1 = default channel, else index of a RegisterComm channel
  end;
```

`EventInfo` [3.07.9] describes the event that ended the wait:
```pascal
procedure TMyWorker.AfterWait(waitFor: TWaitFor; awaited: TWaitFor.TWaitForResult);
var info: TOmniWorkerEventInfo;
begin
  inherited;
  info := EventInfo(awaited);
  if info.EventType = etTimer then Log('Timer %d', [info.TimerID]);
end;
```

**Events watched by the internal loop:**
- TerminateEvent
- TerminateWhen events/tokens
- New messages from task controller (Comm)
- New messages from registered comms (RegisterComm)
- Registered wait objects (RegisterWaitObject)
- Timers (SetTimer)
- Windows messages (when MsgWait is used)

**CRITICAL — Initialize override pattern:**
```pascal
function TMyWorker.Initialize: boolean;
begin
  Result := inherited Initialize;
  if not Result then Exit;
  FMyResource := TMyResource.Create;
end;

procedure TMyWorker.Cleanup;
begin
  FreeAndNil(FMyResource);
  inherited Cleanup;
end;
```

**Message handlers (Delphi message dispatch):**
```pascal
const MSG_DO_WORK = 1;

type
  TMyWorker = class(TOmniWorker)
  public
    procedure MsgDoWork(var msg: TOmniMessage); message MSG_DO_WORK;
  end;

procedure TMyWorker.MsgDoWork(var msg: TOmniMessage);
begin
  // msg.MsgID = MSG_DO_WORK
  // msg.MsgData contains the data
end;
```

Message IDs: 0..$FFFE. ID $FFFF is reserved for Invoke.

**RegisterComm — direct task-to-task messaging:**
```pascal
// Create channel in owner:
FCommChannel := CreateTwoWayChannel(1024);

// In worker Initialize:
Task.RegisterComm(ctComm);   // starts listening

// In worker Cleanup:
Task.UnregisterComm(ctComm);

// Send from worker:
ctComm.Send(MSG_FORWARDING, data);
```

**Timers from inside worker:**
```pascal
Task.SetTimer(1, 500, 'OnTimer');
Task.ClearTimer(1);
```

---

### IOmniTask

The worker-side interface (available inside task's thread).

```pascal
IOmniTask = interface
  procedure ClearTimer(timerID: integer = 0);
  procedure Enforced(forceExecution: boolean = true);
  procedure Invoke(remoteFunc: TOmniTaskInvokeFunction);        // execute in owner thread
  procedure InvokeOnSelf(remoteFunc: TOmniTaskInvokeFunction);  // execute in own task [3.07.3]
  procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
  procedure RegisterWaitObject(waitObject: THandle;
    responseHandler: TOmniWaitObjectMethod); overload;
  procedure RegisterWaitObject(waitObject: THandle;
    responseHandler: TOmniWaitObjectProc); overload;   // [3.07.8] anonymous method (Delphi 2009+); TOmniWaitObjectProc = reference to procedure
  procedure SetException(exceptionObject: pointer);
  procedure SetExitStatus(exitCode: integer; const exitMessage: string);
  procedure SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TOmniMessageID); overload;
  procedure SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TProc); overload;
  procedure SetTimer(timerID: integer; interval_ms: cardinal;
    const timerMessage: TProc<integer>); overload;
  procedure Terminate;
  function  Terminated: boolean;
  function  Stopped: boolean;
  procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
  procedure UnregisterWaitObject(waitObject: THandle);
  property CancellationToken: IOmniCancellationToken;
  property Comm: IOmniCommunicationEndpoint;
  property Counter: IOmniCounter;
  property Lock: TSynchroObject;
  property Name: string;
  property Param: TOmniValueContainer;
  property TerminateEvent: THandle;   // signalled when Terminate is called
  property ThreadData: IInterface;
  property UniqueID: int64;
end;
```

**Key behaviors:**
- `Terminate`: causes task to stop; sets `Terminated = true`
- `Terminated`: returns `true` when termination requested
- `TerminateEvent`: Windows event handle; wait on this for periodic work
- `Invoke`: posts anonymous method to **owner thread's** message queue
- `InvokeOnSelf` [3.07.3]: posts to task's **own** queue (TOmniWorker only)

**Parameter access:**
```pascal
task.Param['NamedParam']  // string-indexed
task.Param[0]             // integer-indexed (positional)
```

**Simple task termination patterns:**
```pascal
// Pattern 1: using TerminateEvent
procedure Worker(const task: IOmniTask);
begin
  while WaitForSingleObject(task.TerminateEvent, 1000) = WAIT_TIMEOUT do
    DoPeriodicWork;
end;

// Pattern 2: polling Terminated
procedure Worker(const task: IOmniTask);
begin
  while not task.Terminated do begin
    DoPeriodicWork;
    Sleep(1000);
  end;
end;
```

**Processing messages in simple task:**
```pascal
procedure RunHello(const task: IOmniTask);
var msg: TOmniMessage; msgID: word; msgData: TOmniValue;
begin
  repeat
    case DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent,
           false, INFINITE)
    of
      WAIT_OBJECT_0: break;   // terminate
      WAIT_OBJECT_1:          // new message
        while task.Comm.Receive(msgID, msgData) do begin
          case msgID of
            MSG_SOMETHING: ...;
          end;
        end;
    end;
  until false;
end;
```

---

### Task Groups

```pascal
function CreateTaskGroup: IOmniTaskGroup;

IOmniTaskGroup = interface
  function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
  function  GetEnumerator: IOmniTaskControlListEnumerator;
  function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
  function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
  function  RunAll: IOmniTaskGroup;
  procedure SendToAll(const msg: TOmniMessage);
  function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
  function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
  function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  property Tasks: IOmniTaskControlList;
end;
```

No size limit since [3.04].

`WaitForAll` and `TerminateAll` no longer crash on an empty group [3.07.11].
