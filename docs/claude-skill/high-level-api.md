# OTL reference: High-Level API (Parallel.*)

`IOmniTaskConfig` and every `Parallel.*` abstraction: Async, Async/Await, Future, Join, ParallelTask, BackgroundWorker, Pipeline, For, ForEach, ForkJoin, Map, TimedTask, Channel, Select, Merge, Race.

## Contents

- Key Concepts
- GlobalParallelPool
- IOmniTaskConfig — Task Configuration
- Parallel.Async
- Async/Await
- Parallel.Future<T>
- Parallel.Join
- Parallel.ParallelTask
- Parallel.BackgroundWorker
- Parallel.Pipeline
- Parallel.For
- Parallel.ForEach
- Parallel.ForkJoin
- Parallel.Map
- Parallel.TimedTask
- Parallel.Channel<T> [3.09]
- Parallel.Select [3.09]
- Parallel.Merge<T> [3.09]
- Parallel.Race<T> [3.09]


All high-level abstractions are in `OtlParallel`. Created through the `Parallel` factory class. Require anonymous methods (Delphi 2009+, recommended XE+).

### Key Concepts

**Fluent interfaces:** Most methods return `Self`, enabling method chaining.

**Life cycle:** An abstraction lives as long as its interface reference is alive. For `NoWait` operations, **always store the interface in a field** — not a local variable. When the interface goes out of scope, tasks are terminated.

**Delegates:** Anonymous methods, normal procedures, and object methods are all interchangeable.

**Thread pools:** All high-level tasks use `GlobalParallelPool` by default. Override via `TaskConfig.ThreadPool(...)`.

### GlobalParallelPool

```pascal
function GlobalParallelPool: IOmniThreadPool;
```

Shared pool used by all high-level abstractions. Configure like any `IOmniThreadPool`.

---

### IOmniTaskConfig — Task Configuration

Created via `Parallel.TaskConfig`:

```pascal
class function Parallel.TaskConfig: IOmniTaskConfig;

IOmniTaskConfig = interface
  procedure Apply(const task: IOmniTaskControl);
  function  CancelWith(const token: IOmniCancellationToken): IOmniTaskConfig;
  function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskConfig;
  function  NoThreadPool: IOmniTaskConfig;           // [3.07.2] use new thread, not pool
  function  OnMessage(eventDispatcher: TObject): IOmniTaskConfig; overload;
  function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
  function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
  function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskConfig; overload;
  function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskConfig; overload;
  function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskConfig; overload;
  function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskConfig; overload;
  function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskConfig;
  function  ThreadPool(const threadPool: IOmniThreadPool): IOmniTaskConfig;
  function  WithCounter(const counter: IOmniCounter): IOmniTaskConfig;
  function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskConfig; overload;
  function  WithLock(const lock: IOmniCriticalSection): IOmniTaskConfig; overload;
end;
```

**Thread pool selection:**
- No `TaskConfig` → `GlobalParallelPool`
- `TaskConfig.ThreadPool(aPool)` → `aPool`
- `TaskConfig.ThreadPool(nil)` → default `GlobalOmniThreadPool`
- `TaskConfig.NoThreadPool` → new dedicated thread
- Otherwise → `GlobalParallelPool`

**MonitorWith** [3.09]: can be used in the task configuration passed to `Join`, `For`, `ForEach`, `Future` and `Pipeline` (before 3.09 it raised *Task can be only monitored with a single monitor* or hung). `Parallel.Async` still cannot be combined with `MonitorWith`.

---

### Parallel.Async

Fire-and-forget background task.

```pascal
type TOmniTaskDelegate = reference to procedure(const task: IOmniTask);

class procedure Parallel.Async(task: TProc; taskConfig: IOmniTaskConfig = nil); overload;
class procedure Parallel.Async(task: TOmniTaskDelegate; taskConfig: IOmniTaskConfig = nil); overload;
```

**Example:**
```pascal
Parallel.Async(
  procedure
  begin
    MessageBeep($FFFFFFFF);
  end);

// With IOmniTask access for Invoke:
Parallel.Async(
  procedure (const task: IOmniTask)
  var page: string;
  begin
    HttpGet('server', 80, 'page.htm', page, '');
    task.Invoke(
      procedure
      begin
        Memo1.Lines.Add('Length = ' + IntToStr(Length(page)));
      end);
  end);
```

**Exception handling:** Unhandled exceptions re-raised in `OnTerminated` handler.

```pascal
Parallel.Async(
  procedure begin raise Exception.Create('oops'); end,
  Parallel.TaskConfig.OnTerminated(
    procedure (const task: IOmniTaskControl)
    var excp: Exception;
    begin
      if assigned(task.FatalException) then begin
        excp := task.DetachException;
        Log('Exception: ' + excp.Message);
        FreeAndNil(excp);
      end;
    end));
```

---

### Async/Await

Background operation + UI callback after completion. Uses standalone `Async` function (not `Parallel.Async`):

```pascal
Async(
  procedure begin Sleep(5000); end).  // runs in background thread
Await(
  procedure begin button.Enabled := true; end); // runs in main thread after completion
```

**Caveat:** Exceptions in the Async part are currently not handled by OTL.

---

### Parallel.Future\<T\>

Background calculation that returns a result.

```pascal
type
  TOmniFutureDelegate<T> = reference to function: T;
  TOmniFutureDelegateEx<T> = reference to function(const task: IOmniTask): T;

class function Parallel.Future<T>(action: TOmniFutureDelegate<T>;
  taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;
class function Parallel.Future<T>(action: TOmniFutureDelegateEx<T>;
  taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;

IOmniFuture<T> = interface
  procedure Cancel;
  function  DetachException: Exception;
  function  FatalException: Exception;
  function  IsCancelled: boolean;
  function  IsDone: boolean;
  function  TryValue(timeout_ms: cardinal; var value: T): boolean;
  function  Value: T;
  function  WaitFor(timeout_ms: cardinal): boolean;
end;
```

**Example:**
```pascal
var FCalc: IOmniFuture<integer>;

FCalc := Parallel.Future<integer>(
  function: integer
  var i: integer;
  begin
    Result := 0;
    for i := 1 to 100000 do
      Result := Result + i;
  end);
// ... do other work ...
Result := FCalc.Value;  // blocks until done
FCalc := nil;
```

**Cancellation (cooperative):**
```pascal
function CountTo100(const task: IOmniTask): integer;
var i: integer;
begin
  for i := 1 to 100 do begin
    Sleep(100);
    Result := i;
    if task.CancellationToken.IsSignalled then break;
  end;
end;

FCountFuture := Parallel.Future<integer>(CountTo100);
FCountFuture.Cancel;
FCountFuture.WaitFor(INFINITE);
```

**Exception handling:** Exceptions re-raised when `Value` is called. Use `FatalException`/`DetachException` to inspect without re-raising.

```pascal
// Method 1: try/except around Value
try
  Log(future.Value);
except on E: Exception do Log('Exception: ' + E.Message); end;

// Method 2: check FatalException after WaitFor
future.WaitFor(INFINITE);
if assigned(future.FatalException) then ...
else Log(future.Value);

// Method 3: detach and own the exception
future.WaitFor(INFINITE);
excFuture := future.DetachException;
try
  if assigned(excFuture) then ... else Log(future.Value);
finally FreeAndNil(excFuture); end;
```

---

### Parallel.Join

Run multiple tasks in parallel and wait for all to complete.

```pascal
type TOmniJoinDelegate = reference to procedure(const joinState: IOmniJoinState);

class function Parallel.Join: IOmniParallelJoin; overload;
class function Parallel.Join(const task1, task2: TProc): IOmniParallelJoin; overload;
class function Parallel.Join(const task1, task2: TOmniJoinDelegate): IOmniParallelJoin; overload;
class function Parallel.Join(const tasks: array of TProc): IOmniParallelJoin; overload;
class function Parallel.Join(const tasks: array of TOmniJoinDelegate): IOmniParallelJoin; overload;

IOmniParallelJoin = interface
  function Cancel: IOmniParallelJoin;
  function DetachException: Exception;
  function Execute: IOmniParallelJoin;
  function FatalException: Exception;
  function IsCancelled: boolean;
  function IsExceptional: boolean;
  function NumTasks(numTasks: integer): IOmniParallelJoin;
  function OnStop(const stopCode: TProc): IOmniParallelJoin; overload;
  function OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelJoin; overload;
  function OnStopInvoke(const stopCode: TProc): IOmniParallelJoin;   // [3.07.2]
  function Task(const task: TProc): IOmniParallelJoin; overload;
  function Task(const task: TOmniJoinDelegate): IOmniParallelJoin; overload;
  function TaskConfig(const config: IOmniTaskConfig): IOmniParallelJoin;
  function NoWait: IOmniParallelJoin;
  function Terminate(maxWait_ms: cardinal = INFINITE): boolean;   // [3.07.9]
  function WaitFor(timeout_ms: cardinal): boolean;
end;

IOmniJoinState = interface
  procedure Cancel;
  function  IsCancelled: boolean;
  function  IsExceptional: boolean;
  property Task: IOmniTask read GetTask;
end;
```

**NumTasks:** positive = exact count; negative = cores minus that (at least 1 task [3.07.10]); 0 = exception.

**OnStop** is called from a worker thread. Use `OnStopInvoke` [3.07.2] to automatically execute in the owner thread.

**Terminate** [3.07.9]: waits up to `maxWait_ms` like `WaitFor` and returns `True` if the tasks stopped; otherwise kills the still-running threads (last resort, dangerous) and returns `False`.

**NoWait lifetime** [3.09]: releasing the last reference to a `NoWait` Join without `WaitFor`/`Terminate` raises an exception in the destructor.

**Exception handling:** Multiple exceptions wrapped in `EJoinException`:
```pascal
EJoinException = class(Exception)
  procedure Add(iTask: integer; taskException: Exception);
  function  Count: integer;
  property Inner[idxException: integer]: TJoinInnerException read GetInner; default;
end;

try
  Parallel.Join([proc1, proc2]).Execute;
except
  on E: EJoinException do
    for iInnerExc := 0 to E.Count - 1 do
      Log('Task #%d: %s', [E[iInnerExc].TaskNumber, E[iInnerExc].FatalException.Message]);
end;
```

---

### Parallel.ParallelTask

Run the same code in multiple parallel threads.

```pascal
class function Parallel.ParallelTask: IOmniParallelTask;

type TOmniParallelTaskDelegate = reference to procedure(const task: IOmniTask);

IOmniParallelTask = interface
  function  Cancel: IOmniParallelTask;                              // [3.07.9]
  function  Execute(const aTask: TProc): IOmniParallelTask; overload;
  function  Execute(const aTask: TOmniParallelTaskDelegate): IOmniParallelTask; overload;
  function  IsCancelled: boolean;                                   // [3.07.9]
  function  NoWait: IOmniParallelTask;
  function  NumTasks(numTasks: integer): IOmniParallelTask;
  function  OnStop(const stopCode: TProc): IOmniParallelTask; overload;
  function  OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelTask; overload;
  function  OnStopInvoke(const stopCode: TProc): IOmniParallelTask;   // [3.07.2]
  function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelTask;
  function  Terminate(maxWait_ms: cardinal = INFINITE): boolean;    // [3.07.9]
  function  WaitFor(timeout_ms: cardinal): boolean;
end;
```

`Cancel`, `IsCancelled` and `Terminate` [3.07.9] work as in `Join`. A `NoWait` Parallel task must be waited for (`WaitFor`/`Terminate`) before it is destroyed [3.09].

**Helper:**
```pascal
class function Parallel.CompleteQueue(const queue: IOmniBlockingCollection): TProc;
// Returns TProc that calls queue.CompleteAdding — useful in OnStop
```

---

### Parallel.BackgroundWorker

Client/server pattern: background threads processing work items from a queue.

```pascal
class function Parallel.BackgroundWorker: IOmniBackgroundWorker;

type
  TOmniBackgroundWorkerDelegate = reference to procedure(const workItem: IOmniWorkItem);
  TOmniWorkItemDoneDelegate = reference to procedure(
    const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
  TOmniTaskInitializerDelegate = reference to procedure(var taskState: TOmniValue);
  TOmniTaskFinalizerDelegate = reference to procedure(const taskState: TOmniValue);

IOmniBackgroundWorker = interface
  function  CreateWorkItem(const data: TOmniValue): IOmniWorkItem;
  procedure CancelAll; overload;
  procedure CancelAll(upToUniqueID: int64); overload;
  function  Config: IOmniWorkItemConfig;
  function  Execute(const aTask: TOmniBackgroundWorkerDelegate = nil): IOmniBackgroundWorker;
  function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniBackgroundWorker;
  function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniBackgroundWorker;
  function  NumTasks(numTasks: integer): IOmniBackgroundWorker;
  function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
  function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
  function  OnStop(stopCode: TProc): IOmniBackgroundWorker; overload;
  function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniBackgroundWorker; overload;
  function  OnStopInvoke(stopCode: TProc): IOmniBackgroundWorker;  // [3.07.2]
  procedure Schedule(const workItem: IOmniWorkItem;
    const workItemConfig: IOmniWorkItemConfig = nil);
  function  TaskConfig(const config: IOmniTaskConfig): IOmniBackgroundWorker;
  function  Terminate(maxWait_ms: cardinal): boolean;
  function  WaitFor(maxWait_ms: cardinal): boolean;
end;

IOmniWorkItem = interface
  function  DetachException: Exception;
  function  FatalException: Exception;
  function  IsExceptional: boolean;
  property CancellationToken: IOmniCancellationToken read GetCancellationToken;
  property Data: TOmniValue read GetData;
  property Result: TOmniValue read GetResult write SetResult;
  property SkipCompletionHandler: boolean read GetSkipCompletionHandler write SetSkipCompletionHandler;
  property Task: IOmniTask read GetTask;
  property TaskState: TOmniValue read GetTaskState;
  property UniqueID: int64 read GetUniqueID;
end;

IOmniWorkItemConfig = interface
  function  OnExecute(const aTask: TOmniBackgroundWorkerDelegate): IOmniWorkItemConfig;
  function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
  function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
end;
```

**Key points:**
- Default: 1 worker task (`NumTasks(1)`); a negative `NumTasks` never gives fewer than 1 task [3.07.10]
- `OnRequestDone`: called in **owner thread** — safe for UI updates
- `OnRequestDone_Asy`: called in **worker thread** — careful with shared state
- `Initialize`/`Finalize`: per-task state passed via `workItem.TaskState`
- Work items get sequential `UniqueID` starting at 1
- Cancel one item: `workItem.CancellationToken.Signal`
- Cancel all: `worker.CancelAll` or `worker.CancelAll(upToUniqueID)`
- `SkipCompletionHandler := True` prevents completion callbacks

**Example:**
```pascal
FBackgroundWorker := Parallel.BackgroundWorker.NumTasks(2)
  .Execute(
    procedure (const workItem: IOmniWorkItem)
    begin
      workItem.Result := workItem.Data.AsInteger * 3;
    end)
  .OnRequestDone(
    procedure (const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem)
    begin
      lbLog.Items.Add(Format('%d * 3 = %d',
        [workItem.Data.AsInteger, workItem.Result.AsInteger]));
    end);

FBackgroundWorker.Schedule(FBackgroundWorker.CreateWorkItem(Random(100)));

// Stop:
FBackgroundWorker.Terminate(INFINITE);
FBackgroundWorker := nil;
```

---

### Parallel.Pipeline

Multi-stage data processing pipeline where each stage runs in its own thread(s).

```pascal
type
  TPipelineStageDelegate = reference to procedure(
    const input, output: IOmniBlockingCollection);
  TPipelineStageDelegateEx = reference to procedure(
    const input, output: IOmniBlockingCollection; const task: IOmniTask);
  TPipelineSimpleStageDelegate = reference to procedure(
    const input: TOmniValue; var output: TOmniValue);

class function Parallel.Pipeline: IOmniPipeline; overload;
class function Parallel.Pipeline(const stages: array of TPipelineStageDelegate;
  const input: IOmniBlockingCollection = nil): IOmniPipeline; overload;

IOmniPipeline = interface
  procedure Cancel;
  procedure DeThrottle;                      // [3.09]
  function  From(const queue: IOmniBlockingCollection): IOmniPipeline;
  function  HandleExceptions: IOmniPipeline;
  function  NoThrottling: IOmniPipeline;    // [3.07]
  function  NumTasks(numTasks: integer): IOmniPipeline;
  function  OnStop(stopCode: TProc): IOmniPipeline; overload;
  function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniPipeline; overload;
  function  OnStopInvoke(stopCode: TProc): IOmniPipeline;   // [3.07.2]
  function  Run: IOmniPipeline;
  function  Stage(pipelineStage: TPipelineSimpleStageDelegate;
    taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
  function  Stage(pipelineStage: TPipelineStageDelegate;
    taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
  function  Stage(pipelineStage: TPipelineStageDelegateEx;
    taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
  function  Stages(const pipelineStages: array of TPipelineSimpleStageDelegate;
    taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
  function  Throttle(numEntries: integer; unblockAtCount: integer = 0): IOmniPipeline;
  function  WaitFor(timeout_ms: cardinal): boolean;
  property Input: IOmniBlockingCollection read GetInput;
  property Output: IOmniBlockingCollection read GetOutput;
  property PipelineStage[idxStage: integer]: IOmniPipelineStage read GetPipelineStage;
end;
```

**Stage types:**
- **Generator:** writes to `output` only (first stage)
- **Mutator:** reads from `input`, writes to `output` (middle stages)
- **Aggregator:** reads from `input`, writes aggregate to `output` (last stage)
- **Simple stage:** receives one `TOmniValue`, produces zero or one `TOmniValue`

**NumTasks:** Affects the stage(s) just added with `Stage`/`Stages`. If called before any stage, sets default for all.

**Throttling:** Default 10,240 elements per queue. Custom: `Throttle(maxItems)`. `unblockAtCount` defaults to 75% of `maxItems`. Disable before `Run`: `NoThrottling` [3.07]. Disable on a *running* pipeline and release all stages blocked on a full queue: `DeThrottle` [3.09] (pipeline-level equivalent of `IOmniBlockingCollection.DeThrottle`).

**Exception propagation:** Exceptions pass through queues automatically. Stage calls `HandleExceptions` to inspect:
```pascal
if value.IsException then begin
  value.AsException.Free;
  outVal.Clear;
end else ...
```

**Minimal example:**
```pascal
var sum: integer;
sum := Parallel.Pipeline
  .Stage(procedure (const input, output: IOmniBlockingCollection)
    var i: integer;
    begin for i := 1 to 1000000 do output.Add(i); end)
  .Stage(procedure (const input: TOmniValue; var output: TOmniValue)
    begin output := input.AsInteger * 3; end)
  .Stage(procedure (const input, output: IOmniBlockingCollection)
    var sum: integer; value: TOmniValue;
    begin
      sum := 0;
      for value in input do Inc(sum, value);
      output.Add(sum);
    end)
  .Run.Output.Next;
```

---

### Parallel.For

Simple parallel loop over an integer range or typed array.

```pascal
// Integer range
class function Parallel.For(low, high: integer): IOmniParallelSimpleLoop;

// Int64 range [3.09]
class function Parallel.For(first, last: int64; step: int64 = 1): IOmniParallelSimpleLoop;

// Array iteration [3.06]
class function Parallel.For<T>(const arr: TArray<T>): IOmniParallelSimpleLoop<T>;

type
  TOmniIteratorSimpleSimpleDelegate = reference to procedure(value: integer);
  TOmniIteratorSimpleDelegate = reference to procedure(taskIndex, value: integer);
  TOmniIteratorSimpleFullDelegate = reference to procedure(
    const task: IOmniTask; taskIndex, value: integer);

  TOmniSimpleTaskInitializerDelegate = reference to procedure(
    taskIndex, fromIndex, toIndex: integer);
  TOmniSimpleTaskFinalizerDelegate = reference to procedure(
    taskIndex, fromIndex, toIndex: integer);

IOmniParallelSimpleLoop = interface
  function  CancelWith(const token: IOmniCancellationToken): IOmniParallelSimpleLoop;
  function  NoWait: IOmniParallelSimpleLoop;
  function  NumTasks(taskCount: integer): IOmniParallelSimpleLoop;
  function  OnStop(stopCode: TProc): IOmniParallelSimpleLoop; overload;
  function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop; overload;
  function  OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop;   // [3.07.2]
  function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelSimpleLoop;
  procedure Execute(loopBody: TOmniIteratorSimpleSimpleDelegate); overload;
  procedure Execute(loopBody: TOmniIteratorSimpleDelegate); overload;
  procedure Execute(loopBody: TOmniIteratorSimpleFullDelegate); overload;
  function  Initialize(taskInitializer: TOmniSimpleTaskInitializerDelegate):
    IOmniParallelSimpleLoop; overload;
  function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerDelegate):
    IOmniParallelSimpleLoop; overload;
  function  WaitFor(maxWait_ms: cardinal): boolean;
end;
```

**Default NumTasks:** `NumberOfCores - 1` with `NoWait`; `NumberOfCores` otherwise (a negative `NumTasks` never gives fewer than 1 task [3.07.10]).

**Int64 ranges** [3.09]: `IOmniParallelSimpleLoop` has extra overloads of `Execute`, `Initialize` and `Finalize` whose `value`, `fromIndex` and `toIndex` are `int64`: `TOmniIteratorSimpleSimpleDelegate64 = procedure(value: int64)`, `TOmniIteratorSimpleDelegate64 = procedure(taskIndex: integer; value: int64)`, `TOmniIteratorSimpleFullDelegate64 = procedure(const task: IOmniTask; taskIndex: integer; value: int64)`, `TOmniSimpleTaskInitializerDelegate64`/`TOmniSimpleTaskFinalizerDelegate64 = procedure(taskIndex: integer; fromIndex, toIndex: int64)`, plus `...TaskDelegate64` variants with a leading `const task: IOmniTask`. Integer bounds still select the integer overload. `last` must be < `High(int64) - step`. The task index stays `integer`; an `integer` initializer/finalizer on a range that does not fit into an integer raises an exception - use the `int64` variants.

```pascal
Count.Value := 0;
Parallel.For(1, int64(5000000000)).Execute(
  procedure (value: int64)
  begin
    if (value mod 7) = 0 then Count.Increment;
  end);
```

**Exceptions** [3.09]: exceptions from the loop body, initializer and finalizer are collected and, after all tasks stop, re-raised from `Execute` as `EJoinException` (see Join). Not raised with `NoWait` - catch inside the body. Before 3.09 they were lost.

Input is split into N sequential ranges. Each task gets `taskIndex` (0-based) and `fromIndex`..`toIndex` in initializer/finalizer.

**Performance note:** `Parallel.For` is faster than `Parallel.ForEach` but less powerful.

**Example:**
```pascal
PrimeCount.Value := 0;
Parallel.For(1, 1000000).Execute(
  procedure (value: integer)
  begin
    if IsPrime(value) then PrimeCount.Increment;
  end);
```

---

### Parallel.ForEach

Powerful parallel loop over various data sources.

```pascal
// Number ranges
class function Parallel.ForEach(low, high: integer; step: integer = 1):
  IOmniParallelLoop<integer>; overload;

// Enumerable collections
class function Parallel.ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>; overload;
class function Parallel.ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>; overload;
class function Parallel.ForEach<T>(const enumerable: IEnumerable<T>): IOmniParallelLoop<T>; overload; // [3.07.7]

// Blocking collections
class function Parallel.ForEach(const source: IOmniBlockingCollection): IOmniParallelLoop; overload;
class function Parallel.ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>; overload;

// Custom enumerator delegate
class function Parallel.ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop; overload;
class function Parallel.ForEach<T>(enumerator: TEnumeratorDelegate<T>): IOmniParallelLoop<T>; overload;

type
  TEnumeratorDelegate = reference to function(var next: TOmniValue): boolean;
  TEnumeratorDelegate<T> = reference to function(var next: T): boolean;

  TOmniAggregatorDelegate = reference to procedure(var aggregate: TOmniValue;
    const value: TOmniValue);
  TOmniIteratorDelegate = reference to procedure(const value: TOmniValue);
  TOmniIteratorDelegate<T> = reference to procedure(const value: T);
  TOmniIteratorStateDelegate = reference to procedure(
    const value: TOmniValue; var taskState: TOmniValue);
  TOmniTaskInitializerDelegate = reference to procedure(var taskState: TOmniValue);
  TOmniTaskFinalizerDelegate = reference to procedure(const taskState: TOmniValue);
  TOmniTaskStopDelegate = reference to procedure(const task: IOmniTask);

IOmniParallelLoop = interface
  function  Aggregate(defaultAggregateValue: TOmniValue;
    aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop;
  function  AggregateSum: IOmniParallelAggregatorLoop;
  procedure Execute(loopBody: TOmniIteratorDelegate); overload;
  function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
  function  Initialize(taskInitializer: TOmniTaskInitializerDelegate):
    IOmniParallelInitializedLoop;
  function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
  function  NoWait: IOmniParallelLoop;
  function  NumTasks(taskCount: integer): IOmniParallelLoop;
  function  OnStop(stopCode: TProc): IOmniParallelLoop; overload;
  function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop; overload;
  function  OnStopInvoke(stopCode: TProc): IOmniParallelLoop;   // [3.07.2]
  function  PreserveOrder: IOmniParallelLoop;
  function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop;
end;
```

**Default NumTasks:** `NumberOfCores - 1` with `NoWait` or `PreserveOrder`; `NumberOfCores` otherwise (a negative `NumTasks` never gives fewer than 1 task [3.07.10]).

**CRITICAL — NoWait lifecycle pattern:**
```pascal
var loop: IOmniParallelLoop<integer>;
loop := Parallel.ForEach(1, N).NoWait;
loop.OnStopInvoke(
  procedure
  begin
    loop := nil;  // destroy after all workers done
  end);
loop.Execute(
  procedure (const value: integer)
  begin ... end);
```

**Aggregation:**
```pascal
// Count primes (thread-safe, no locking needed):
numPrimes := Parallel.ForEach(1, CMaxPrime)
  .AggregateSum
  .Execute(
    procedure (const value: integer; var result: TOmniValue)
    begin
      if IsPrime(value) then result := 1;
    end);
```

**PreserveOrder (output ordered like input):**
```pascal
Parallel.ForEach(1, CMaxPrime)
  .PreserveOrder
  .Into(primeQueue)
  .Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin if IsPrime(value) then res := value; end);
```

**Task initialization/finalization:**
```pascal
Parallel.ForEach(1, CHighPrime)
  .Initialize(
    procedure (var taskState: TOmniValue)
    begin taskState.AsInteger := 0; end)
  .Finalize(
    procedure (const taskState: TOmniValue)
    begin
      lockNum.Acquire;
      try numPrimes := numPrimes + taskState.AsInteger;
      finally lockNum.Release; end;
    end)
  .Execute(
    procedure (const value: integer; var taskState: TOmniValue)
    begin if IsPrime(value) then taskState.AsInteger := taskState.AsInteger + 1; end);
```

**NoWait over a blocking collection** [3.09]: the loop keeps the source of `ForEach(IOmniValueEnumerable/IOmniBlockingCollection)` alive until all its tasks stop (before, releasing the last reference to the collection early could hang the loop).

**Exception handling** [3.09]: exceptions raised in the loop body (also `Into`/`Aggregate` variants, initializers and finalizers) are collected; after all tasks stop they are re-raised from `Execute` in the calling thread as `EJoinException` (same as `Join` and `For`). After a failure the tasks stop taking new elements. Not raised with `NoWait` - catch inside the body. If a worker task cannot be created/started the error is raised from `Execute` instead of hanging. Before 3.09 there was no handling and the body had to be wrapped in `try/except` (which still works).

Also fixed in 3.09: `ForEach<TOmniValue>` (`CastTo<TOmniValue>` used to raise "cannot be converted to record") and enumerables that return objects (for example `TListView.Items`, via `TOmniValue.AsTValue`).

---

### Parallel.ForkJoin

Divide-and-conquer framework.

```pascal
class function Parallel.ForkJoin: IOmniForkJoin; overload;
class function Parallel.ForkJoin<T>: IOmniForkJoin<T>; overload;

type
  TOmniForkJoinDelegate = reference to procedure(const compute: IOmniCompute);
  TOmniForkJoinDelegate<T> = reference to function: T;

IOmniForkJoin<T> = interface
  function  Compute(action: TOmniForkJoinDelegate<T>): IOmniCompute<T>;
  function  NumTasks(numTasks: integer): IOmniForkJoin<T>;
  function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin<T>;
end;

IOmniCompute<T> = interface
  procedure Execute;
  function  IsDone: boolean;
  function  TryValue(timeout_ms: cardinal; var value: T): boolean;
  function  Value: T;
end;
```

**Default NumTasks:** Number of cores available to the process (a negative `NumTasks` never gives fewer than 1 task [3.07.10]).

**Caution:** Uses lots of stack space — increase Maximum Stack Size in project options.

**Exception handling:** No built-in. Always catch/handle exceptions inside the `Compute` action.

---

### Parallel.Map

Parallel transformation of an array (Delphi XE+ only).

```pascal
type TMapProc<T1,T2> = reference to function(const source: T1; var target: T2): boolean;

class function Parallel.Map<T1,T2>: IOmniParallelMapper<T1,T2>; overload;
class function Parallel.Map<T1,T2>(const source: TArray<T1>;
  mapper: TMapProc<T1,T2>): TArray<T2>; overload;   // shorthand

IOmniParallelMapper<T1,T2> = interface
  function  Execute(mapper: TMapProc<T1,T2>): IOmniParallelMapper<T1,T2>;
  function  NoWait: IOmniParallelMapper<T1,T2>;
  function  NumTasks(numTasks: integer): IOmniParallelMapper<T1,T2>;
  function  OnStopInvoke(stopCode: TProc): IOmniParallelMapper<T1,T2>;   // [3.07.2]
  function  Result: TArray<T2>;
  function  Source(const data: TArray<T1>; makeCopy: boolean = false):
    IOmniParallelMapper<T1,T2>;
  function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelMapper<T1,T2>;
  function  WaitFor(maxWait_ms: cardinal): boolean;
end;
```

Output order is preserved — results correspond to input positions.

**Example:**
```pascal
odds := Parallel.Map<integer,string>(numbers,
  function (const source: integer; var dest: string): boolean
  begin
    Result := Odd(source);
    if Result then dest := IntToStr(source);
  end);
```

---

### Parallel.TimedTask

Threaded timer — executes code at regular intervals in a background thread.

```pascal
class function Parallel.TimedTask(const threadName: string = ''): IOmniTimedTask;
// threadName [3.07.10]: name of the timer thread (shown in the debugger); default 'Timed task'

IOmniTimedTask = interface
  function  Every(interval_ms: integer): IOmniTimedTask;
  function  Execute(const aTask: TProc): IOmniTimedTask; overload;
  function  Execute(const aTask: TOmniTaskDelegate): IOmniTimedTask; overload;
  procedure ExecuteNow;    // execute immediately, reset timer
  procedure Start;
  procedure Stop;
  function  TaskConfig(const config: IOmniTaskConfig): IOmniTimedTask;
  function  Terminate(maxWait_ms: cardinal): boolean;
  function  WaitFor(maxWait_ms: cardinal): boolean;
  property Active: boolean;
  property Interval: integer;
end;
```

**Notes:**
- Timer starts automatically when `Execute` is called if `Interval` is already set.
- Setting `Interval := 0` or negative disables the timer.
- Setting the interval resets the timer countdown.
- `Active := true` ≡ `Start`; `Active := false` ≡ `Stop`.
- Setting `IOmniTimedTask` to nil calls `Terminate(INFINITE)` automatically.

---

### Parallel.Channel\<T\> [3.09]

Typed, bounded queue connecting producers and consumers (modelled on Go channels; backported from OmniThreadLibrary-NG). **Delphi XE+ only.** `Channel`, `Select`, `Merge` and `Race` are all in `OtlParallel`.

```pascal
type
  IOmniChannel<T> = interface
    function  Sender: IOmniChannelSender<T>;
    function  Receiver: IOmniChannelReceiver<T>;
    procedure Close;
  end;

  IOmniChannelSender<T> = interface
    procedure Send(const value: T);                       // waits while the channel is full
    function  TrySend(const value: T; timeout_ms: cardinal = 0): boolean;
    procedure Close;
    function  IsClosed: boolean;
  end;

  IOmniChannelReceiver<T> = interface
    function  Receive: T;                                 // waits while the channel is empty
    function  TryReceive(out value: T; timeout_ms: cardinal = 0): boolean;  // INFINITE allowed
    function  IsClosed: boolean;
    function  IsEmpty: boolean;
    property  Count: integer read GetCount;
  end;

  Parallel = class
    class function Channel<T>(capacity: integer = 128): IOmniChannel<T>;
  end;
```

- `capacity` = max number of waiting values; zero or negative = unlimited.
- Pass only `Sender` to producers and only `Receiver` to consumers so the compiler enforces the direction.
- `Send` on a closed channel raises `ECollectionCompleted`; `TrySend` returns `False` (timeout, or closed).
- `Close` = no more data will be sent; values already in the channel can still be received. `IsClosed` is `True` after `Close` even if data remains.
- `Receive` raises `ECollectionCompleted` when the channel is closed and empty. `TryReceive` returns `False` on timeout or closed+empty.
- A small capacity throttles a fast producer (like blocking-collection throttling).

```pascal
channel := Parallel.Channel<integer>(10);
Parallel.Async(
  procedure
  var i: integer;
  begin
    for i := 1 to 100 do channel.Sender.Send(i);
    channel.Sender.Close;
  end);
while channel.Receiver.TryReceive(value, INFINITE) do
  Log('Received %d', [value]);
```

---

### Parallel.Select [3.09]

Waits on several channels at once and runs the handler of the first case that has data. **Delphi XE+ only.**

```pascal
type
  TOmniSelectResult = (srHandled, srTimeout, srAllClosed, srDefault);

  IOmniSelect = interface
    function Wait(timeout_ms: cardinal = INFINITE): TOmniSelectResult;
  end;

  Parallel = class
    class function Select(const cases: array of IOmniSelectCase): IOmniSelect;
  end;

  SelectCase = record
    class function Receive<T>(const receiver: IOmniChannelReceiver<T>;
      const handler: TProc<T>): IOmniSelectCase; static;
    class function Default(const handler: TProc): IOmniSelectCase; static;
  end;
```

- `Wait` removes one value from a channel that has data, calls its handler **in the thread that called `Wait`** and returns `srHandled`.
- If several channels have data they are served round-robin (the search starts after the case handled last), so a busy channel cannot starve the others.
- Results: `srHandled` (a handler ran), `srTimeout` (no data in `timeout_ms`), `srAllClosed` (all channels closed and empty, or no cases), `srDefault` (no data and the default handler ran).
- With a `SelectCase.Default` case `Wait` never blocks.

```pascal
select := Parallel.Select([
  SelectCase.Receive<integer>(numbers.Receiver,
    procedure (value: integer) begin Log('Number: %d', [value]); end),
  SelectCase.Receive<string>(names.Receiver,
    procedure (value: string) begin Log('Name: %s', [value]); end)
]);
while select.Wait = srHandled do
  ;   // ends when both channels are closed and empty (srAllClosed)
```

---

### Parallel.Merge\<T\> [3.09]

Combines several channels of the same type into one. **Delphi XE+ only.**

```pascal
class function Parallel.Merge<T>(const receivers: array of IOmniChannelReceiver<T>;
  capacity: integer = 128): IOmniChannelReceiver<T>;
```

- A background thread (a Select loop) forwards values to a new output channel; only its receiving end is returned.
- The output channel is closed when all inputs are closed and their data has been forwarded.
- `capacity` is the capacity of the output channel. Order across inputs is undefined; order within one input is preserved.

```pascal
merged := Parallel.Merge<integer>([channel1.Receiver, channel2.Receiver]);
while merged.TryReceive(value, INFINITE) do
  Log('Received %d', [value]);
```

---

### Parallel.Race\<T\> [3.09]

Returns the first value that appears in any of several channels (for example, ask several servers and take the quickest answer). **Delphi XE+ only.**

```pascal
type
  ESelectTimeout = class(Exception);

class function Parallel.Race<T>(const receivers: array of IOmniChannelReceiver<T>;
  timeout_ms: cardinal = INFINITE): T;
class function Parallel.TryRace<T>(const receivers: array of IOmniChannelReceiver<T>;
  out value: T; timeout_ms: cardinal = INFINITE): boolean;
```

- `Race` raises `ESelectTimeout` if nothing arrives in `timeout_ms` or all channels close first; `TryRace` returns `False` instead.
- Only one value is taken; later values stay in their channels.

```pascal
if not Parallel.TryRace<string>(channels, Result, 5000) then
  Result := '(no answer)';
```
