# OTL reference: Thread pool and miscellaneous

Thread pool; counters, aligned integers, `IOmniIntegerSet`, `Environment`, `TOmniEventMonitor`.

## Contents

- Thread Pool
- Miscellaneous
  - TOmniCounter / IOmniCounter
  - TOmniAlignedInt32 / TOmniAlignedInt64
  - IOmniIntegerSet
  - Environment Object
  - TOmniEventMonitor

## Thread Pool

```pascal
function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;
function GlobalOmniThreadPool: IOmniThreadPool;  // default low-level pool
function GlobalParallelPool: IOmniThreadPool;    // default high-level pool

IOmniThreadPool = interface
  function  Cancel(taskID: int64; timeout_ms: int64 = -1): boolean; overload;   // [3.07.9] timeout_ms overrides WaitOnTerminate_sec; -1 = use it
  function  Cancel(taskID: int64; signalCancellationToken: boolean;
    timeout_ms: int64 = -1): boolean; overload;
  procedure CancelAll; overload;
  procedure CancelAll(signalCancellationToken: boolean); overload;
  function  CountExecuting: integer;
  function  CountQueued: integer;
  function  IsIdle: boolean;
  function  MonitorWith(const monitor: IOmniThreadPoolMonitor): IOmniThreadPool;
  procedure SetThreadDataFactory(const value: TOTPThreadDataFactoryFunction); overload;
  property Affinity: IOmniIntegerSet;
  property IdleWorkerThreadTimeout_sec: integer;   // default 10; 0 = never terminate
  property MaxExecuting: integer;    // default = number of cores; 0 = stop pool; -1 = unlimited
  property MaxQueued: integer;       // default 0 = unlimited
  property MaxQueuedTime_sec: integer; // default 0 = unlimited
  property MinWorkers: integer;      // default 0; pre-creates threads
  property Name: string;
  property NumCores: integer;
  property WaitOnTerminate_sec: integer;  // default 30; before TerminateThread is called
end;
```

**Thread data factory:** Creates per-thread data accessible via `task.ThreadData`:
```pascal
pool.SetThreadDataFactory(
  function: IInterface
  begin
    Result := CreateDatabaseConnection as IInterface;
  end);

// In worker:
myConn := task.ThreadData as IMyDBConnection;
```

---

## Miscellaneous

### TOmniCounter / IOmniCounter

Thread-safe integer counter:

```pascal
IOmniCounter = interface
  function  Increment: integer;
  function  Decrement: integer;
  function  Take(count: integer): integer; overload;
  function  Take(count: integer; var taken: integer): boolean; overload;
  property Value: integer;
end;

TOmniCounter = record
  procedure Initialize;
  function  Increment: integer;
  function  Decrement: integer;
  property Value: integer;
end;

function CreateCounter(initialValue: integer = 0): IOmniCounter;
```

`Take(count)`: atomically decrements by `min(count, current_value)`, returns amount taken.

### TOmniAlignedInt32 / TOmniAlignedInt64

Atomically-accessible aligned integers [3.06]:

```pascal
TOmniAlignedInt32 = record
  procedure Initialize; inline;
  function  Add(value: integer): integer; inline;
  function  CAS(oldValue, newValue: integer): boolean;
  function  Decrement: integer; overload; inline;
  function  Increment: integer; overload; inline;
  function  Subtract(value: integer): integer; inline;
  property Value: integer;
end;

TOmniAlignedInt64 = record
  function CAS(oldValue, newValue: int64): boolean;
  property Value: int64;
end;
```

### IOmniIntegerSet

Set of non-negative integers (more than 256 elements, unlike a Delphi `set`; wasteful `TArray<boolean>` storage, so not for large values; created for processor affinity). `Add`, `Assign`, `Clear`, `Contains`, `Count`, `IsEmpty`, `Remove`, `Item[idx]`, `OnChange`; `AsArray`, `AsBits`, `AsIntArray`, `AsMask: uint64` (only if all values < 64). `AsMask` (and the `TOmniGroupAffinity.Create` affinity mask) changed from `int64` to `uint64` in 3.07.9 so a mask with the top bit set works.

### Environment Object

Global singleton for system/process/thread info:

```pascal
function Environment: IOmniEnvironment;

IOmniEnvironment = interface
  property Process: IOmniProcessEnvironment;
  property System: IOmniSystemEnvironment;
  property Thread: IOmniThreadEnvironment;
end;

IOmniAffinity = interface
  property AsString: string;        // settable
  property Count: integer;          // settable
  property CountPhysical: integer;  // physical cores only
  property Mask: DSiNativeUInt;     // settable
end;
```

```pascal
// Force process to use only 2 cores:
Environment.Process.Affinity.Count := 2;
```

### TOmniEventMonitor

VCL component for monitoring tasks and thread pools:

```pascal
TOmniEventMonitor = class(TComponent, IOmniTaskControlMonitor, IOmniThreadPoolMonitor)
published
  property OnTaskMessage: TOmniMonitorTaskMessageEvent;
  property OnTaskTerminated: TOmniMonitorTaskEvent;
  property OnTaskUndeliveredMessage: TOmniMonitorTaskMessageEvent;
  property OnPoolThreadCreated: TOmniMonitorPoolThreadEvent;
  property OnPoolThreadDestroying: TOmniMonitorPoolThreadEvent;
  property OnPoolThreadKilled: TOmniMonitorPoolThreadEvent;
  property OnPoolWorkItemCompleted: TOmniMonitorPoolWorkItemEvent;
end;
```

Attach to task: `task.MonitorWith(eventMonitor)`. Attach to pool: `pool.MonitorWith(eventMonitor)`.
