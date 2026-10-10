# OTL reference: Synchronization Primitives

Critical sections, `TOmniCS`, `Locked<T>`, MREW, `TLightweightMREWEx`, cancellation tokens, waitable values, resource count, `Atomic<T>`, `TWaitFor`, lock manager, single-thread checker, `TOmniSynchronizer`.

## Contents

- IOmniCriticalSection
- TOmniCS
- Locked<T>
- TOmniMREW
- TLightweightMREWEx [3.08]
- IOmniCancellationToken
- IOmniWaitableValue / TOmniWaitableValue
- IOmniResourceCount (Inverse Semaphore / Countdown)
- Atomic<T> — Optimistic Initialization
- TWaitFor
- TOmniLockManager<K>
- TOmniSingleThreadUseChecker
- TOmniSynchronizer<T> (OtlSync.Utils)


All in `OtlSync` unit (except `TOmniSynchronizer`, in `OtlSync.Utils`).

### IOmniCriticalSection

Interface-based critical section (auto-destroyed via reference counting):

```pascal
IOmniCriticalSection = interface
  procedure Acquire;
  procedure Release;
  function  GetSyncObj: TSynchroObject;
  property LockCount: integer;
end;

function CreateOmniCriticalSection: IOmniCriticalSection;
```

Reentrant. `LockCount` tracks acquisition depth (useful for debugging).

C++Builder (`BCB` defined) [3.09]: C++ sees `IOmniCriticalSection.Release` as `Leave` (it would clash with `IUnknown::Release`); the same applies to `IOmniResourceCount.Release`. The Delphi interfaces are unchanged.

### TOmniCS

Record-based critical section (zero-initialization safe, lazy init on first `Acquire`):

```pascal
TOmniCS = record
  procedure Initialize;
  procedure Acquire; inline;
  procedure Release; inline;
  property LockCount: integer;
  property SyncObj: TSynchroObject;
end;
```

```pascal
var lock: TOmniCS;
lock.Acquire;
try ... finally lock.Release; end;
```

### Locked\<T\>

Combines a value with a critical section for declarative thread-safe access:

```pascal
Locked<T> = record
  type TFactory = reference to function: T;
  type TProcT = reference to procedure(const value: T);

  constructor Create(const value: T; ownsObject: boolean = true);
  class operator Implicit(const value: Locked<T>): T; inline;
  class operator Implicit(const value: T): Locked<T>; inline;
  function  Initialize(factory: TFactory): T; overload;
  procedure Acquire; inline;
  procedure Release; inline;
  function  Enter: T; inline;                     // [3.08] Acquire that returns the value
  procedure Leave; inline;                        // [3.08]
  procedure Locked(proc: TProc); overload; inline;
  procedure Locked(proc: TProcT); overload; inline;
  function  BeginRead: T; inline;                 // [3.08] shared lock, Delphi 11+ only
  procedure EndRead; inline;
  function  TryBeginRead: boolean; inline;
  function  BeginWrite: T; inline;                // [3.08] exclusive lock, Delphi 11+ only
  procedure EndWrite; inline;
  function  TryBeginWrite: boolean;
  procedure Free;
  property IsInitialized: boolean read FInitialized;   // [3.08]
  property Value: T read GetValue;
end;
```

On Delphi 11+ `Locked<T>` uses a `TLightweightMREWEx` lock instead of a critical section: `Acquire`, `Enter` and `BeginWrite` take the exclusive (write) lock (reentrant, so repeated `Acquire` from one thread still works), `BeginRead` the shared one (several readers at once). Older Delphis still use a critical section and have no read/write methods.

```pascal
var lockedIntf: Locked<IGpIntegerList>;
lockedIntf := TGpIntegerList.CreateInterface;
lockedIntf.Acquire;
try ProcessList(lockedIntf);
finally lockedIntf.Release; end;

// Or: execute while locked
lockedObj.Locked(procedure begin ... end);
lockedObj.Locked(procedure(const value: TMyClass) begin value.DoSomething; end);

// Pessimistic initialization:
function GetSharedObj: TMyObj;
begin
  Result := lockedObj.Initialize(function: TMyObj begin Result := TMyObj.Create; end);
end;
```

### TOmniMREW

Lightweight multiple-readers/exclusive-writer lock (busy-wait, no OS primitives):

```pascal
TOmniMREW = record
  procedure EnterReadLock; inline;
  procedure EnterWriteLock; inline;
  procedure ExitReadLock; inline;
  procedure ExitWriteLock; inline;
  function  TryEnterReadLock(timeout_ms: integer = 0): boolean;   // [3.07.6]
  function  TryEnterWriteLock(timeout_ms: integer = 0): boolean;  // [3.07.6]
end;
```

**Caution:** Busy-waits (spin-loops). Fast for short locks; do NOT hold for long durations.

Up to 3.07.7 `TryEnterReadLock`/`TryEnterWriteLock` wrongly returned `True` when the timeout expired; fixed in 3.07.8.

### TLightweightMREWEx [3.08]

Reentrant multiple-readers-exclusive-writer lock built on the RTL `TLightweightMREW` (Windows slim reader/writer lock). **Delphi 11 Alexandria+ only** (`OTL_HasLightweightMREW` defined).

```pascal
TLightweightMREWEx = record
  procedure BeginRead;
  function  TryBeginRead: boolean;
  procedure EndRead;
  procedure BeginWrite;
  function  TryBeginWrite: boolean;
  procedure EndWrite;
  property AllowReadInsideWrite: boolean read FAllowReadInsideWrite write SetAllowReadInsideWrite;
end;

ILightweightMREWEx   // same methods and property, by reference
TLightweightMREWExImpl = class(..., ILightweightMREWEx)   // wraps the record
```

- Write locks are reentrant (each `BeginWrite`/`TryBeginWrite` needs an `EndWrite`).
- Read locks are reentrant too and safe to nest even while a writer waits (a raw SRW lock deadlocks there); nested `BeginRead` only increments a counter.
- A thread owning the write lock must not call `BeginRead`/`TryBeginRead` (exception) unless `AllowReadInsideWrite := True` (set before first use, else exception); the read then succeeds as a no-op nested acquisition.
- Read → write upgrade is not possible: `BeginWrite`/`TryBeginWrite` while holding a read lock raises an exception.
- Misuse raises `TLightweightMREWEx.Method: reason` (`EndRead` without `BeginRead`, `EndWrite` by a non-owner, outermost `EndWrite` while an inner read lock is held).
- Do not copy/move the record while any lock is held (its address is its identity). Release all locks before a thread ends.

```pascal
lock := TLightweightMREWExImpl.Create;   // lock: ILightweightMREWEx
lock.BeginRead;
try
  lock.BeginRead;  // nested read OK
  lock.EndRead;
finally lock.EndRead; end;
```

### IOmniCancellationToken

Shared cancellation signal for multiple tasks:

```pascal
IOmniCancellationToken = interface
  procedure Clear;
  function  IsSignalled: boolean;
  procedure Signal;
  property Handle: THandle;  // can be waited on with WaitForSingleObject
end;

function CreateOmniCancellationToken: IOmniCancellationToken;
```

Tasks must cooperatively check `IsSignalled` and exit.

### IOmniWaitableValue / TOmniWaitableValue

Synchronous request/response between threads:

```pascal
IOmniWaitableValue = interface
  procedure Reset;
  procedure Signal; overload;
  procedure Signal(const data: TOmniValue); overload;
  function  WaitFor(maxWait_ms: cardinal = INFINITE): boolean;
  property Handle: THandle;
  property Value: TOmniValue;
end;

function CreateWaitableValue: IOmniWaitableValue; overload;
function CreateWaitableValue(const value: TOmniValue): IOmniWaitableValue; overload;   // [3.07.8]
// TOmniWaitableValue.Create(const value: TOmniValue) [3.07.8]
```

The constructor/overload with `value` sets the initial content of `Value`; the waitable value is still unsignalled, `value` is only what `Value` returns before `Signal(data)`.

```pascal
// Owner thread:
var res: TOmniWaitableValue;
res := TOmniWaitableValue.Create;
try
  task.Invoke(@TMyWorker.DoWork, [param, res]);
  res.WaitFor(INFINITE);
  processResult(res.Value);
finally FreeAndNil(res); end;

// Worker:
procedure TMyWorker.DoWork(const params: TOmniValue);
begin
  // do work...
  (params[1].AsObject as TOmniWaitableValue).Signal(result_value);
end;
```

### IOmniResourceCount (Inverse Semaphore / Countdown)

Signalled when count drops to zero:

```pascal
IOmniResourceCount = interface
  function  Allocate: cardinal;         // decrements; blocks if 0; returns new count
  function  Release: cardinal;          // increments; returns new count (C++Builder [3.09]: Leave)
  function  TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal = 0): boolean;
  property Handle: THandle;            // signalled when count = 0
end;

function CreateResourceCount(initialCount: integer): IOmniResourceCount;
```

### Atomic\<T\> — Optimistic Initialization

For interface and class types (Delphi 2010+ required for RTTI variant):

```pascal
Atomic<T> = class
  type TFactory = reference to function: T;
  class function Initialize(var storage: T; factory: TFactory): T; overload;
  class function Initialize(var storage: T): T; overload;  // OTL_ERTTI only
end;

// Two-parameter version [3.06] (Delphi XE+):
Atomic<I; T:constructor> = class
  class function Initialize(var storage: I): I;
end;
```

```pascal
function GetSharedObj: IMyInterface;
begin
  Result := Atomic<IMyInterface>.Initialize(FShared,
    function: IMyInterface begin Result := TMyImpl.Create; end);
end;

// Simpler two-type form:
Atomic<IMyInterface, TMyImpl>.Initialize(FShared);
```

### TWaitFor

Wait on more than 64 handles simultaneously:

```pascal
TWaitFor = class
  type TWaitResult = (waAwaited, waTimeout, waFailed, waIOCompletion);
  type THandleInfo = record Index: integer; end;
  type THandles = array of THandleInfo;

  constructor Create; overload;
  constructor Create(const handles: array of THandle); overload;
  function  MsgWaitAny(timeout_ms, wakeMask, flags: cardinal): TWaitResult;
  procedure SetHandles(const handles: array of THandle);
  function  WaitAll(timeout_ms: cardinal): TWaitResult;
  function  WaitAny(timeout_ms: cardinal; alertable: boolean = false): TWaitResult;
  property Signalled: THandles;
end;
```

```pascal
wf := TWaitFor.Create([handle1, handle2]);
try
  if wf.WaitAny(INFINITE) = waAwaited then
    for info in wf.Signalled do
      if info.Index = 0 then HandleEvent1
      else HandleEvent2;
finally FreeAndNil(wf); end;
```

### TOmniLockManager\<K\>

Lock any value by key (entity-level locking):

```pascal
IOmniLockManager<K> = interface
  function  Lock(const key: K; timeout_ms: cardinal): boolean;
  function  LockUnlock(const key: K; timeout_ms: cardinal): IOmniLockManagerAutoUnlock;
  procedure Unlock(const key: K);
end;
```

Reentrant, fair (FIFO). `LockUnlock` returns auto-unlock interface (released when out of scope).

### TOmniSingleThreadUseChecker

Debugging helper to enforce single-thread access:

```pascal
TOmniSingleThreadUseChecker = record
  procedure AttachToCurrentThread;
  procedure Check;        // always active
  procedure DebugCheck;   // active only if OTL_CheckThreadSafety defined
end;
```

### TOmniSynchronizer\<T\> (OtlSync.Utils) [3.07.9]

Named manual-reset events, mostly for multi-threaded unit tests. Unit `OtlSync.Utils`, **Delphi 2009+**. An event is created the first time its name is used, so `Signal` and `WaitFor` may come in any order.

```pascal
type
  IOmniSynchronizer<T> = interface
    function  Count: integer;                       // number of known events
    procedure Signal(const name: T);
    function  WaitFor(const name: T; timeout: cardinal = INFINITE): boolean;
  end;

  TOmniSynchronizer<T> = class(TInterfacedObject, IOmniSynchronizer<T>)
    constructor Create;
    procedure Reset(const name: T);                 // class only, not in the interface
    ...
  end;

  IOmniSynchronizer = IOmniSynchronizer<string>;
  TOmniSynchronizer = TOmniSynchronizer<string>;
```

```pascal
sync := TOmniSynchronizer.Create;
Parallel.Async(procedure begin {work} sync.Signal('work done'); end);
if not sync.WaitFor('work done', 5000) then
  raise Exception.Create('Background work did not complete in 5 seconds');
```

`T` can be any type usable as a dictionary key (for example an enumeration).
