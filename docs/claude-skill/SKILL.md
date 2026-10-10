---
name: otl-reference
description: |
  OmniThreadLibrary (OTL) API reference, extracted from "Parallel Programming with OmniThreadLibrary" by Primož Gabrijelčič.
  Use this skill when working with OTL code — looking up API signatures, usage patterns, choosing abstractions,
  or understanding threading patterns. Trigger on any question about Parallel.*, CreateTask, TOmniWorker,
  IOmniTaskControl, IOmniTask, synchronization primitives (TOmniCS, IOmniCriticalSection, etc.),
  blocking collections, pipelines, ForEach, Future, Join, or other OTL types.
---

# OmniThreadLibrary (OTL) Reference

**Source:** "Parallel Programming with OmniThreadLibrary" by Primož Gabrijelčič (covers OTL 3.09)
**Library:** Windows-only (32-bit and 64-bit; VCL, service and console applications; no FireMonkey, no FreePascal). Delphi 2007 or newer (up to and including Delphi 13 Florence). The high-level API requires Delphi 2009+; Delphi XE or newer is recommended (some parts do not work in 2009/2010 due to compiler bugs). Channel/Select/Merge/Race require Delphi XE+. Non-Windows support was removed in 3.09 (see OmniThreadLibrary-NG).
**License:** BSD — free for any use.
**Version tags:** `[3.07.2]`, `[3.09]` etc. in the reference files mark the OTL release that introduced a feature.

---

## Units Overview

| Unit | Contents |
|------|----------|
| `OtlCollections` | `IOmniBlockingCollection`, `TOmniBlockingCollection` |
| `OtlComm` | `IOmniCommunicationEndpoint`, `TOmniTwoWayChannel`, `TOmniMessageQueue` |
| `OtlCommon` | `TOmniValue`, `TOmniRecord<T>`, `TOmniRecordWrapper<T>`, `TOmniWaitableValue`, `TOmniValueContainer`, `TOmniCounter`, `IOmniIntegerSet`, `Environment`, `TOmniAlignedInt32/64` |
| `OtlContainerObserver` | Container observer implementation |
| `OtlContainers` | Lock-free collections: bounded stack, bounded queue, dynamic queue |
| `OtlDataManager` | Backend support for ForEach |
| `OtlEventMonitor` | `TOmniEventMonitor` component |
| `OtlParallel` | All high-level abstractions: `Parallel.*` (including Channel, Select, Merge, Race [3.09]) |
| `OtlSync` | `IOmniCriticalSection`, `TOmniCS`, `TOmniMREW`, `TLightweightMREWEx`, `IOmniResourceCount`, `IOmniCancellationToken`, `Atomic<T>`, `Locked<T>`, `IOmniLockManager<K>`, `TWaitFor`, `TOmniSingleThreadUseChecker` |
| `OtlSync.Utils` | `TOmniSynchronizer<T>` — named events for multi-threaded tests (Delphi 2009+) [3.07.9] |
| `OtlTask` | `IOmniTask` interface |
| `OtlTaskControl` | `IOmniTaskControl` interface, low-level task implementation |
| `OtlThreadPool` | Thread pool implementation, `IOmniThreadPool` |

For the complete list of every unit in the library — including internal/support units (registration,
logging, benchmarking, platform compatibility, etc.) not covered above — see
[unit-index.md](unit-index.md).

---

## Reference files

Read the file for the area you need; each starts with a table of contents.

| File | Covers |
|------|--------|
| [tomnivalue.md](tomnivalue.md) | `TOmniValue` variant record: data access, safe conversion, type tests, generics, arrays, records, ownership, `TOmniValueObj`. |
| [high-level-api.md](high-level-api.md) | `IOmniTaskConfig` and every `Parallel.*` abstraction: Async, Async/Await, Future, Join, ParallelTask, BackgroundWorker, Pipeline, For (incl. Int64), ForEach, ForkJoin, Map, TimedTask, Channel, Select, Merge, Race. |
| [low-level-api.md](low-level-api.md) | `CreateTask`, `IOmniTaskControl` (incl. `DirectExecute`), `TOmniWorker` (incl. `EventInfo`), `IOmniTask`, task groups. |
| [synchronization.md](synchronization.md) | Critical sections, `TOmniCS`, `Locked<T>`, MREW, `TLightweightMREWEx`, cancellation tokens, waitable values, resource count, `Atomic<T>`, `TWaitFor`, lock manager, single-thread checker, `TOmniSynchronizer`. |
| [messaging.md](messaging.md) | Communication endpoints, `TOmniTwoWayChannel`, message-loop requirement. |
| [collections.md](collections.md) | `IOmniBlockingCollection`, lock-free stack and queues, container observers. |
| [thread-pool-misc.md](thread-pool-misc.md) | Thread pool; counters, aligned integers, `IOmniIntegerSet`, `Environment`, `TOmniEventMonitor`. |
| [unit-index.md](unit-index.md) | Every unit in the library, including internal/support units. |

---

## Patterns and Best Practices

### Choosing the Right Abstraction

| Use Case | Abstraction |
|----------|-------------|
| Fire and forget | `Parallel.Async` |
| Background + UI callback | `Async/Await` |
| Background result | `Parallel.Future<T>` |
| Run N tasks in parallel | `Parallel.Join` |
| Same code in N threads | `Parallel.ParallelTask` |
| Work queue / client-server | `Parallel.BackgroundWorker` |
| Multi-stage data processing | `Parallel.Pipeline` |
| Simple parallel for loop | `Parallel.For` (faster) |
| Complex loop with aggregation/ordering | `Parallel.ForEach` (more powerful) |
| Divide-and-conquer | `Parallel.ForkJoin` |
| Array transformation | `Parallel.Map` |
| Periodic background task | `Parallel.TimedTask` |
| Typed producer/consumer link with close semantics (Go-style) [3.09] | `Parallel.Channel<T>` |
| React to whichever of several channels has data first [3.09] | `Parallel.Select` |
| Combine several channels into one [3.09] | `Parallel.Merge<T>` |
| First answer from several sources wins [3.09] | `Parallel.Race<T>` / `TryRace<T>` |
| Full control, message-based | `TOmniWorker` + `CreateTask` |
| Simplest possible background task | `CreateTask` + procedure |

### NumTasks Conventions

Used across all high-level abstractions:
- Positive `N` → exactly N worker tasks
- Negative `-N` → `[available cores] - N` tasks, but never fewer than 1 [3.07.10]
- `0` → exception

### OnStop vs OnStopInvoke

- `OnStop(TProc)`: called from worker thread (be careful with UI)
- `OnStop(TOmniTaskStopDelegate)`: called from worker thread; use `task.Invoke(...)` for main-thread code
- `OnStopInvoke` [3.07.2]: automatically invokes in owner thread — simplest approach

### Lifecycle of Async Abstractions (NoWait)

When using `NoWait`, store the interface in a field — never a local variable:

```pascal
// CORRECT pattern using OnStopInvoke [3.07.2]:
FLoop := Parallel.ForEach(1, N).NoWait;
FLoop.OnStopInvoke(
  procedure
  begin
    FLoop := nil;  // executed in owner thread after all workers done
  end);
FLoop.Execute(procedure (const value: integer) begin ... end);

// WRONG — loop interface destroyed immediately:
Parallel.ForEach(1, N).NoWait.Execute(...);
```

### Join/ParallelTask with NoWait must be waited for

[3.09] Destroying a `Parallel.Join` or `Parallel.ParallelTask` started with `NoWait` without calling `WaitFor` (or `Terminate`) first raises an exception from the destructor.

### Compile-time options

- `OTL_OLDCPU` [3.09]: only relevant for 32-bit. No longer defined by default; the lock-free queues use SSE2 (`MoveDPtr`) and the `OtlContainers` initialization raises an exception at startup on a CPU without SSE2. Define `OTL_OLDCPU` in the project options for CPUs without SSE2.
- `DSiNoTimerResolution` [3.09]: stops `DSiWin32` from raising the system timer resolution to 1 ms (`timeBeginPeriod(1)`) for the life of the process; `Sleep(1)` is then rounded up to ~15.6 ms.
- `BCB` (C++Builder): some overloaded properties and `Release` methods get different names in the generated headers (`AsArrayItemByName`/`AsArrayItemOV`, `ItemByName`/`ItemOV`, `Leave` for `IOmniCriticalSection.Release`/`IOmniResourceCount.Release`).

### TOmniWorker Resource Management

**Always** allocate resources in `Initialize` and release in `Cleanup` — never in constructor/destructor:

```pascal
TMyWorker = class(TOmniWorker)
strict private
  FConnection: TDatabaseConnection;
protected
  function  Initialize: boolean; override;
  procedure Cleanup; override;
public
  procedure MsgProcessRequest(var msg: TOmniMessage); message MSG_PROCESS;
end;

function TMyWorker.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then
    FConnection := TDatabaseConnection.Create;
end;

procedure TMyWorker.Cleanup;
begin
  FreeAndNil(FConnection);
  inherited Cleanup;
end;
```

### Anonymous Method Closure Capture

**CRITICAL:** Anonymous methods capture variables **by reference**. In loops, all closures share the **same variable reference**.

```pascal
// BROKEN — all callbacks use last handler value:
for sub in subscriptions do begin
  handler := GetHandler(sub);
  QueueCallback(procedure begin handler(); end);  // BUG!
end;

// CORRECT — helper method captures by value:
function MakeCallback(handler: THandlerProc): TProc;
begin
  Result := procedure begin handler(); end;
end;

for sub in subscriptions do
  QueueCallback(MakeCallback(GetHandler(sub)));
```

**Inline variables do NOT fix this.** Only the helper method pattern works.

### Exception Handling Summary

| Abstraction | Exception Behavior |
|-------------|-------------------|
| `Async` | Re-raised in `OnTerminated` handler |
| `Future<T>` | Re-raised when `Value` is called; inspect via `FatalException`/`DetachException` |
| `Join` | All exceptions wrapped in `EJoinException`; re-raised in `WaitFor`/`Execute` |
| `ParallelTask` | Same as Join |
| `Pipeline` | Exceptions propagate through queues; intercept with `HandleExceptions` |
| `For` | [3.09] Collected, re-raised from `Execute` as `EJoinException` (not with `NoWait`); before 3.09 exceptions were lost |
| `ForEach` | [3.09] Collected, re-raised from `Execute` as `EJoinException` (not with `NoWait`); tasks stop taking input after a failure; before 3.09 use `try/except` in the loop body (still works) |
| `ForkJoin` | No built-in handling; use `try/except` in action |
| `BackgroundWorker` | Stored in `workItem.FatalException`; accessing `Result` re-raises |

### Parallel.For vs Parallel.ForEach

- `Parallel.For`: splits range into sequential blocks, one block per task. Faster. Has an `Int64` overload [3.09].
- `Parallel.ForEach`: work-stealing scheduler, aggregation, PreserveOrder, cancellation. More powerful but slower.

Use `Parallel.For` for CPU-bound loops over numeric ranges without ordering requirements.

### Combining OnMessage and OnTerminated (Low-Level)

```pascal
FTask := CreateTask(TMyWorker.Create)
  .OnMessage(WM_RESULT,
    procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
    begin
      ProcessResult(msg.MsgData.AsString);
    end)
  .OnTerminated(
    procedure
    begin
      btnStart.Enabled := true;
      FTask := nil;
    end)
  .Run;
```
