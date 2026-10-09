# Issue repro tests

One folder per GitHub issue; each has a `run.bat` that builds the repro for Win32 and Win64 and
runs it (exit code 0 = pass, 1 = fail). A repro is expected to FAIL while the bug exists.
To run against OTL-NG, compile the same `.dpr` with `-U` and `-I` pointing to
`h:\RAZVOJ\OmniThreadLibrary-NG` (and its `FastMM4` subfolder).

## Batch 1 status

Fixed on branch `bugfix-batch1` (classic) and `bugfix-batch1` (NG): #224 (classic only), #201 (both),
#154 (classic only), #177 (both). #199 and #191 were already fixed; #72 is postponed. All repros pass
after the fixes. Classic DUnit suite (102 tests) and NG DUnitX suite (341 tests) pass.

Known limitation after the #154 fix: `Parallel.Async` (and any `OnMessage`/`OnTerminated` handler) still
cannot be combined with `MonitorWith`, because only the internal monitor dispatches per-task handlers.

| Issue | Classic OTL | Repro | OTL-NG |
|-------|-------------|-------|--------|
| #199 `kernel.dll` in `DynaLoadAPIs` | Already fixed (no `'kernel.dll'` left in `src\DSiWin32.pas`; `GInterlockedCompareExchange64` removed) | not needed | Not applicable - NG no longer uses DSiWin32 |
| #224 AV in `DSiTimeGetTime64` at shutdown | Applies: `DSiWin32` finalization deletes `GDSiTimeGetTime64Safe` while a surviving pool's maintenance timer can still call `DSiTimeGetTime64` | `224_ShutdownAV` - fails in `leak` mode (32/64-bit), `noleak` control passes | Not applicable - `OtlPlatform.Time` is a plain `TStopwatch` record, no critical section to destroy |
| #201 `TOmniValueQueue` AV on destroy | Applies: `FreeAndNil(FInnerQueue)` nils the field, then `TQueue.Destroy` fires `OnNotify` -> `CollectionNotifyEvent` reads `FInnerQueue.Count` | `201_ValueQueueDestroy` - fails (32/64-bit, CS and spinlock variants) | Applies - identical code, same repro fails |
| #154 `MonitorWith` in task config | Applies: `ApplyConfig` attaches the user monitor, then `Unobserved` attaches an internal one -> `SetMonitor` raises. Affects Async, Join, For, ForEach, Future, Pipeline. For/ForEach/Future hang and Join raises an AV (secondary effects, not yet analysed) | `154_TaskConfigMonitorWith` - all 6 cases fail | Not applicable - all 6 cases pass (NG `Unobserved` works differently) |
| #177 madExcept / alignment assertions | Applies: `TOmniBaseQueue.Initialize` asserts 16-byte alignment of `AllocMem` results but never guarantees it (bounded stack/queue align explicitly) | `177_QueueAlignment` - uses an allocator that returns 8-byte aligned blocks instead of madExcept; fails 32/64-bit | Applies - same assertions (line 1389/1390), repro fails |
| #191 compiler hints | All six cited sites already fixed; no hints from `OtlParallel` or `GpLists` with `-H+ -W+` (dcc32/dcc64) | not needed | No hints from those sites; unrelated H2077 in NG `OtlThreadPool.pas(1000)` |
| #72 C++Builder `.hpp` errors | Postponed until C++Builder is installed | | |

## Batch 2 status

| Issue | Classic OTL | Repro | OTL-NG |
|-------|-------------|-------|--------|
| #49 `ForEach<TOmniValue>` over a blocking collection with `PreserveOrder`/`Into` | The `GetNext` abstract error was already fixed, but `TOmniValue.CastTo<TOmniValue>` raised 'cannot be converted to record', so every `ForEach<TOmniValue>` body failed (and hung, see #57). Fixed in OtlCommon 1.56c. | `49_PreserveOrderCollection` | Same bug, fixed in OtlCommon 3.03 |
| #57 exception in `For`/`ForEach` body | `For` lost the exception, `ForEach` (also `Into`) hung. Exceptions are now collected and raised as `EJoinException` in the caller (OtlParallel 1.56). | `57_ForException` | Same bugs, fixed in OtlParallel 3.03 |
| #213 loop hangs when workers cannot be started | Destructor waited for workers that were never started. Fixed in OtlParallel 1.56a. | `213_AllocateHwndFails` (window quota exhaustion; `ForEachTaskCreateRaises` also runs on NG) | Same hang (reproduced with an `OnTaskCreate` hook that raises), fixed in OtlParallel 3.04; NG creates no window for the internal monitor |
| #24 AV hides the original error when the monitor window cannot be created | `TOmniEventMonitor.Destroy` touched the nil dictionaries. Fixed in OtlEventMonitor 1.11b. | `213_AllocateHwndFails` | Already fixed in NG |
| #60 AV with pool + monitor (x64) | Cannot reproduce on current code (32/64-bit, also with the pool being replaced while its task runs) | `60_PoolMonitor` (regression test) | not checked, same test applies |
| #40 pool stops scheduling during thread shrink | Cannot reproduce (heavy variant: 200 tasks, 60 threads, 1 s idle timeout, 25 rounds) | `40_PoolScheduleDuringShrink` (regression test) | not checked |
| #131 futures in a dynamic array | Cannot reproduce (reporter's program x 30 iterations) | `131_FutureArray` (regression test) | not checked |
| #38 AV in the HL-III pipeline demo | Not an OTL bug as far as I can tell: after Stop the demo's `WM_STOPPED` handler calls `btnStopClick` again, which dereferences `FSpider`, already set to nil | none | n/a |

Note found while testing #40: tasks started with `.Unobserved` are only released when the creating thread
pumps messages (the internal monitor is a window). A console program or a tight loop in the main thread
that never pumps messages accumulates ~600 KB per task. This is probably what is behind the open memory
issues #194, #78 and #69 (Batch 3).

## Batch 3 status (memory)

| Issue | Classic OTL | Repro | OTL-NG |
|-------|-------------|-------|--------|
| #78 nested `Parallel.Async`, #152 `Async` started from a task, #69 pipelines started from `Parallel.For` tasks | Real leak: tasks created in a non-main thread get an internal monitor window in that thread; nothing there processes messages, so finished tasks were never released (380 MB / 300 nested Asyncs, 1.4 GB for the pipeline case). Fixed in OtlTaskControl 1.43f: such a thread processes its monitor's pending messages whenever it creates the next internal monitor. | `69_UnobservedLeaks` (NestedAsync, NestedAsyncTask, NestedPipeline) | Not affected (uses background observers); all cases pass |
| #194 repeated `Parallel.For` in a button click, #200 `Parallel.ForEach` memory, #197 `Terminate(0)`/app_22 | Works as designed (documented: OTL cleans up through window messages, so the main thread must process messages; a console program must pump them). Memory is released as soon as messages are processed. #197 additionally leaks ~12 KB per thread killed by `TerminateThread`, which cannot be avoided. | `69_UnobservedLeaks` `ForLoopPumped` (messages pumped: flat), `ForLoopUnpumped` (grows by design) | Same: main thread must pump |
| #182 `ERROR_NOT_ENOUGH_QUOTA` | The observer gave up after ~5 s of a full receiver queue. Now waits up to 60 s. OtlContainerObserver 1.07. | `182_PostQuota` | No such code in NG |
| #183 bad `TWaiter` cast | `TWaitFor.Awaited_Asy` raced with `RegisterWaitHandles` growing the list; fixed by the #227 fix (74d8a30a), no new repro | existing `59_TWaitFor` test | `TWaitFor.TWaiter` does not exist in NG |

Observation (NG): repeated `Parallel.For` makes the NG thread pool grow to ~150 threads (classic: 16) on a 32-core
machine and keep them for 10+ s; memory use plateaus (~100 MB in a 32-bit process). Not a leak, but worth a look.

## Batch 4 status

| Issue | Classic OTL | Repro | OTL-NG |
|-------|-------------|-------|--------|
| #50 `ReceiveWait` swallows OTL's internal messages | Real bug: `Receive`/`ReceiveWait` returned internal messages (e.g. the queued `Execute` of `Run(@Method)`) to the user code. They are now put back into the queue; OTL reads with `IOmniCommunicationEndpointInternal.ReceiveAny`. OtlComm 1.14. | `50_ReceiveWaitInternal` (Run, Invoke, Receive) | Same bug, fixed in OtlComm 3.03 |
| #176 `ForEach` over objects | `TOmniValue.SetAsTValue` had no `tkClass` case. Fixed in OtlCommon 1.56d. | `176_ForEachObjects` | Same, fixed in OtlCommon 3.04 |
| #68 timer resolution 1 ms | DSiWin32 calls `timeBeginPeriod(1)` for the process lifetime. New define `DSiNoTimerResolution` skips it (default unchanged). DSiWin32 2.16c. | `68_TimerResolution` (checks the import, as the resolution is system wide) | Not affected |
| #173 Error 1400 on close | Cannot reproduce (timed tasks started from a pool thread, the pool destroyed before the tasks). The linked commit only improved the error text. Likely a thread-lifetime problem in the application: the task owner thread is gone when the task terminates. | `173_TimedTaskInAsync` (regression test) | Passes |
| #180 `Parallel.For` with `Int64` | Feature request, not a bug (`IOmniParallelSimpleLoop` is `integer` based throughout). Not implemented. | - | same |
| #165, #166 | #166: mixed builds, answered in the issue, no reply since 2021. #165: no repro and Delphi XE6 / OTL 3.05; not actionable. | - | - |

## NoWait/Into exit hang (found while testing #49)

`Parallel.ForEach(collection).PreserveOrder.NoWait.Into(queue)` could hang the process at exit. The loop's
enumerator referenced the input collection without owning it (`obceCollection_ref`), so a program that
released its last reference to the collection before the loop had finished left a worker waiting on a
destroyed collection. Reproduced in about 1 of 6 runs in classic and 2 of 15 in NG, 0 of 30 after the fix.
Fix: the loop keeps the source of `ForEach(IOmniValueEnumerable/IOmniBlockingCollection)` alive
(OtlParallel 1.56b / NG 3.05). Making the enumerator itself hold an interface reference was tried and rejected:
code that owns a `TOmniBlockingCollection` as an object (the unit tests do) would have the collection destroyed
when the enumerator is released. Test: `NoWaitIntoExit`.

## #72 C++Builder headers

Reproduced with C++Builder 13 (bcc32c / bcc64, Clang based): the generated `DSiWin32.hpp`, `OtlCommon.hpp` and
`OtlSync.hpp` did not compile (and `GpLists.hpp`, which `OtlSync.hpp` includes, had a further error).

| Problem | Fix |
|---------|-----|
| DSiWin32 constants (`FILE_ANY_ACCESS`, `THREAD_ALL_ACCESS`, `SC_MINIMIZE`, ... 25 of them) clash with Windows SDK macros | `{$EXTERNALSYM}` for them (DSiWin32 2.16d) |
| Overloaded indexed properties `TOmniValue.AsArrayItem` and `TOmniValueContainer.Item` ("duplicate member") | With `BCB` defined: `AsArrayItemByName`, `AsArrayItemOV`, `ItemByName`, `ItemOV` replace the overloads (OtlCommon 1.56e). Delphi API unchanged. Internal `Task.Param['x']` became `Task.Param.ByName('x')`. |
| `IOmniCriticalSection.Release` / `IOmniResourceCount.Release` conflict with `IUnknown::Release` | With `BCB` defined: `[HPPGEN]` makes the C++ header declare them as `Leave` (same vtable slot, so a C++ call to `Leave` runs the Delphi `Release`) (OtlSync 2.3) |
| `IGpMovingAverager<T>` has no GUID per instantiation, so the generated `GetInterface` operators in `GpLists.hpp` do not compile | `{$EXTERNALSYM}` for `TGpIntAverager`, `TGpUIntAverager`, `TGpFPAverager` (GpLists 1.89) |

Test `72_CppBuilder`: generates the headers with `dcc32/dcc64 -JPHNE -DBCB`, compiles every header on its own with
`bcc32c` and `bcc64`, then links and runs `UseOtl.cpp` (critical section via `Leave`, `ItemByName`, resource
count) against the generated objects. The same changes were applied to OTL-NG (OtlCommon 3.05, OtlSync 3.12,
OtlParallel 3.06); all its main units' headers compile.

## Batch 6

| Issue | Classic OTL | Repro | OTL-NG |
|-------|-------------|-------|--------|
| #180 `Parallel.For` does not support Int64 | Added `Parallel.For(first, last, step: Int64)`, Int64 loop bodies (`procedure(value: Int64)`, `taskIndex` and `task` variants) and Int64 initializers/finalizers; the loop range, partitions and step are Int64 internally. Integer overloads unchanged; an integer initializer/finalizer on a range that does not fit into an integer raises. OtlParallel 1.57 | `180_Int64For` | Same change, OtlParallel 3.07; DUnitX test `TestRegressions.TestForInt64` |
