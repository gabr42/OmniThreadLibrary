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
