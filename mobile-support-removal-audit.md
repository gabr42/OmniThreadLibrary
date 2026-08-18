# Audit: incorrect removals in commit e0546ab1 ("Remove POSIX/non-Windows support")

Related: #226

## Summary

Commit `e0546ab1` ("Remove POSIX/non-Windows support; OmniThreadLibrary is now
Windows-only", 2026-07-08) removed all code guarded by `{$IFDEF OTL_MobileSupport}`
on the assumption that this guard meant "non-Windows only". It did not.

`OtlOptions.inc` defined it as:

```pascal
{$IF CompilerVersion >= 29} //DXE8
  {$DEFINE OTL_MobileSupport}
{$IFEND}
```

No platform check — `OTL_MobileSupport` was set for any Delphi >= XE8, Windows
included. Code guarded by a *bare* `{$IFDEF OTL_MobileSupport}` (not nested
inside `{$IFNDEF MSWINDOWS}` / the `{$ELSE}` branch of an `{$IFDEF MSWINDOWS}`)
compiled fine on Windows and was deleted anyway.

Code nested the other way - inside `{$IFNDEF MSWINDOWS}` or the `{$ELSE}` of
`{$IFDEF MSWINDOWS}`, or guarded by `{$IFDEF POSIX}` / `{$IF defined(LINUX) or
defined(ANDROID)}` - was genuinely non-Windows-only and was correctly removed.

Full commit touched 14 core units; a text search for `OTL_MobileSupport` across
the whole diff shows it only appears (besides its own definition in
`OtlOptions.inc`) in three units: `OtlContainers.pas`, `OtlContainerObserver.pas`,
and `OtlSync.pas`. The other 11 touched units (`OtlCollections.pas`,
`OtlComm.pas`, `OtlCommon.Utils.pas`, `OtlCommon.pas`, `OtlDataManager.pas`,
`OtlEventMonitor.pas`, `OtlLogger.pas`, `OtlParallel.pas`, `OtlTask.pas`,
`OtlTaskControl.pas`, `OtlThreadPool.pas`) only had genuine `MSWINDOWS`-else /
`POSIX` / `LINUX`/`ANDROID` code removed - no misclassified Windows-compiling
code found there.

## Incorrectly removed (restore as unconditional Windows code)

### OtlContainers.pas - FIXED (this branch)

- `IOmniValueQueue` (interface)
- `TOmniValueQueue`, `TOmniValueQueueCS`, `TOmniValueQueueSpin` (classes)
- `CreateOmniValueQueue` (factory function)

Restored unconditionally; `Generics.Collections` added to the implementation
`uses` clause. Compiles clean, smoke-tested (enqueue/dequeue round trip).

### OtlSync.pas - NOT YET RESTORED

A parallel, cross-platform-flavoured synchronization layer that coexisted with
the classic Windows-handle-based primitives (`TOmniCriticalSection`,
`TOmniResourceCount`, `TWaitFor`, etc.) - not a replacement for them:

- Interfaces: `IOmniSynchroObserver`, `IOmniSynchro`, `IOmniSynchroObject`,
  `IOmniEvent`, `IOmniCountdownEvent`
- Classes: `TOmniSynchroObject`, `TSynchroSpin`, `TOmniCountdownEvent`,
  `TOmniEvent`
- Factory functions: `CreateOmniEvent`, `CreateOmniCountdownEvent`

Evidence this was meant to build on Windows: `TOmniSynchroObject.Handle` had
its own nested `{$IFDEF MSWINDOWS}` branch inside the otherwise-bare
`OTL_MobileSupport` block.

Diff locations (line numbers in `git show e0546ab1 -- OtlSync.pas`, old-file
side): interface declarations around old-file lines 216-306 (~2778-2844 in the
raw commit diff), class declarations around old-file lines ~640-720
(~3131-3233 in the raw diff, excluding the nested `{$IFNDEF MSWINDOWS}` bit for
`TOneCondition`/`TAllCondition`/`TPreSignalData`, which stays removed), forward
declarations near `CreateResourceCount`, and the method bodies immediately
before `{ TInterlockedEx }` in the implementation section.

### OtlContainerObserver.pas - NOT YET RESTORED

Depends on `IOmniEvent` from `OtlSync.pas` above:

- `TOmniContainerEventObserver` (class) and its implementation
  `TOmniContainerEventObserverImpl`
- `CreateContainerEventObserver` (factory function)
- `uses` additions: `System.SyncObjs`, `System.Generics.Collections`

## Correctly removed - verified genuinely non-Windows-only

- `TSynchroWaitFor` and its full implementation, `TOneCondition`,
  `TAllCondition`, `TPreSignalData` - all nested inside the `{$ELSE}` branch of
  `{$IFDEF MSWINDOWS}` (Windows already has its own `TWaitFor`).
- The mobile variant of `TOmniResourceCount` (`{$ELSE}{$IFDEF
  OTL_MobileSupport}` branch of the `IOmniResourceCount` class).
- `IOmniCancellationToken.Event`/`GetEvent` (the non-Windows `{$ELSE}` branch;
  Windows keeps `Handle`/`GetHandle`).
- The `{$IF defined(LINUX) or defined(ANDROID)}` `TryBeginRead`/`TryBeginWrite`
  timeout overloads on `TLightweightMREWEx` and friends.
- All removals in the other 11 touched units (genuine `MSWINDOWS`-else /
  `POSIX` code).

## Next step

Restore the OtlSync.pas / OtlContainerObserver.pas pieces listed above as
plain unconditional Windows code (same approach as `OtlContainers.pas`), then
recompile and smoke-test the sync primitives before closing #226.
