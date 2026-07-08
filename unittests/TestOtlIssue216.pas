///<summary>Regression test for issue #216 - data corruption caused by an ABA race on the
///   task's TerminatedEvent handle.
///
///   At the end of TOmniTask.InternalExecute the worker thread captures the TerminatedEvent
///   handle, releases MonitorLock, and only then calls SetEvent(eventTerminate). For a
///   *pooled* task (Schedule / Parallel.*), otcThread is nil, so TOmniTaskControl.Destroy
///   does not join the worker thread before it closes the handle. Destroy closes
///   TerminatedEvent while holding MonitorLock; because the worker has already released that
///   lock, Destroy (running on another thread) can close the handle inside the
///   (release MonitorLock .. SetEvent) window, leaving the worker to SetEvent a handle that
///   has already been closed - and possibly recycled by the OS for an unrelated object.
///
///   The test uses the OTL_RaceTest hook (GOtlRaceTestHook, compiled into OtlTaskControl
///   only when OTL_RaceTest is defined) to stop the worker thread inside that window. A
///   separate "closer" thread then reproduces exactly what Destroy does to the handle -
///   acquire MonitorLock, CloseHandle(TerminatedEvent) - and the hook checks whether the
///   handle is still valid at the point SetEvent is about to be called.
///
///   Why the closer models Destroy rather than calling it: while the task is terminating the
///   task controller is still held by several internal OTL references, so it cannot be
///   destroyed on demand from the test. The closer performs the identical lock+close
///   sequence Destroy performs (OtlTaskControl.pas, TOmniTaskControl.Destroy), so it is an
///   accurate proxy and, crucially, contends on the *same* MonitorLock the fix relies on.
///
///   Buggy code (SetEvent after MonitorLock is released): the hook runs with the lock free,
///   the closer closes the handle, and the probe sees an invalid handle -> test FAILS.
///   Fixed code (SetEvent while MonitorLock is still held): the hook runs with the lock held,
///   the closer blocks on MonitorLock.Acquire and cannot close the handle in time, the probe
///   sees a valid handle -> test PASSES.</summary>
///<author>Primoz Gabrijelcic</author>
///<remarks><para>
///   Creation date     : 2026-07-08
///   Last modification : 2026-07-08
///   Version           : 1.00
///</para></remarks>
unit TestOtlIssue216;

interface

{$I OtlOptions.inc}

{$IFDEF OTL_RaceTest}
uses
  TestFramework;

type
  TTestIssue216 = class(TTestCase)
  strict private
    FTriggerClose    : THandle;   // hook (worker thread) -> closer: "close the handle now"
    FCloseDone       : THandle;   // closer -> hook: "handle closed"
    FProbeDone       : THandle;   // hook -> test: "probe finished, safe to check results"
    FExpectedHandle  : THandle;   // the TerminatedEvent handle under observation
    FHookFired       : boolean;
    FHandleWasInvalid: boolean;   // oracle: was the handle already closed at :SetEvent?
  published
    procedure TestTerminatedEventValidAtSetEvent;
  end;
{$ENDIF OTL_RaceTest}

implementation

{$IFDEF OTL_RaceTest}
uses
  Windows,
  Classes,
  OtlSync,
  OtlTask,
  OtlTaskControl;

type
  ///<summary>Reproduces, on a separate thread, exactly what TOmniTaskControl.Destroy does to
  ///   the TerminatedEvent handle: acquire MonitorLock, then CloseHandle. On buggy OTL the
  ///   Acquire succeeds immediately (worker already released the lock); on fixed OTL it blocks
  ///   until the worker signals the event and releases the lock.</summary>
  TCloserThread = class(TThread)
  strict private
    FSharedInfo: TOmniSharedTaskInfo;
    FTrigger   : THandle;
    FDone      : THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(sharedInfo: TOmniSharedTaskInfo; trigger, done: THandle);
  end;

constructor TCloserThread.Create(sharedInfo: TOmniSharedTaskInfo; trigger, done: THandle);
begin
  FSharedInfo := sharedInfo;
  FTrigger := trigger;
  FDone := done;
  inherited Create(false);
end; { TCloserThread.Create }

procedure TCloserThread.Execute;
begin
  if WaitForSingleObject(FTrigger, 5000) <> WAIT_OBJECT_0 then
    Exit;
  // Mirror TOmniTaskControl.Destroy: close TerminatedEvent while holding MonitorLock.
  FSharedInfo.MonitorLock.Acquire;
  try
    if FSharedInfo.TerminatedEvent <> 0 then begin
      CloseHandle(FSharedInfo.TerminatedEvent);
      FSharedInfo.TerminatedEvent := 0; // prevent the real Destroy from double-closing
    end;
  finally FSharedInfo.MonitorLock.Release; end;
  SetEvent(FDone);
end; { TCloserThread.Execute }

{ empty task body - all we need is for the task to reach InternalExecute teardown }
procedure EmptyTaskBody(const task: IOmniTask);
begin
end; { EmptyTaskBody }

procedure TTestIssue216.TestTerminatedEventValidAtSetEvent;
var
  closer : TCloserThread;
  taskCtl: IOmniTaskControl;
begin
  FHookFired := false;
  FHandleWasInvalid := false;
  FTriggerClose := CreateEvent(nil, true, false, nil);
  FCloseDone := CreateEvent(nil, true, false, nil);
  FProbeDone := CreateEvent(nil, true, false, nil);
  closer := nil;
  try
    taskCtl := CreateTask(EmptyTaskBody, 'Issue216');
    FExpectedHandle := (taskCtl as IOmniTaskControlInternals).TerminatedEvent;

    closer := TCloserThread.Create(
      (taskCtl as IOmniTaskControlSharedInfo).SharedInfo, FTriggerClose, FCloseDone);

    GOtlRaceTestHook :=
      procedure (const eventTerminate: TOmniTransitionEvent)
      var
        flags: DWORD;
      begin
        // Fired on the (pooled) worker thread, in TOmniTask.InternalExecute, right before
        // SetEvent(eventTerminate). Ignore unrelated pool tasks.
        if eventTerminate <> FExpectedHandle then
          Exit;
        FHookFired := true;
        // Let the closer thread reproduce Destroy's "acquire MonitorLock, CloseHandle" here.
        SetEvent(FTriggerClose);
        // Bounded wait: on FIXED OTL the closer is blocked on MonitorLock (the lock is still
        // held here) and never signals, so we time out and observe a still-valid handle
        // instead of deadlocking.
        WaitForSingleObject(FCloseDone, 2000);
        // Oracle: is the handle we are about to SetEvent on still valid?
        if not GetHandleInformation(eventTerminate, flags) then
          FHandleWasInvalid := (GetLastError = ERROR_INVALID_HANDLE);
        SetEvent(FProbeDone);
      end;

    taskCtl.Schedule; // pooled task -> otcThread stays nil (the configuration that fails)

    CheckEquals(WAIT_OBJECT_0, WaitForSingleObject(FProbeDone, 10000),
      'Task did not reach the pre-SetEvent hook / probe did not complete');
    closer.WaitFor; // on fixed OTL the closer completes only after the worker releases the lock

    Check(FHookFired, 'Race-test hook did not fire - test is not exercising the code path');
    Check(not FHandleWasInvalid,
      'Issue #216: TerminatedEvent was closed (CloseHandle) before SetEvent - the handle ' +
      'was already invalid at the point TOmniTask.InternalExecute signals it.');
  finally
    GOtlRaceTestHook := nil;
    if assigned(closer) then begin
      SetEvent(FTriggerClose); // in case the hook never fired, release the closer's wait
      closer.WaitFor;
      closer.Free;
    end;
    CloseHandle(FTriggerClose);
    CloseHandle(FCloseDone);
    CloseHandle(FProbeDone);
  end;
end; { TTestIssue216.TestTerminatedEventValidAtSetEvent }

initialization
  RegisterTest(TTestIssue216.Suite);
{$ENDIF OTL_RaceTest}
end.
