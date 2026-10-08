program Issue224;

{ Repro for GitHub issue #224: access violation in DSiTimeGetTime64 at shutdown.

  A thread pool that outlives the finalization of DSiWin32 keeps running its
  maintenance timer (TOTPWorker.MainteinanceTimer), which calls DSiTimeGetTime64
  while an idle worker exists. DSiWin32's finalization has already deleted
  GDSiTimeGetTime64Safe by then.

  Usage:  Issue224 leak     - pool is kept alive past unit finalization (expected FAIL while bug exists)
          Issue224 noleak   - pool is released normally (control, must PASS)

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Issue224.LateFinal, // must be first!
  Winapi.Windows,
  System.SysUtils,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool;

procedure NoOp(const task: IOmniTask);
begin
end; { NoOp }

var
  pool: IOmniThreadPool;
  leak: boolean;

begin
  leak := SameText(ParamStr(1), 'leak');
  pool := CreateThreadPool('Issue224');
  CreateTask(NoOp, 'NoOp').Unobserved.Schedule(pool);
  Sleep(500); // task finished, its worker is now idle => maintenance timer calls DSiTimeGetTime64
  if leak then
    pool._AddRef; // never released => pool and its timer survive unit finalization
  Writeln('Mode: ', ParamStr(1));
  pool := nil;
end.
