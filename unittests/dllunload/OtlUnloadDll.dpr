library OtlUnloadDll;

// Used by UnloadHost; see dllunload.bat.

uses
  SysUtils,
  OtlTask, OtlTaskControl, OtlThreadPool, OtlParallel,
  UnloadOwner in 'UnloadOwner.pas';

const
  CPrivatePool = 1;
  CIdle        = 2;
  CLeaked      = 4; // pool is never released
  CParallel    = 8;
  CThreads     = 16; // tasks in their own threads, released in finalization
  CStuck       = 32; // task ignores cancellation
  CIdleTimeout = 64; // workers are being removed from the pool when the DLL is unloaded
  COwnDllProc  = 128; // DLL replaces DllProc
  COwned       = 512; // pool is released in the finalization of a unit
  CInit        = 256; // OTL is used in the DLL initialization

var
  GTask: IOmniTaskControl;

procedure Go(mode: integer); stdcall;
var
  i   : integer;
  pool: IOmniThreadPool;
begin
  if mode and CPrivatePool = 0 then
    pool := GlobalOmniThreadPool
  else begin
    pool := OtlThreadPool.CreateThreadPool('dll');
    if mode and CLeaked <> 0 then
      pool._AddRef;
  end;
  if mode and COwned <> 0 then
    GOwnedPool := pool;
  if mode and CIdleTimeout <> 0 then
    pool.IdleWorkerThreadTimeout_sec := 1;
  if mode and CParallel <> 0 then
    Parallel.&For(1, 1000).Execute(procedure (i: integer) begin end);
  if mode and CThreads <> 0 then begin
    GTask := CreateTask(
      procedure (const task: IOmniTask)
      begin
        while not task.Terminated do
          Sleep(1);
      end).Run;
    CreateTask(procedure (const task: IOmniTask) begin Sleep(50); end).Unobserved.Run;
  end;
  for i := 1 to 2 do
    CreateTask(
      procedure (const task: IOmniTask)
      begin
        if mode and CStuck <> 0 then
          Sleep(5000)
        else
          Sleep(300);
      end).Unobserved.Schedule(pool);
  Sleep(100);
  if mode and CIdle <> 0 then
    Sleep(500);
  if mode and CIdleTimeout <> 0 then
    Sleep(800 + Random(1200));
end;

function Shutdown(timeout_ms: cardinal): boolean; stdcall;
begin
  if assigned(GTask) then
    GTask.Terminate;
  GTask := nil;
  Result := StopAllOmniThreads(timeout_ms);
end;

procedure OwnDllProc(reason: integer);
begin
end;

exports
  Go, Shutdown;

var
  mode: integer;

begin
  Randomize;
  mode := StrToIntDef(GetEnvironmentVariable('OTL_UNLOAD_MODE'), 0); // set in UnloadHost
  if mode and COwnDllProc <> 0 then
    DllProc := OwnDllProc;
  if mode and CInit <> 0 then
    CreateTask(procedure (const task: IOmniTask) begin end).WaitFor(10); // not started
end.
