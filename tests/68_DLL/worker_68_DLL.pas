unit worker_68_DLL;

// How to use OmniThreadLibrary in a DLL:
//
// 1. Don't use OTL in the initialization and finalization of units, in DllMain, or in the
//    begin..end block of the DLL. Windows owns the loader lock at that time, and threads can
//    neither start nor exit.
//
// 2. Export a function that stops everything (PrepareForUnload). It must
//      - prevent new tasks from being started,
//      - tell running tasks to stop and wait until they do that,
//      - release all OTL objects (task controllers, pools, futures ...),
//      - call StopAllOmniThreads, which waits until all threads have really exited.
//
// 3. The program must call this function before FreeLibrary and before it exits. If it returns
//    False, the program must not call FreeLibrary. Code that is still running would disappear
//    and the program would crash.
//
// 4. Tasks must stop quickly when they are told to. OTL does not kill threads.
//
// 5. Exported functions in this demo must be called from the main thread of the program.

interface

uses
  Windows;

function  StartWork(notifyWindow: HWND; maxNumber: integer; ignoreCancel: LongBool): integer; stdcall;
procedure CancelWork; stdcall;
function  PrepareForUnload(timeout_ms: cardinal): LongBool; stdcall;

implementation

uses
  SysUtils,
  DSiWin32,
  OtlCommon,
  OtlSync,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool,
  OtlParallel,
  api_68_DLL;

var
  GCancel  : IOmniCancellationToken; // shared by all jobs
  GJobs    : IOmniTaskGroup;
  GLastJob : integer;
  GPool    : IOmniThreadPool;
  GStopping: boolean;

function IsPrime(number: integer): boolean;
var
  i: integer;
begin
  Result := number >= 2;
  i := 2;
  while Result and (i <= number div i) do begin
    Result := (number mod i) <> 0;
    Inc(i);
  end;
end; { IsPrime }

{:PostMessage does not wait for the main thread. Don't use SendMessage here. The main thread may
  be waiting for this task in PrepareForUnload and both would wait forever.
}
procedure Notify(const task: IOmniTask; msg: cardinal; value: integer);
begin
  PostMessage(HWND(task.Param['Window'].AsInt64), msg, WPARAM(task.Param['JobID'].AsInteger), value);
end; { Notify }

{:Counts primes from 1 to MaxNumber. Executed in a background thread.
}
procedure CountPrimes(const task: IOmniTask);
const
  CNumBlocks = 100;
var
  block    : integer;
  blockSize: integer;
  count    : TOmniAlignedInt32;
begin
  blockSize := task.Param['MaxNumber'].AsInteger div CNumBlocks;
  count.Value := 0;
  for block := 0 to CNumBlocks - 1 do begin
    if task.CancellationToken.IsSignalled then
      break; //for
    // High-level abstractions can be used in a DLL, too. This call returns when the loop is done.
    Parallel.&For(block * blockSize + 1, (block + 1) * blockSize)
      .Execute(
        procedure (number: integer)
        begin
          if IsPrime(number) then
            count.Increment;
        end);
    Notify(task, WM_JOB_PROGRESS, block + 1);
  end;
  if task.CancellationToken.IsSignalled then
    Notify(task, WM_JOB_COMPLETED, -1)
  else
    Notify(task, WM_JOB_COMPLETED, count.Value);
end; { CountPrimes }

{:An example of a badly behaved task. It cannot be stopped.
}
procedure StubbornTask(const task: IOmniTask);
begin
  Sleep(15000);
  Notify(task, WM_JOB_COMPLETED, 0);
end; { StubbornTask }

{:Releases task controllers of completed jobs.
}
procedure RemoveCompletedJobs;
var
  iJob: integer;
begin
  for iJob := GJobs.Tasks.Count - 1 downto 0 do
    if GJobs.Tasks[iJob].WaitFor(0) then
      GJobs.Tasks.Delete(iJob);
end; { RemoveCompletedJobs }

function StartWork(notifyWindow: HWND; maxNumber: integer; ignoreCancel: LongBool): integer;
var
  job: IOmniTaskControl;
begin
  Result := 0;
  if GStopping then
    Exit;

  if not assigned(GPool) then begin // created on first use and not in the initialization section
    GPool := CreateThreadPool('68_DLL');
    GJobs := CreateTaskGroup;
  end;

  RemoveCompletedJobs;
  // Cancelled token stays cancelled. Jobs that are still running keep using the old one.
  if (not assigned(GCancel)) or GCancel.IsSignalled then
    GCancel := CreateOmniCancellationToken;

  Inc(GLastJob);
  if ignoreCancel then
    job := CreateTask(StubbornTask, 'Stubborn task')
  else
    job := CreateTask(CountPrimes, 'Count primes');
  job.SetParameter('JobID', GLastJob);
  job.SetParameter('Window', int64(notifyWindow));
  job.SetParameter('MaxNumber', maxNumber);
  job.CancelWith(GCancel);
  GJobs.Add(job);
  job.Schedule(GPool);
  Result := GLastJob;
end; { StartWork }

procedure CancelWork;
begin
  if assigned(GCancel) then
    GCancel.Signal;
end; { CancelWork }

function PrepareForUnload(timeout_ms: cardinal): LongBool;
var
  elapsed_ms: int64;
  start_ms  : int64;
begin
  Result := false;
  start_ms := DSiTimeGetTime64;

  // From now on, StartWork doesn't start new jobs.
  GStopping := true;

  // Stop all jobs.
  CancelWork;
  if assigned(GJobs) then begin
    RemoveCompletedJobs;
    if (GJobs.Tasks.Count > 0) and (not GJobs.WaitForAll(timeout_ms)) then
      Exit; // some job is still running, DLL must stay loaded
  end;

  // Release all OTL objects.
  GJobs := nil;
  GCancel := nil;
  GPool := nil;

  // Stop all thread pools (including the one used by Parallel) and wait for all threads.
  elapsed_ms := DSiTimeGetTime64 - start_ms;
  if elapsed_ms > timeout_ms then
    elapsed_ms := timeout_ms;
  Result := StopAllOmniThreads(timeout_ms - cardinal(elapsed_ms));
end; { PrepareForUnload }

end.
