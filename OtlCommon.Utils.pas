///<summary>Stuff common to the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2026, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://en.delphipraxis.net/forum/32-omnithreadlibrary/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : GJ, Lee_Nover, scarre, Sean B. Durkin
///
///   Creation date     : 2011-08-31
///   Last modification : 2026-11-24
///   Version           : 1.03
///</para><para>
///   History:
///     1.03: 2026-11-24
///       - Added tracking of threads that were created by OTL and detection of
///         DLL unload and process exit.
///     1.02: 2026-04-15
///       - Fixed: OTL_HasTThreadCurrentThread ifdefs were dead code, preventing
///         SetThreadDescription (Windows 10+ thread naming API) from being loaded.
///         Removed the guards since TThread.CurrentThread is always available.
///       - Fixed: FreeLibrary was called on a handle obtained via GetModuleHandle
///         (which does not increment the reference count). Removed the incorrect
///         FreeLibrary call.
///     1.01: 2026-03-18
///       - On Windows, SetThreadName also calls SetThreadDescription API
///         if it is available.
///     1.0b: 2018-02-26
///       - Semantics of OTL_DontSetThreadName was reversed.
///     1.0a: 2017-11-28
///       - Did not include OtlOptions.inc
///     1.0: 2011-08-31
///       - [Lee_Nover] SetThreadName implementation moved here. Disabled
///         debug info for the unit. That way, debugger doesn't stop on
///         SetThreadName while single-stepping in another thread.
///</para></remarks>

unit OtlCommon.Utils;

{$I OtlOptions.inc}
{$DEBUGINFO OFF}

interface

uses
  Windows,
  Classes;

const
  /// <summary>When a DLL is being unloaded, all waits in OTL together take at most that long.</summary>
  COmniUnloadTimeout_ms = 30000;

type
  POmniTrackedThread = ^TOmniTrackedThread;
  TOmniTrackedThread = record
    Next  : POmniTrackedThread;
    Handle: THandle; // own handle, valid after the TThread is destroyed
    IsDone: boolean;
  end; { TOmniTrackedThread }

procedure SetThreadName(const name: string);

// Tracking of threads, created by OTL. For internal use.

/// <summary>Must be called before the thread is started.</summary>
function  TrackThread(thread: TThread): POmniTrackedThread;

/// <summary>Must be the last thing that the thread does.</summary>
procedure TrackedThreadDone(tracked: POmniTrackedThread);

/// <summary>Number of threads that are still alive. A thread that is done counts as alive
///   until it exits only if `untilExit` is set.</summary>
function  NumTrackedThreads(untilExit: boolean): integer;

/// <summary>True in ExitProcess. All other threads were already killed.</summary>
function  ProcessIsExiting: boolean;

/// <summary>True if the DLL that contains this code is being unloaded with FreeLibrary. Loader
///   lock is owned so threads cannot exit; waiting for that would block forever.</summary>
function  ModuleIsUnloading: boolean;

/// <summary>Must be called first in the finalization of units that wait for threads. Needed if
///   the DLL has replaced DllProc so that unload was not detected.</summary>
procedure ModuleFinalization;

/// <summary>Remaining part of COmniUnloadTimeout_ms.</summary>
function  UnloadTimeLeft_ms: cardinal;

/// <summary>Destroys the thread object. If the module is being unloaded and the thread did not
///   exit yet, the object is left alone.</summary>
procedure FreeThread(thread: TThread);

implementation

uses
  SysUtils;

{$IFDEF OTL_HasNameThreadForDebugging}
type
  TSetThreadDescription = function(hThread: THandle; threadDescription: PWideChar): HRESULT; stdcall;

var
  GSetThreadDescription: TSetThreadDescription;
{$ENDIF OTL_HasNameThreadForDebugging}

threadvar
  LastThreadName: string[255];

{$IFDEF OTL_DontSetThreadName}
procedure SetThreadName(const name: string);
begin
  // do nothing
end; { SetThreadName }
{$ELSE}

{$IFDEF OTL_HasNameThreadForDebugging}

procedure SetThreadName(const name: string);
var
  ansiName: AnsiString;
begin
  ansiName := AnsiString(name);
  if ansiName = LastThreadName then
    Exit;

  TThread.NameThreadForDebugging({$IFDEF OTL_NameThreadHasStringParameter}name{$ELSE}ansiName{$ENDIF});
  if assigned(GSetThreadDescription) then
    GSetThreadDescription(TThread.CurrentThread.Handle, PChar(name));

  LastThreadName := ansiName;
end; { SetThreadName }

{$ELSE ~OTL_HasNameThreadForDebugging}
{$WARN SYMBOL_PLATFORM OFF}

procedure SetThreadName(const name: string);
type
  TThreadNameInfo = record
    FType    : LongWord; // must be 0x1000
    FName    : PAnsiChar;// pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags   : LongWord; // reserved for future use, must be zero
  end; { TThreadNameInfo }
var
  ansiName      : AnsiString;
  threadNameInfo: TThreadNameInfo;
begin
  if DebugHook <> 0 then begin
    ansiName := AnsiString(name);
    if ansiName = LastThreadName then
      Exit;
    threadNameInfo.FType := $1000;
    threadNameInfo.FName := PAnsiChar(ansiName);
    threadNameInfo.FThreadID := $FFFFFFFF;
    threadNameInfo.FFlags := 0;
    try
      RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
    except {ignore} end;
    LastThreadName := ansiName;
  end;
end; { SetThreadName }

{$WARN SYMBOL_PLATFORM ON}
{$ENDIF ~OTL_HasNameThreadForDebugging}
{$ENDIF ~OTL_DontSetThreadName}

var
  // Linked list, protected by a spinlock. Not an object as it must survive finalization.
  GTrackedLock   : integer;
  GTrackedThreads: POmniTrackedThread;

procedure LockTracked;
begin
  while InterlockedCompareExchange(GTrackedLock, 1, 0) <> 0 do
    Sleep(0);
end; { LockTracked }

procedure UnlockTracked;
begin
  InterlockedExchange(GTrackedLock, 0);
end; { UnlockTracked }

/// Removes threads that have exited. Caller must own the lock.
procedure PruneTracked;
var
  link   : ^POmniTrackedThread;
  tracked: POmniTrackedThread;
begin
  link := @GTrackedThreads;
  while assigned(link^) do begin
    tracked := link^;
    if tracked^.IsDone and (WaitForSingleObject(tracked^.Handle, 0) = WAIT_OBJECT_0) then begin
      link^ := tracked^.Next;
      CloseHandle(tracked^.Handle);
      Dispose(tracked);
    end
    else
      link := @tracked^.Next;
  end;
end; { PruneTracked }

function TrackThread(thread: TThread): POmniTrackedThread;
begin
  New(Result);
  Result^.Handle := 0;
  Result^.IsDone := false;
  if not DuplicateHandle(GetCurrentProcess, thread.Handle, GetCurrentProcess, @Result^.Handle,
           SYNCHRONIZE, false, 0) then
  begin
    Dispose(Result);
    RaiseLastOSError;
  end;
  LockTracked;
  try
    PruneTracked;
    Result^.Next := GTrackedThreads;
    GTrackedThreads := Result;
  finally UnlockTracked; end;
end; { TrackThread }

procedure TrackedThreadDone(tracked: POmniTrackedThread);
begin
  if assigned(tracked) then
    tracked^.IsDone := true; // tracked may be already disposed after this
end; { TrackedThreadDone }

function NumTrackedThreads(untilExit: boolean): integer;
var
  tracked: POmniTrackedThread;
begin
  Result := 0;
  LockTracked;
  try
    PruneTracked;
    tracked := GTrackedThreads;
    while assigned(tracked) do begin
      if untilExit or (not tracked^.IsDone) then
        Inc(Result);
      tracked := tracked^.Next;
    end;
  finally UnlockTracked; end;
end; { NumTrackedThreads }

var
  GNextDllProcEx : TDllProcEx;
  GUnloading     : boolean;
  GUnloadStart_ms: cardinal;

function  RtlDllShutdownInProgress: ByteBool; stdcall; external 'ntdll.dll';

function ProcessIsExiting: boolean;
begin
  Result := RtlDllShutdownInProgress;
end; { ProcessIsExiting }

procedure MarkUnloading;
begin
  if GUnloading or ProcessIsExiting then
    Exit;
  GUnloadStart_ms := GetTickCount;
  GUnloading := true;
end; { MarkUnloading }

/// Called before units are finalized. Not called if the DLL assigns its own DllProc and doesn't
/// call the previous one; ModuleFinalization is then called from the finalization of OTL units.
procedure OtlDllProcEx(reason: integer;
  reserved: {$IFDEF OTL_DllProcExHasPointerParameter}pointer{$ELSE}integer{$ENDIF});
begin
  if (reason = DLL_PROCESS_DETACH) and (pointer(reserved) = nil) then // otherwise process is exiting
    MarkUnloading;
  if assigned(GNextDllProcEx) then
    GNextDllProcEx(reason, reserved);
end; { OtlDllProcEx }

procedure ModuleFinalization;
begin
  if ModuleIsLib and (not ModuleIsPackage) then
    MarkUnloading;
end; { ModuleFinalization }

function ModuleIsUnloading: boolean;
begin
  Result := GUnloading;
end; { ModuleIsUnloading }

function UnloadTimeLeft_ms: cardinal;
var
  elapsed_ms: cardinal;
begin
  elapsed_ms := cardinal(int64(GetTickCount) - int64(GUnloadStart_ms)); // GetTickCount can wrap
  if elapsed_ms >= COmniUnloadTimeout_ms then
    Result := 0
  else
    Result := COmniUnloadTimeout_ms - elapsed_ms;
end; { UnloadTimeLeft_ms }

procedure FreeThread(thread: TThread);
const
  CPoll_ms = 50;
var
  handle: THandle;
begin
  if not assigned(thread) then
    Exit;
  if not ProcessIsExiting then begin
    handle := thread.Handle;
    if ModuleIsUnloading then begin
      if WaitForSingleObject(handle, 0) <> WAIT_OBJECT_0 then
        Exit;
    end
    else if ModuleIsLib and (not ModuleIsPackage)
            and (GetCurrentThreadID <> MainThreadID) // TThread.WaitFor calls CheckSynchronize in the main thread
            and (not thread.Suspended) then
    begin
      thread.Terminate;
      while WaitForSingleObject(handle, CPoll_ms) = WAIT_TIMEOUT do
        if ModuleIsUnloading then // unload has started while we were waiting
          Exit;
    end;
  end;
  thread.Free;
end; { FreeThread }

initialization
  if ModuleIsLib and (not ModuleIsPackage) then begin
    GNextDllProcEx := DllProcEx;
    DllProcEx := OtlDllProcEx;
  end;
  {$IFDEF OTL_HasNameThreadForDebugging}
  GSetThreadDescription := GetProcAddress(GetModuleHandle('kernel32.dll'), 'SetThreadDescription');
  {$ENDIF OTL_HasNameThreadForDebugging}
end.
