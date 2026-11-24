unit api_68_DLL;

// Interface between the program and the DLL. They are compiled separately (and could even be
// written with different compilers), so don't pass strings, objects, interfaces (including
// OmniThreadLibrary interfaces), and anonymous methods between them.

interface

uses
  Windows, Messages;

const
  CDllName = 'dll_68_DLL.dll';

  // Posted from the DLL to the notification window. WParam = job ID.
  WM_JOB_PROGRESS  = WM_USER + 1; // LParam = percent
  WM_JOB_COMPLETED = WM_USER + 2; // LParam = number of primes or -1 if the job was cancelled

type
  /// <summary>Starts a background job and returns immediately. Result is the job ID or 0 if the
  ///   DLL is being prepared for unload.</summary>
  TStartWork = function (notifyWindow: HWND; maxNumber: integer; ignoreCancel: LongBool): integer; stdcall;

  /// <summary>Tells all jobs to stop and returns immediately.</summary>
  TCancelWork = procedure; stdcall;

  /// <summary>Must be called before FreeLibrary. If it returns False, some job is still running
  ///   and FreeLibrary must not be called. The function can be called again later.</summary>
  TPrepareForUnload = function (timeout_ms: cardinal): LongBool; stdcall;

implementation

end.
