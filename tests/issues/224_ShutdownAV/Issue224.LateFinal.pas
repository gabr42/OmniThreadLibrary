unit Issue224.LateFinal;

{ Must be the FIRST unit in the program's uses clause. It is then initialized
  before - and therefore finalized after - every OTL/DSiWin32 unit, which gives
  us a window in which DSiWin32's globals are already destroyed but the process
  is still running. In that window, any OTL thread still alive can touch them.

  Access violations anywhere in the process are counted with a vectored
  exception handler, because OTL swallows exceptions raised in timer handlers
  and a plain "did it crash?" check would miss them. }

interface

var
  AVCount: integer = 0;

implementation

uses
  Winapi.Windows;

type
  PExceptionRecord_ = ^TExceptionRecord_;
  TExceptionRecord_ = record
    ExceptionCode: cardinal;
  end;

  PExceptionPointers_ = ^TExceptionPointers_;
  TExceptionPointers_ = record
    ExceptionRecord: PExceptionRecord_;
  end;

const
  CAccessViolation = $C0000005;
  CLateWindow_ms   = 3500; // > 3 ticks of the thread pool maintenance timer (1 s)

function AddVectoredExceptionHandler(first: ULONG; handler: pointer): pointer; stdcall;
  external 'kernel32.dll';

function CountAV(excInfo: PExceptionPointers_): LONG; stdcall;
begin
  if excInfo.ExceptionRecord.ExceptionCode = CAccessViolation then
    AtomicIncrement(AVCount);
  Result := 0; // EXCEPTION_CONTINUE_SEARCH
end; { CountAV }

initialization
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  AddVectoredExceptionHandler(1, @CountAV);
finalization
  Sleep(CLateWindow_ms);
  if AVCount > 0 then begin
    Writeln('FAIL: ', AVCount, ' access violation(s) after DSiWin32 finalization');
    ExitCode := 1;
  end
  else
    Writeln('PASS: no access violations');
end.
