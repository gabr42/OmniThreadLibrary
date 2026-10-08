program Issue68;

{ Issue #68: DSiWin32 raises the Windows timer resolution to 1 ms for the whole lifetime of any
  process that uses OmniThreadLibrary, which keeps the CPU from saving power.

  This program shows the effect. It reads the current system timer resolution with
  NtQueryTimerResolution. Build it normally and with -DDSiNoTimerResolution and compare.
  (The value is system wide, so it is only meaningful if no other program is requesting a
  high resolution at the time.)

  Usage: Issue68 [expect-low|expect-high]
    expect-high - exit code 1 if the resolution is NOT 1 ms (default build)
    expect-low  - exit code 1 if the resolution is 1 ms (-DDSiNoTimerResolution build)
    no argument - only print the resolution; exit code 0 }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  DSiWin32;

function NtQueryTimerResolution(var minimum, maximum, current: ULONG): LONG; stdcall;
  external 'ntdll.dll';

var
  current, maximum, minimum: ULONG;

begin
  NtQueryTimerResolution(minimum, maximum, current);
  Writeln(Format('timer resolution: current %.2f ms (finest %.2f ms, coarsest %.2f ms)',
    [current / 10000, maximum / 10000, minimum / 10000]));
  Sleep(50);
  if SameText(ParamStr(1), 'expect-high') and (current > 10000) then begin
    Writeln('FAIL: expected the 1 ms resolution');
    ExitCode := 1;
  end
  else if SameText(ParamStr(1), 'expect-low') and (current <= 10000) then begin
    Writeln('FAIL: the 1 ms resolution is in effect');
    ExitCode := 1;
  end;
end.
