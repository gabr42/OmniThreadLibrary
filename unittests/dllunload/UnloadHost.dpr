program UnloadHost;

// UnloadHost <mode> <free|exit|shutdown|stuck> [rounds]
// Loads OtlUnloadDll.dll, runs tasks in it and
//   free    : calls FreeLibrary
//   exit    : exits with the DLL still loaded
//   shutdown: calls StopAllOmniThreads and FreeLibrary
//   stuck   : as shutdown, but StopAllOmniThreads must fail first and then succeed
// Stays alive for a while after FreeLibrary; threads that would still run in the DLL would crash.

{$APPTYPE CONSOLE}

uses
  Windows, TlHelp32, SysUtils;

type
  TGo       = procedure (mode: integer); stdcall;
  TShutdown = function (timeout_ms: cardinal): boolean; stdcall;

function NumThreads: integer;
var
  entry: TThreadEntry32;
  snap : THandle;
begin
  Result := 0;
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  entry.dwSize := SizeOf(entry);
  if Thread32First(snap, entry) then
    repeat
      if entry.th32OwnerProcessID = GetCurrentProcessId then
        Inc(result);
    until not Thread32Next(snap, entry);
  CloseHandle(snap);
end;

var
  after   : integer;
  before  : integer;
  i       : integer;
  lib     : HMODULE;
  start_ms: cardinal;

begin
  before := NumThreads;
  SetEnvironmentVariable('OTL_UNLOAD_MODE', PChar(ParamStr(1)));
  for i := 1 to StrToIntDef(ParamStr(3), 1) do begin
    lib := LoadLibrary('OtlUnloadDll.dll');
    if lib = 0 then
      RaiseLastOSError;
    TGo(GetProcAddress(lib, 'Go'))(StrToInt(ParamStr(1)));
    if ParamStr(2) = 'exit' then
      Halt(0);
    if ParamStr(2) = 'stuck' then
      if TShutdown(GetProcAddress(lib, 'Shutdown'))(500) then
        Halt(10);
    if ParamStr(2) <> 'free' then begin
      if not TShutdown(GetProcAddress(lib, 'Shutdown'))(10000) then
        Halt(11);
      after := NumThreads;
      // some OS worker threads may be running in addition
      if after > before + 12 then
        Halt(12);
    end;
    start_ms := GetTickCount;
    FreeLibrary(lib);
    if GetModuleHandle('OtlUnloadDll.dll') <> 0 then
      Halt(13);
    if GetTickCount - start_ms > 4000 then
      Halt(14);
    Sleep(500);
  end;
  Sleep(1500);
  after := NumThreads;
  if after > before + 12 then
    Halt(15);
end.
