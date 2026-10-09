program LibraryListShutdown;

{ A thread that outlives the finalization of DSiWin32 must be able to call DSiLoadLibrary and
  DSiGetProcAddress without crashing. DSiWin32's finalization used to destroy the library list and its
  critical section, so a thread that was inside DSiLoadLibrary at that moment crashed.

  The program starts a thread that calls DSiGetProcAddress in a loop and ends right away. LateFinal
  (the first unit, so it is finalized last) keeps the process alive for a while after DSiWin32 has
  been finalized and counts the access violations.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  LateFinal, // must be first!
  Winapi.Windows,
  System.Classes,
  DSiWin32;

var
  i: integer;
begin
  for i := 1 to 8 do
    TThread.CreateAnonymousThread(
      procedure
      begin
        while true do begin
          DSiGetProcAddress('kernel32.dll', 'GetTickCount');
          DSiGetProcAddress('winmm.dll', 'timeGetTime');
          DSiGetProcAddress('user32.dll', 'GetDC');
          DSiGetProcAddress('gdi32.dll', 'GetPixel');
        end;
      end).Start;
  Sleep(200); // let the threads run
end.
