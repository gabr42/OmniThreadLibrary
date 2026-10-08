program Issue182;

{ Repro for GitHub issue #182: ERROR_NOT_ENOUGH_QUOTA (1816) raised by
  TOmniContainerWindowsMessageObserver.Notify/Send when the receiving thread does not process
  its message queue for a while (the reporter's machine was swapping, so the GUI thread stalled).

  A thread's posted-message queue holds 10,000 messages. The test fills the queue of a window,
  has a worker thread notify the observer of that window, and starts draining the queue only
  after a delay that is longer than the old retry budget (about 5 s) but still short for a
  machine that is swapping. Notify must wait for the queue instead of raising.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  OtlContainerObserver;

const
  CDrainDelay_ms = 8000;
  CWM_Fill       = WM_USER + 1;
  CWM_Notify     = WM_USER + 2;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  GNotifyError: string;
  GNotifyDone : boolean;
  GNotified   : integer;

function WndProc(wnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if msg = CWM_Notify then
    Inc(GNotified);
  Result := DefWindowProc(wnd, msg, wParam, lParam);
end; { WndProc }

var
  cls     : TWndClass;
  filled  : integer;
  msg     : TMsg;
  observer: TOmniContainerWindowsMessageObserver;
  start   : cardinal;
  wnd     : HWND;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  TThread.CreateAnonymousThread(procedure begin Sleep(60000); Fail('hang'); end).Start;
  FillChar(cls, SizeOf(cls), 0);
  cls.lpfnWndProc := @WndProc;
  cls.hInstance := HInstance;
  cls.lpszClassName := 'Issue182Window';
  if Winapi.Windows.RegisterClass(cls) = 0 then
    Fail('RegisterClass failed');
  wnd := CreateWindowEx(0, 'Issue182Window', nil, 0, 0, 0, 0, 0, HWND_MESSAGE, 0, HInstance, nil);
  if wnd = 0 then
    Fail('CreateWindowEx failed');
  // fill the posted-message queue of this thread
  filled := 0;
  while PostMessage(wnd, CWM_Fill, 0, 0) do
    Inc(filled);
  if GetLastError <> ERROR_NOT_ENOUGH_QUOTA then
    Fail('unexpected error while filling the queue: ' + IntToStr(GetLastError));
  Writeln('queue full after ', filled, ' messages');

  observer := CreateContainerWindowsMessageObserver(wnd, CWM_Notify, 0, 0);
  start := GetTickCount;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        observer.Notify;
      except
        on E: Exception do
          GNotifyError := E.ClassName + ': ' + E.Message;
      end;
      GNotifyDone := true;
    end).Start;

  // this thread is "stalled" for a while, then it starts processing messages again
  Sleep(CDrainDelay_ms);
  while not GNotifyDone or (GetTickCount - start < CDrainDelay_ms + 100) do begin
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    Sleep(10);
    if GetTickCount - start > CDrainDelay_ms + 20000 then
      Fail('Notify did not return');
  end;
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
    DispatchMessage(msg);
  if GNotifyError <> '' then
    Fail(Format('Notify raised after %d ms: %s', [GetTickCount - start, GNotifyError]));
  if GNotified <> 1 then
    Fail(Format('notification was delivered %d times, expected once', [GNotified]));
  Writeln('PASS');
  Flush(System.Output);
end.
