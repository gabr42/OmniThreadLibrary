program Issue131;

{ Repro for GitHub issue #131: IOmniFuture<T> stored in a dynamic array, then .Value called on
  every element, occasionally crashed (TList.IndexOutOfBounds, AV in FAwaitedLock.Acquire).
  The maintainer could not reproduce it with Delphi 10.3 and the then-current source, so this
  is a regression/stress test; it repeats the reporter's program many times.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlParallel;

const
  CNumFutures = 70;
  CRepeats    = 30;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

procedure ProcessMessages;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end; { ProcessMessages }

procedure TestFutures(iteration: integer);
var
  j: integer;
  x: array of IOmniFuture<integer>;
begin
  SetLength(x, CNumFutures);
  for j := 0 to High(x) do
    x[j] := Parallel.Future<integer>(
      function: integer
      begin
        Sleep(Random(30));
        Result := 42;
      end);
  ProcessMessages;
  for j := 0 to High(x) do begin
    if x[j].Value <> 42 then
      Fail(Format('iteration %d, future %d returned a wrong value', [iteration, j]));
    x[j] := nil;
  end;
  SetLength(x, 0);
end; { TestFutures }

var
  i: integer;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  Randomize;
  TThread.CreateAnonymousThread(procedure begin Sleep(120000); Fail('hang'); end).Start;
  try
    for i := 1 to CRepeats do
      TestFutures(i);
  except
    on E: Exception do
      Fail(E.ClassName + ': ' + E.Message);
  end;
  Writeln('PASS');
  Flush(System.Output);
end.
