program Issue57;

{ Repro for GitHub issue #57: an exception raised in the body of Parallel.For or
  Parallel.ForEach hangs the loop (and is lost).

  Usage: Issue57 <For|ForEach|ForEachInto|Join>
  Expected (Join is the reference behaviour): the call returns - it must not hang - and
  the exception reaches the caller.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlCollections,
  OtlParallel;

type
  EMyException = class(Exception);

const
  CWatchdog_ms = 10000;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  caseName: string;
  outQueue: IOmniBlockingCollection;
  raised  : boolean;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  caseName := ParamStr(1);
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(CWatchdog_ms);
      Fail(caseName + ': hang (no result in ' + IntToStr(CWatchdog_ms) + ' ms)');
    end).Start;
  raised := false;
  try
    if SameText(caseName, 'For') then
      Parallel.&For(0, 1).Execute(
        procedure (i: integer)
        begin
          raise EMyException.Create('Error Message');
        end)
    else if SameText(caseName, 'ForEach') then
      Parallel.ForEach(0, 9).Execute(
        procedure (const value: integer)
        begin
          raise EMyException.Create('Error Message');
        end)
    else if SameText(caseName, 'ForEachInto') then begin
      outQueue := TOmniBlockingCollection.Create;
      Parallel.ForEach(0, 9).Into(outQueue).Execute(
        procedure (const value: integer; var res: TOmniValue)
        begin
          raise EMyException.Create('Error Message');
        end);
    end
    else if SameText(caseName, 'Join') then
      Parallel.Join(
        procedure begin raise EMyException.Create('Error Message'); end,
        procedure begin raise EMyException.Create('Error Message'); end).Execute
    else
      Fail('unknown case ' + caseName);
  except
    on E: Exception do begin
      raised := true;
      Writeln('exception reached the caller: ', E.ClassName, ': ', E.Message);
    end;
  end;
  if not raised then
    Fail(caseName + ': no exception reached the caller');
  Writeln('PASS: ', caseName);
end.
