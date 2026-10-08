program NoWaitIntoExit;

{ A Parallel.ForEach(...).NoWait.Into(queue) loop must not hang the process at exit when the program
  ends right after it has taken the last result from the queue (found while testing issue #49).

  Usage: NoWaitIntoExit <exit|release|wait> [range|collection] [ordered]
    ordered    - PreserveOrder is used
    range      - ForEach over an integer range (default)
    collection - ForEach over a TOmniBlockingCollection that is filled after Execute
    exit    - the program ends immediately after the last item was taken; the loop interface
              is released when the program's globals are finalized
    release - the loop interface is released explicitly right after the last item was taken
    wait    - control: waits for the output queue to complete (and a bit longer) before ending

  Exit code: 0 = pass, 1 = fail (hang at exit is reported by a watchdog thread). }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon,
  OtlCollections,
  OtlParallel;

const
  CNumItems  = 200;
  CWatchdog_ms = 15000;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  GInQueue : IOmniBlockingCollection;
  GLoop    : IOmniParallelLoop<integer>;
  GOutQueue: IOmniBlockingCollection;

var
  i       : integer;
  mode    : string;
  value   : TOmniValue;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  mode := ParamStr(1);
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(CWatchdog_ms);
      Fail(mode + ': hang at exit');
    end).Start;
  GOutQueue := TOmniBlockingCollection.Create;
  if SameText(ParamStr(2), 'collection') then begin
    GInQueue := TOmniBlockingCollection.Create;
    GLoop := Parallel.ForEach<integer>(GInQueue);
  end
  else
    GLoop := Parallel.ForEach(1, CNumItems);
  if SameText(ParamStr(3), 'ordered') then
    GLoop := GLoop.PreserveOrder;
  GLoop.NoWait.Into(GOutQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      res := value * 2;
    end);
  if assigned(GInQueue) then begin
    for i := 1 to CNumItems do
      GInQueue.Add(i);
    GInQueue.CompleteAdding;
  end;
  for i := 1 to CNumItems do
    if not GOutQueue.TryTake(value, 5000) then
      Fail('output ended after ' + IntToStr(i - 1) + ' items');
  if SameText(mode, 'release') then
    GLoop := nil
  else if SameText(mode, 'wait') then begin
    while not GOutQueue.IsCompleted do
      Sleep(10);
    Sleep(200);
  end;
  Writeln('PASS: ', mode, ' (end of the main block)');
  Flush(System.Output);
end.
