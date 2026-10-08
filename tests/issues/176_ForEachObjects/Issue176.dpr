program Issue176;

{ Repro for GitHub issue #176: Parallel.ForEach<T> over an object that has a GetEnumerator
  method returning objects (the reporter used TListView.Items) failed with
  'TValue of type tkClass cannot be converted to TOmniValue'.

  The test uses a TCollection, which is in System.Classes, instead of a TListView.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OtlCommon,
  OtlParallel;

const
  CNumItems = 100;

procedure Fail(const msg: string);
begin
  Writeln('FAIL: ', msg);
  Flush(System.Output);
  ExitProcess(1);
end; { Fail }

var
  collection: TCollection;
  count     : integer;
  i         : integer;
  sum       : integer;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  TThread.CreateAnonymousThread(procedure begin Sleep(20000); Fail('hang'); end).Start;
  collection := TCollection.Create(TCollectionItem);
  try
    for i := 1 to CNumItems do
      collection.Add;
    try
      Parallel.ForEach<TCollectionItem>(collection).Execute(
        procedure (const item: TCollectionItem)
        begin
          TInterlocked.Increment(count);
          TInterlocked.Add(sum, item.Index + 1);
        end);
    except
      on E: Exception do
        Fail(E.ClassName + ': ' + E.Message);
    end;
  finally FreeAndNil(collection); end;
  if count <> CNumItems then
    Fail(Format('%d items processed, expected %d', [count, CNumItems]));
  if sum <> CNumItems * (CNumItems + 1) div 2 then
    Fail(Format('wrong items processed (sum %d)', [sum]));
  Writeln('PASS');
  Flush(System.Output);
end.
