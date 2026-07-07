unit TestStressBlockingCollection1;

interface

{$I OtlOptions.inc}

{$IFDEF Unicode}
uses
  TestFramework, Windows, SysUtils, Classes,
  OtlCollections, OtlCommon, OtlSync;

type
  // High-iteration stress for IOmniBlockingCollection. Producer races
  // CompleteAdding vs consumer draining. Reduced from 1000 → 100 iters.
  TStressIOmniBlockingCollection = class(TTestCase)
  published
    procedure StressTestCompleteAdding;
  end;
{$ENDIF Unicode}

implementation

{$IFDEF Unicode}
uses
  OtlParallel;

const
  CIterations = 100; // reduced from NG's 1000 (10x) — still seconds long

procedure TStressIOmniBlockingCollection.StressTestCompleteAdding;
var
  coll     : IOmniBlockingCollection;
  iTest    : integer;
  lastAdded: integer;
  lastRead : TOmniValue;
begin
  lastAdded := -1;
  lastRead := -2;
  for iTest := 1 to CIterations do begin
    coll := TOmniBlockingCollection.Create;
    lastAdded := -1;
    lastRead := -2;
    Parallel.Join([
      procedure
      var
        i: integer;
      begin
        for i := 1 to 100000 do begin
          if not coll.TryAdd(i) then
            break;
          lastAdded := i;
        end;
      end,

      procedure
      begin
        Sleep(1);
        coll.CompleteAdding;
      end,

      procedure
      begin
        while coll.TryTake(lastRead, INFINITE) do
          ;
      end
    ]).Execute;
    if (lastAdded > 0) and (lastRead.AsInteger > 0)
       and (lastAdded <> lastRead.AsInteger)
    then
      break;
  end;
  CheckEquals(lastAdded, lastRead.AsInteger,
    Format('lastAdded=%d lastRead=%d mismatch', [lastAdded, lastRead.AsInteger]));
end;

initialization
  RegisterTest(TStressIOmniBlockingCollection.Suite);
{$ENDIF Unicode}
end.
