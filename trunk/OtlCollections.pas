unit OtlCollection;

///  - BlockingCollection
///    http://blogs.msdn.com/pfxteam/archive/2009/11/06/9918363.aspx

interface

uses
  OtlCommon;

type
  IOmniBlockingCollectionEnumerator = interface ['{7A5AA8F4-5ED8-40C3-BDC3-1F991F652F9E}']
    function  GetCurrent: TOmniValue;
    function  MoveNext: boolean;
    property Current: TOmniValue read GetCurrent;
  end; { IOmniBlockingCollectionEnumerator }

  IOmniBlockingCollection = interface ['{208EFA15-1F8F-4885-A509-B00191145D38}']
    procedure Add(const value: TOmniValue);
    procedure CompleteAdding;
    function  GetEnumerator: IOmniBlockingCollectionEnumerator;
    function  Take(var value: TOmniValue): boolean;
    function  TryAdd(const value: TOmniValue): boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: integer = 0): boolean;
  end; { IOmniBlockingCollection }

function CreateBlockingCollection: IOmniBlockingCollection;

implementation

uses
  OtlContainers;

function CreateBlockingCollection: IOmniBlockingCollection;
begin
  // TODO -cMM: CreateBlockingCollection default body inserted
  Result :=  nil;
end;

type
  TOmniBlockingCollection = class(TInterfacedObject, IOmniBlockingCollection)
  strict private
    obcCollection: TOmniQueue;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const value: TOmniValue);
    procedure CompleteAdding;
    function  GetEnumerator: IOmniBlockingCollectionEnumerator;
    function  Take(var value: TOmniValue): boolean;
    function  TryAdd(const value: TOmniValue): boolean;
    function  TryTake(var value: TOmniValue; timeout_ms: integer = 0): boolean;
  end; { TOmniBlockingCollection }

procedure TOmniBlockingCollection.Add(const value: TOmniValue);
begin

end;

procedure TOmniBlockingCollection.CompleteAdding;
begin

end;

constructor TOmniBlockingCollection.Create;
begin
  inherited Create;
//  obcCollection := TOmniQueue.Create;
end; { TOmniBlockingCollection.Create }

destructor TOmniBlockingCollection.Destroy;
begin
//  FreeAndNil(obcCollection);
  inherited Destroy;
end; function TOmniBlockingCollection.GetEnumerator: IOmniBlockingCollectionEnumerator;
begin

end;

function TOmniBlockingCollection.Take(var value: TOmniValue): boolean;
begin

end;

function TOmniBlockingCollection.TryAdd(const value: TOmniValue): boolean;
begin

end;

function TOmniBlockingCollection.TryTake(var value: TOmniValue;
  timeout_ms: integer): boolean;
begin

end;

{ TOmniBlockingCollection.Destroy }

(*
TryAdd:
  if FCompleted then
    return false
  else
    queue.enqueue

Add:
  if not TryAdd then
    raise exception

TryTake:
  if queue.dequeue then
    return true
  else if FCompleted then
    return false
  else
    set up observer
    while not queue.dequeue do
      wait on observer and on Completed
    remove observer
    return not FCompleted

Take:
  if not TryTake then
    raise exception

CompleteAdding:
  FCompleted := true
  signal Completed

enumerator:
  calls Take from MoveNext
*)

end.

