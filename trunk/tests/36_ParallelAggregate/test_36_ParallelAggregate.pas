unit test_36_ParallelAggregate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmParallelAggregateDemo = class(TForm)
    btnCountParallel: TButton;
    btnCountSequential: TButton;
    btnSumParallel: TButton;
    btnSumSequential: TButton;
    inpMaxPrime: TSpinEdit;
    inpMaxSummand: TSpinEdit;
    inpNumCPU: TSpinEdit;
    Label1: TLabel;
    Label3: TLabel;
    lblCountPrimes: TLabel;
    lbLog: TListBox;
    btnSumParallel2: TButton;
    btnCountParallel2: TButton;
    procedure btnCountParallel2Click(Sender: TObject);
    procedure btnCountParallelClick(Sender: TObject);
    procedure btnCountSequentialClick(Sender: TObject);
    procedure btnSumParallel2Click(Sender: TObject);
    procedure btnSumParallelClick(Sender: TObject);
    procedure btnSumSequentialClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function  IsPrime(i: integer): boolean;
    procedure Log(const msg: string; const params: array of const);
  end;

var
  frmParallelAggregateDemo: TfrmParallelAggregateDemo;

implementation

uses
  DSiWin32,
  OtlCommon,
  OtlSync,
  OtlParallel;

{$R *.dfm}

procedure TfrmParallelAggregateDemo.btnCountParallel2Click(Sender: TObject);
var
  lockNum  : TOmniCS;
  numPrimes: integer;
  start    : int64;
begin
  start := DSiTimeGetTime64;
  numPrimes := 0;
  Parallel.ForEach(1, inpMaxPrime.Value)
    .NumTasks(inpNumCPU.Value)
    .Initialize(
      procedure (var taskState: TOmniValue)
      begin
        taskState.AsInteger := 0;
      end)
    .Finalize(
      procedure (const taskState: TOmniValue)
      begin
        lockNum.Acquire;
        try
          numPrimes := numPrimes + taskState.AsInteger;
        finally lockNum.Release; end;
      end)
    .Execute(
      procedure (const value: integer; var taskState: TOmniValue)
      begin
        if IsPrime(value) then
          taskState.AsInteger := taskState.AsInteger + 1;
      end
    );
  start := DSiTimeGetTime64 - start;
  Log('%d primes from 1 to %d; calculation took %d ms', [numPrimes, inpMaxPrime.Value, start]);
end;

procedure TfrmParallelAggregateDemo.btnCountParallelClick(Sender: TObject);
var
  numPrimes: integer;
  start    : int64;
begin
  start := DSiTimeGetTime64;
  numPrimes :=
    Parallel.ForEach(1, inpMaxPrime.Value)
    .NumTasks(inpNumCPU.Value)
    .Aggregate(0,
      procedure (var aggregate: TOmniValue; const value: TOmniValue)
      begin
        aggregate := aggregate.AsInt64 + value.AsInt64;
      end)
    .Execute(
      procedure (const value: integer; var result: TOmniValue)
      begin
        if IsPrime(value) then
          Result := 1;
      end
    );
  start := DSiTimeGetTime64 - start;
  Log('%d primes from 1 to %d; calculation took %d ms', [numPrimes, inpMaxPrime.Value, start]);
end;

procedure TfrmParallelAggregateDemo.btnCountSequentialClick(Sender: TObject);
var
  i        : integer;
  numPrimes: integer;
  start    : int64;
begin
  start := DSiTimeGetTime64;
  numPrimes := 0;
  for i := 1 to inpMaxPrime.Value do
    if IsPrime(i) then
      Inc(numPrimes);
  start := DSiTimeGetTime64 - start;
  Log('%d primes from 1 to %d; calculation took %d ms', [numPrimes, inpMaxPrime.Value, start]);
end;

procedure TfrmParallelAggregateDemo.btnSumParallel2Click(Sender: TObject);
var
  lockSum: TOmniCS;
  start  : int64;
  sum    : int64;
begin
  start := DSiTimeGetTime64;
  sum := 0;
  Parallel
    .ForEach(1, inpMaxSummand.Value)
    .NumTasks(inpNumCPU.Value)
    .Initialize(
      procedure (var taskState: TOmniValue)
      begin
        taskState := 0;
      end)
    .Finalize(
      procedure (const taskState: TOmniValue)
      begin
        lockSum.Acquire;
        try
          sum := sum + taskState.AsInt64;
        finally lockSum.Release; end;
      end
    )
    .Execute(
      procedure (const value: integer; var taskState: TOmniValue)
      begin
        taskState.AsInt64 := taskState.AsInt64 + value;
      end
    );
  start := DSiTimeGetTime64 - start;
  Log('Sum(1..%d) = %d; calculation took %d ms', [inpMaxSummand.Value, sum, start]);
end;

procedure TfrmParallelAggregateDemo.btnSumParallelClick(Sender: TObject);
var
  start: int64;
  sum  : int64;
begin
  start := DSiTimeGetTime64;
  sum :=
    Parallel
    .ForEach(1, inpMaxSummand.Value)
    .NumTasks(inpNumCPU.Value)
    .AggregateSum
    .Execute(
      procedure (const value: integer; var result: TOmniValue)
      begin
        Result := value;
      end
    );
  start := DSiTimeGetTime64 - start;
  Log('Sum(1..%d) = %d; calculation took %d ms', [inpMaxSummand.Value, sum, start]);
end;

procedure TfrmParallelAggregateDemo.btnSumSequentialClick(Sender: TObject);
var
  i    : integer;
  start: int64;
  sum  : int64;
begin
  start := DSiTimeGetTime64;
  sum := 0;
  for i := 1 to inpMaxSummand.Value do
    Inc(sum, i);
  start := DSiTimeGetTime64 - start;
  Log('Sum(1..%d) = %d; calculation took %d ms', [inpMaxSummand.Value, sum, start]);
end;

procedure TfrmParallelAggregateDemo.FormCreate(Sender: TObject);
begin
  inpNumCPU.MaxValue := 64;
  inpNumCPU.Value := Environment.Process.Affinity.Count;
end;

function TfrmParallelAggregateDemo.IsPrime(i: integer): boolean;
var
  j: integer;
begin
  Result := false;
  if i <= 1 then
    Exit;
  for j := 2 to Round(Sqrt(i)) do
    if (i mod j) = 0 then
      Exit;
  Result := true;
end;

procedure TfrmParallelAggregateDemo.Log(const msg: string; const params: array of const);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format(msg, params));
end;

end.
