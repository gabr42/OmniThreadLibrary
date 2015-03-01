unit test_57_For;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  GpStuff, OtlParallel, OtlCommon, OtlSync, OtlTask;

type
  TfrmParallelForDemo = class(TForm)
    btnStart: TButton;
    lbLog: TListBox;
    btnCancelWith: TButton;
    btnTest: TButton;
    procedure btnCancelWithClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    FCounter: TGp4AlignedInt;
    procedure Log(const msg: string);
    function Sum1: TOmniIteratorSimpleSimpleDelegate;
    function Sum2: TOmniIteratorSimpleDelegate;
    function Sum3: TOmniIteratorSimpleFullDelegate;
  public
    procedure Test(step, expected: integer);
    { Public declarations }
  end;

var
  frmParallelForDemo: TfrmParallelForDemo;

implementation

{$R *.dfm}

type
  TBucket = array [0..9] of integer;

procedure TfrmParallelForDemo.btnCancelWithClick(Sender: TObject);
var
  cancel: IOmniCancellationToken;
  cnt   : IOmniCounter;
begin
  cancel := CreateOmniCancellationToken;
  cnt := CreateCounter(0);
  Parallel.For(1, 100).CancelWith(cancel).Execute(
    procedure (value: integer)
    begin
      if value = 13 then
        cancel.Signal;
      cnt.Increment;
    end);
  Log('Total: ' + IntToStr(cnt.Value));
end;

procedure TfrmParallelForDemo.btnStartClick(Sender: TObject);
var
  testData: array of integer;
  i: integer;
  numTasks: integer;
  buckets: array of TBucket;
  j: integer;
  total: integer;
  s: string;
begin
  Log('Initializing data');

  //Create test data
  SetLength(testData, 100*1024*1024 {100 MB});
  Parallel.For(Low(testData), High(testData)).Execute(
    procedure (idx: integer)
    begin
      testData[idx] := Random(MaxInt);
    end);

  //Create intermediate buckets
  numTasks := Environment.Process.Affinity.Count;
  SetLength(buckets, numTasks);

  Log('Started processing');

  //Create repetition counts of last digits in random data sample
  Parallel.For(Low(testData), High(testData))
    .NumTasks(numTasks)
    .Initialize(
      procedure (taskIndex, fromIndex, toIndex: integer)
      begin
        FillChar(buckets[taskIndex], SizeOf(TBucket), 0);
      end)
    .Execute(
      procedure (taskIndex, idx: integer)
      var
        lastDigit: integer;
      begin
        lastDigit := testData[idx] mod 10;
        buckets[taskIndex][lastDigit] := buckets[taskIndex][lastDigit] + 1;
      end);

  //Aggregate & log results
  total := 0;
  for j := 0 to 9 do begin
    for i := 1 to numTasks - 1 do
      buckets[0][j] := buckets[0][j] + buckets[i][j];
    Log(Format('Random numbers end in %d %d times', [j, buckets[0][j]]));
    Inc(total, buckets[0][j]);
  end;
  if total = Length(testData) then s := 'CORRECT' else s := 'INCORRECT';
  Log(Format('Tested %d numbers, which is %s', [total, s]));
end;

procedure TfrmParallelForDemo.btnTestClick(Sender: TObject);
begin
  FCounter.Value := 0;
  Parallel.For(1, 100).Execute(Sum1());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(1, 100, 2).Execute(Sum1());
  Test(1, 100 * 100{99 is actual last element, 100 is skipped} div 4);

  FCounter.Value := 0;
  Parallel.For(100, 1, -1).Execute(Sum1());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(99, 0, -2).Execute(Sum1());
  Test(1, 100 * 100 div 4);

  FCounter.Value := 0;
  Parallel.For(1, 100).Execute(Sum2());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(1, 100, 2).Execute(Sum2());
  Test(1, 100 * 100 div 4);

  FCounter.Value := 0;
  Parallel.For(100, 1, -1).Execute(Sum2());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(99, 0, -2).Execute(Sum2());
  Test(1, 100 * 100 div 4);

  FCounter.Value := 0;
  Parallel.For(1, 100).Execute(Sum3());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(1, 100, 2).Execute(Sum3());
  Test(1, 100 * 100 div 4);

  FCounter.Value := 0;
  Parallel.For(100, 1, -1).Execute(Sum3());
  Test(1, 100 * 101 div 2);

  FCounter.Value := 0;
  Parallel.For(99, 0, -2).Execute(Sum3());
  Test(1, 100 * 100 div 4);
end;

procedure TfrmParallelForDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

function TfrmParallelForDemo.Sum1: TOmniIteratorSimpleSimpleDelegate;
begin
  Result := procedure (value: integer)
    begin
      FCounter.Add(value);
    end;
end;

function TfrmParallelForDemo.Sum2: TOmniIteratorSimpleDelegate;
begin
  Result := procedure (taskIndex, value: integer)
    begin
      FCounter.Add(value);
    end;
end;

function TfrmParallelForDemo.Sum3: TOmniIteratorSimpleFullDelegate;
begin
  Result := procedure (const task: IOmniTask; taskIndex, value: integer)
    begin
      FCounter.Add(value);
    end;
end;

procedure TfrmParallelForDemo.Test(step, expected: integer);
begin
  Log(Format('#%d: result = %d, expected = %d, %s', [step, FCounter.Value, expected,
    IFF(FCounter.Value = expected, 'OK', '*** ERROR ***')]));
end;

end.
