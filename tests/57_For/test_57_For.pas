unit test_57_For;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  OtlParallel, OtlCommon;

type
  TfrmParallelForDemo = class(TForm)
    btnStart: TButton;
    lbLog: TListBox;
    procedure btnStartClick(Sender: TObject);
  private
    procedure Log(const msg: string);
  public
    { Public declarations }
  end;

var
  frmParallelForDemo: TfrmParallelForDemo;

implementation

{$R *.dfm}

type
  TBucket = array [0..9] of integer;

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

procedure TfrmParallelForDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

end.
