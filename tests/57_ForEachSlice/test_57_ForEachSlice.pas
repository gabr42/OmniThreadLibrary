unit test_57_ForEachSlice;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlParallel, OtlCommon;

type
  TfrmForEachSlice = class(TForm)
    btnStart: TButton;
    lbLog: TListBox;
    procedure btnStartClick(Sender: TObject);
  private
    procedure Log(const msg: string);
  public
    { Public declarations }
  end;

var
  frmForEachSlice: TfrmForEachSlice;

implementation

{$R *.dfm}

type
  TBucket = array [0..9] of integer;

procedure TfrmForEachSlice.btnStartClick(Sender: TObject);
var
  testData: array of integer;
  i: integer;
  numTasks: integer;
  buckets: array of TBucket;
  j: Integer;
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
      procedure (taskIndex: integer)
      begin
        FillChar(buckets[taskIndex], SizeOf(TBucket), 0);
      end)
    .Execute(
      procedure (taskIndex, idx: integer)
      var
        lastDigit: integer;
      begin
        lastDigit := testData[idx] div 10;
        buckets[taskIndex][lastDigit] := buckets[taskIndex][lastDigit] + 1;
      end);

  //Aggregate results
  for i := 1 to numTasks - 1 do
    for j := 0 to 9 do
      buckets[0][j] := buckets[0][j] + buckets[i][j];
end;

procedure TfrmForEachSlice.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

end.
