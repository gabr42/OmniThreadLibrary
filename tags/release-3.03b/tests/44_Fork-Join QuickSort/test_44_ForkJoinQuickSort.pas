unit test_44_ForkJoinQuickSort;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlParallel;

const
  CNumDataPoints = 5000000;

type
  TData = array of integer;
  PData = ^TData;

  TfrmQuickSortDemo = class(TForm)
    btnSortOnAll     : TButton;
    btnSortOnOne     : TButton;
    lbLog            : TListBox;
    procedure btnSortOnAllClick(Sender: TObject);
    procedure btnSortOnOneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FData   : TData;
    procedure GenerateData;
    procedure VerifyData;
  end;

var
  frmQuickSortDemo: TfrmQuickSortDemo;

implementation

uses
  DSiWin32;

{$R *.dfm}

{$R-}

const
  CSortThreshold = 1000;

type
  TSorter = class
  strict protected
    FData: TData;
    constructor Create(const data: TData);
    destructor  Destroy; override;
    procedure InsertionSort(left, right: integer);
    function  Partition(left, right, pivotIndex: integer): integer;
    procedure QuickSort(dataLow, dataHigh: integer); virtual; abstract;
  end;

  TSequentialSorter = class(TSorter)
  strict protected
    procedure QuickSort(left, right: integer); override;
  public
    class procedure Sort(const data: TData);
  end;

  TParallelSorter = class(TSorter)
  strict private
    FForkJoin: IOmniForkJoin;
  strict protected
    constructor Create(const data: TData);
    procedure QuickSort(left, right: integer); override;
  public
    class procedure Sort(const data: TData);
  end;

{ TfrmTestOTL }

procedure TfrmQuickSortDemo.btnSortOnAllClick(Sender: TObject);
var
  time: int64;
begin
  time := DSiTimeGetTime64;

  TParallelSorter.Sort(FData);

  lbLog.ItemIndex := lbLog.Items.Add(Format('Sorted in %d ms', [DSiTimeGetTime64 - time]));
  VerifyData;
end;

procedure TfrmQuickSortDemo.btnSortOnOneClick(Sender: TObject);
var
  time: int64;
begin
  time := DSiTimeGetTime64;

  TSequentialSorter.Sort(FData);

  lbLog.ItemIndex := lbLog.Items.Add(Format('Sorted in %d ms', [DSiTimeGetTime64 - time]));
  VerifyData;
end;

procedure TfrmQuickSortDemo.FormCreate(Sender: TObject);
begin
  GenerateData;
end; { TfrmQuickSortDemo.FormCreate }

procedure TfrmQuickSortDemo.GenerateData;
var
  iData: integer;
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Generating %d pseudorandom numbers',
    [CNumDataPoints]));
  lblog.Update;
  RandSeed := CNumDataPoints;
  SetLength(FData, CNumDataPoints);
  for iData := Low(FData) to High(FData) do
    FData[iData] := Random(High(integer));
  lbLog.ItemIndex := lbLog.Items.Add('Generated');
end; { TfrmQuickSortDemo.GenerateData }

procedure TfrmQuickSortDemo.VerifyData;
var
  iData: integer;
begin
  lbLog.ItemIndex := lbLog.Items.Add('Verifying...');
  for iData := Low(FData) to High(FData) - 1 do
    if FData[iData] > FData[iData + 1] then
      raise Exception.CreateFmt('[%d]%d > [%d]%d', [iData, FData[iData], iData+1, FData[iData+1]]);
  lbLog.ItemIndex := lbLog.Items.Add('OK');
end; { TfrmQuickSortDemo.VerifyData }

{ TSorter }

constructor TSorter.Create(const data: TData);
begin
  inherited Create;
  FData := data;
end; { TSorter.Create }

destructor TSorter.Destroy;
begin
  inherited;
end;

procedure TSorter.InsertionSort(left, right: integer);
var
  i    : integer;
  j    : integer;
  value: integer;
begin
  for i := left + 1 to right do begin
    value := FData[i];
    j := i - 1;
    repeat
      if FData[j] > value then begin
        FData[j+1] := FData[j];
        j := j - 1;
        if j < 0 then
          break; //repeat
      end
      else
        break; //repeat
    until false;
    FData[j+1] := value;
  end;
end; { TSorter.InsertionSort }

function TSorter.Partition(left, right, pivotIndex: integer): integer;
var
  i         : integer;
  pivotValue: integer;
  storeIndex: integer;
  tmp       : integer;
begin
  pivotValue := FData[pivotIndex];
  tmp := FData[pivotIndex]; FData[pivotIndex] := FData[right]; FData[right] := tmp;
  storeIndex := left;
  for i := left to right - 1 do
    if FData[i] <= pivotValue then begin
      tmp := FData[i]; FData[i] := FData[storeIndex]; FData[storeIndex] := tmp;
      Inc(storeIndex);
    end;
  tmp := FData[storeIndex]; FData[storeIndex] := FData[right]; FData[right] := tmp;
  Result := storeIndex;
end; { TSorter.Partition }

{ TSequentialSorter }

procedure TSequentialSorter.QuickSort(left, right: integer);
var
  pivotIndex: integer;
begin
  if right > left then begin
    if (right - left) <= CSortThreshold then
      InsertionSort(left, right)
    else begin
      pivotIndex := Partition(left, right, (left + right) div 2);
      QuickSort(left, pivotIndex - 1);
      QuickSort(pivotIndex + 1, right);
    end;
  end;
end; { TSequentialSorter.QuickSort }

class procedure TSequentialSorter.Sort(const data: TData);
begin
  with TSequentialSorter.Create(data) do try
    QuickSort(Low(data), High(data));
  finally {with.}Free; end;
end; { TSequentialSorter.Sort }

{ TParallelSorter }

constructor TParallelSorter.Create(const data: TData);
begin
  inherited Create(data);
  FForkJoin := Parallel.ForkJoin;
end; { TParallelSorter.Create }

procedure TParallelSorter.QuickSort(left, right: integer);
var
  pivotIndex: integer;
  sortLeft  : IOmniCompute;
  sortRight : IOmniCompute;
begin
  if right > left then begin
    if (right - left) <= CSortThreshold then
      InsertionSort(left, right)
    else begin
      pivotIndex := Partition(left, right, (left + right) div 2);
      sortLeft := FForkJoin.Compute(
        procedure
        begin
          QuickSort(left, pivotIndex - 1);
        end);
      sortRight := FForkJoin.Compute(
        procedure
        begin
          QuickSort(pivotIndex + 1, right);
        end);
      sortLeft.Await;
      sortRight.Await;
    end;
  end;
end; { TParallelSorter.QuickSort }

class procedure TParallelSorter.Sort(const data: TData);
begin
  with TParallelSorter.Create(data) do try
    QuickSort(Low(data), High(data));
  finally {with.}Free; end;
end; { TParallelSorter.Sort }

initialization
  Randomize;
end.
