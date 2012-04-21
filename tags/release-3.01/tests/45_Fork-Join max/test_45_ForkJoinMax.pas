unit test_45_ForkJoinMax;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlParallel;

const
  CNumDataPoints = 10000000;

type
  TData = array of integer;
  PData = ^TData;

  TfrmQuickSortDemo = class(TForm)
    btnMaxOnAll: TButton;
    btnMaxOnOne: TButton;
    lbLog            : TListBox;
    procedure btnMaxOnAllClick(Sender: TObject);
    procedure btnMaxOnOneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FData   : TData;
    procedure GenerateData;
    function  ParallelMax(const forkJoin: IOmniForkJoin<integer>; left, right: integer):
      integer;
    function  SequentialMax(left, right: integer): integer;
  end;

var
  frmQuickSortDemo: TfrmQuickSortDemo;

implementation

uses
  Math,
  DSiWin32;

{$R *.dfm}

{$R-}

const
  CSeqThreshold = 1000;

{ TfrmTestOTL }

procedure TfrmQuickSortDemo.btnMaxOnAllClick(Sender: TObject);
var
  max : integer;
  time: int64;
begin
  time := DSiTimeGetTime64;

  max := ParallelMax(Parallel.ForkJoin<integer>, Low(FData), High(FData));

  lbLog.ItemIndex := lbLog.Items.Add(Format('Max = %d, found in %d ms', [max, DSiTimeGetTime64 - time]));
end; { TfrmQuickSortDemo.btnMaxOnAllClick }

procedure TfrmQuickSortDemo.btnMaxOnOneClick(Sender: TObject);
var
  max : integer;
  time: int64;
begin
  time := DSiTimeGetTime64;

  max := SequentialMax(Low(FData), High(FData));

  lbLog.ItemIndex := lbLog.Items.Add(Format('Max = %d, found in %d ms', [max, DSiTimeGetTime64 - time]));
end; { TfrmQuickSortDemo.btnMaxOnOneClick }

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

function TfrmQuickSortDemo.ParallelMax(const forkJoin: IOmniForkJoin<integer>; left,
  right: integer): integer;
var
  computeLeft : IOmniCompute<integer>;
  computeRight: IOmniCompute<integer>;
  mid         : integer;

  function Compute(left, right: integer): IOmniCompute<integer>;
  begin
    Result := forkJoin.Compute(
      function: integer
      begin
        Result := ParallelMax(forkJoin, left, right);
      end
    );
  end; { Compute }

begin { TfrmQuickSortDemo.ParallelMax }
  if (right - left) < CSeqThreshold then
    Result := SequentialMax(left, right)
  else begin
    mid := (left + right) div 2;
    computeLeft := Compute(left, mid);
    computeRight := Compute(mid + 1, right);
    Result := Max(computeLeft.Value, computeRight.Value);
    // THIS IS WRONG:
    // Result := Max(Compute(left, mid).Value, Compute(mid + 1, right).Value);
  end;
end; { TfrmQuickSortDemo.ParallelMax }

function TfrmQuickSortDemo.SequentialMax(left, right: integer): integer;
var
  iData: integer;
begin
  Result := FData[left];
  for iData := left + 1 to right do
    if FData[iData] > Result then
      Result := FData[iData];
end; { TfrmQuickSortDemo.SequentialMax }

initialization
  Randomize;
end.
