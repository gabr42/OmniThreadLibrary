unit test_36_ParallelAggregate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmParallelAggregateDemo = class(TForm)
    Label1: TLabel;
    inpMaxSummand: TSpinEdit;
    btnSumParallel: TButton;
    lbLog: TListBox;
    btnSumSerial: TButton;
    procedure btnSumParallelClick(Sender: TObject);
    procedure btnSumSerialClick(Sender: TObject);
  private
    procedure Log(const msg: string; const params: array of const);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmParallelAggregateDemo: TfrmParallelAggregateDemo;

implementation

uses
  DSiWin32,
  OtlCommon,
  OtlParallel;

{$R *.dfm}

procedure TfrmParallelAggregateDemo.btnSumParallelClick(Sender: TObject);
var
  start: int64;
  sum  : int64;
begin
  start := DSiTimeGetTime64;
  sum :=
    Parallel.ForEach(1, inpMaxSummand.Value)
    .Aggregate(procedure (var aggregate: TOmniValue; const value: TOmniValue) begin
      aggregate.AsInt64 := aggregate.AsInt64 + value.AsInt64;
    end)
    .Execute(function (const value: TOmniValue): TOmniValue begin
      Result := value;
    end);
  start := DSiTimeGetTime64 - start;
  Log('Sum(1..%d) = %d; calculation took %d ms', [inpMaxSummand.Value, sum, start]);
end;

procedure TfrmParallelAggregateDemo.btnSumSerialClick(Sender: TObject);
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

procedure TfrmParallelAggregateDemo.Log(const msg: string; const params: array of const);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format(msg, params));
end;

end.
