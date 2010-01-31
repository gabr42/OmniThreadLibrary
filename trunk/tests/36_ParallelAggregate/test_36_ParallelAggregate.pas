unit test_36_ParallelAggregate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TfrmParallelAggregateDemo = class(TForm)
    Label1: TLabel;
    inpMaxSummand: TSpinEdit;
    btnSumNumbers: TButton;
    Button2: TButton;
    Label2: TLabel;
    outSum: TEdit;
    procedure btnSumNumbersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmParallelAggregateDemo: TfrmParallelAggregateDemo;

implementation

uses
  OtlCommon,
  OtlParallel;

{$R *.dfm}

procedure TfrmParallelAggregateDemo.btnSumNumbersClick(Sender: TObject);
begin
  outSum.Text := IntToStr(
    Parallel.ForEach(1, inpMaxSummand.Value)
    .Aggregate(procedure (var aggregate: TOmniValue; const value: TOmniValue) begin
      aggregate.AsInt64 := aggregate.AsInt64 + value.AsInt64;
    end)
    .Execute(function (const value: TOmniValue): TOmniValue begin
      Result := value;
    end));
end;

end.
