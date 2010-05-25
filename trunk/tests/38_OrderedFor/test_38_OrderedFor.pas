unit test_38_OrderedFor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmOderedForDemo = class(TForm)
    btnUnorderedPrimes: TButton;
    lbLog: TListBox;
    btnOrderedPrimes: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure btnUnorderedPrimesClick(Sender: TObject);
    procedure btnOrderedPrimesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function IsPrime(i: integer): boolean;
  public
  end;

var
  frmOderedForDemo: TfrmOderedForDemo;

implementation

uses
  OtlCommon,
  OtlCollections,
  OtlParallel;

{$R *.dfm}

function TfrmOderedForDemo.IsPrime(i: integer): boolean;
var
  j: integer;
begin
  Result := false;
  if i <= 0 then
    Exit;
  for j := 2 to Round(Sqrt(i)) do
    if (i mod j) = 0 then
      Exit;
  Result := true;
end;

procedure TfrmOderedForDemo.btnUnorderedPrimesClick(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait
    .OnStop(
      procedure
      begin
        primeQueue.CompleteAdding;
      end)
    .Execute(
      procedure (value: int64)
      begin
        if IsPrime(value) then
          primeQueue.Add(value);
      end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

procedure TfrmOderedForDemo.Button3Click(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait.Into(primeQueue).Execute(
    procedure (value: int64; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

procedure TfrmOderedForDemo.btnOrderedPrimesClick(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).Into(primeQueue).PreserveOrder.NoWait.Execute(
    procedure (value: int64; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

procedure TfrmOderedForDemo.Button1Click(Sender: TObject);
var
  prime      : TOmniValue;
  resultQueue: IOmniBlockingCollection;
begin
  resultQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait.Execute(
    procedure (value: int64; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end
  )
  .ForEach.Into(resultQueue).NoWait.Execute(
    procedure (value: int64; var res: TOmniValue)
    begin
      // Sophie Germain primes
      if IsPrime(2*value + 1) then
        res := value;
    end
  );
  for prime in resultQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

procedure TfrmOderedForDemo.Button2Click(Sender: TObject);
var
  prime: TOmniValue;
begin
  for prime in
    Parallel.ForEach(1, 1000).Execute(
      procedure (value: int64; var res: TOmniValue)
      begin
        if IsPrime(value) then
          res := value;
      end
    )
    .Enumerate // returns TOmniBlockingCollection
  do
    lbLog.Items.Add(IntToStr(prime));
end;

end.
