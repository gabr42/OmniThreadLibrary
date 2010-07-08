unit test_38_OrderedFor;

{ TODO 1 -ogabr : Test results in all demoes. }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmOderedForDemo = class(TForm)
    btnUnorderedPrimes1: TButton;
    lbLog: TListBox;
    btnOrderedPrimes: TButton;
    Button1: TButton;
    btnUnorderedPrimes2: TButton;
    procedure btnUnorderedPrimes1Click(Sender: TObject);
    procedure btnOrderedPrimesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnUnorderedPrimes2Click(Sender: TObject);
  private
    function IsPrime(i: integer): boolean;
  public
  end;

var
  frmOderedForDemo: TfrmOderedForDemo;

implementation

uses
  DSiWin32,
  OtlCommon,
  OtlCollections,
  OtlParallel;

{$R *.dfm}

function TfrmOderedForDemo.IsPrime(i: integer): boolean;
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

procedure TfrmOderedForDemo.btnUnorderedPrimes1Click(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: TOmniBlockingCollection;
begin
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait
    .OnStop(
      procedure
      begin
        primeQueue.CompleteAdding;
      end)
    .Execute(
      procedure (const value: integer)
      begin
        if IsPrime(value) then begin
          primeQueue.Add(value);
//          Sleep(200); // enable to see how results from different threads are added during the calculation
        end;
      end);
  for prime in primeQueue do begin
    lbLog.Items.Add(IntToStr(prime));
    lbLog.Update;
  end;
end;

procedure TfrmOderedForDemo.btnUnorderedPrimes2Click(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait.Into(primeQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
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
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1{000}).NumTasks(1).PreserveOrder.NoWait.Into(primeQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

procedure TfrmOderedForDemo.Button1Click(Sender: TObject);
var
  dataQueue  : IOmniBlockingCollection;
  prime      : TOmniValue;
  resultQueue: IOmniBlockingCollection;
begin
  lbLog.Clear;
  dataQueue := TOmniBlockingCollection.Create;
  resultQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 1000).NoWait.Into(dataQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end
  );
  Parallel.ForEach<integer>(dataQueue as IOmniValueEnumerable).NoWait.Into(resultQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      // Sophie Germain primes
      if IsPrime(2*value + 1) then
        res := value;
    end
  );
  for prime in resultQueue do
    lbLog.Items.Add(IntToStr(prime));
end;

end.
