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
    btnUnorderedCancel: TButton;
    cbRepeatTest: TCheckBox;
    procedure btnUnorderedPrimes1Click(Sender: TObject);
    procedure btnOrderedPrimesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnUnorderedPrimes2Click(Sender: TObject);
  private
    function IsPrime(i: integer): boolean;
    procedure VerifyResult;
    procedure RepeatTest(var msg: TMessage); message WM_USER;
  public
  end;

var
  frmOderedForDemo: TfrmOderedForDemo;

implementation

uses
  DSiWin32,
  GpLists,
  OtlCommon,
  OtlSync,
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

procedure TfrmOderedForDemo.RepeatTest(var msg: TMessage);
begin
  case Random(2) of
    0: btnUnorderedPrimes1.Click;
    1: btnUnorderedPrimes2.Click;
    2: btnOrderedPrimes.Click;
  end;
end;

procedure TfrmOderedForDemo.VerifyResult;
var
  iItem: integer;
  order: string;
  value: integer;
  value2: integer;
  primes: TGpIntegerList;
  error: boolean;
begin
  if lbLog.Items.Count = 0 then
    lbLog.Items.Add('Error, empty result list!')
  else begin
    error := false;
    order := 'ordered';
    value := StrToInt(lbLog.Items[0]);
    primes := TGpIntegerList.Create;
    try
      primes.Add(value);
      for iItem := 1 to lbLog.Items.Count - 1 do begin
        value2 := StrToInt(lbLog.Items[iItem]);
        if value2 <= value then
          order := 'unordered';
        primes.Add(value2);
        value := value2;
      end;
      primes.Sort;
      for iItem := 1 to 2000 do begin
        if IsPrime(iItem) then begin
          if not primes.Contains(iItem) then begin
            error := true;
            break; //for
          end
        end
        else if primes.Contains(iItem) then begin
          error := true;
          break; //for
        end;
      end; //for
      if error then
        lbLog.Items.Add('ERROR, list is ' + order)
      else begin
        lbLog.Items.Add('OK, list is ' + order);
        if cbRepeatTest.Checked then
          PostMessage(Handle, WM_USER, 0, 0);
      end;
    finally FreeAndNil(primes); end;
  end;
  lbLog.ItemIndex := lbLog.Items.Count - 1;
end;

procedure TfrmOderedForDemo.btnUnorderedPrimes1Click(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  btnUnorderedPrimes1.Enabled := false;
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 2000).NoWait
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
  VerifyResult;
  btnUnorderedPrimes1.Enabled := true;
end;

procedure TfrmOderedForDemo.btnUnorderedPrimes2Click(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  btnUnorderedPrimes2.Enabled := false;
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 2000).NoWait.Into(primeQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
  VerifyResult;
  btnUnorderedPrimes2.Enabled := true;
end;

procedure TfrmOderedForDemo.btnOrderedPrimesClick(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  (Sender as TButton).Enabled := false;
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, 2000).CancelWith(GOmniCancellationToken).PreserveOrder.NoWait.Into(primeQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
      if (Sender = btnUnorderedCancel) and (value = 511 {arbitrary}) then
        GOmniCancellationToken.Signal;
    end);
  for prime in primeQueue do
    lbLog.Items.Add(IntToStr(prime));
  VerifyResult;
  GOmniCancellationToken.Clear;
  (Sender as TButton).Enabled := true;
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
  Parallel.ForEach(1, 2000).NoWait.Into(dataQueue).Execute(
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
