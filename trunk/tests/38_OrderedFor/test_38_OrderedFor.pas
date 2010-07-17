unit test_38_OrderedFor;

{ TODO 1 -ogabr : Test results in all demoes. }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  OtlEventMonitor,
  OtlThreadPool;

type
  TfrmOderedForDemo = class(TForm)
    btnUnorderedPrimes1: TButton;
    lbLog: TListBox;
    btnOrderedPrimes: TButton;
    btnUnorderedPrimes2: TButton;
    btnUnorderedCancel: TButton;
    cbRepeatTest: TCheckBox;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    procedure btnUnorderedPrimes1Click(Sender: TObject);
    procedure btnOrderedPrimesClick(Sender: TObject);
    procedure btnUnorderedPrimes2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FEventMonitor: TOmniEventMonitor;
    procedure HandleThreadCreated(const pool: IOmniThreadPool; threadID: integer);
    procedure HandleThreadDestroying(const pool: IOmniThreadPool; threadID: integer);
    procedure HandleThreadKilled(const pool: IOmniThreadPool; threadID: integer);
    function  IsPrime(i: integer): boolean;
    function  NumCores: integer;
    procedure VerifyResult;
    procedure RepeatTest;
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

const
  CMaxTest = 20000;

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

function TfrmOderedForDemo.NumCores: integer;
begin
  Result := Random(Environment.Process.Affinity.Count*2) + 1;
  lbLog.Items.Add(Format('Running on %d cores', [Result]));
end;

procedure TfrmOderedForDemo.RepeatTest;
begin
  case Random(3) of
    0: btnUnorderedPrimes1.Click;
    1: btnUnorderedPrimes2.Click;
    2: btnOrderedPrimes.Click;
  end;
end;

procedure TfrmOderedForDemo.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  RepeatTest;
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
  if lbLog.Items.Count <= 1 then
    lbLog.Items.Add('Error, empty result list!')
  else begin
    error := false;
    order := 'ordered';
    value := StrToInt(lbLog.Items[1]);
    primes := TGpIntegerList.Create;
    try
      primes.Add(value);
      for iItem := 2 to lbLog.Items.Count - 1 do begin
        value2 := StrToInt(lbLog.Items[iItem]);
        if value2 <= value then
          order := 'unordered';
        primes.Add(value2);
        value := value2;
      end;
      primes.Sort;
      for iItem := 1 to CMaxTest do begin
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
          Timer1.Enabled := true;
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
  Parallel.ForEach(1, CMaxTest).NumTasks(NumCores).NoWait
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
//    lbLog.Update;
    Application.ProcessMessages; // because we want to receive pool notification messages
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
  Parallel.ForEach(1, CMaxTest).NoWait.NumTasks(NumCores).Into(primeQueue).Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
    end);
  for prime in primeQueue do begin
    lbLog.Items.Add(IntToStr(prime));
//    lbLog.Update;
    Application.ProcessMessages; // because we want to receive pool notification messages
  end;
  VerifyResult;
  btnUnorderedPrimes2.Enabled := true;
end;

procedure TfrmOderedForDemo.FormCreate(Sender: TObject);
begin
  FEventMonitor := TOmniEventMonitor.Create(Self);
  FEventMonitor.OnPoolThreadCreated := HandleThreadCreated;
  FEventMonitor.OnPoolThreadDestroying := HandleThreadDestroying;
  FEventMonitor.OnPoolThreadKilled := HandleThreadKilled;
  GForEachPool.MonitorWith(FEventMonitor);
end;

procedure TfrmOderedForDemo.HandleThreadCreated(const pool: IOmniThreadPool;
  threadID: integer);
begin
  StatusBar1.SimpleText := Format('Thread %d created', [threadID]);
  StatusBar1.Update;
end;

procedure TfrmOderedForDemo.HandleThreadDestroying(const pool: IOmniThreadPool;
  threadID: integer);
begin
  StatusBar1.SimpleText := Format('Thread %d destroyed', [threadID]);
end;

procedure TfrmOderedForDemo.HandleThreadKilled(const pool: IOmniThreadPool;
  threadID: integer);
begin
  StatusBar1.SimpleText := Format('Thread %d killed', [threadID]);
end;

procedure TfrmOderedForDemo.btnOrderedPrimesClick(Sender: TObject);
var
  prime     : TOmniValue;
  primeQueue: IOmniBlockingCollection;
begin
  (Sender as TButton).Enabled := false;
  lbLog.Clear;
  primeQueue := TOmniBlockingCollection.Create;
  Parallel.ForEach(1, CMaxTest)
    .CancelWith(GOmniCancellationToken)
    .NumTasks(NumCores)
    .PreserveOrder
    .NoWait
    .Into(primeQueue)
    .Execute(
    procedure (const value: integer; var res: TOmniValue)
    begin
      if IsPrime(value) then
        res := value;
      if (Sender = btnUnorderedCancel) and (value = 511 {arbitrary}) then
        GOmniCancellationToken.Signal;
    end);
  for prime in primeQueue do begin
    lbLog.Items.Add(IntToStr(prime));
//    lbLog.Update;
    Application.ProcessMessages; // because we want to receive pool notification messages
  end;
  VerifyResult;
  GOmniCancellationToken.Clear;
  (Sender as TButton).Enabled := true;
end;

end.
