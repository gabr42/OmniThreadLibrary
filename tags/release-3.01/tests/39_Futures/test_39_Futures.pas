unit test_39_Futures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlParallel;

type
  TfrmFuturesDemo = class(TForm)
    btnTestFuture1: TButton;
    lbLog: TListBox;
    btnTestFuture2: TButton;
    btnTestFuture3: TButton;
    btnTestFuture4: TButton;
    btnTestFuture5: TButton;
    btnTestFuture6: TButton;
    btnTestFuture7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestFuture1Click(Sender: TObject);
    procedure btnTestFuture2Click(Sender: TObject);
    procedure btnTestFuture3Click(Sender: TObject);
    procedure btnTestFuture4Click(Sender: TObject);
    procedure btnTestFuture5Click(Sender: TObject);
    procedure btnTestFuture6Click(Sender: TObject);
    procedure btnTestFuture7Click(Sender: TObject);
  private
    function CountPrimesTo(high: integer): IOmniFuture<integer>;
    function CountPrimesToHigh(high: integer): integer;
    function CountPrimesToMethod: integer;
  public
  end;

var
  frmFuturesDemo: TfrmFuturesDemo;

implementation

uses
  OtlCommon;

{$R *.dfm}
{$I OtlOptions.inc}

const
  CPrimesHigh = 1000000;

function IsPrime(i: integer): boolean;
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

function CountPrimesToFunc(const task: IOmniTask): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to CPrimesHigh do
    if IsPrime(i) then begin
      Inc(Result);
      if task.CancellationToken.IsSignalled then
        Exit;
    end;
end;

procedure TfrmFuturesDemo.FormCreate(Sender: TObject);
begin
  {$IFNDEF Unicode}
  btnTestFuture7.Enabled := false;
  {$ENDIF Unicode}
end;

{ TfrmFuturesDemo }

procedure TfrmFuturesDemo.btnTestFuture1Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  // Use future-generating method; call Value twice for testing.
  numPrimes := CountPrimesTo(CPrimesHigh);
//   do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture2Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  // Use fully coded delegate, create future twice for testing.
  numPrimes := TOmniFuture<integer>.Create(function: integer begin Result := 0 end);
  numPrimes := TOmniFuture<integer>.Create(function: integer
    var
      i: integer;
    begin
      Result := 0;
      for i := 1 to CPrimesHigh do
        if IsPrime(i) then
          Inc(Result);
    end
  );
//   do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture3Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  // Use function-calling delegate.
  numPrimes := TOmniFuture<integer>.Create(function: integer
    begin
      Result := CountPrimesToHigh(CPrimesHigh);
    end
  );
//   do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture4Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  // Use method instead of delegate, test IsDone.
  numPrimes := TOmniFuture<integer>.Create(CountPrimesToMethod);
  while not numPrimes.IsDone do begin
    Sleep(200);
    lbLog.Items.Add('... not yet done');
    lbLog.Update;
  end;
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture5Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  // Use function instead of delegate, test Cancel, IsCancelled and Value after cancellation (must raise exception).
  numPrimes := TOmniFuture<integer>.CreateEx(CountPrimesToFunc);
  Sleep(200);
  if not numPrimes.IsDone then begin
    lbLog.Items.Add('... not yet done, canceling');
    lbLog.Update;
    numPrimes.Cancel;
  end;
  lbLog.Items.Add(Format('... IsCancelled = %s', [BoolToStr(numPrimes.IsCancelled, true)]));
  lbLog.Items.Add(Format('... IsDone = %s', [BoolToStr(numPrimes.IsDone, true)]));
  lbLog.Update;
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture6Click(Sender: TObject);
var
  countPrimes: integer;
  numPrimes  : IOmniFuture<integer>;
begin
  // Test TryValue.
  numPrimes := TOmniFuture<integer>.Create(CountPrimesToMethod);
  while not numPrimes.TryValue(200, countPrimes) do begin
    lbLog.Items.Add('... not yet done');
    lbLog.Update;
  end;
  lbLog.Items.Add(Format('%d primes from 1 to %d', [countPrimes, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture7Click(Sender: TObject);
{$IFDEF Unicode}
var
  numPrimes: IOmniFuture<integer>;
{$ENDIF Unicode}
begin
  {$IFDEF Unicode}
  numPrimes := TOmniFuture<integer>.Create(function: integer
    begin
      Result := Parallel.ForEach(1, CPrimesHigh).AggregateSum.Execute(
        procedure (const value: integer; var result: TOmniValue)
        begin
          if IsPrime(value) then
            result := 1;
        end
      );
    end
  );
//   do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
  {$ENDIF Unicode}
end;

function TfrmFuturesDemo.CountPrimesTo(high: integer): IOmniFuture<integer>;
begin
  Result := Parallel.Future<integer>(function: integer
    var
      i: integer;
    begin
      Result := 0;
      for i := 1 to high do
        if IsPrime(i) then
          Inc(Result);
    end
  );
end;

function TfrmFuturesDemo.CountPrimesToHigh(high: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to high do
    if IsPrime(i) then
      Inc(Result);
end;

function TfrmFuturesDemo.CountPrimesToMethod: integer;
begin
  Result := CountPrimesToHigh(CPrimesHigh);
end;

end.
