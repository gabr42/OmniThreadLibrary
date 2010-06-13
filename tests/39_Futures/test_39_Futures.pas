unit test_39_Futures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl;

type
  TOmniFutureDelegate<T> = reference to function: T;

  IOmniFuture<T> = interface
    function  Value: T;
  end; { IOmniFuture<T> }

  TOmniFuture<T> = class(TInterfacedObject, IOmniFuture<T>)
  private
    ofResult: T;
    ofTask  : IOmniTaskControl;
  public
    constructor Create(action: TOmniFutureDelegate<T>);
    function  Value: T;
  end; { TOmniFuture<T> }

  TfrmFuturesDemo = class(TForm)
    btnTestFuture1: TButton;
    lbLog: TListBox;
    btnTestFuture2: TButton;
    btnTestFuture3: TButton;
    procedure btnTestFuture1Click(Sender: TObject);
    procedure btnTestFuture2Click(Sender: TObject);
    procedure btnTestFuture3Click(Sender: TObject);
  private
    function CountPrimesTo(high: integer): TOmniFuture<integer>;
    function CountPrimesToHigh(high: integer): integer;
  public
  end;

var
  frmFuturesDemo: TfrmFuturesDemo;

implementation

const
  CPrimesHigh = 1000000;

{$R *.dfm}

function IsPrime(i: integer): boolean;
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

{ TfrmFuturesDemo }

function TfrmFuturesDemo.CountPrimesTo(high: integer): TOmniFuture<integer>;
begin
  Result := TOmniFuture<integer>.Create(function: integer
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

procedure TfrmFuturesDemo.btnTestFuture1Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  numPrimes := CountPrimesTo(CPrimesHigh);
  // do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

procedure TfrmFuturesDemo.btnTestFuture2Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
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
  // do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

function TfrmFuturesDemo.CountPrimesToHigh(high: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to CPrimesHigh do
    if IsPrime(i) then
      Inc(Result);
end;

procedure TfrmFuturesDemo.btnTestFuture3Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  numPrimes := TOmniFuture<integer>.Create(function: integer
    begin
      Result := CountPrimesToHigh(CPrimesHigh);
    end
  );
  // do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

{ TOmniFuture<T> }

constructor TOmniFuture<T>.Create(action: TOmniFutureDelegate<T>);
begin
  ofTask := CreateTask(procedure (const task: IOmniTask)
    begin
      ofResult := action();
    end,
     'TOmniFuture action').Run;
end; { TOmniFuture<T>.Create }

function TOmniFuture<T>.Value: T;
begin
  ofTask.Terminate;
  ofTask := nil;
  Result := ofResult;
end; { TOmniFuture<T>.Value }

end.
