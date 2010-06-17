unit test_39_Futures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl;

type
  TOmniFutureDelegate<T> = reference to function: T;
  TOmniFutureFunction<T> = function: T;
  TOmniFutureMethod<T>   = function: T of object;

  IOmniFuture<T> = interface
    procedure Cancel;
    function  IsCanceled: boolean;
    function  IsDone: boolean;
    function  Value: T;
  end; { IOmniFuture<T> }

  { TODO : how to pass cancellation token to the action? }
  { TODO : maybe add Value(timeout_ms) }
  TOmniFuture<T> = class(TInterfacedObject, IOmniFuture<T>)
  strict private
    ofCanceled: boolean;
    ofResult  : T;
    ofTask    : IOmniTaskControl;
  public
    constructor Create(action: TOmniFutureDelegate<T>); overload;
    constructor Create(action: TOmniFutureFunction<T>); overload;
    constructor Create(action: TOmniFutureMethod<T>); overload;
    procedure Cancel;
    function  IsCanceled: boolean; inline;
    function  IsDone: boolean;
    function  Value: T;
  end; { TOmniFuture<T> }

  TfrmFuturesDemo = class(TForm)
    btnTestFuture1: TButton;
    lbLog: TListBox;
    btnTestFuture2: TButton;
    btnTestFuture3: TButton;
    btnTestFuture4: TButton;
    btnTestFuture5: TButton;
    procedure btnTestFuture1Click(Sender: TObject);
    procedure btnTestFuture2Click(Sender: TObject);
    procedure btnTestFuture3Click(Sender: TObject);
    procedure btnTestFuture4Click(Sender: TObject);
    procedure btnTestFuture5Click(Sender: TObject);
  private
    function CountPrimesTo(high: integer): TOmniFuture<integer>;
    function CountPrimesToHigh(high: integer): integer;
    function CountPrimesToMethod: integer;
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

function CountPrimesToFunc: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to CPrimesHigh do
    if IsPrime(i) then
      Inc(Result);
end;

{ TfrmFuturesDemo }

procedure TfrmFuturesDemo.btnTestFuture1Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  numPrimes := CountPrimesTo(CPrimesHigh);
  // do something else
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
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

procedure TfrmFuturesDemo.btnTestFuture4Click(Sender: TObject);
var
  numPrimes: IOmniFuture<integer>;
begin
  numPrimes := TOmniFuture<integer>.Create(CountPrimesToFunc);
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
  numPrimes := TOmniFuture<integer>.Create(CountPrimesToMethod);
  Sleep(200);
  if not numPrimes.IsDone then begin
    lbLog.Items.Add('... not yet done, canceling');
    lbLog.Update;
    numPrimes.Cancel;
  end;
  if numPrimes.IsCanceled then begin
    lbLog.Items.Add('... computation was canceled');
    lbLog.Update;
  end;
  lbLog.Items.Add(Format('%d primes from 1 to %d', [numPrimes.Value, CPrimesHigh]));
end;

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

{ TOmniFuture<T> }

constructor TOmniFuture<T>.Create(action: TOmniFutureDelegate<T>);
begin
  ofTask := CreateTask(procedure (const task: IOmniTask)
    begin
      ofResult := action();
    end,
     'TOmniFuture action').Schedule;
end; { TOmniFuture<T>.Create }

constructor TOmniFuture<T>.Create(action: TOmniFutureFunction<T>);
begin
  ofTask := CreateTask(procedure (const task: IOmniTask)
    begin
      ofResult := action();
    end,
     'TOmniFuture action').Schedule;
end; { TOmniFuture<T>.Create }

procedure TOmniFuture<T>.Cancel;
begin
  if IsCanceled then begin
    ofCanceled := true;
    if assigned(ofTask) then
      ofTask.CancellationToken.Signal;
  end;
end; { TOmniFuture }

constructor TOmniFuture<T>.Create(action: TOmniFutureMethod<T>);
begin
  ofTask := CreateTask(procedure (const task: IOmniTask)
    begin
      ofResult := action();
    end,
     'TOmniFuture action').Schedule;
end; { TOmniFuture<T>.Create }

function TOmniFuture<T>.IsCanceled: boolean;
begin
  Result := ofCanceled;
end; { TOmniFuture }

function TOmniFuture<T>.IsDone: boolean;
begin
  Result := (not assigned(ofTask)) or ofTask.WaitFor(0);
end; { TOmniFuture }

function TOmniFuture<T>.Value: T;
begin
  if assigned(ofTask) then begin
    ofTask.Terminate;
    ofTask := nil;
  end;
  Result := ofResult;
end; { TOmniFuture<T>.Value }

end.
