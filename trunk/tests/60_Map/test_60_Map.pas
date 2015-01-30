unit test_60_Map;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlParallel, OtlTask;

type
  TfrmTestParallelMap = class(TForm)
    btnMap: TButton;
    lbLog: TListBox;
    btnMap2: TButton;
    btnSerial: TButton;
    btnParallel: TButton;
    procedure btnMap2Click(Sender: TObject);
    procedure btnMapClick(Sender: TObject);
    procedure btnParallelClick(Sender: TObject);
    procedure btnSerialClick(Sender: TObject);
  private
    FMapper: IOmniParallelMapper<integer,string>;
    FTestData: TArray<integer>;
    function IsPrime(num: integer): boolean;
    procedure LogResult(const res: TArray<integer>; time_ms: int64);
    function MapOdds(const source: integer; var dest: string): boolean;
    procedure PrepareTestData;
    function ToString(const arr: TArray<string>): string; reintroduce;
    procedure ShowResult(const task: IOmniTask);
  public
  end;

var
  frmTestParallelMap: TfrmTestParallelMap;

implementation

uses
  DSiWin32;

const
  CSourceSize = 50;
  CLargeSize = 1000000;

{$R *.dfm}

procedure TfrmTestParallelMap.btnMapClick(Sender: TObject);
var
  i      : integer;
  numbers: TArray<integer>;
  odds   : TArray<string>;
begin
  SetLength(numbers, CSourceSize);
  for i := Low(numbers) to High(numbers) do
    numbers[i] := i;

  odds := Parallel.Map<integer,string>(numbers, MapOdds);

  lbLog.Items.Add(ToString(odds));
end;

procedure TfrmTestParallelMap.btnMap2Click(Sender: TObject);
var
  i      : integer;
  numbers: TArray<integer>;
begin
  SetLength(numbers, CSourceSize);
  for i := Low(numbers) to High(numbers) do
    numbers[i] := i;

  FMapper := Parallel.Map<integer,string>;
  FMapper.Source(numbers, true);
  FMapper.NumTasks(4);
  FMapper.NoWait;
  FMapper.OnStop(ShowResult);
  FMapper.Execute(MapOdds);
end;

procedure TfrmTestParallelMap.btnParallelClick(Sender: TObject);
var
  output: TArray<integer>;
  time  : int64;
begin
  PrepareTestData;

  time := DSiTimeGetTime64;
  output := Parallel.Map<integer,integer>(FTestData,
    function (const source: integer; var target: integer): boolean
    begin
      Result := IsPrime(source);
      target := source;
    end);
  time := DSiElapsedTime64(time);

  LogResult(output, time);
end;

procedure TfrmTestParallelMap.btnSerialClick(Sender: TObject);
var
  data  : integer;
  outIdx: integer;
  output: TArray<integer>;
  time  : int64;
begin
  PrepareTestData;

  time := DSiTimeGetTime64;
  SetLength(output, Length(FTestData));
  outIdx := Low(output) - 1;
  for data in FTestData do
    if IsPrime(data) then begin
      Inc(outIdx);
      output[outIdx] := data;
    end;
  SetLength(output, outIdx + 1);
  time := DSiElapsedTime64(time);

  LogResult(output, time);
end;

function TfrmTestParallelMap.IsPrime(num: integer): boolean;
var
  j: integer;
begin
  Result := false;
  if num <= 1 then
    Exit;
  for j := 2 to Round(Sqrt(num)) do
    if (num mod j) = 0 then
      Exit;
  Result := true;
end;

procedure TfrmTestParallelMap.LogResult(const res: TArray<integer>; time_ms: int64);
var
  i  : integer;
  sum: int64;
begin
  sum := 0;
  for i := Low(res) to High(res) do
    Inc(sum, res[i]);
  lbLog.Items.Add(Format('%d ms, %d primes, sum = %d', [time_ms, Length(res), sum]));
end;

function TfrmTestParallelMap.MapOdds(const source: integer; var dest: string): boolean;
begin
  Result := Odd(source);
  if Result then
    dest := IntTostr(source);
end;

procedure TfrmTestParallelMap.PrepareTestData;
var
  i: integer;
begin
  SetLength(FTestData, CLargeSize);
  for i := Low(FTestData) to High(FTestData) do
    FTestData[i] := 1000000 + i;
end;

procedure TfrmTestParallelMap.ShowResult(const task: IOmniTask);
begin
  //we are still in a background thread so schedule work for the main thread
  task.Invoke(
    procedure
    begin
      lbLog.Items.Add(ToString(FMapper.Result));
      FMapper := nil;
    end);
end;

function TfrmTestParallelMap.ToString(const arr: TArray<string>): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(arr) to High(arr) do begin
    if i > Low(arr) then
      Result := Result + ',';
    Result := Result + arr[i];
  end;
end;

end.
