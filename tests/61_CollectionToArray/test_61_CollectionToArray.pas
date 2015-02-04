unit test_61_CollectionToArray;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlCollections, OtlCommon, OtlParallel;

type
  TfrmTestCollectionToArray = class(TForm)
    btnTestAsync: TButton;
    btnTestInteger  : TButton;
    btnTestInterface: TButton;
    btnTestObject   : TButton;
    btnTestRecord   : TButton;
    btnTestString   : TButton;
    lbLog           : TListBox;
    procedure btnTestAsyncClick(Sender: TObject);
    procedure btnTestIntegerClick(Sender: TObject);
    procedure btnTestInterfaceClick(Sender: TObject);
    procedure btnTestObjectClick(Sender: TObject);
    procedure btnTestRecordClick(Sender: TObject);
    procedure btnTestStringClick(Sender: TObject);
  end;

var
  frmTestCollectionToArray: TfrmTestCollectionToArray;

implementation

uses
  Generics.Collections,
  DSiWin32,
  GpStuff;

{$R *.dfm}

const
  CTestSize = 1000000;
  CPrimeCount = 78498; //https://primes.utm.edu/howmany.html
                       //set to -1 if you don't know

type
  TTestRec = TPair<integer, string>;

  ITestIntf = interface ['{434E3212-E330-4120-B42E-85E2E8F0C06C}']
    function GetKey: integer;
    function GetValue: string;
    procedure SetKey(const value: integer);
    procedure SetValue(const value: string);
  //
    property Key: integer read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
  end;

  TTestObj = class(TInterfacedObject, ITestIntf)
  strict private
    FKey: integer;
    FValue: string;
  strict protected
    function GetKey: integer;
    function GetValue: string;
    procedure SetKey(const value: integer);
    procedure SetValue(const value: string);
  public
    constructor Create(AKey: integer; const AValue: string);
    property Key: integer read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
  end;

function IsPrime(num: integer): boolean;
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

procedure TfrmTestCollectionToArray.btnTestAsyncClick(Sender: TObject);
var
  coll: TOmniBlockingCollection;
  arr: TArray<integer>;
begin
  //ToArray<T> converter with background data generation
  coll := TOmniBlockingCollection.Create;

  Parallel.Async(
    procedure
    var
      i: integer;
    begin
      for i := 1 to CTestSize do
        if IsPrime(i) then
          coll.Add(i);
      coll.CompleteAdding;
    end);

  //conversion will run in parallel with the generator above
  arr := TOmniBlockingCollection.ToArray<integer>(coll);
  //convertor will only exit when collection is 'completed' (i.e. when Parallel.Async finishes its job)

  if CPrimeCount > 0 then
    Assert(Length(arr) = CPrimeCount);
  lbLog.Items.Add(Format('%d primes, first five are %d, %d, %d, %d, %d', [
    Length(arr), arr[0], arr[1], arr[2], arr[3], arr[4]]));
end;

procedure TfrmTestCollectionToArray.btnTestIntegerClick(Sender: TObject);
var
  arr : TArray<integer>;
  coll: IOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  coll := TOmniBlockingCollection.Create;
  for i := 1 to CTestSize do
    coll.Add(i);
  coll.CompleteAdding;

  time := DSiTimeGetTime64;
  arr := TOmniBlockingCollection.ToArray<integer>(coll);
  time := DSiElapsedTime64(time);

  Assert(Length(arr) = CTestSize);
  for i := Low(arr) to High(arr) do
    Assert(arr[i] = (i+1));
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmTestCollectionToArray.btnTestInterfaceClick(Sender: TObject);
var
  arr : TArray<ITestIntf>;
  coll: IOmniBlockingCollection;
  i   : integer;
  intf: ITestIntf;
  time: int64;
begin
  coll := TOmniBlockingCollection.Create;
  for i := 1 to CTestSize do begin
    intf := TTestObj.Create(i, IntToStr(i));
    coll.Add(intf);
  end;
  coll.CompleteAdding;

  time := DSiTimeGetTime64;
  arr := TOmniBlockingCollection.ToArray<ITestIntf>(coll);
  time := DSiElapsedTime64(time);

  Assert(Length(arr) = CTestSize);
  for i := Low(arr) to High(arr) do begin
    Assert(arr[i].Key = (i+1));
    Assert(arr[i].Value = IntToStr(i+1));
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmTestCollectionToArray.btnTestObjectClick(Sender: TObject);
var
  arr : TArray<TTestObj>;
  coll: IOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  coll := TOmniBlockingCollection.Create;
  for i := 1 to CTestSize do
    coll.Add(TTestObj.Create(i, IntToStr(i)));
  coll.CompleteAdding;

  time := DSiTimeGetTime64;
  arr := TOmniBlockingCollection.ToArray<TTestObj>(coll);
  time := DSiElapsedTime64(time);

  Assert(Length(arr) = CTestSize);
  for i := Low(arr) to High(arr) do begin
    Assert(arr[i].Key = (i+1));
    Assert(arr[i].Value = IntToStr(i+1));
    arr[i].Free;
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmTestCollectionToArray.btnTestRecordClick(Sender: TObject);
var
  arr : TArray<TTestRec>;
  coll: IOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  coll := TOmniBlockingCollection.Create;
  for i := 1 to CTestSize do
    coll.Add(TOmniValue.FromRecord<TTestRec>(TTestRec.Create(i, IntToStr(i))));
  coll.CompleteAdding;

  time := DSiTimeGetTime64;
  arr := TOmniBlockingCollection.ToArray<TTestRec>(coll);
  time := DSiElapsedTime64(time);

  Assert(Length(arr) = CTestSize);
  for i := Low(arr) to High(arr) do begin
    Assert(arr[i].Key = (i+1));
    Assert(arr[i].Value = IntToStr(i+1));
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmTestCollectionToArray.btnTestStringClick(Sender: TObject);
var
  arr : TArray<string>;
  coll: IOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  coll := TOmniBlockingCollection.Create;
  for i := 1 to CTestSize do
    coll.Add(IntToStr(i));
  coll.CompleteAdding;

  time := DSiTimeGetTime64;
  arr := TOmniBlockingCollection.ToArray<string>(coll);
  time := DSiElapsedTime64(time);

  Assert(Length(arr) = CTestSize);
  for i := Low(arr) to High(arr) do
    Assert(arr[i] = IntToStr(i+1));
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

{ TTestObj }

constructor TTestObj.Create(AKey: integer; const AValue: string);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

function TTestObj.GetKey: integer;
begin
  Result := FKey;
end;

function TTestObj.GetValue: string;
begin
  Result := FValue;
end;

procedure TTestObj.SetKey(const value: integer);
begin
  FKey := value;
end;

procedure TTestObj.SetValue(const value: string);
begin
  FValue := value;
end;

end.
