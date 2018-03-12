unit test_67_ArrayToCollections;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  OtlCollections;

type
  TfrmArrayToCollection = class(TForm)
    btnTestInteger: TButton;
    btnTestString: TButton;
    btnTestRecord: TButton;
    btnTestObject: TButton;
    btnTestInterface: TButton;
    lbLog: TListBox;
    procedure btnTestIntegerClick(Sender: TObject);
    procedure btnTestStringClick(Sender: TObject);
    procedure btnTestRecordClick(Sender: TObject);
    procedure btnTestObjectClick(Sender: TObject);
    procedure btnTestInterfaceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmArrayToCollection: TfrmArrayToCollection;

implementation

uses
  Generics.Collections,
  DSiWin32,
  OtlCommon;

{$R *.dfm}

const
  CTestSize = 1000000;

type
  TTestRec = TPair<integer, string>;

  ITestIntf = interface ['{39846992-D898-4E4F-B652-23D3E44832F7}']
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

{ TfrmArrayToCollection }

procedure TfrmArrayToCollection.btnTestIntegerClick(Sender: TObject);
var
  arr : TArray<integer>;
  coll: IOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  SetLength(arr, CTestSize);
  for i := 1 to CTestSize do
    arr[i-1] := i;

  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.FromArray<integer>(arr);
  time := DSiElapsedTime64(time);

  Assert(coll.Count = CTestSize);
  for i := 1 to CTestSize do
    Assert(coll.Next = arr[i-1]);
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmArrayToCollection.btnTestInterfaceClick(Sender: TObject);
var
  arr : TArray<ITestIntf>;
  coll: IOmniBlockingCollection;
  i   : integer;
  intf: ITestIntf;
  time: int64;
begin
  SetLength(arr, CTestSize);
  for i := 1 to CTestSize do
    arr[i-1] := TTestObj.Create(i, IntToStr(i));

  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.FromArray<ITestIntf>(arr);
  time := DSiElapsedTime64(time);

  Assert(coll.Count = CTestSize);
  for i := 1 to CTestSize do begin
    intf := coll.Next.AsInterface as ITestIntf;
    Assert(intf.Key = arr[i-1].Key);
    Assert(intf.Value = arr[i-1].Value);
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmArrayToCollection.btnTestObjectClick(Sender: TObject);
var
  arr : TArray<TTestObj>;
  coll: IOmniBlockingCollection;
  i   : integer;
  obj : TTestObj;
  time: int64;
begin
  SetLength(arr, CTestSize);
  for i := 1 to CTestSize do
    arr[i-1] := TTestObj.Create(i, IntToStr(i));

  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.FromRange<TTestObj>(arr);
  time := DSiElapsedTime64(time);

  Assert(coll.Count = CTestSize);
  for i := 1 to CTestSize do begin
    obj := TTestObj(coll.Next.AsObject);
    Assert(obj.Key = arr[i-1].Key);
    Assert(obj.Value = arr[i-1].Value);
    obj.Free;
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmArrayToCollection.btnTestRecordClick(Sender: TObject);
var
  arr : TArray<TTestRec>;
  coll: IOmniBlockingCollection;
  i   : integer;
  rec : TTestRec;
  time: int64;
begin
  SetLength(arr, CTestSize);
  for i := 1 to CTestSize do begin
    arr[i-1].Key := i;
    arr[i-1].Value := IntToStr(i);
  end;

  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.FromArray<TTestRec>(arr);
  time := DSiElapsedTime64(time);

  Assert(coll.Count = CTestSize);
  for i := 1 to CTestSize do begin
    rec := coll.Next.ToRecord<TTestRec>;
    Assert(rec.Key = arr[i-1].Key);
    Assert(rec.Value = arr[i-1].Value);
  end;
  lbLog.Items.Add('Done ' + IntToStr(time));
end;

procedure TfrmArrayToCollection.btnTestStringClick(Sender: TObject);
var
  arr : TArray<string>;
  coll: TOmniBlockingCollection;
  i   : integer;
  time: int64;
begin
  SetLength(arr, CTestSize);
  for i := 1 to CTestSize do
    arr[i-1] := IntToStr(i);

  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.Create;
  coll.AddRange<string>(arr);
  time := DSiElapsedTime64(time);

  Assert(coll.Count = CTestSize);
  for i := 1 to CTestSize do
    Assert(coll.Next = arr[i-1]);
  lbLog.Items.Add('Done ' + IntToStr(time));

  coll.Free;
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
