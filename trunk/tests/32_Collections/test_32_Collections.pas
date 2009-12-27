unit test_32_Collections;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Contnrs,
  DSiWin32,
  GpLists,
  GpStuff,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlCollections,
  OtlEventMonitor;

type
  TfrmTestOtlCollections = class(TForm)
    btn1to1    : TButton;
    btn2to2    : TButton;
    btn3to3    : TButton;
    btn4to4    : TButton;
    btnTest    : TButton;
    btnTestIntf: TButton;
    cbRepeat   : TCheckBox;
    lbLog      : TListBox;
    OtlMonitor : TOmniEventMonitor;
    btn8to8: TButton;
    rgCollectionType: TRadioGroup;
    btn1to7: TButton;
    btn7to1: TButton;
    procedure btn1to7Click(Sender: TObject);
    procedure btn7to1Click(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnTestIntfClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OtlMonitorTaskTerminated(const task: IOmniTaskControl);
    procedure StartTest(Sender: TObject);
  private
    FChanCollection: TOmniBaseCollection;
    FDstCollection : TOmniBaseCollection;
    FForwarders    : array of IOmniTaskControl;
    FNumWorkers    : TGp4AlignedInt;
    FReaders       : array of IOmniTaskControl;
    FSrcCollection : TOmniBaseCollection;
    FStartTime     : int64;
    procedure CheckResult;
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const params: array of const); overload;
    procedure PrepareForwarders(numForwarders: integer);
    procedure PrepareReaders(numReaders: integer);
    procedure PrepareTest(numForwarders, numReaders: integer);
    procedure StopForwarders;
    procedure StopReaders;
    procedure StopWorkers;
    procedure WMRestartTest(var msg: TMessage); message WM_USER;
  strict protected
    function CreateCollection: TOmniBaseCollection;
  end; { TfrmTestOtlCollections }

var
  frmTestOtlCollections: TfrmTestOtlCollections;

implementation

const
  CCountThreadedTest = 1000000;
  CCountSingleTest   = 100000;

var
  GForwardersCount: TGp4AlignedInt;
  GReadersCount   : TGp4AlignedInt;
  GStopForwarders : boolean;
  GStopReaders    : boolean;

{$R *.dfm}

procedure ForwarderWorker(const task: IOmniTask);
var
  chanColl: TOmniBaseCollection;
  srcColl : TOmniBaseCollection;
  value   : TOmniValue;
begin
  value := task.ParamByName['Source'];  srcColl := TOmniBaseCollection(value.AsObject);
  value := task.ParamByName['Channel']; chanColl := TOmniBaseCollection(value.AsObject);
  while not GStopForwarders do
    while srcColl.TryDequeue(value) do begin
      chanColl.Enqueue(value);
      if GForwardersCount.Increment = CCountThreadedTest then begin
        GStopForwarders := true;
        break; //while
      end;
    end;
end; { ForwarderWorker }

procedure ReaderWorker(const task: IOmniTask);
var
  chanColl: TOmniBaseCollection;
  dstColl : TOmniBaseCollection;
  value   : TOmniValue;
begin
  value := task.ParamByName['Channel'];     chanColl := TOmniBaseCollection(value.AsObject);
  value := task.ParamByName['Destination']; dstColl := TOmniBaseCollection(value.AsObject);
  while not GStopReaders do
    while chanColl.TryDequeue(value) do begin
      dstColl.Enqueue(value);
      if GReadersCount.Increment = CCountThreadedTest then begin
        GStopReaders := true;
        break; //while
      end;
    end;
end; { ReaderWorker }

{ TfrmTestOtlCollections }

procedure TfrmTestOtlCollections.btn1to7Click(Sender: TObject);
begin
  PrepareTest(1, 7);
end; { TfrmTestOtlCollections.btn1to7Click }

procedure TfrmTestOtlCollections.btn7to1Click(Sender: TObject);
begin
  PrepareTest(7, 1);
end; { TfrmTestOtlCollections.btn7to1Click }

procedure TfrmTestOtlCollections.btnTestClick(Sender: TObject);
var
  coll : TOmniBaseCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  coll := CreateCollection;
  try
    for loop := 1 to 10 do begin
      for i := 1 to CCountSingleTest do
        coll.Enqueue(i);
      for i := 1 to CCountSingleTest do begin
        qi := coll.Dequeue;
        if qi.AsInteger <> i then
          raise Exception.CreateFmt('Expected %d', [i]);
      end;
      if coll.TryDequeue(value) then
        raise Exception.Create('Collection is not empty at the end');
    end;
  finally FreeAndNil(coll); end;
  time := DSiTimeGetTime64 - time;
  Log('TOmniBaseCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestClick }

procedure TfrmTestOtlCollections.btnTestIntfClick(Sender: TObject);
var
  coll : TOmniBaseCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  coll := CreateCollection;
  try
    for loop := 1 to 10 do begin
      for i := 1 to CCountSingleTest do
        coll.Enqueue(CreateCounter(i));
      for i := 1 to CCountSingleTest do begin
        qi := coll.Dequeue;
        if (qi.AsInterface as IOmniCounter).Value <> i then
          raise Exception.CreateFmt('Expected %d', [i]);
      end;
      if coll.TryDequeue(value) then
        raise Exception.Create('Collection is not empty at the end');
    end; //for loop
  finally FreeAndNil(coll); end;
  time := DSiTimeGetTime64 - time;
  Log('TOmniBaseCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestIntfClick }

procedure TfrmTestOtlCollections.CheckResult;
var
  i: integer;
  testList: TGpIntegerList;
  value: TOmniValue;
begin
  try
    testList := TGpIntegerList.Create;
    try
      while FDstCollection.TryDequeue(value) do
        testList.Add(value.AsInteger);
      testList.Sorted := true;
      if testList.Count <> CCountThreadedTest then
        raise Exception.CreateFmt('Expected %d items, got %d', [CCountThreadedTest, testList.Count]);
      for i := 1 to CCountThreadedTest do
        if testList[i-1] <> i then
          raise Exception.CreateFmt('Got value %d at position %d', [testList[i], i-1]);
    finally FreeAndNil(testList); end;
  finally StopWorkers; end;
end; { TfrmTestOtlCollections.CheckResult }

function TfrmTestOtlCollections.CreateCollection: TOmniBaseCollection;
begin
  if rgCollectionType.ItemIndex = 0 then
    Result := TOmniBaseCollection.Create
  else
    Result := TOmniCollection.Create;
end; { TfrmTestOtlCollections.CreateCollection }

procedure TfrmTestOtlCollections.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  StopWorkers;
end; { TfrmTestOtlCollections.FormCloseQuery }

procedure TfrmTestOtlCollections.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('[hh:nn:ss] ', Now) + msg);
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOtlCollections.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOtlCollections.OtlMonitorTaskTerminated(const task: IOmniTaskControl);
var
  time: int64;
begin
  if FNumWorkers.Decrement = 0 then begin
    time := DSiTimeGetTime64 - FStartTime;
    Log('All worker threads terminated, execution time = %d', [time]);
    CheckResult;
    if cbRepeat.Checked then
      PostMessage(Handle, WM_USER, 0, 0);
  end;
end; { TfrmTestOtlCollections.OtlMonitorTaskTerminated }

procedure TfrmTestOtlCollections.PrepareForwarders(numForwarders: integer);
var
  iForwarder: integer;
begin
  SetLength(FForwarders, numForwarders);
  for iForwarder := Low(FForwarders) to High(FForwarders) do begin
    FForwarders[iForwarder] :=
      CreateTask(ForwarderWorker, Format('Forwarder %d', [iForwarder]))
      .SetParameter('Source', FSrcCollection)
      .SetParameter('Channel', FChanCollection)
      .MonitorWith(OtlMonitor)
      .Run;
  end;
end; { TfrmTestOtlCollections.PrepareForwarders }

procedure TfrmTestOtlCollections.PrepareReaders(numReaders: integer);
var
  iReader: integer;
begin
  SetLength(FReaders, numReaders);
  for iReader := Low(FReaders) to High(FReaders) do begin
    FReaders[iReader] :=
      CreateTask(ReaderWorker, Format('Reader %d', [iReader]))
      .SetParameter('Channel', FChanCollection)
      .SetParameter('Destination', FDstCollection)
      .MonitorWith(OtlMonitor)
      .Run;
  end;
end; { TfrmTestOtlCollections.PrepareReaders }

procedure TfrmTestOtlCollections.PrepareTest(numForwarders, numReaders: integer);
var
  i: integer;
begin
  StopForwarders;
  StopReaders;
  GStopForwarders := false;
  GStopReaders := false;
  GForwardersCount.Value := 0;
  GReadersCount.Value := 0;
  Log('%d -> %d', [numForwarders, numReaders]);
  FSrcCollection := CreateCollection;
  FDstCollection := CreateCollection;
  FChanCollection := CreateCollection;
  FNumWorkers.Value := numForwarders + numReaders;
  for i := 1 to CCountThreadedTest do
    FSrcCollection.Enqueue(i);
  FStartTime := DSiTimeGetTime64;
  PrepareReaders(numReaders);
  PrepareForwarders(numForwarders);
end; { TfrmTestOtlCollections.PrepareTest }

procedure TfrmTestOtlCollections.StartTest(Sender: TObject);
begin
  PrepareTest(TButton(Sender).Tag, TButton(Sender).Tag);
end; { TfrmTestOtlCollections.StartTest }

procedure TfrmTestOtlCollections.StopForwarders;
var
  iForwarder: integer;
begin
  for iForwarder := Low(FForwarders) to High(FForwarders) do
    if assigned(FForwarders[iForwarder]) then begin
      FForwarders[iForwarder].Terminate;
      FForwarders[iForwarder] := nil;
    end;
  SetLength(FForwarders, 0);
end; { TfrmTestOtlCollections.StopForwarders }

procedure TfrmTestOtlCollections.StopReaders;
var
  iReader: integer;
begin
  for iReader := Low(FReaders) to High(FReaders) do
    if assigned(FReaders[iReader]) then begin
      FReaders[iReader].Terminate;
      FReaders[iReader] := nil;
    end;
  SetLength(FReaders, 0);
end; { TfrmTestOtlCollections.StopReaders }

procedure TfrmTestOtlCollections.StopWorkers;
begin
  StopForwarders;
  StopReaders;
  FreeAndNil(FSrcCollection);
  FreeAndNil(FDstCollection);
  FreeAndNil(FChanCollection);
end; { TfrmTestOtlCollections.StopWorkers }

procedure TfrmTestOtlCollections.WMRestartTest(var msg: TMessage);
begin
  PrepareTest(Random(8)+1, Random(8)+1);
end; { TfrmTestOtlCollections.WMRestartTest }

initialization
  Assert(SizeOf(cardinal) = SizeOf(pointer));
end.
