unit test_32_Collections;

// TODO 1 -oPrimoz Gabrijelcic : Test interface passing

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
  OtlCollections,
  OtlEventMonitor;

type
  TfrmTestOtlCollections = class(TForm)
    lbLog     : TListBox;
    OtlMonitor: TOmniEventMonitor;
    btnTest   : TButton;
    btn2to2   : TButton;
    btn3to3: TButton;
    btn4to4: TButton;
    btn1to1: TButton;
    btnTestIntf: TButton;
    procedure btn1to1Click(Sender: TObject);
    procedure btn2to2Click(Sender: TObject);
    procedure btn3to3Click(Sender: TObject);
    procedure btn4to4Click(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnTestIntfClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OtlMonitorTaskTerminated(const task: IOmniTaskControl);
  private
    FChanCollection: TOmniCollection;
    FDstCollection : TOmniCollection;
    FForwarders    : array of IOmniTaskControl;
    FNumWorkers    : TGp4AlignedInt;
    FReaders       : array of IOmniTaskControl;
    FSrcCollection : TOmniCollection;
    FStartTime     : int64;
    FStopForwarders: boolean;
    FStopReaders   : boolean;
    procedure CheckResult;
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const params: array of const); overload;
    procedure LogCollectionStat(coll: TOmniCollection; const collName: string);
    procedure PrepareForwarders(numForwarders: integer);
    procedure PrepareReaders(numReaders: integer);
    procedure PrepareTest(numForwarders, numReaders: integer);
    procedure StopForwarders;
    procedure StopReaders;
    procedure StopWorkers;
    procedure WMRestartTest(var msg: TMessage); message WM_USER;
  end; { TfrmTestOtlCollections }

var
  frmTestOtlCollections: TfrmTestOtlCollections;

implementation

const
  CCountThreadedTest = 1000000;
  CCountSingleTest   = 100000;

{$R *.dfm}

procedure ForwarderWorker(const task: IOmniTask);
var
  chanColl: TOmniCollection;
  srcColl : TOmniCollection;
  stopFlag: PBoolean;
  value   : TOmniValue;
begin
  value := task.ParamByName['Source'];  srcColl := TOmniCollection(value.AsObject);
  value := task.ParamByName['Channel']; chanColl := TOmniCollection(value.AsObject);
  value := task.ParamByName['Stop'];    stopFlag := PBoolean(value.AsPointer);
  value := -1;
  while not stopFlag^ do
    while srcColl.TryDequeue(value) do begin
      chanColl.Enqueue(value);
      if value = CCountThreadedTest then begin
        stopFlag^ := true;
        break; //while
      end;
    end;
end; { ForwarderWorker }

procedure ReaderWorker(const task: IOmniTask);
var
  chanColl: TOmniCollection;
  dstColl : TOmniCollection;
  stopFlag: PBoolean;
  value   : TOmniValue;
begin
  value := task.ParamByName['Channel'];     chanColl := TOmniCollection(value.AsObject);
  value := task.ParamByName['Destination']; dstColl := TOmniCollection(value.AsObject);
  value := task.ParamByName['Stop'];        stopFlag := PBoolean(value.AsPointer);
  value := -1;
  while not stopFlag^ do
    while chanColl.TryDequeue(value) do begin
      dstColl.Enqueue(value);
      if value = CCountThreadedTest then begin
        stopFlag^ := true;
        break; //while
      end;
    end;
end; { ReaderWorker }

{ TfrmTestOtlCollections }

procedure TfrmTestOtlCollections.btn1to1Click(Sender: TObject);
begin
  PrepareTest(1, 1);
end; { TfrmTestOtlCollections.btn1to1Click }

procedure TfrmTestOtlCollections.btn2to2Click(Sender: TObject);
begin
  PrepareTest(2, 2);
end; { TfrmTestOtlCollections.btn2to2Click }

procedure TfrmTestOtlCollections.btn3to3Click(Sender: TObject);
begin
  PrepareTest(3, 3);
end; { TfrmTestOtlCollections.btn3to3Click }

procedure TfrmTestOtlCollections.btn4to4Click(Sender: TObject);
begin
  PrepareTest(4, 4);
end; { TfrmTestOtlCollections.btn4to4Click }

procedure TfrmTestOtlCollections.btnTestClick(Sender: TObject);
var
  coll : TOmniCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  coll := TOmniCollection.Create;
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
  Log('TOmniCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestClick }

procedure TfrmTestOtlCollections.btnTestIntfClick(Sender: TObject);
var
  coll : TOmniCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  coll := TOmniCollection.Create;
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
    end;
  finally FreeAndNil(coll); end;
  time := DSiTimeGetTime64 - time;
  Log('TOmniCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestIntfClick }

procedure TfrmTestOtlCollections.CheckResult;
var
  i: integer;
  testList: TGpIntegerList;
  value: TOmniValue;
begin
  LogCollectionStat(FSrcCollection, 'Source');
  LogCollectionStat(FChanCollection, 'Channel');
  LogCollectionStat(FDstCollection, 'Destination');
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
  StopWorkers;
end; { TfrmTestOtlCollections.CheckResult }

procedure TfrmTestOtlCollections.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  StopWorkers;
end; { TfrmTestOtlCollections.FormCloseQuery }

procedure TfrmTestOtlCollections.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOtlCollections.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOtlCollections.LogCollectionStat(coll: TOmniCollection; const collName:
  string);
begin
  {$IFDEF DEBUG}
  Log('%s: %6d / %6d / %3d / %3d / %3d / %3d / %3d / %3d / %3d / %3d / %3d / %3d / %3d', [collName,
    coll.NumEnqueued.Value, coll.NumDequeued.Value,
    coll.NumTrueAlloc.Value, coll.NumReusedAlloc.Value,
    coll.LoopEnqFree.Value, coll.LoopEnqEOL.Value, coll.LoopEnqExtending.Value,
    coll.LoopEnqOther.Value, coll.LoopDeqAllocated.Value, coll.LoopDeqRemoving.Value,
    coll.LoopDeqOther.Value, coll.LoopReader.Value, coll.LoopGC.Value]);
  {$ENDIF DEBUG}
end; { TfrmTestOtlCollections.LogCollectionStat }

procedure TfrmTestOtlCollections.OtlMonitorTaskTerminated(const task: IOmniTaskControl);
var
  time: int64;
begin
  if FNumWorkers.Decrement = 0 then begin
    time := DSiTimeGetTime64 - FStartTime;
    Log('All worker threads terminated, execution time = %d', [time]);
    CheckResult;
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
      .SetParameter('Stop', @FStopForwarders)
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
      .SetParameter('Stop', @FStopReaders)
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
  FStopForwarders := false;
  FStopReaders := false;
  FSrcCollection := TOmniCollection.Create;
  FDstCollection := TOmniCollection.Create;
  FChanCollection := TOmniCollection.Create;
  FNumWorkers.Value := numForwarders + numReaders;
  for i := 1 to CCountThreadedTest do
    FSrcCollection.Enqueue(i);
  FStartTime := DSiTimeGetTime64;
  PrepareReaders(numReaders);
  PrepareForwarders(numForwarders);
end; { TfrmTestOtlCollections.PrepareTest }

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
end;

procedure TfrmTestOtlCollections.WMRestartTest(var msg: TMessage);
begin
  btn2to2.Click;
end;

initialization
  Assert(SizeOf(cardinal) = SizeOf(pointer));
end.
