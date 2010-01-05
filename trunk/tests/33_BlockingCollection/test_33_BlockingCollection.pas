unit test_33_BlockingCollection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Contnrs, Spin,
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
  TfrmTestOmniBlockingCollection = class(TForm)
    btn1to1         : TButton;
    btn1to7         : TButton;
    btn2to2         : TButton;
    btn3to3         : TButton;
    btn4to4         : TButton;
    btn7to1         : TButton;
    btn8to8         : TButton;
    btnTest         : TButton;
    btnTestIntf     : TButton;
    cbRepeat        : TCheckBox;
    lbLog           : TListBox;
    OtlMonitor      : TOmniEventMonitor;
    rgCollectionType: TRadioGroup;
    lblNumCPU: TLabel;
    inpNumCPU: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure btn1to7Click(Sender: TObject);
    procedure btn7to1Click(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnTestIntfClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure inpNumCPUChange(Sender: TObject);
    procedure OtlMonitorTaskTerminated(const task: IOmniTaskControl);
    procedure StartTest(Sender: TObject);
  private
    FChanCollection: TOmniBlockingCollection;
    FDstCollection : TOmniBlockingCollection;
    FForwarders    : array of IOmniTaskControl;
    FNumWorkers    : TGp4AlignedInt;
    FReaders       : array of IOmniTaskControl;
    FSrcCollection : TOmniBlockingCollection;
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
    function  UseTryTake: boolean;
    procedure WMRestartTest(var msg: TMessage); message WM_USER;
  strict protected
  end; { TfrmTestOtlCollections }

var
  frmTestOmniBlockingCollection: TfrmTestOmniBlockingCollection;

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
  chanColl  : TOmniBlockingCollection;
  srcColl   : TOmniBlockingCollection;
  useTryTake: boolean;
  value     : TOmniValue;

  function MyTake: boolean;
  begin
    repeat
      if srcColl.TryTake(value) then begin
        Result := true;
        Exit;
      end
      else if srcColl.IsCompleted then begin
        Result := false;
        Exit;
      end
      else
        DSiYield;
    until false;
  end; { MyTake }

begin
  value := task.ParamByName['Source'];     srcColl := TOmniBlockingCollection(value.AsObject);
  value := task.ParamByName['Channel'];    chanColl := TOmniBlockingCollection(value.AsObject);
  useTryTake := task.ParamByName['UseTryTake'];
  repeat
    if useTryTake then begin
      if not MyTake then
        break //repeat
    end
    else if not srcColl.Take(value) then
      break; //repeat
    chanColl.Add(value);
    if GForwardersCount.Increment = CCountThreadedTest then begin
      GStopForwarders := true;
      chanColl.CompleteAdding;
      break; //repeat
    end;
  until false;
end; { ForwarderWorker }

procedure ReaderWorker(const task: IOmniTask);
var
  chanColl  : TOmniBlockingCollection;
  dstColl   : TOmniBlockingCollection;
  useTryTake: boolean;
  value     : TOmniValue;

  function MyTake: boolean;
  begin
    repeat
      if chanColl.TryTake(value) then begin
        Result := true;
        Exit;
      end
      else if chanColl.IsCompleted then begin
        Result := false;
        Exit;
      end
      else
        DSiYield;
    until false;
  end; { MyTake }

  
begin
  value := task.ParamByName['Channel'];     chanColl := TOmniBlockingCollection(value.AsObject);
  value := task.ParamByName['Destination']; dstColl := TOmniBlockingCollection(value.AsObject);
  useTryTake := task.ParamByName['UseTryTake'];
  repeat
    if useTryTake then begin
      if not MyTake then
        break; //repeat
    end
    else if not chanColl.Take(value) then
      break; //repeat
    dstColl.Add(value);
    if GReadersCount.Increment = CCountThreadedTest then begin
      GStopReaders := true;
      dstColl.CompleteAdding;
      break; //while
    end;
  until false;
end; { ReaderWorker }

{ TfrmTestOtlCollections }

procedure TfrmTestOmniBlockingCollection.FormCreate(Sender: TObject);
begin
  inpNumCPU.MaxValue := Environment.Process.Affinity.Count;
  inpNumCPU.Value := inpNumCPU.MaxValue;
end; { TfrmTestOmniBlockingCollection.FormCreate }

procedure TfrmTestOmniBlockingCollection.btn1to7Click(Sender: TObject);
begin
  PrepareTest(1, 7);
end; { TfrmTestOtlCollections.btn1to7Click }

procedure TfrmTestOmniBlockingCollection.btn7to1Click(Sender: TObject);
begin
  PrepareTest(7, 1);
end; { TfrmTestOtlCollections.btn7to1Click }

procedure TfrmTestOmniBlockingCollection.btnTestClick(Sender: TObject);
var
  coll : TOmniBlockingCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  for loop := 1 to 10 do begin
    coll := TOmniBlockingCollection.Create;
    try
      for i := 1 to CCountSingleTest do
        coll.Add(i);
      coll.CompleteAdding;
      for i := 1 to CCountSingleTest do begin
        if not coll.Take(qi) then
          raise Exception.CreateFmt('Take failed at element %d', [i]);
        if qi.AsInteger <> i then
          raise Exception.CreateFmt('Expected %d', [i]);
      end;
      if coll.TryTake(value) then
        raise Exception.Create('Collection is not empty at the end');
    finally FreeAndNil(coll); end;
  end;
  time := DSiTimeGetTime64 - time;
  Log('TOmniBlockingCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestClick }

procedure TfrmTestOmniBlockingCollection.btnTestIntfClick(Sender: TObject);
var
  coll : TOmniBlockingCollection;
  i    : integer;
  loop : integer;
  qi   : TOmniValue;
  time : int64;
  value: TOmniValue;
begin
  time := DSiTimeGetTime64;
  coll := TOmniBlockingCollection.Create;
  try
    for loop := 1 to 10 do begin
      for i := 1 to CCountSingleTest do
        coll.Add(CreateCounter(i));
      coll.CompleteAdding;
      for i := 1 to CCountSingleTest do begin
        if not coll.Take(qi) then
          raise Exception.CreateFmt('Take failed at element %d', [i]);
        if (qi.AsInterface as IOmniCounter).Value <> i then
          raise Exception.CreateFmt('Expected %d', [i]);
      end;
      if coll.TryTake(value) then
        raise Exception.Create('Collection is not empty at the end');
    end; //for loop
  finally FreeAndNil(coll); end;
  time := DSiTimeGetTime64 - time;
  Log('TOmniBlockingCollection, 10x (%d enqueues and %0:d dequeues), %d ms', [CCountSingleTest, time]);
end; { TfrmTestOtlCollections.btnTestIntfClick }

procedure TfrmTestOmniBlockingCollection.CheckResult;
var
  i: integer;
  testList: TGpIntegerList;
  value: TOmniValue;
begin
  try
    testList := TGpIntegerList.Create;
    try
      while FDstCollection.Take(value) do
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

procedure TfrmTestOmniBlockingCollection.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  StopWorkers;
end; { TfrmTestOtlCollections.FormCloseQuery }

procedure TfrmTestOmniBlockingCollection.inpNumCPUChange(Sender: TObject);
begin
  Environment.Process.Affinity.Count := inpNumCPU.Value;
end; { TfrmTestOmniBlockingCollection.inpNumCPUChange }

procedure TfrmTestOmniBlockingCollection.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('[hh:nn:ss] ', Now) + msg);
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOmniBlockingCollection.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end; { TfrmTestOtlCollections.Log }

procedure TfrmTestOmniBlockingCollection.OtlMonitorTaskTerminated(const task: IOmniTaskControl);
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

procedure TfrmTestOmniBlockingCollection.PrepareForwarders(numForwarders: integer);
var
  iForwarder: integer;
begin
  SetLength(FForwarders, numForwarders);
  for iForwarder := Low(FForwarders) to High(FForwarders) do begin
    FForwarders[iForwarder] :=
      CreateTask(ForwarderWorker, Format('Forwarder %d', [iForwarder]))
      .SetParameter('Source', FSrcCollection)
      .SetParameter('Channel', FChanCollection)
      .SetParameter('UseTryTake', UseTryTake)
      .MonitorWith(OtlMonitor)
      .Run;
  end;
end; { TfrmTestOtlCollections.PrepareForwarders }

procedure TfrmTestOmniBlockingCollection.PrepareReaders(numReaders: integer);
var
  iReader: integer;
begin
  SetLength(FReaders, numReaders);
  for iReader := Low(FReaders) to High(FReaders) do begin
    FReaders[iReader] :=
      CreateTask(ReaderWorker, Format('Reader %d', [iReader]))
      .SetParameter('Channel', FChanCollection)
      .SetParameter('Destination', FDstCollection)
      .SetParameter('UseTryTake', UseTryTake)
      .MonitorWith(OtlMonitor)
      .Run;
  end;
end; { TfrmTestOtlCollections.PrepareReaders }

procedure TfrmTestOmniBlockingCollection.PrepareTest(numForwarders, numReaders: integer);
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
  FSrcCollection := TOmniBlockingCollection.Create;
  FDstCollection := TOmniBlockingCollection.Create;
  FChanCollection := TOmniBlockingCollection.Create;
  FNumWorkers.Value := numForwarders + numReaders;
  for i := 1 to CCountThreadedTest do
    FSrcCollection.Add(i);
  FSrcCollection.CompleteAdding;
  FStartTime := DSiTimeGetTime64;
  PrepareReaders(numReaders);
  PrepareForwarders(numForwarders);
end; { TfrmTestOtlCollections.PrepareTest }

procedure TfrmTestOmniBlockingCollection.StartTest(Sender: TObject);
begin
  PrepareTest(TButton(Sender).Tag, TButton(Sender).Tag);
end; { TfrmTestOtlCollections.StartTest }

procedure TfrmTestOmniBlockingCollection.StopForwarders;
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

procedure TfrmTestOmniBlockingCollection.StopReaders;
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

procedure TfrmTestOmniBlockingCollection.StopWorkers;
begin
  StopForwarders;
  StopReaders;
  FreeAndNil(FSrcCollection);
  FreeAndNil(FDstCollection);
  FreeAndNil(FChanCollection);
end; { TfrmTestOtlCollections.StopWorkers }

function TfrmTestOmniBlockingCollection.UseTryTake: boolean;
begin
  if rgCollectionType.ItemIndex = 0 then
    Result := false
  else if rgCollectionType.ItemIndex = 1 then
    Result := true
  else
    Result := Random(2) = 1;
end; { TfrmTestOmniBlockingCollection.UseTryTake }

procedure TfrmTestOmniBlockingCollection.WMRestartTest(var msg: TMessage);
begin
  PrepareTest(Random(8)+1, Random(8)+1);
end; { TfrmTestOtlCollections.WMRestartTest }

initialization
  Assert(SizeOf(cardinal) = SizeOf(pointer));
end.
