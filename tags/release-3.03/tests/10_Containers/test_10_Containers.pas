unit test_10_Containers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Contnrs,
  GpLists,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlEventMonitor;

type
  TfrmTestOtlContainers = class(TForm)
    btnAllTests                : TButton;
    btnBaseQueue1to2           : TButton;
    btnBaseQueue1to4           : TButton;
    btnBaseQueue2to1           : TButton;
    btnBaseQueue2to2           : TButton;
    btnBaseQueue4to1           : TButton;
    btnBaseQueue4to4           : TButton;
    btnBaseQueueAll            : TButton;
    btnBaseQueueCorrectnessTest: TButton;
    btnBaseQueueStressTest     : TButton;
    btnBaseStack1to2           : TButton;
    btnBaseStack1to4           : TButton;
    btnBaseStack2to1           : TButton;
    btnBaseStack2to2           : TButton;
    btnBaseStack4to1           : TButton;
    btnBaseStack4to4           : TButton;
    btnBaseStackAll            : TButton;
    btnBaseStackCorrectnessTest: TButton;
    btnBaseStackStressTest     : TButton;
    btnClearLog                : TButton;
    btnQueue1to2               : TButton;
    btnQueue1to4               : TButton;
    btnQueue2to1               : TButton;
    btnQueue2to2               : TButton;
    btnQueue4to1               : TButton;
    btnQueue4to4               : TButton;
    btnQueueAll                : TButton;
    btnQueueCorrectnessTest    : TButton;
    btnQueueStressTest         : TButton;
    btnSaveLog                 : TButton;
    btnStack1to2               : TButton;
    btnStack1to4               : TButton;
    btnStack2to1               : TButton;
    btnStack2to2               : TButton;
    btnStack4to1               : TButton;
    btnStack4to4               : TButton;
    btnStackAll                : TButton;
    btnStackCorrectnessTest    : TButton;
    btnStackStressTest         : TButton;
    inpTestDuration_sec        : TLabeledEdit;
    lbLog                      : TListBox;
    OmniEventMonitor1          : TOmniEventMonitor;
    SaveDialog                 : TSaveDialog;
    procedure btnAllTestsClick(Sender: TObject);
    procedure btnBaseQueueAllClick(Sender: TObject);
    procedure btnBaseQueueStressTestClick(Sender: TObject);
    procedure btnBaseStackAllClick(Sender: TObject);
    procedure btnBaseStackStressTestClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnQueueAllClick(Sender: TObject);
    procedure btnQueueCorrectnessTestClick(Sender: TObject);
    procedure btnQueueStressTestClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnStackAllClick(Sender: TObject);
    procedure btnStackCorrectnessTestClick(Sender: TObject);
    procedure btnStackStressTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  private
    FAllTestsStart   : int64;
    FBaseQueue       : TOmniBaseBoundedQueue;
    FBaseStack       : TOmniBaseBoundedStack;
    FCounter         : IOmniCounter;
    FNumReaders      : integer;
    FNumWriters      : integer;
    FQueue           : TOmniBoundedQueue;
    FQueuedTests     : TGpIntegerObjectList;
    FReaders         : array [1..4] of IOmniTaskControl;
    FReaderThroughput: integer;
    FStack           : TOmniBoundedStack;
    FTestName        : string;
    FWriters         : array [1..4] of IOmniTaskControl;
    FWriterThroughput: integer;
    function  GetTestDuration_sec: integer;
    procedure Log(const msg: string);
    procedure ScheduleAllBaseQueueTests;
    procedure ScheduleAllBaseStackTests;
    procedure ScheduleAllQueueTests;
    procedure ScheduleAllStackTests;
    procedure StartBaseQueueStressTest(numWriters, numReaders: integer);
    procedure StartBaseStackStressTest(numWriters, numReaders: integer);
    procedure StartFirstTest;
    procedure StartQueueStressTest(numWriters, numReaders: integer);
    procedure StartStackStressTest(numWriters, numReaders: integer);
  strict protected
    procedure AllocateTasks(numWriters, numReaders: integer);
  end;

var
  frmTestOtlContainers: TfrmTestOtlContainers;

implementation

uses
  DSiWin32,
  GpStuff;

{$R *.dfm}

const
  CTestQueueLength  = 1000;

  // GUI -> thread messages
  MSG_START_STACK_STRESS_TEST      = 1;
  MSG_START_QUEUE_STRESS_TEST      = 2;
  MSG_START_STACK_WRITE            = 3;
  MSG_START_STACK_READ             = 4;
  MSG_START_QUEUE_WRITE            = 5;
  MSG_START_QUEUE_READ             = 6;
  MSG_START_BASE_STACK_STRESS_TEST = 7;
  MSG_START_BASE_QUEUE_STRESS_TEST = 8;

  // thread -> GUI messages
  MSG_TEST_END              = 100;
  MSG_TEST_FAILED           = 101;
  MSG_STACK_WRITE_COMPLETED = 102;
  MSG_STACK_READ_COMPLETED  = 103;
  MSG_QUEUE_WRITE_COMPLETED = 104;
  MSG_QUEUE_READ_COMPLETED  = 105;
  MSG_FULL_STOP             = 106;
  MSG_WRITER_THROUGHPUT     = 107;
  MSG_READER_THROUGHPUT     = 108;

type
  TTestSuite = (tsDump, tsMessageExchange);

  TCommTester = class(TOmniWorker)
  strict private
    ctBaseQueue: TOmniBaseBoundedQueue;
    ctBaseStack: TOmniBaseBoundedStack;
    ctQueue    : TOmniBoundedQueue;
    ctStack    : TOmniBoundedStack;
  strict protected
    procedure Fail(const reason: string);
  public
    constructor Create(baseStack: TOmniBaseBoundedStack; baseQueue: TOmniBaseBoundedQueue; stack:
      TOmniBoundedStack; queue: TOmniBoundedQueue);
    property BaseQueue: TOmniBaseBoundedQueue read ctBaseQueue;
    property BaseStack: TOmniBaseBoundedStack read ctBaseStack;
    property Queue: TOmniBoundedQueue read ctQueue;
    property Stack: TOmniBoundedStack read ctStack;
  end; { TCommTester }

  TCommWriter = class(TCommTester)
  public
    procedure OMStartBaseQueueStressTest(var msg: TOmniMessage); message MSG_START_BASE_QUEUE_STRESS_TEST;
    procedure OMStartBaseStackStressTest(var msg: TOmniMessage); message MSG_START_BASE_STACK_STRESS_TEST;
    procedure OMStartQueueStressTest(var msg: TOmniMessage); message MSG_START_QUEUE_STRESS_TEST;
    procedure OMStartQueueWrite(var msg: TOmniMessage); message MSG_START_QUEUE_WRITE;
    procedure OMStartStackStressTest(var msg: TOmniMessage); message MSG_START_STACK_STRESS_TEST;
    procedure OMStartStackWrite(var msg: TOmniMessage); message MSG_START_STACK_WRITE;
  end;

  TCommReader = class(TCommTester)
  public
    procedure OMStartBaseQueueStressTest(var msg: TOmniMessage); message MSG_START_BASE_QUEUE_STRESS_TEST;
    procedure OMStartBaseStackStressTest(var msg: TOmniMessage); message MSG_START_BASE_STACK_STRESS_TEST;
    procedure OMStartQueueRead(var msg: TOmniMessage); message MSG_START_QUEUE_READ;
    procedure OMStartQueueStressTest(var msg: TOmniMessage); message MSG_START_QUEUE_STRESS_TEST;
    procedure OMStartStackRead(var msg: TOmniMessage); message MSG_START_STACK_READ;
    procedure OMStartStackStressTest(var msg: TOmniMessage); message MSG_START_STACK_STRESS_TEST;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestOtlContainers.AllocateTasks(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  for iTask := 1 to numReaders do
    if not assigned(FReaders[iTask]) then
      FReaders[iTask] := OmniEventMonitor1.Monitor(CreateTask(
        TCommReader.Create(FBaseStack, FBaseQueue, FStack, FQueue), 'reader'))
        .WithCounter(FCounter)
        .Run;
  for iTask := 1 to numWriters do
    if not assigned(FWriters[iTask]) then
      FWriters[iTask] := OmniEventMonitor1.Monitor(CreateTask(
        TCommWriter.Create(FBaseStack, FBaseQueue, FStack, FQueue), 'writer'))
        .WithCounter(FCounter)
        .Run;
end;

procedure TfrmTestOtlContainers.btnAllTestsClick(Sender: TObject);
begin
  ScheduleAllBaseStackTests;
  ScheduleAllStackTests;
  ScheduleAllBaseQueueTests;
  ScheduleAllQueueTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestOtlContainers.btnBaseQueueAllClick(Sender: TObject);
begin
  ScheduleAllBaseQueueTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestOtlContainers.btnBaseQueueStressTestClick(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TButton).Tag;
  StartBaseQueueStressTest(tag div 10, tag mod 10);
end;

procedure TfrmTestOtlContainers.btnBaseStackAllClick(Sender: TObject);
begin
  ScheduleAllBaseStackTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestOtlContainers.btnBaseStackStressTestClick(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TButton).Tag;
  StartBaseStackStressTest(tag div 10, tag mod 10);
end;

procedure TfrmTestOtlContainers.btnClearLogClick(Sender: TObject);
begin
  lbLog.Clear;
end;

procedure TfrmTestOtlContainers.btnQueueAllClick(Sender: TObject);
begin
  ScheduleAllQueueTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestOtlContainers.btnQueueCorrectnessTestClick(Sender: TObject);
var
  container: TOmniBaseBoundedQueue;
begin
  Log('Writing to queue');
  if Sender = btnBaseQueueCorrectnessTest then
    container := FBaseQueue
  else
    container := FQueue;
  container.Empty;
  FWriters[1].Comm.Send(MSG_START_QUEUE_WRITE, [CTestQueueLength, cardinal(container)]);
end;

procedure TfrmTestOtlContainers.btnQueueStressTestClick(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TButton).Tag;
  StartQueueStressTest(tag div 10, tag mod 10);
end;

procedure TfrmTestOtlContainers.btnSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    lbLog.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmTestOtlContainers.btnStackAllClick(Sender: TObject);
begin
  ScheduleAllStackTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestOtlContainers.btnStackCorrectnessTestClick(Sender: TObject);
var
  container: TOmniBaseBoundedStack;
begin
  Log('Writing to stack');
  if Sender = btnBaseStackCorrectnessTest then
    container := FBaseStack
  else
    container := FStack;
  container.Empty;
  FWriters[1].Comm.Send(MSG_START_STACK_WRITE, [CTestQueueLength, cardinal(container)]);
end;

procedure TfrmTestOtlContainers.btnStackStressTestClick(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TButton).Tag;
  StartStackStressTest(tag div 10, tag mod 10);
end;

procedure TfrmTestOtlContainers.FormCreate(Sender: TObject);
begin
  {$IFDEF CPUX64}Log('64-bit');{$ELSE}Log('32-bit');{$ENDIF}
  FBaseStack := TOmniBaseBoundedStack.Create;
  FBaseStack.Initialize(CTestQueueLength, SizeOf(integer));
  FStack := TOmniBoundedStack.Create(CTestQueueLength, SizeOf(integer));
  FBaseQueue := TOmniBaseBoundedQueue.Create;
  FBaseQueue.Initialize(CTestQueueLength, SizeOf(integer));
  FQueue := TOmniBoundedQueue.Create(CTestQueueLength, SizeOf(integer));
  FCounter := CreateCounter;
  FQueuedTests := TGpIntegerObjectList.Create(false);
  AllocateTasks(1, 1);
end;

procedure TfrmTestOtlContainers.FormDestroy(Sender: TObject);
var
  iTask: integer;
begin
  for iTask := Low(FWriters) to High(FWriters) do
    if assigned(FWriters[iTask]) then begin
      FWriters[iTask].Terminate;
      FWriters[iTask] := nil;
    end;
  for iTask := Low(FReaders) to High(FReaders) do
    if assigned(FReaders[iTask]) then begin
      FReaders[iTask].Terminate;
      FReaders[iTask] := nil;
    end;
  FreeAndNil(FQueuedTests);
  FreeAndNil(FBaseStack);
  FreeAndNil(FStack);
  FreeAndNil(FBaseQueue);
  FreeAndNil(FQueue);
end;

function TfrmTestOtlContainers.GetTestDuration_sec: integer;
var
  c: integer;
begin
  Val(inpTestDuration_sec.Text, Result, c);
  if c <> 0 then
    Result := 10;
end; { TfrmTestOtlContainers.GetTestDuration_sec }

procedure TfrmTestOtlContainers.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn ', Now) + msg);
  Application.ProcessMessages;
end;

procedure TfrmTestOtlContainers.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
var
  f  : textfile;
begin
  case msg.MsgID of
    MSG_FULL_STOP:
      begin
        Log('All tasks stopped');
        Log(Format('Total reader throughput: %d msg/s', [FReaderThroughput]));
        Log(Format('Total writer throughput: %d msg/s', [FWriterThroughput]));
        if msg.MsgData = 'base stack' then
          btnBaseStackCorrectnessTest.Click
        else if msg.MsgData = 'stack' then
          btnStackCorrectnessTest.Click
        else if msg.MsgData = 'base queue' then
          btnBaseQueueCorrectnessTest.Click
        else
          btnQueueCorrectnessTest.Click;
        AssignFile(f, 'containers_bench.csv');
        if FileExists('containers_bench.csv') then
          Append(f)
        else
          Rewrite(f);
        Writeln(f, '"', FTestName, '",', FNumReaders, ',', FNumWriters, ',',
          FReaderThroughput, ',', FWriterThroughput);
        CloseFile(f);
      end;
    MSG_TEST_END:
      Log(string(msg.MsgData));
    MSG_WRITER_THROUGHPUT:
      Inc(FWriterThroughput, msg.MsgData);
    MSG_READER_THROUGHPUT:
      Inc(FReaderThroughput, msg.MsgData);
    MSG_STACK_WRITE_COMPLETED:
      begin
        Log('Reading from stack');
        FReaders[1].Comm.Send(MSG_START_STACK_READ,
          [CTestQueueLength, cardinal(msg.MsgData)]);
      end;
    MSG_STACK_READ_COMPLETED, MSG_QUEUE_READ_COMPLETED:
      begin
        Log('Test completed');
        if FQueuedTests.Count > 0 then begin
          FQueuedTests.Delete(0);
          if FQueuedTests.Count = 0 then
            Log(Format('All tests completed. Total run time = %d seconds',
              [Round((DSiTimeGetTime64 - FAllTestsStart)/1000)]))
          else
            StartFirstTest;
        end;
      end;
    MSG_QUEUE_WRITE_COMPLETED:
      begin
        Log('Reading from queue');
        FReaders[1].Comm.Send(MSG_START_QUEUE_READ,
          [CTestQueueLength, cardinal(msg.MsgData)]);
      end;
    MSG_TEST_FAILED:
      Log('Write test failed. ' + msg.MsgData);
    else
      Log(Format('Unknown message %d', [msg.MsgID]));
  end; //case
end;

procedure TfrmTestOtlContainers.ScheduleAllBaseQueueTests;
var
  numReaders: integer;
  numWriters: integer;
begin
  for numWriters in [1, 2, 4] do
    for numReaders in [1, 2, 4] do
      FQueuedTests.AddObject(10*numWriters + numReaders, btnBaseQueueStressTest);
end;

procedure TfrmTestOtlContainers.ScheduleAllBaseStackTests;
var
  numReaders: integer;
  numWriters: integer;
begin
  for numWriters in [1, 2, 4] do
    for numReaders in [1, 2, 4] do
      FQueuedTests.AddObject(10*numWriters + numReaders, btnBaseStackStressTest);
end;

procedure TfrmTestOtlContainers.ScheduleAllQueueTests;
var
  numReaders: integer;
  numWriters: integer;
begin
  for numWriters in [1, 2, 4] do
    for numReaders in [1, 2, 4] do
      FQueuedTests.AddObject(10*numWriters + numReaders, btnQueueStressTest);
end;

procedure TfrmTestOtlContainers.ScheduleAllStackTests;
var
  numReaders: integer;
  numWriters: integer;
begin
  for numWriters in [1, 2, 4] do
    for numReaders in [1, 2, 4] do
      FQueuedTests.AddObject(10*numWriters + numReaders, btnStackStressTest);
end;

procedure TfrmTestOtlContainers.StartBaseQueueStressTest(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second base queue stress test, %d -> %d',
    [GetTestDuration_sec, numWriters, numReaders]));
  FTestName := 'base queue';
  FNumWriters := numWriters;
  FNumReaders := numReaders;
  FWriterThroughput := 0;
  FReaderThroughput := 0;
  AllocateTasks(numWriters, numReaders);
  FReaderThroughput := 0;
  FBaseQueue.Empty;
  FCounter.Value := numReaders + numWriters;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_BASE_QUEUE_STRESS_TEST, GetTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_BASE_QUEUE_STRESS_TEST, GetTestDuration_sec);
end;

procedure TfrmTestOtlContainers.StartBaseStackStressTest(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second base stack stress test, %d -> %d',
    [GetTestDuration_sec, numWriters, numReaders]));
  FTestName := 'base stack';
  FNumWriters := numWriters;
  FNumReaders := numReaders;
  FWriterThroughput := 0;
  FReaderThroughput := 0;
  AllocateTasks(numWriters, numReaders);
  FBaseStack.Empty;
  FCounter.Value := numReaders + numWriters;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, GetTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, GetTestDuration_sec);
end;

procedure TfrmTestOtlContainers.StartFirstTest;
var
  button: TButton;
  oldTag: integer;
begin
  button := (FQueuedTests.Objects[0] as TButton);
  oldTag := button.Tag;
  button.Tag := FQueuedTests[0];
  button.Click;
  button.Tag := oldTag;
end;

procedure TfrmTestOtlContainers.StartQueueStressTest(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second queue stress test, %d -> %d',
    [GetTestDuration_sec, numWriters, numReaders]));
  FTestName := 'queue';
  FNumWriters := numWriters;
  FNumReaders := numReaders;
  FWriterThroughput := 0;
  FReaderThroughput := 0;
  AllocateTasks(numWriters, numReaders);
  FQueue.Empty;
  FCounter.Value := numReaders + numWriters;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_QUEUE_STRESS_TEST, GetTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_QUEUE_STRESS_TEST, GetTestDuration_sec);
end;

procedure TfrmTestOtlContainers.StartStackStressTest(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second stack stress test, %d -> %d',
    [GetTestDuration_sec, numWriters, numReaders]));
  FTestName := 'stack';
  FNumWriters := numWriters;
  FNumReaders := numReaders;
  FWriterThroughput := 0;
  FReaderThroughput := 0;
  AllocateTasks(numWriters, numReaders);
  FStack.Empty;
  FCounter.Value := numReaders + numWriters;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_STACK_STRESS_TEST, GetTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_STACK_STRESS_TEST, GetTestDuration_sec);
end;

{ TCommTester }

constructor TCommTester.Create(baseStack: TOmniBaseBoundedStack; baseQueue: TOmniBaseBoundedQueue;
  stack: TOmniBoundedStack; queue: TOmniBoundedQueue);
begin
  inherited Create;
  ctStack := stack;
  ctQueue := queue;
  ctBaseStack := BaseStack;
  ctBaseQueue := baseQueue;
end;

procedure TCommTester.Fail(const reason: string);
begin
  Task.Comm.Send(MSG_TEST_FAILED, reason);
end;

{ TCommWriter }

procedure TCommWriter.OMStartBaseQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numEnqueued: integer;
  numLoops   : word;
  numSkipped : integer;
  startTime  : int64;
  throughput : integer;
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numEnqueued := 0;
  numSkipped := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    Inc(counter);
    if BaseQueue.Enqueue(counter) then
      Inc(numEnqueued)
    else begin
      Inc(numSkipped);
      DSiYield;
    end;
  until false;
  throughput := Round(numEnqueued/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d enqueued, %d skipped; %d msg/s',
    [time - startTime, numEnqueued, numSkipped, throughput]));
  Task.Comm.Send(MSG_WRITER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'base queue');
end;

procedure TCommWriter.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter   : integer;
  endTime   : int64;
  numLoops  : word;
  numPushed : integer;
  numSkipped: integer;
  startTime : int64;
  throughput: integer;
  time      : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numPushed := 0;
  numSkipped := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    Inc(counter);
    if BaseStack.Push(counter) then
      Inc(numPushed)
    else begin
      Inc(numSkipped);
      DSiYield;
    end;
  until false;
  throughput := Round(numPushed/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d pushed, %d skipped; %d msg/s',
    [time - startTime, numPushed, numSkipped, throughput]));
  Task.Comm.Send(MSG_WRITER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'base stack');
end;

procedure TCommWriter.OMStartQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numEnqueued: integer;
  numLoops   : word;
  numSkipped : integer;
  startTime  : int64;
  throughput : integer;
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numEnqueued := 0;
  numSkipped := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    Inc(counter);
    if Queue.Enqueue(counter) then
      Inc(numEnqueued)
    else begin
      Inc(numSkipped);
      DSiYield;
    end;
  until false;
  throughput := Round(numEnqueued/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d enqueued, %d skipped; %d msg/s',
    [time - startTime, numEnqueued, numSkipped, throughput]));
  Task.Comm.Send(MSG_WRITER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'queue');
end;

procedure TCommWriter.OMStartQueueWrite(var msg: TOmniMessage);
var
  container: TOmniBaseBoundedQueue;
  iRepeat  : integer;
  item     : integer;
  numItems : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseBoundedQueue(cardinal(msg.MsgData[1]));
  for iRepeat := 1 to 2 do begin
    if not container.IsEmpty then begin
      Fail('Queue is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to numItems do
        if not container.Enqueue(item) then begin
          Fail(Format('Failed to enqueue item %d', [item]));
          Exit;
        end;
      if not container.IsFull then begin
        Fail('Queue is not full at the end');
        Exit;
      end
      else begin
        item := numItems + 1;
        if container.Enqueue(item) then begin
          Fail('Managed to enqueue item into a full Queue');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_QUEUE_WRITE_COMPLETED, cardinal(container));
      end; //for item
    end;
    if iRepeat = 1 then begin
      container.Dequeue(item);
      container.Empty;
    end;
  end; //for iRepeat
end;

procedure TCommWriter.OMStartStackStressTest(var msg: TOmniMessage);
var
  counter   : integer;
  endTime   : int64;
  numLoops  : word;
  numPushed : integer;
  numSkipped: integer;
  startTime : int64;
  throughput: integer;
  time      : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numPushed := 0;
  numSkipped := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    Inc(counter);
    if Stack.Push(counter) then
      Inc(numPushed)
    else begin
      Inc(numSkipped);
      DSiYield;
    end;
  until false;
  throughput := Round(numPushed/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d pushed, %d skipped; %d msg/s',
    [time - startTime, numPushed, numSkipped, throughput]));
  Task.Comm.Send(MSG_WRITER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'stack');
end;

procedure TCommWriter.OMStartStackWrite(var msg: TOmniMessage);
var
  container: TOmniBaseBoundedStack;
  iRepeat  : integer;
  item     : integer;
  numItems : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseBoundedStack(cardinal(msg.MsgData[1]));
  for iRepeat := 1 to 2 do begin
    if not container.IsEmpty then begin
      Fail('Stack is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to numItems do
        if not container.Push(item) then begin
          Fail(Format('Failed to push item %d', [item]));
          Exit;
        end;
      if not container.IsFull then begin
        Fail('Stack is not full at the end');
        Exit;
      end
      else begin
        item := numItems + 1;
        if container.Push(item) then begin
          Fail('Managed to push item onto a full stack');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_STACK_WRITE_COMPLETED, cardinal(container));
      end; //for item
    end;
    if iRepeat = 1 then
      container.Empty;
  end; //for iRepeat
end;

{ TCommReader }

procedure TCommReader.OMStartBaseQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numDequeued: integer;
  numEmpty   : integer;
  numLoops   : word;
  startTime  : int64;
  throughput : integer;
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numDequeued := 0;
  numEmpty := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    if BaseQueue.Dequeue(counter) then
      Inc(numDequeued)
    else begin
      Inc(numEmpty);
      DSiYield;
    end;
  until false;
  throughput := Round(numDequeued/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d dequeued, %d empty; %d msg/s',
    [time - startTime, numDequeued, numEmpty, throughput]));
  Task.Comm.Send(MSG_READER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'base queue');
end;

procedure TCommReader.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter  : integer;
  endTime  : int64;
  numEmpty : integer;
  numLoops : word;
  numPopped: integer;
  startTime: int64;
  throughput: integer;
  time     : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numPopped := 0;
  numEmpty := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    if BaseStack.Pop(counter) then
      Inc(numPopped)
    else begin
      Inc(numEmpty);
      DSiYield;
    end;
  until false;
  throughput := Round(numPopped/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d popped, %d empty; %d msg/s',
    [time - startTime, numPopped, numEmpty, throughput]));
  Task.Comm.Send(MSG_READER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'base stack');
end;

procedure TCommReader.OMStartQueueRead(var msg: TOmniMessage);
var
  container: TOmniBaseBoundedQueue;
  item     : integer;
  numItems : integer;
  stItem   : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseBoundedQueue(cardinal(msg.MsgData[1]));
  if not container.IsFull then
    Fail('Queue is not full at the beginning')
  else begin
    for item := 1 to numItems do begin
      if not container.Dequeue(stItem) then begin
        Fail(Format('Failed to dequeue item %d', [item]));
        Exit;
      end;
      if (stItem <> item) then begin
        Fail(Format('Dequeued item %d not equal to expected item %d', [stItem, item]));
        Exit;
      end;
    end;
    if not container.IsEmpty then
      Fail('Queue is not empty at the end')
    else begin
      if container.Dequeue(item) then
        Fail('Managed to dequeue item from an empty Queue')
      else
        Task.Comm.Send(MSG_QUEUE_READ_COMPLETED, 0);
    end;
  end;
end;

procedure TCommReader.OMStartQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numDequeued: integer;
  numEmpty   : integer;
  numLoops   : word;
  startTime  : int64;
  throughput : integer;
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numDequeued := 0;
  numEmpty := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    if Queue.Dequeue(counter) then
      Inc(numDequeued)
    else begin
      Inc(numEmpty);
      DSiYield;
    end;
  until false;
  throughput := Round(numDequeued/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d dequeued, %d empty; %d msg/s',
    [time - startTime, numDequeued, numEmpty, throughput]));
  Task.Comm.Send(MSG_READER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'queue');
end;

procedure TCommReader.OMStartStackRead(var msg: TOmniMessage);
var
  container: TOmniBaseBoundedStack;
  item     : integer;
  numItems : integer;
  stItem   : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseBoundedStack(cardinal(msg.MsgData[1]));
  if not container.IsFull then
    Fail('Stack is not full at the beginning')
  else begin
    for item := numItems downto 1 do begin
      if not container.Pop(stItem) then begin
        Fail(Format('Failed to pop item %d', [item]));
        Exit;
      end;
      if (stItem <> item) then begin
        Fail(Format('Popped item %d not equal to expected item %d', [stItem, item]));
        Exit;
      end;
    end;
    if not container.IsEmpty then
      Fail('Stack is not empty at the end')
    else begin
      if container.Pop(item) then
        Fail('Managed to pop item from an empty stack')
      else
        Task.Comm.Send(MSG_STACK_READ_COMPLETED, 0);
    end;
  end;
end;

procedure TCommReader.OMStartStackStressTest(var msg: TOmniMessage);
var
  counter   : integer;
  endTime   : int64;
  numEmpty  : integer;
  numLoops  : word;
  numPopped : integer;
  startTime : int64;
  throughput: integer;
  time      : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData.AsInt64 * 1000;
  counter := 0;
  numPopped := 0;
  numEmpty := 0;
  numLoops := 0;
  repeat
    {$Q-}Inc(numLoops);{$Q+}
    if numLoops = 0 then begin
      time := DSiTimeGetTime64;
      if time > endTime then
        break; //repeat
    end;
    if Stack.Pop(counter) then
      Inc(numPopped)
    else begin
      Inc(numEmpty);
      DSiYield;
    end;
  until false;
  throughput := Round(numPopped/((time - startTime)/1000));
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d popped, %d empty; %d msg/s',
    [time - startTime, numPopped, numEmpty, throughput]));
  Task.Comm.Send(MSG_READER_THROUGHPUT, throughput);
  if Task.Counter.Decrement = 0 then
    Task.Comm.Send(MSG_FULL_STOP, 'stack');
end;

end.
