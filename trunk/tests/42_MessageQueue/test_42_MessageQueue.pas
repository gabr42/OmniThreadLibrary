unit test_42_MessageQueue;

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
  TfrmTestMessageQueue = class(TForm)
    btnClearLog            : TButton;
    btnQueue1to2           : TButton;
    btnQueue1to4           : TButton;
    btnQueue2to1           : TButton;
    btnQueue2to2           : TButton;
    btnQueue4to1           : TButton;
    btnQueue4to4           : TButton;
    btnQueueAll            : TButton;
    btnQueueCorrectnessTest: TButton;
    btnQueueStressTest     : TButton;
    btnSaveLog             : TButton;
    inpTestDuration_sec    : TLabeledEdit;
    lbLog                  : TListBox;
    OmniEventMonitor1      : TOmniEventMonitor;
    SaveDialog             : TSaveDialog;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnQueueAllClick(Sender: TObject);
    procedure btnQueueCorrectnessTestClick(Sender: TObject);
    procedure btnQueueStressTestClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; msg: TOmniMessage);
  private
    FAllTestsStart   : int64;
    FCounter         : IOmniCounter;
    FNumReaders      : integer;
    FNumWriters      : integer;
    FMessageQueue    : TOmniMessageQueue;
    FQueuedTests     : TGpIntegerObjectList;
    FReaders         : array [1..4] of IOmniTaskControl;
    FReaderThroughput: integer;
    FTestName        : string;
    FWriters         : array [1..4] of IOmniTaskControl;
    FWriterThroughput: integer;
    function  GetTestDuration_sec: integer;
    procedure Log(const msg: string);
    procedure ScheduleAllQueueTests;
    procedure StartFirstTest;
    procedure StartQueueStressTest(numWriters, numReaders: integer);
  strict protected
    procedure AllocateTasks(numWriters, numReaders: integer);
  end;

var
  frmTestMessageQueue: TfrmTestMessageQueue;

implementation

uses
  DSiWin32,
  GpStuff;

{$R *.dfm}

const
  CTestQueueLength  = 1000;

  // GUI -> thread messages
  MSG_START_QUEUE_STRESS_TEST      = 2;
  MSG_START_QUEUE_WRITE            = 5;
  MSG_START_QUEUE_READ             = 6;

  // thread -> GUI messages
  MSG_TEST_END              = 100;
  MSG_TEST_FAILED           = 101;
  MSG_QUEUE_WRITE_COMPLETED = 104;
  MSG_QUEUE_READ_COMPLETED  = 105;
  MSG_FULL_STOP             = 106;
  MSG_WRITER_THROUGHPUT     = 107;
  MSG_READER_THROUGHPUT     = 108;

type
  TTestSuite = (tsDump, tsMessageExchange);

  TCommTester = class(TOmniWorker)
  strict private
    ctMQueue: TOmniMessageQueue;
  strict protected
    procedure Fail(const reason: string);
  public
    constructor Create(messageQueue: TOmniMessageQueue);
    property MQueue: TOmniMessageQueue read ctMQueue;
  end; { TCommTester }

  TCommWriter = class(TCommTester)
  public
    procedure OMStartQueueStressTest(var msg: TOmniMessage); message MSG_START_QUEUE_STRESS_TEST;
    procedure OMStartQueueWrite(var msg: TOmniMessage); message MSG_START_QUEUE_WRITE;
  end;

  TCommReader = class(TCommTester)
  public
    procedure OMStartQueueRead(var msg: TOmniMessage); message MSG_START_QUEUE_READ;
    procedure OMStartQueueStressTest(var msg: TOmniMessage); message MSG_START_QUEUE_STRESS_TEST;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestMessageQueue.AllocateTasks(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  for iTask := 1 to numReaders do
    if not assigned(FReaders[iTask]) then
      FReaders[iTask] := OmniEventMonitor1.Monitor(CreateTask(
        TCommReader.Create(FMessageQueue), 'reader'))
        .WithCounter(FCounter)
        .Run;
  for iTask := 1 to numWriters do
    if not assigned(FWriters[iTask]) then
      FWriters[iTask] := OmniEventMonitor1.Monitor(CreateTask(
        TCommWriter.Create(FMessageQueue), 'writer'))
        .WithCounter(FCounter)
        .Run;
end;

procedure TfrmTestMessageQueue.btnClearLogClick(Sender: TObject);
begin
  lbLog.Clear;
end;

procedure TfrmTestMessageQueue.btnQueueAllClick(Sender: TObject);
begin
  ScheduleAllQueueTests;
  FAllTestsStart := DSiTimeGetTime64;
  StartFirstTest;
end;

procedure TfrmTestMessageQueue.btnQueueCorrectnessTestClick(Sender: TObject);
begin
  Log('Writing to queue');
  FMessageQueue.Empty;
  FWriters[1].Comm.Send(MSG_START_QUEUE_WRITE, [CTestQueueLength, cardinal(FMessageQueue)]);
end;

procedure TfrmTestMessageQueue.btnQueueStressTestClick(Sender: TObject);
var
  tag: integer;
begin
  tag := (Sender as TButton).Tag;
  StartQueueStressTest(tag div 10, tag mod 10);
end;

procedure TfrmTestMessageQueue.btnSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    lbLog.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmTestMessageQueue.FormCreate(Sender: TObject);
begin
  FMessageQueue := TOmniMessageQueue.Create(CTestQueueLength);
  FCounter := CreateCounter;
  FQueuedTests := TGpIntegerObjectList.Create(false);
  AllocateTasks(1, 1);
end;

procedure TfrmTestMessageQueue.FormDestroy(Sender: TObject);
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
  FreeAndNil(FMessageQueue);
end;

function TfrmTestMessageQueue.GetTestDuration_sec: integer;
var
  c: integer;
begin
  Val(inpTestDuration_sec.Text, Result, c);
  if c <> 0 then
    Result := 10;
end; { TfrmTestOtlContainers.GetTestDuration_sec }

procedure TfrmTestMessageQueue.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn ', Now) + msg);
  Application.ProcessMessages;
end;

procedure TfrmTestMessageQueue.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  msg: TOmniMessage);
var
  f  : textfile;
begin
  case msg.MsgID of
    MSG_FULL_STOP:
      begin
        Log('All tasks stopped');
        Log(Format('Total reader throughput: %d msg/s', [FReaderThroughput]));
        Log(Format('Total writer throughput: %d msg/s', [FWriterThroughput]));
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
    MSG_QUEUE_READ_COMPLETED:
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

procedure TfrmTestMessageQueue.ScheduleAllQueueTests;
var
  numReaders: integer;
  numWriters: integer;
begin
  for numWriters in [1, 2, 4] do
    for numReaders in [1, 2, 4] do
      FQueuedTests.AddObject(10*numWriters + numReaders, btnQueueStressTest);
end;

procedure TfrmTestMessageQueue.StartFirstTest;
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

procedure TfrmTestMessageQueue.StartQueueStressTest(numWriters, numReaders: integer);
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
  FMessageQueue.Empty;
  FCounter.Value := numReaders + numWriters;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_QUEUE_STRESS_TEST, GetTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_QUEUE_STRESS_TEST, GetTestDuration_sec);
end;

{ TCommTester }

constructor TCommTester.Create(messageQueue: TOmniMessageQueue);
begin
  inherited Create;
  ctMQueue := messageQueue;
end;

procedure TCommTester.Fail(const reason: string);
begin
  Task.Comm.Send(MSG_TEST_FAILED, reason);
end;

{ TCommWriter }

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
    if MQueue.Enqueue(TOmniMessage.Create(1, counter)) then
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
  container: TOmniMessageQueue;
  iRepeat  : integer;
  item     : integer;
  numItems : integer;
  qMsg     : TOmniMessage;
begin
  numItems := msg.MsgData[0];
  container := TOmniMessageQueue(cardinal(msg.MsgData[1]));
  for iRepeat := 1 to 2 do begin
    if not container.IsEmpty then begin
      Fail('Queue is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to numItems do
        if not container.Enqueue(TOmniMessage.Create(1, item)) then begin
          Fail(Format('Failed to enqueue item %d', [item]));
          Exit;
        end;
      if not container.IsFull then begin
        Fail('Queue is not full at the end');
        Exit;
      end
      else begin
        item := numItems + 1;
        if container.Enqueue(TOmniMessage.Create(1, item)) then begin
          Fail('Managed to enqueue item into a full Queue');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_QUEUE_WRITE_COMPLETED, cardinal(container));
      end; //for item
    end;
    if iRepeat = 1 then begin
      qMsg := container.Dequeue;
      container.Empty;
    end;
  end; //for iRepeat
end;

{ TCommReader }

procedure TCommReader.OMStartQueueRead(var msg: TOmniMessage);
var
  container: TOmniMessageQueue;
  item     : integer;
  numItems : integer;
  qMsg     : TOmniMessage;
begin
  numItems := msg.MsgData[0];
  container := TOmniMessageQueue(cardinal(msg.MsgData[1]));
  if not container.IsFull then
    Fail('Queue is not full at the beginning')
  else begin
    for item := 1 to numItems do begin
      if not container.TryDequeue(qMsg) then begin
        Fail(Format('Failed to dequeue item %d', [item]));
        Exit;
      end;
      if (qMsg.MsgData.AsInteger <> item) then begin
        Fail(Format('Dequeued item %d not equal to expected item %d', [qMsg.MsgData.AsInteger, item]));
        Exit;
      end;
    end;
    if not container.IsEmpty then
      Fail('Queue is not empty at the end')
    else begin
      if container.TryDequeue(qMsg) then
        Fail('Managed to dequeue item from an empty Queue')
      else
        Task.Comm.Send(MSG_QUEUE_READ_COMPLETED, 0);
    end;
  end;
end;

procedure TCommReader.OMStartQueueStressTest(var msg: TOmniMessage);
var
  counter    : TOmniMessage;
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
    if MQueue.TryDequeue(counter) then
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

end.
