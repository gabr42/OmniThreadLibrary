unit testOtlContainers1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlTaskEvents;

type
  TfrmTestOtlContainers = class(TForm)
    btnBaseQueueStressTest : TButton;
    btnBaseStack1to2       : TButton;
    btnBaseStack2to1       : TButton;
    btnBaseStack2to2       : TButton;
    btnBaseStack4to1       : TButton;
    btnBaseStackStressTest : TButton;
    btnQueueCorrectnessTest: TButton;
    btnQueueStressTest     : TButton;
    btnSaveLog             : TButton;
    btnStack1to2           : TButton;
    btnStack2to1           : TButton;
    btnStack2to2           : TButton;
    btnStack4to1           : TButton;
    btnStackCorrectnessTest: TButton;
    btnStackStressTest     : TButton;
    lbLog                  : TListBox;
    OmniTaskEventDispatch1 : TOmniTaskEventDispatch;
    SaveDialog             : TSaveDialog;
    btnBaseStackCorrectnessTest: TButton;
    procedure btnBaseQueueStressTestClick(Sender: TObject);
    procedure btnBaseStack1to2Click(Sender: TObject);
    procedure btnBaseStack2to1Click(Sender: TObject);
    procedure btnBaseStack2to2Click(Sender: TObject);
    procedure btnBaseStack4to1Click(Sender: TObject);
    procedure btnBaseStackStressTestClick(Sender: TObject);
    procedure btnQueueCorrectnessTestClick(Sender: TObject);
    procedure btnQueueStressTestClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnStack1to2Click(Sender: TObject);
    procedure btnStack2to1Click(Sender: TObject);
    procedure btnStack2to2Click(Sender: TObject);
    procedure btnStack4to1Click(Sender: TObject);
    procedure btnStackCorrectnessTestClick(Sender: TObject);
    procedure btnStackStressTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
  private
    FBaseQueue: TOmniBaseQueue;
    FBaseStack: TOmniBaseStack;
    FQueue    : TOmniQueue;
    FReaders  : array [1..4] of IOmniTaskControl;
    FStack    : TOmniStack;
    FWriters  : array [1..4] of IOmniTaskControl;
    procedure Log(const msg: string);
    procedure StartBaseStackStressTest(numWriters, numReaders: integer);
    procedure StartStackStress(numWriters, numReaders: integer);
  strict protected
    procedure AllocateTasks(numWriters, numReaders: integer);
  end;

var
  frmTestOtlContainers: TfrmTestOtlContainers;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  CTestQueueLength  = 1000;
  CTestDuration_sec = 10;

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

type
  TTestSuite = (tsDump, tsMessageExchange);

  TCommTester = class(TOmniWorker)
  strict private
    ctBaseQueue: TOmniBaseQueue;
    ctBaseStack: TOmniBaseStack;
    ctQueue    : TOmniQueue;
    ctStack    : TOmniStack;
  strict protected
    procedure Fail(const reason: string);
  public
    constructor Create(baseStack: TOmniBaseStack; baseQueue: TOmniBaseQueue; stack:
      TOmniStack; queue: TOmniQueue);
    property BaseQueue: TOmniBaseQueue read ctBaseQueue;
    property BaseStack: TOmniBaseStack read ctBaseStack;
    property Queue: TOmniQueue read ctQueue;
    property Stack: TOmniStack read ctStack;
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

  TCommReader = class(TComMTester)
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
      FReaders[iTask] := OmniTaskEventDispatch1.Monitor(CreateTask(
        TCommReader.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
  for iTask := 1 to numWriters do
    if not assigned(FWriters[iTask]) then
      FWriters[iTask] := OmniTaskEventDispatch1.Monitor(CreateTask(
        TCommWriter.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
end;

procedure TfrmTestOtlContainers.btnBaseQueueStressTestClick(Sender: TObject);
begin
  Log('Starting 6 second base container stress test');
  FBaseQueue.Empty;
  FReaders[1].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, CTestDuration_sec);
  FWriters[1].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, CTestDuration_sec);
end;

procedure TfrmTestOtlContainers.btnBaseStack1to2Click(Sender: TObject);
begin
  StartBaseStackStressTest(1, 2);
end;

procedure TfrmTestOtlContainers.btnBaseStack2to1Click(Sender: TObject);
begin
  StartBaseStackStressTest(2, 1);
end;

procedure TfrmTestOtlContainers.btnBaseStack2to2Click(Sender: TObject);
begin
  StartBaseStackStressTest(2, 2);
end;

procedure TfrmTestOtlContainers.btnBaseStack4to1Click(Sender: TObject);
begin
  StartBaseStackStressTest(4, 1);
end;

procedure TfrmTestOtlContainers.btnBaseStackStressTestClick(Sender: TObject);
begin
  StartBaseStackStressTest(1, 1);
end;

procedure TfrmTestOtlContainers.btnQueueCorrectnessTestClick(Sender: TObject);
begin
  Log('Writing to queue');
  FQueue.Empty;
  FWriters[1].Comm.Send(MSG_START_QUEUE_WRITE, CTestQueueLength);
end;

procedure TfrmTestOtlContainers.btnQueueStressTestClick(Sender: TObject);
begin
  Log('Starting CTestDuration_sec second queue stress test');
  FQueue.Empty;
  FReaders[1].Comm.Send(MSG_START_QUEUE_STRESS_TEST, CTestDuration_sec);
  FWriters[1].Comm.Send(MSG_START_QUEUE_STRESS_TEST, CTestDuration_sec);
end;

procedure TfrmTestOtlContainers.btnSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    lbLog.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmTestOtlContainers.btnStack1to2Click(Sender: TObject);
begin
  StartStackStress(1, 2);
end;

procedure TfrmTestOtlContainers.btnStack2to1Click(Sender: TObject);
begin
  StartStackStress(2, 1);
end;

procedure TfrmTestOtlContainers.btnStack2to2Click(Sender: TObject);
begin
  StartStackStress(2, 2);
end;

procedure TfrmTestOtlContainers.btnStack4to1Click(Sender: TObject);
begin
  StartStackStress(4, 1);
end;

procedure TfrmTestOtlContainers.btnStackCorrectnessTestClick(Sender: TObject);
var
  container: TOmniBaseStack;
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
begin
  StartStackStress(1, 1);
end;

procedure TfrmTestOtlContainers.FormCreate(Sender: TObject);
begin
  FBaseStack := TOmniBaseStack.Create;
  FBaseStack.Initialize(CTestQueueLength, SizeOf(integer));
  FStack := TOmniStack.Create(CTestQueueLength, SizeOf(integer), [coEnableNotify]);
  FBaseQueue := TOmniBaseQueue.Create;
  FBaseQueue.Initialize(CTestQueueLength, SizeOf(integer));
  FQueue := TOmniQueue.Create(CTestQueueLength, SizeOf(integer), [coEnableNotify]);
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
  FreeAndNil(FBaseStack);
  FreeAndNil(FStack);
  FreeAndNil(FBaseQueue);
  FreeAndNil(FQueue);
end;

procedure TfrmTestOtlContainers.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  Application.ProcessMessages;
end;

procedure TfrmTestOtlContainers.OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  if task.Comm.Receive(msg) then
    case msg.MsgID of
      MSG_TEST_END:
        Log(string(msg.MsgData));
      MSG_STACK_WRITE_COMPLETED:
        begin
          Log('Reading from stack');
          FReaders[1].Comm.Send(MSG_START_STACK_READ,
            [CTestQueueLength, cardinal(msg.MsgData[0])]);
        end;
      MSG_STACK_READ_COMPLETED:
        Log('Test completed');
      MSG_QUEUE_WRITE_COMPLETED:
        begin
          Log('Reading from queue');
          FReaders[1].Comm.Send(MSG_START_QUEUE_READ, CTestQueueLength);
        end;
      MSG_QUEUE_READ_COMPLETED:
        Log('Test completed');
      MSG_TEST_FAILED:
        Log('Write test failed. ' + msg.MsgData);
      else
        Log(Format('Unknown message %d', [msg.MsgID]));
    end; //case             
end;

procedure TfrmTestOtlContainers.StartBaseStackStressTest(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second base stack stress test, %d -> %d',
    [CTestDuration_sec, numWriters, numReaders]));
  AllocateTasks(numWriters, numReaders);
  FBaseStack.Empty;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, CTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, CTestDuration_sec);
end;

procedure TfrmTestOtlContainers.StartStackStress(numWriters, numReaders: integer);
var
  iTask: integer;
begin
  Log(Format('Starting %d second stack stress test, %d -> %d',
    [CTestDuration_sec, numWriters, numReaders]));
  AllocateTasks(numWriters, numReaders);
  FStack.Empty;
  for iTask := 1 to numReaders do
    FReaders[iTask].Comm.Send(MSG_START_STACK_STRESS_TEST, CTestDuration_sec);
  for iTask := 1 to numWriters do
    FWriters[iTask].Comm.Send(MSG_START_STACK_STRESS_TEST, CTestDuration_sec);
end;

{ TCommTester }

constructor TCommTester.Create(baseStack: TOmniBaseStack; baseQueue: TOmniBaseQueue;
  stack: TOmniStack; queue: TOmniQueue);
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
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numSkipped);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d enqueued, %d skipped; %d msg/s',
    [time - startTime, numEnqueued, numSkipped, Round(numEnqueued/((time - startTime)/1000))]));
end;

procedure TCommWriter.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter   : integer;
  endTime   : int64;
  numLoops  : word;
  numPushed : integer;
  numSkipped: integer;
  startTime : int64;
  time      : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numSkipped);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d pushed, %d skipped; %d msg/s',
    [time - startTime, numPushed, numSkipped, Round(numPushed/((time - startTime)/1000))]));
end;

procedure TCommWriter.OMStartQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numEnqueued: integer;
  numLoops   : word;
  numSkipped : integer;
  startTime  : int64;
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numSkipped);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d enqueued, %d skipped; %d msg/s',
    [time - startTime, numEnqueued, numSkipped, Round(numEnqueued/((time - startTime)/1000))]));
end;

procedure TCommWriter.OMStartQueueWrite(var msg: TOmniMessage);
var
  iRepeat: integer;
  item   : integer;
begin
  for iRepeat := 1 to 2 do begin
    if not Queue.IsEmpty then begin
      Fail('Queue is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to msg.MsgData do
        if not Queue.Enqueue(item) then begin
          Fail(Format('Failed to enqueue item %d', [item]));
          Exit;
        end;
      if not Queue.IsFull then begin
        Fail('Queue is not full at the end');
        Exit;
      end
      else begin
        item := msg.MsgData + 1;
        if Queue.Enqueue(item) then begin
          Fail('Managed to enqueue item into a full Queue');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_QUEUE_WRITE_COMPLETED, 0);
      end; //for item
    end;
    if iRepeat = 1 then begin
      Queue.Dequeue(item);
      Queue.Empty;
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
  time      : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numSkipped);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Writer completed in %d ms; %d pushed, %d skipped; %d msg/s',
    [time - startTime, numPushed, numSkipped, Round(numPushed/((time - startTime)/1000))]));
end;

procedure TCommWriter.OMStartStackWrite(var msg: TOmniMessage);
var
  container: TOmniBaseStack;
  iRepeat  : integer;
  item     : integer;
  numItems : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseStack(cardinal(msg.MsgData[1]));
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
          Task.Comm.Send(MSG_STACK_WRITE_COMPLETED, [container]);
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
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numEmpty);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d dequeued, %d empty; %d msg/s',
    [time - startTime, numDequeued, numEmpty, Round(numDequeued/((time - startTime)/1000))]));
end;

procedure TCommReader.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter  : integer;
  endTime  : int64;
  numEmpty : integer;
  numLoops : word;
  numPopped: integer;
  startTime: int64;
  time     : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numEmpty);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d popped, %d empty; %d msg/s',
    [time - startTime, numPopped, numEmpty, Round(numPopped/((time - startTime)/1000))]));
end;

procedure TCommReader.OMStartQueueRead(var msg: TOmniMessage);
var
  item  : integer;
  stItem: integer;
begin
  if not Queue.IsFull then
    Fail('Queue is not full at the beginning')
  else begin
    for item := 1 to msg.MsgData do begin
      if not Queue.Dequeue(stItem) then begin
        Fail(Format('Failed to dequeue item %d', [item]));
        Exit;
      end;
      if (stItem <> item) then begin
        Fail(Format('Dequeued item %d not equal to expected item %d', [stItem, item]));
        Exit;
      end;
    end;
    if not Queue.IsEmpty then
      Fail('Queue is not empty at the end')
    else begin
      if Queue.Dequeue(item) then
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
  time       : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numEmpty);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d dequeued, %d empty; %d msg/s',
    [time - startTime, numDequeued, numEmpty, Round(numDequeued/((time - startTime)/1000))]));
end;

procedure TCommReader.OMStartStackRead(var msg: TOmniMessage);
var
  container: TOmniBaseStack;
  item     : integer;
  numItems : integer;
  stItem   : integer;
begin
  numItems := msg.MsgData[0];
  container := TOmniBaseStack(cardinal(msg.MsgData[1]));
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
  counter  : integer;
  endTime  : int64;
  numEmpty : integer;
  numLoops : word;
  numPopped: integer;
  startTime: int64;
  time     : int64;
begin
  startTime := DSiTimeGetTime64;
  time := startTime;
  endTime := startTime + msg.MsgData * 1000;
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
    else
      Inc(numEmpty);
  until false;
  Task.Comm.Send(MSG_TEST_END, Format(
    'Reader completed in %d ms; %d popped, %d empty; %d msg/s',
    [time - startTime, numPopped, numEmpty, Round(numPopped/((time - startTime)/1000))]));
end;

end.
