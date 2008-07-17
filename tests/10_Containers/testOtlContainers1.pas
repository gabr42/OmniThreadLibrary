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
    btnBaseStackStressTest : TButton;
    btnQueueCorrectnessTest: TButton;
    btnQueueStressTest     : TButton;
    btnStack1to2           : TButton;
    btnStack2to1           : TButton;
    btnStack2to2           : TButton;
    btnStackCorrectnessTest: TButton;
    btnStackStressTest     : TButton;
    lbLog                  : TListBox;
    OmniTaskEventDispatch1 : TOmniTaskEventDispatch;
    procedure btnBaseQueueStressTestClick(Sender: TObject);
    procedure btnBaseStackStressTestClick(Sender: TObject);
    procedure btnQueueCorrectnessTestClick(Sender: TObject);
    procedure btnQueueStressTestClick(Sender: TObject);
    procedure btnStack1to2Click(Sender: TObject);
    procedure btnStack2to1Click(Sender: TObject);
    procedure btnStack2to2Click(Sender: TObject);
    procedure btnStackCorrectnessTestClick(Sender: TObject);
    procedure btnStackStressTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
  private
    FBaseQueue: TOmniBaseQueue;
    FBaseStack: TOmniBaseStack;
    FQueue    : TOmniQueue;
    FReader   : IOmniTaskControl;
    FReader2  : IOmniTaskControl;
    FStack    : TOmniStack;
    FWriter   : IOmniTaskControl;
    FWriter2  : IOmniTaskControl;
    procedure Log(const msg: string);
  end;

var
  frmTestOtlContainers: TfrmTestOtlContainers;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  CTestQueueLength = 1000;

  // GUI -> thread messages
  MSG_START_STACK_STRESS_TEST      = 1;
  MSG_START_QUEUE_STRESS_TEST      = 2;
  MSG_START_STACK_WRITE            = 3;
  MSG_START_STACK_READ             = 4;
  MSG_START_QUEUE_WRITE            = 5;
  MSG_START_QUEUE_READ             = 60;
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

procedure TfrmTestOtlContainers.btnBaseQueueStressTestClick(Sender: TObject);
begin
  Log('Starting 60 second base container stress test');
  FBaseQueue.Empty;
  FReader.Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnBaseStackStressTestClick(Sender: TObject);
begin
  Log('Starting 60 second base stack stress test');
  FBaseStack.Empty;
  FReader.Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_BASE_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnQueueCorrectnessTestClick(Sender: TObject);
begin
  Log('Writing to queue');
  FQueue.Empty;
  FWriter.Comm.Send(MSG_START_QUEUE_WRITE, CTestQueueLength);
end;

procedure TfrmTestOtlContainers.btnQueueStressTestClick(Sender: TObject);
begin
  Log('Starting 60 second queue stress test');
  FReader.Comm.Send(MSG_START_QUEUE_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_QUEUE_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnStack1to2Click(Sender: TObject);
begin
  Log('Starting 60 second stack stress test, 1 -> 2');
  FStack.Empty;
  FReader.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FReader2.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnStack2to1Click(Sender: TObject);
begin
  Log('Starting 60 second stack stress test, 2 -> 1');
  FStack.Empty;
  FReader.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter2.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnStack2to2Click(Sender: TObject);
begin
  Log('Starting 60 second stack stress test, 2 -> 2');
  FStack.Empty;
  FReader.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FReader2.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter2.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.btnStackCorrectnessTestClick(Sender: TObject);
begin
  Log('Writing to stack');
  FStack.Empty;
  FWriter.Comm.Send(MSG_START_STACK_WRITE, CTestQueueLength);
end;

procedure TfrmTestOtlContainers.btnStackStressTestClick(Sender: TObject);
begin
  Log('Starting 60 second stack stress test, 1 -> 1');
  FStack.Empty;
  FReader.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_STACK_STRESS_TEST, 60 {seconds});
end;

procedure TfrmTestOtlContainers.FormCreate(Sender: TObject);
begin
  FBaseStack := TOmniBaseStack.Create;
  FBaseStack.Initialize(CTestQueueLength, SizeOf(integer));
  FStack := TOmniStack.Create(CTestQueueLength, SizeOf(integer), [coEnableNotify]);
  FBaseQueue := TOmniBaseQueue.Create;
  FBaseQueue.Initialize(CTestQueueLength, SizeOf(integer));
  FQueue := TOmniQueue.Create(CTestQueueLength, SizeOf(integer), [coEnableNotify]);
  FWriter := OmniTaskEventDispatch1.Monitor(CreateTask(TCommWriter.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
  FWriter2:= OmniTaskEventDispatch1.Monitor(CreateTask(TCommWriter.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
  FReader := OmniTaskEventDispatch1.Monitor(CreateTask(TCommReader.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
  FReader2:= OmniTaskEventDispatch1.Monitor(CreateTask(TCommReader.Create(FBaseStack, FBaseQueue, FStack, FQueue))).FreeOnTerminate.Run;
end;

procedure TfrmTestOtlContainers.FormDestroy(Sender: TObject);
begin
  FWriter.Terminate;
  FWriter2.Terminate;
  FReader.Terminate;
  FReader2.Terminate;
  FreeAndNil(FBaseStack);
  FreeAndNil(FStack);
  FreeAndNil(FBaseQueue);
  FreeAndNil(FQueue);
end;

procedure TfrmTestOtlContainers.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
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
          FReader.Comm.Send(MSG_START_STACK_READ, CTestQueueLength);
        end;
      MSG_STACK_READ_COMPLETED:
        Log('Test completed');
      MSG_QUEUE_WRITE_COMPLETED:
        begin
          Log('Reading from queue');
          FReader.Comm.Send(MSG_START_QUEUE_READ, CTestQueueLength);
        end;
      MSG_QUEUE_READ_COMPLETED:
        Log('Test completed');
      MSG_TEST_FAILED:
        Log('Write test failed. ' + msg.MsgData);
      else
        Log(Format('Unknown message %d', [msg.MsgID]));
    end; //case             
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
  numSkipped : integer;
  startTime  : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numEnqueued := 0;
  numSkipped := 0;
  while DSiTimeGetTime64 < endTime do begin
    Inc(counter);
    if BaseQueue.Enqueue(counter) then
      Inc(numEnqueued)
    else
      Inc(numSkipped);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Writer completed; %d enqueued, %d skipped',
    [numEnqueued, numSkipped]));
end;

procedure TCommWriter.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter   : integer;
  endTime   : int64;
  numPushed : integer;
  numSkipped: integer;
  startTime : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numPushed := 0;
  numSkipped := 0;
  while DSiTimeGetTime64 < endTime do begin
    Inc(counter);
    if BaseStack.Push(counter) then
      Inc(numPushed)
    else
      Inc(numSkipped);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Writer completed; %d pushed, %d skipped',
    [numPushed, numSkipped]));
end;

procedure TCommWriter.OMStartQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numEnqueued: integer;
  numSkipped : integer;
  startTime  : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numEnqueued := 0;
  numSkipped := 0;
  while DSiTimeGetTime64 < endTime do begin
    Inc(counter);
    if Queue.Enqueue(counter) then
      Inc(numEnqueued)
    else
      Inc(numSkipped);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Writer completed; %d enqueued, %d skipped',
    [numEnqueued, numSkipped]));
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
  numPushed : integer;
  numSkipped: integer;
  startTime : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numPushed := 0;
  numSkipped := 0;
  while DSiTimeGetTime64 < endTime do begin
    Inc(counter);
    if Stack.Push(counter) then
      Inc(numPushed)
    else
      Inc(numSkipped);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Writer completed; %d pushed, %d skipped',
    [numPushed, numSkipped]));
end;

procedure TCommWriter.OMStartStackWrite(var msg: TOmniMessage);
var
  iRepeat: integer;
  item   : integer;
begin
  for iRepeat := 1 to 2 do begin
    if not Stack.IsEmpty then begin
      Fail('Stack is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to msg.MsgData do
        if not Stack.Push(item) then begin
          Fail(Format('Failed to push item %d', [item]));
          Exit;
        end;
      if not Stack.IsFull then begin
        Fail('Stack is not full at the end');
        Exit;
      end
      else begin
        item := msg.MsgData + 1;
        if Stack.Push(item) then begin
          Fail('Managed to push item onto a full stack');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_STACK_WRITE_COMPLETED, 0);
      end; //for item
    end;
    if iRepeat = 1 then
      Stack.Empty;
  end; //for iRepeat
end;

{ TCommReader }

procedure TCommReader.OMStartBaseQueueStressTest(var msg: TOmniMessage);
var
  counter    : integer;
  endTime    : int64;
  numDequeued: integer;
  numEmpty   : integer;
  startTime  : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numDequeued := 0;
  numEmpty := 0;
  while DSiTimeGetTime64 < endTime do begin
    if BaseQueue.Dequeue(counter) then
      Inc(numDequeued)
    else
      Inc(numEmpty);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Reader completed; %d dequeued, %d empty',
    [numDequeued, numEmpty]));
end;

procedure TCommReader.OMStartBaseStackStressTest(var msg: TOmniMessage);
var
  counter  : integer;
  endTime  : int64;
  numEmpty : integer;
  numPopped: integer;
  startTime: int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numPopped := 0;
  numEmpty := 0;
  while DSiTimeGetTime64 < endTime do begin
    if BaseStack.Pop(counter) then
      Inc(numPopped)
    else
      Inc(numEmpty);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Reader completed; %d popped, %d empty',
    [numPopped, numEmpty]));
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
  startTime  : int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numDequeued := 0;
  numEmpty := 0;
  while DSiTimeGetTime64 < endTime do begin
    if Queue.Dequeue(counter) then
      Inc(numDequeued)
    else
      Inc(numEmpty);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Reader completed; %d dequeued, %d empty',
    [numDequeued, numEmpty]));
end;

procedure TCommReader.OMStartStackRead(var msg: TOmniMessage);
var
  item  : integer;
  stItem: integer;
begin
  if not Stack.IsFull then
    Fail('Stack is not full at the beginning')
  else begin
    for item := msg.MsgData downto 1 do begin
      if not Stack.Pop(stItem) then begin
        Fail(Format('Failed to pop item %d', [item]));
        Exit;
      end;
      if (stItem <> item) then begin
        Fail(Format('Popped item %d not equal to expected item %d', [stItem, item]));
        Exit;
      end;
    end;
    if not Stack.IsEmpty then
      Fail('Stack is not empty at the end')
    else begin
      if Stack.Pop(item) then
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
  numPopped: integer;
  startTime: int64;
begin
  startTime := DSiTimeGetTime64;
  endTime := startTime + msg.MsgData * 1000;
  counter := 0;
  numPopped := 0;
  numEmpty := 0;
  while DSiTimeGetTime64 < endTime do begin
    if Stack.Pop(counter) then
      Inc(numPopped)
    else
      Inc(numEmpty);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Reader completed; %d popped, %d empty',
    [numPopped, numEmpty]));
end;

end.
