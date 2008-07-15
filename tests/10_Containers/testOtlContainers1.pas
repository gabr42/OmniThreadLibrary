unit testOtlContainers1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlContainers,
  OtlComm,
  OtlTaskEvents;

type
  TfrmTestOtlContainers = class(TForm)
    btnBufferCorrectnessTest: TButton;
    btnBufferStressTest     : TButton;
    btnStackCorrectnessTest : TButton;
    btnStackStressTest      : TButton;
    lbLog                   : TListBox;
    OmniTaskEventDispatch1  : TOmniTaskEventDispatch;
    btnStack2to1            : TButton;
    btnStack1to2            : TButton;
    btnStack2to2            : TButton;
    procedure btnBufferCorrectnessTestClick(Sender: TObject);
    procedure btnBufferStressTestClick(Sender: TObject);
    procedure btnStack1to2Click(Sender: TObject);
    procedure btnStack2to1Click(Sender: TObject);
    procedure btnStack2to2Click(Sender: TObject);
    procedure btnStackCorrectnessTestClick(Sender: TObject);
    procedure btnStackStressTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OmniTaskEventDispatch1TaskMessage(task: IOmniTaskControl);
  strict private
    FBuffer: OtlContainers.TOmniRingBuffer;
    FStack : TOmniStack;
  private
    FReader : IOmniTaskControl;
    FReader2: IOmniTaskControl;
    FWriter : IOmniTaskControl;
    FWriter2: IOmniTaskControl;
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
  MSG_START_STACK_STRESS_TEST  = 1;
  MSG_START_BUFFER_STRESS_TEST = 2;
  MSG_START_STACK_WRITE        = 3;
  MSG_START_STACK_READ         = 4;
  MSG_START_BUFFER_WRITE       = 5;
  MSG_START_BUFFER_READ        = 6;

  // thread -> GUI messages
  MSG_TEST_END               = 100;
  MSG_TEST_FAILED            = 101;
  MSG_STACK_WRITE_COMPLETED  = 102;
  MSG_STACK_READ_COMPLETED   = 103;
  MSG_BUFFER_WRITE_COMPLETED = 104;
  MSG_BUFFER_READ_COMPLETED  = 105;

type
  TTestSuite = (tsDump, tsMessageExchange);

  TCommTester = class(TOmniWorker)
  strict private
    ctBuffer: OtlContainers.TOmniRingBuffer;
    ctStack : TOmniStack;
  strict protected
    procedure Fail(const reason: string);
  public
    constructor Create(stack: TOmniStack; ringBuffer: OtlContainers.TOmniRingBuffer);
    property Buffer: OtlContainers.TOmniRingBuffer read ctBuffer;
    property Stack: TOmniStack read ctStack;
  end; { TCommTester }

  TCommWriter = class(TCommTester)
  strict protected
  public
    procedure OMStartBufferStressTest(var msg: TOmniMessage); message MSG_START_BUFFER_STRESS_TEST;
    procedure OMStartStackStressTest(var msg: TOmniMessage); message MSG_START_STACK_STRESS_TEST;
    procedure OMStartBufferWrite(var msg: TOmniMessage); message MSG_START_BUFFER_WRITE;
    procedure OMStartStackWrite(var msg: TOmniMessage); message MSG_START_STACK_WRITE;
  end;

  TCommReader = class(TComMTester)
  public
    procedure OMStartBufferStressTest(var msg: TOmniMessage); message MSG_START_BUFFER_STRESS_TEST;
    procedure OMStartStackStressTest(var msg: TOmniMessage); message MSG_START_STACK_STRESS_TEST;
    procedure OMStartBufferRead(var msg: TOmniMessage); message MSG_START_BUFFER_READ;
    procedure OMStartStackRead(var msg: TOmniMessage); message MSG_START_STACK_READ;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestOtlContainers.btnBufferCorrectnessTestClick(Sender: TObject);
begin
  Log('Writing to ring buffer');
  FBuffer.Empty;
  FWriter.Comm.Send(MSG_START_BUFFER_WRITE, CTestQueueLength);
end;

procedure TfrmTestOtlContainers.btnBufferStressTestClick(Sender: TObject);
begin
  Log('Starting 60 second ring buffer stress test');
  FReader.Comm.Send(MSG_START_BUFFER_STRESS_TEST, 60 {seconds});
  FWriter.Comm.Send(MSG_START_BUFFER_STRESS_TEST, 60 {seconds});
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
  FStack := TOmniStack.Create(CTestQueueLength, SizeOf(integer));
  FBuffer := OtlContainers.TOmniRingBuffer.Create(CTestQueueLength, SizeOf(integer));
  FWriter := OmniTaskEventDispatch1.Monitor(CreateTask(TCommWriter.Create(FStack, FBuffer))).FreeOnTerminate.Run;
  FWriter2:= OmniTaskEventDispatch1.Monitor(CreateTask(TCommWriter.Create(FStack, FBuffer))).FreeOnTerminate.Run;
  FReader := OmniTaskEventDispatch1.Monitor(CreateTask(TCommReader.Create(FStack, FBuffer))).FreeOnTerminate.Run;
  FReader2:= OmniTaskEventDispatch1.Monitor(CreateTask(TCommReader.Create(FStack, FBuffer))).FreeOnTerminate.Run;
end;

procedure TfrmTestOtlContainers.FormDestroy(Sender: TObject);
begin
  FWriter.Terminate;
  FWriter2.Terminate;
  FReader.Terminate;
  FReader2.Terminate;
  FreeAndNil(FStack);
  FreeAndNil(FBuffer);
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
      MSG_BUFFER_WRITE_COMPLETED:
        begin
          Log('Reading from buffer');
          FReader.Comm.Send(MSG_START_BUFFER_READ, CTestQueueLength);
        end;
      MSG_BUFFER_READ_COMPLETED:
        Log('Test completed');
      MSG_TEST_FAILED:
        Log('Write test failed. ' + msg.MsgData);
      else
        Log(Format('Unknown message %d', [msg.MsgID]));
    end; //case             
end;

{ TCommTester }

constructor TCommTester.Create(stack: TOmniStack; ringBuffer:
  OtlContainers.TOmniRingBuffer);
begin
  inherited Create;
  ctStack := stack;
  ctBuffer := ringBuffer;
end;

procedure TCommTester.Fail(const reason: string);
begin
  Task.Comm.Send(MSG_TEST_FAILED, reason);
end;

{ TCommWriter }

procedure TCommWriter.OMStartBufferStressTest(var msg: TOmniMessage);
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
    if Buffer.Enqueue(counter) then
      Inc(numEnqueued)
    else
      Inc(numSkipped);
  end;
  Task.Comm.Send(MSG_TEST_END, Format('Writer completed; %d enqueued, %d skipped',
    [numEnqueued, numSkipped]));
end;

procedure TCommWriter.OMStartBufferWrite(var msg: TOmniMessage);
var
  iRepeat: integer;
  item   : integer;
begin
  for iRepeat := 1 to 2 do begin
    if not Buffer.IsEmpty then begin
      Fail('Buffer is not empty at the beginning');
      Exit;
    end
    else begin
      for item := 1 to msg.MsgData do
        if not Buffer.Enqueue(item) then begin
          Fail(Format('Failed to enqueue item %d', [item]));
          Exit;
        end;
      if not Buffer.IsFull then begin
        Fail('Buffer is not full at the end');
        Exit;
      end
      else begin
        item := msg.MsgData + 1;
        if Buffer.Enqueue(item) then begin
          Fail('Managed to enqueue item into a full buffer');
          Exit;
        end
        else if iRepeat = 2 then
          Task.Comm.Send(MSG_BUFFER_WRITE_COMPLETED, 0);
      end; //for item
    end;
    if iRepeat = 1 then begin
      Buffer.Dequeue(item);
      Buffer.Empty;
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

procedure TCommReader.OMStartBufferRead(var msg: TOmniMessage);
var
  item  : integer;
  stItem: integer;
begin
  if not Buffer.IsFull then
    Fail('Buffer is not full at the beginning')
  else begin
    for item := 1 to msg.MsgData do begin
      if not Buffer.Dequeue(stItem) then begin
        Fail(Format('Failed to dequeue item %d', [item]));
        Exit;
      end;
      if (stItem <> item) then begin
        Fail(Format('Dequeued item %d not equal to expected item %d', [stItem, item]));
        Exit;
      end;
    end;
    if not Buffer.IsEmpty then
      Fail('Buffer is not empty at the end')
    else begin
      if Buffer.Dequeue(item) then
        Fail('Managed to dequeue item from an empty buffer')
      else
        Task.Comm.Send(MSG_BUFFER_READ_COMPLETED, 0);
    end;
  end;
end;

procedure TCommReader.OMStartBufferStressTest(var msg: TOmniMessage);
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
    if Buffer.Dequeue(counter) then
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
