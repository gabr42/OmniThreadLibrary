unit test_11_ThreadPool;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlThreadPool,
  OtlEventMonitor;

const
  WM_THREAD_STATE_CHANGED = WM_USER;
  WM_OBJECT_DESTROYED     = WM_USER + 1;

type
  TfrmTestOtlThreadPool = class(TForm)
    btnRunTask: TButton;
    btnScheduleTask: TButton;
    lbLog  : TListBox;
    btnSchedule6: TButton;
    btnScheduleAndCancel: TButton;
    btnCancelLong: TButton;
    btnCancelAll: TButton;
    btnSchedule80: TButton;
    btnSchedule80All: TButton;
    btnSaveLog: TButton;
    SaveDialog: TSaveDialog;
    btnScheduleUnobserved: TButton;
    btnScheduleObserved: TButton;
    btnScheduleAndWait: TButton;
    btnScheduleAndTerminate: TButton;
    btnTestZeroExecutorThreads: TButton;
    OmniTED: TOmniEventMonitor;
    procedure btnCancelAllClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnSchedule6Click(Sender: TObject);
    procedure btnScheduleAndCancelClick(Sender: TObject);
    procedure btnScheduleAndWaitClick(Sender: TObject);
    procedure btnScheduleClick(Sender: TObject);
    procedure btnScheduleObservedClick(Sender: TObject);
    procedure btnScheduleUnobservedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OmniTEDPoolThreadCreated(const pool: IOmniThreadPool; threadID: integer);
    procedure OmniTEDPoolThreadDestroying(const pool: IOmniThreadPool; threadID: integer);
    procedure OmniTEDPoolThreadKilled(const pool: IOmniThreadPool; threadID: integer);
    procedure OmniTEDPoolWorkItemCompleted(const pool: IOmniThreadPool; taskID: Int64);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OmniTEDTaskTerminated(const task: IOmniTaskControl);
    procedure btnTestZeroExecutorThreadsClick(Sender: TObject);
  private
    FObservableTask: IOmniTaskControl;
    FThreadPool: IOmniThreadPool;
    procedure Log(const msg: string);
    procedure LogPoolStatus;
    procedure ReleaseObservableTask(const task: IOmniTaskControl);
    procedure WMThreadStateChanged(var msg: TMessage); message WM_THREAD_STATE_CHANGED;
    procedure WMObjectDestroyed(var msg: TMessage); message WM_OBJECT_DESTROYED;
  end;

var
  frmTestOtlThreadPool: TfrmTestOtlThreadPool;

implementation

uses
  DSiWin32,
  OtlCommon;

{$R *.dfm}

const
  MSG_HELLO = 1;
  MSG_SLEEP = 2;

type
  THelloWorker = class(TOmniWorker)
  strict private
    FDelay_ms: integer;
    FFormHandle: THandle;
    FTaskID    : int64;
  public
    constructor Create(formHandle: THandle; delay_ms: integer = 1000);
    destructor  Destroy; override;
    function  Initialize: boolean; override;
    procedure SleepTask(var msg: TMessage); message MSG_SLEEP;
    procedure Cleanup; override;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestOtlThreadPool.btnCancelAllClick(Sender: TObject);
begin
//  btnScheduleTask.Click;
  btnSchedule6.Click;
  GlobalOmniThreadPool.CancelAll;
end; { TfrmTestOtlThreadPool.btnCancelAllClick }

procedure TfrmTestOtlThreadPool.btnSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    lbLog.Items.SaveToFile(SaveDialog.FileName);
end; { TfrmTestOtlThreadPool.btnSaveLogClick }

procedure TfrmTestOtlThreadPool.btnSchedule6Click(Sender: TObject);
var
  delay_ms: integer;
  iTask   : integer;
  numTasks: integer;
begin
  if Sender = btnSchedule6 then begin
    numTasks := 6;
    delay_ms := 1000;
  end
  else begin
    numTasks := 80;
    delay_ms := 0;
  end;
  if Sender = btnSchedule80All then
    GlobalOmniThreadPool.MaxQueued := 0
  else
    GlobalOmniThreadPool.MaxQueued := 3;
  Log(Format('Scheduling %d tasks. Two should execute immediately, ', [numTasks]));
  if numTasks = 6 then
    Log('three should enter thread queue, one should be rejected (queue too long).')
  else
    Log('some should enter thread queue, some should be rejected (queue too long).');
  for iTask := 1 to numTasks do
    CreateTask(THelloWorker.Create(Handle, delay_ms)).MonitorWith(OmniTED).Schedule;
end;

procedure TfrmTestOtlThreadPool.btnScheduleAndCancelClick(Sender: TObject);
var
  delay_ms: integer;
  i       : integer;
  task    : IOmniTaskControl;
  taskID  : int64;
begin
  if (Sender = btnScheduleAndCancel) or (Sender = btnScheduleAndTerminate) then
    delay_ms := 1000
  else
    delay_ms := 20000;
  GlobalOmniThreadPool.WaitOnTerminate_sec := 3;
  task := CreateTask(THelloWorker.Create(Handle, delay_ms))
    .MonitorWith(OmniTED).Schedule;
  taskID := task.UniqueID;
  for i := 1 to 50 do begin
    Application.ProcessMessages;
    Sleep(10);
  end;
  Log(Format('Cancelling task %d', [taskID]));
  if Sender = btnScheduleAndTerminate then begin
    if task.Terminate(1000) then
      Log(Format('Task %d was terminated', [taskID]))
    else
      Log(Format('Task %d failed to terminate', [taskID]));
  end
  else if GlobalOmniThreadPool.Cancel(taskID) then
    Log(Format('Task %d was cancelled', [taskID]))
  else
    Log(Format('Task %d was killed', [taskID]));
end;

procedure TfrmTestOtlThreadPool.btnScheduleAndWaitClick(Sender: TObject);
var
  iTask    : integer;
  waitGroup: IOmniTaskGroup;
begin
  GlobalOmniThreadPool.MaxQueued := 2;
  Log('Scheduling 3 tasks. Two should execute immediately, one should enter thread queue.');
  waitGroup := CreateTaskGroup;
  for iTask := 1 to 3 do
    CreateTask(THelloWorker.Create(Handle, 1000)).Unobserved.Join(waitGroup).Schedule;
  Log('Waiting on those three tasks. Should take 2 seconds (one for already executing tasks, another for the waiting one)');
  waitGroup.WaitForAll;
  Log('All tasks completed');
end;

procedure TfrmTestOtlThreadPool.btnScheduleClick(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  task := CreateTask(THelloWorker.Create(Handle)).MonitorWith(OmniTED);
  if Sender = btnScheduleTask then
    task.Schedule
  else
    task.Run;
end;

procedure TfrmTestOtlThreadPool.btnScheduleObservedClick(Sender: TObject);
begin
  if assigned(FThreadPool) then
    FThreadPool.CancelAll;
  FThreadPool := CreateThreadPool('Test thread pool')
    .MonitorWith(OmniTED);
  CreateTask(THelloWorker.Create(Handle), 'unobserved task')
    .Unobserved
    .Schedule(FThreadPool);
end;

procedure TfrmTestOtlThreadPool.btnScheduleUnobservedClick(Sender: TObject);
begin
  if assigned(FThreadPool) then
    FThreadPool.CancelAll;
  FThreadPool := CreateThreadPool('Test thread pool');
  FObservableTask := CreateTask(THelloWorker.Create(Handle), 'observed task')
    .OnTerminated(ReleaseObservableTask)
    .Schedule(FThreadPool);
end;

procedure TfrmTestOtlThreadPool.btnTestZeroExecutorThreadsClick(Sender: TObject);
var
  oldExecuting: integer;
  task        : IOmniTaskControl;
begin
  oldExecuting := GlobalOmniThreadPool.MaxExecuting;
  try
    GlobalOmniThreadPool.MaxExecuting := 0;
    Log('Scheduling one task. It should only start executing after one second delay.');
    task := CreateTask(THelloWorker.Create(Handle, 1000)).Unobserved.Schedule;
    Application.ProcessMessages;
    Sleep(1000);
    Log('Starting execution now');
    GlobalOmniThreadPool.MaxExecuting := 1;
    while not task.WaitFor(100) do
      Application.ProcessMessages;
    Log('Tasks completed');
  finally
    GlobalOmniThreadPool.MaxExecuting := oldExecuting;
  end;
end;

procedure TfrmTestOtlThreadPool.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  GlobalOmniThreadPool.CancelAll;
  if assigned(FThreadPool) then begin
    FThreadPool.CancelAll;
    FThreadPool := nil;
  end;
  CanClose := true;
end;

procedure TfrmTestOtlThreadPool.FormCreate(Sender: TObject);
begin
  GlobalOmniThreadPool.MonitorWith(OmniTED);
  GlobalOmniThreadPool.MaxExecuting := 2;
  GlobalOmniThreadPool.MaxQueued := 3;
end;

procedure TfrmTestOtlThreadPool.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss ', Now) + msg);
end;

procedure TfrmTestOtlThreadPool.LogPoolStatus;
begin
  Log(Format('Thread pool status: %d executing / %d queued',
    [GlobalOmniThreadPool.CountExecuting, GlobalOmniThreadPool.CountQueued]));
end;

procedure TfrmTestOtlThreadPool.OmniTEDPoolThreadCreated(const pool: IOmniThreadPool;
  threadID: integer);
begin
  Log(Format('Thread %d created in thread pool %d', [threadID, pool.UniqueID]));
end;

procedure TfrmTestOtlThreadPool.OmniTEDPoolThreadDestroying(const pool: IOmniThreadPool;
  threadID: integer);
begin
  Log(Format('Thread %d destroyed in thread pool %d', [threadID, pool.UniqueID]));
end;

procedure TfrmTestOtlThreadPool.OmniTEDPoolThreadKilled(const pool: IOmniThreadPool;
  threadID: integer);
begin
  Log(Format('Thread %d killed in thread pool %d', [threadID, pool.UniqueID]));
end;

procedure TfrmTestOtlThreadPool.OmniTEDPoolWorkItemCompleted(const pool: IOmniThreadPool;
  taskID: Int64);
begin
  Log(Format('Task %d removed from pool', [taskID]));
  if pool.IsIdle then
    Log(Format('Pool %d is empty', [pool.UniqueID]));
end;

procedure TfrmTestOtlThreadPool.OmniTEDTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  case msg.MsgID of
    MSG_HELLO: begin
      Log(msg.MsgData);
      LogPoolStatus;
    end
    else
      Log(Format('Unknown message %d', [msg.MsgID]));
  end; //case
end;

procedure TfrmTestOtlThreadPool.OmniTEDTaskTerminated(const task: IOmniTaskControl);
begin
  Log(Format('Task %d terminated with status [%d]%s',
    [task.UniqueID, task.ExitCode, task.ExitMessage]));
end;

procedure TfrmTestOtlThreadPool.ReleaseObservableTask(const task: IOmniTaskControl);
begin
  FObservableTask := nil;
end;

procedure TfrmTestOtlThreadPool.WMObjectDestroyed(var msg: TMessage);
begin
  Log(Format('Destroyed worker object for task %d / thread %d', [msg.WParam, msg.LParam]));
end;

procedure TfrmTestOtlThreadPool.WMThreadStateChanged(var msg: TMessage);
begin
  if msg.WParam = 1 then
    Log(Format('Thread %d created', [msg.LParam]))
  else
    Log(Format('Thread %d destroyed', [msg.LParam]));
  LogPoolStatus;
end;

{ THelloWorker }

constructor THelloWorker.Create(formHandle: THandle; delay_ms: integer);
begin
  inherited Create;
  FFormHandle := formHandle;
  FDelay_ms := delay_ms;
end;

destructor THelloWorker.Destroy;
begin
  PostMessage(FFormHandle, WM_OBJECT_DESTROYED, integer(FTaskID), GetCurrentThreadID);
  inherited;
end;

procedure THelloWorker.Cleanup;
begin
  task.Comm.Send(MSG_HELLO, Format('Task %d signing off from thread %d', [task.UniqueID, GetCurrentThreadID]));
end;

function THelloWorker.Initialize: boolean;
begin
  FTaskID := task.UniqueID;
  task.Comm.Send(MSG_HELLO,
    Format('Hello, world! Task %d reporting live from thread %d', [task.UniqueID, GetCurrentThreadID]));
  Task.SetTimer(1, 1, MSG_SLEEP);
  Result := true;
end;

procedure THelloWorker.SleepTask(var msg: TMessage);
begin
  Task.ClearTimer(1);
  Sleep(FDelay_ms);
  Task.Terminate;
end;

end.
