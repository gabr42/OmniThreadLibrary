unit testOtlThreadPool1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlTaskEvents,
  OtlThreadPool;

const
  WM_THREAD_STATE_CHANGED = WM_USER;

type
  TfrmTestOtlThreadPool = class(TForm)
    lbLog  : TListBox;
    OmniTED: TOmniTaskEventDispatch;
    btnSchedule: TButton;
    btnRun: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnScheduleClick(Sender: TObject);
    procedure OmniTEDTaskMessage(task: IOmniTaskControl);
  private
    procedure Asy_ReportWorkerThreadCreated(Sender: TObject; threadID: DWORD);
    procedure Asy_ReportWorkerThreadDestroying(Sender: TObject; threadID: DWORD);
    procedure Log(const msg: string);
    procedure WMThreadStateChanged(var msg: TMessage); message WM_THREAD_STATE_CHANGED;
  strict protected
    procedure HelloWorld(task: IOmniTask);
  end;

var
  frmTestOtlThreadPool: TfrmTestOtlThreadPool;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  MSG_HELLO = 1;

{ TfrmTestOtlComm }

procedure TfrmTestOtlThreadPool.FormDestroy(Sender: TObject);
begin
  GlobalOmniThreadPool.OnWorkerThreadCreated_Asy := nil;
  GlobalOmniThreadPool.OnWorkerThreadDestroying_Asy := nil;
end;

procedure TfrmTestOtlThreadPool.Asy_ReportWorkerThreadCreated(Sender: TObject; threadID:
  DWORD);
begin
  PostMessage(frmTestOtlThreadPool.Handle, WM_THREAD_STATE_CHANGED, 1, threadID);
end;

procedure TfrmTestOtlThreadPool.Asy_ReportWorkerThreadDestroying(Sender: TObject;
  threadID: DWORD);
begin
  PostMessage(frmTestOtlThreadPool.Handle, WM_THREAD_STATE_CHANGED, 0, threadID);
end;

procedure TfrmTestOtlThreadPool.FormCreate(Sender: TObject);
begin
//  OmniTED.Monitor(GlobalOmniThreadPool);
  GlobalOmniThreadPool.MaxExecuting := 2;
//  GlobalOmniThreadPool.MaxQueued := 3; <--- 'tis not working yet
  GlobalOmniThreadPool.OnWorkerThreadCreated_Asy := Asy_ReportWorkerThreadCreated;
  GlobalOmniThreadPool.OnWorkerThreadDestroying_Asy := Asy_ReportWorkerThreadDestroying;
end;

procedure TfrmTestOtlThreadPool.btnScheduleClick(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  task := OmniTED.Monitor(CreateTask(HelloWorld));
  if Sender = btnSchedule then
    task.Schedule
  else
    task.Run;
end;

procedure TfrmTestOtlThreadPool.HelloWorld(task: IOmniTask);
begin
  task.Comm.Send(MSG_HELLO,
    Format('Hello, world! Reporting live from thread %d', [GetCurrentThreadID]));
  Sleep(1000);
  task.Comm.Send(MSG_HELLO, Format('Signing off from thread %d', [GetCurrentThreadID]));
end;

procedure TfrmTestOtlThreadPool.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestOtlThreadPool.OmniTEDTaskMessage(task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  case msg.MsgID of
    MSG_HELLO:
      Log(msg.MsgData);
    else
      Log(Format('Unknown message %d', [msg.MsgID]));
  end; //case
end;

procedure TfrmTestOtlThreadPool.WMThreadStateChanged(var msg: TMessage);
begin
  if msg.WParam = 1 then
    Log(Format('Thread %d created', [msg.LParam]))
  else
    Log(Format('Thread %d destroyed', [msg.LParam]));
end;

end.
