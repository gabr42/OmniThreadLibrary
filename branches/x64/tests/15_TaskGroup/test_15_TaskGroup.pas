unit test_15_TaskGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTestTaskGroup = class(TForm)
    btnStartTasks: TButton;
    btnStopTasks : TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniEventMonitor;
    procedure btnStartTasksClick(Sender: TObject);
    procedure btnStopTasksClick(Sender: TObject);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OmniTEDTaskTerminated(const task: IOmniTaskControl);
  strict private
    FTaskGroup: IOmniTaskGroup;
  private
    procedure Log(const msg: string);
  public
  end;

var
  frmTestTaskGroup: TfrmTestTaskGroup;

implementation

uses
  SyncObjs,
  DSiWin32;

{$R *.dfm}

const
  MSG_INITIALIZING = 1;

type
  TMyWorker = class(TOmniWorker)
  public
    function Initialize: boolean; override;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestTaskGroup.btnStartTasksClick(Sender: TObject);
var
  i: integer;
begin
  FTaskGroup := CreateTaskGroup;
  for i := 1 to 10 do 
    CreateTask(TMyWorker.Create()).MonitorWith(OmniTED).Join(FTaskGroup);
  Log('Starting all tasks');
  FTaskGroup.RunAll;
end;

procedure TfrmTestTaskGroup.btnStopTasksClick(Sender: TObject);
begin
  if assigned(FTaskGroup) then begin
    FTaskGroup.TerminateAll;
    FTaskGroup := nil;
    Log('All stopped');
  end
  else
    Log('Nothing to stop');
end;

procedure TfrmTestTaskGroup.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestTaskGroup.OmniTEDTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  Log(Format('Initializing task %d on thread %d', [integer(msg.MsgData[0]), integer(msg.MsgData[1])]));
end;

procedure TfrmTestTaskGroup.OmniTEDTaskTerminated(const task: IOmniTaskControl);
begin
  Log(Format('Task terminated: %d', [task.UniqueID]));
end;

{ TMyWorker }

function TMyWorker.Initialize: boolean;
begin
  Task.Comm.Send(MSG_INITIALIZING, [Task.UniqueID, GetCurrentThreadID]); 
  Result := true;
end;

end.
