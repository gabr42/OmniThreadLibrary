unit testTaskGroup1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlTaskEvents;

type
  TfrmTestTaskGroup = class(TForm)
    btnStartTasks: TButton;
    btnStopTasks : TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniTaskEventDispatch;
    procedure btnStartTasksClick(Sender: TObject);
    procedure btnStopTasksClick(Sender: TObject);
    procedure OmniTEDTaskTerminated(task: IOmniTaskControl);
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
  DSiWin32,
  SpinLock;

{$R *.dfm}

{ TfrmTestOtlComm }

procedure TfrmTestTaskGroup.btnStartTasksClick(Sender: TObject);
var
  i: integer;
begin
  FTaskGroup := CreateTaskGroup;
  for i := 1 to 10 do begin
    Log(Format('Task started: %d',
      [CreateTask(TOmniWorker.Create()).MonitorWith(OmniTED).FreeOnTerminate.Join(FTaskGroup).Run.UniqueID]));
  end;
end;

procedure TfrmTestTaskGroup.btnStopTasksClick(Sender: TObject);
begin
  FTaskGroup.TerminateAll;
  FTaskGroup := nil;
  Log('All stopped');
end;

procedure TfrmTestTaskGroup.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestTaskGroup.OmniTEDTaskTerminated(task: IOmniTaskControl);
begin
  Log(Format('Task terminated: %d', [task.UniqueID]));
end;

{ TMyWorker }

end.
