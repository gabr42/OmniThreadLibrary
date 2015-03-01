unit test_52_BackgroundWorker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  OtlParallel, StdCtrls;

type
  TForm16 = class(TForm)
    btnWork: TButton;
    lbLog: TListBox;
    btnWork3: TButton;
    btnException: TButton;
    btnCancel: TButton;
    btnCancelAll: TButton;
    btnCancel2: TButton;
    procedure btnCancel2Click(Sender: TObject);
    procedure btnCancelAllClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExceptionClick(Sender: TObject);
    procedure btnWork3Click(Sender: TObject);
    procedure btnWorkClick(Sender: TObject);
  private
    FBackgroundWorker1: IOmniBackgroundWorker;
    FBackgroundWorker2: IOmniBackgroundWorker;
    procedure HandleRequestDone1(const Sender: IOmniBackgroundWorker;
      const workItem: IOmniWorkItem);
    procedure ProcessWorkItem1(const workItem: IOmniWorkItem);
    procedure HandleRequestDone2(const Sender: IOmniBackgroundWorker;
      const workItem: IOmniWorkItem);
    procedure ProcessWorkItem2(const workItem: IOmniWorkItem);
    function ScheduleRandomSleep: IOmniWorkItem;
  public
  end;

var
  Form16: TForm16;

implementation

{$R *.dfm}

uses
  OtlTaskControl,
  OtlComm;

const
  MSG_PROCESSING = 1;

procedure TForm16.btnCancel2Click(Sender: TObject);
var
  workItem: IOmniWorkItem;
begin
  ScheduleRandomSleep;
  workItem := ScheduleRandomSleep;
  ScheduleRandomSleep;
  FBackgroundWorker2.CancelAll(workItem.UniqueID);
end;

procedure TForm16.btnCancelAllClick(Sender: TObject);
begin
  ScheduleRandomSleep;
  ScheduleRandomSleep;
  FBackgroundWorker2.CancelAll;
  ScheduleRandomSleep;
end;

procedure TForm16.btnCancelClick(Sender: TObject);
var
  workItem: IOmniWorkItem;
begin
  ScheduleRandomSleep;
  workItem := ScheduleRandomSleep;
  ScheduleRandomSleep;
  workItem.CancellationToken.Signal;
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  FBackgroundWorker1 := Parallel.BackgroundWorker
    .Execute(ProcessWorkItem1);
  FBackgroundWorker2 := Parallel.BackgroundWorker
    .NumTasks(2)
    .OnRequestDone(HandleRequestDone2)
    .TaskConfig(Parallel.TaskConfig.OnMessage(MSG_PROCESSING,
      procedure (const task: IOmniTaskControl; const msg: TOmniMessage)
      begin
        lbLog.Items.Add(Format('Started processing work item %d', [msg.MsgData.AsInt64]));
      end))
    .Execute(ProcessWorkItem2);
end;

procedure TForm16.btnExceptionClick(Sender: TObject);
begin
  lbLog.ItemIndex := lbLog.Items.Add('Scheduling two work items, first will raise an exception, second will not');
  FBackgroundWorker1.Schedule(FBackgroundWorker1.CreateWorkItem(true),  // raise exception
    FBackgroundWorker1.Config.OnRequestDone(HandleRequestDone1));
  FBackgroundWorker1.Schedule(FBackgroundWorker1.CreateWorkItem(false), // no exception
    FBackgroundWorker1.Config.OnRequestDone(HandleRequestDone1));
end;

procedure TForm16.btnWork3Click(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 3 do
    btnWorkClick(Sender);
end;

procedure TForm16.btnWorkClick(Sender: TObject);
begin
  ScheduleRandomSleep;
end;

procedure TForm16.HandleRequestDone1(const Sender: IOmniBackgroundWorker; const workItem:
  IOmniWorkItem);
var
  eClass: string;
  exc   : Exception;
begin
  if workItem.IsExceptional then begin
    eClass := workItem.FatalException.ClassName;
    exc := workItem.DetachException;
    lbLog.ItemIndex := lbLog.Items.Add(Format('Exception raised in work item: %s:%s',
      [eClass, exc.Message]));
    FreeAndNil(exc);
  end
  else
    lbLog.ItemIndex := lbLog.Items.Add(workItem.Result)
end;

procedure TForm16.HandleRequestDone2(const Sender: IOmniBackgroundWorker; const workItem:
  IOmniWorkItem);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Work item %d returned %d',
    [workItem.UniqueID, workItem.Result.AsInteger]));
end;

procedure TForm16.ProcessWorkItem1(const workItem: IOmniWorkItem);
begin
  if workItem.Data then
    raise Exception.Create('Exception in work item')
  else
    workItem.Result := 'Processing complete';
end;

procedure TForm16.ProcessWorkItem2(const workItem: IOmniWorkItem);
var
  i: integer;
begin
  workItem.Task.Comm.Send(MSG_PROCESSING, workItem.UniqueID);
  for i := 1 to workItem.Data.AsInteger div 100 do begin
    Sleep(100);
    if workItem.CancellationToken.IsSignalled then begin
      workItem.Result := -42;
      Exit;
    end;
  end;
  workItem.Result := - workItem.Data.AsInteger;
end;

function TForm16.ScheduleRandomSleep: IOmniWorkItem;
begin
  Result := FBackgroundWorker2.CreateWorkItem(1000 + 100 * Random(10) {sleep time in ms});
  FBackgroundWorker2.Schedule(Result);
  lbLog.ItemIndex := lbLog.Items.Add(Format('Created work item %d with delay %d',
    [Result.UniqueID, Result.Data.AsInteger]));
end;

end.
