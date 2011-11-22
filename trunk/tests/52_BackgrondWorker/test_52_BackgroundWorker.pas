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
  public
  end;

var
  Form16: TForm16;

implementation

{$R *.dfm}

procedure TForm16.FormCreate(Sender: TObject);
begin
  FBackgroundWorker1 := Parallel.BackgroundWorker
    .Execute(ProcessWorkItem1);
  FBackgroundWorker2 := Parallel.BackgroundWorker
    .NumTasks(2)
    .OnRequestDone(HandleRequestDone2)
    .Execute(ProcessWorkItem2);
end;

procedure TForm16.btnExceptionClick(Sender: TObject);
begin
  lbLog.ItemIndex := lbLog.Items.Add('Scheduling two work items, first will raise an exception, second will not');
  FBackgroundWorker1.Schedule(FBackgroundWorker1.CreateWorkItem(true),
    FBackgroundWorker1.Config.OnRequestDone(HandleRequestDone1)); // raise exception
  FBackgroundWorker1.Schedule(FBackgroundWorker1.CreateWorkItem(false),
    FBackgroundWorker1.Config.OnRequestDone(HandleRequestDone1)); // no exception
end;

procedure TForm16.btnWork3Click(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 3 do
    btnWorkClick(Sender);
end;

procedure TForm16.btnWorkClick(Sender: TObject);
var
  workItem: IOmniWorkItem;
begin
  workItem := FBackgroundWorker2.CreateWorkItem(1000 + 100 * Random(10) {sleep time in ms});
  FBackgroundWorker2.Schedule(workItem);
  lbLog.ItemIndex := lbLog.Items.Add(Format('Created work item %d with delay %d',
    [workItem.UniqueID, workItem.Data.AsInteger]));
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
begin
  Sleep(workItem.Data);
  workItem.Result := - workItem.Data.AsInteger;
end;

end.
