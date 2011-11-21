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
    procedure btnExceptionClick(Sender: TObject);
    procedure btnWork3Click(Sender: TObject);
    procedure btnWorkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBackgroundWorker: IOmniBackgroundWorker;
    procedure HandleRequestDone(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
    procedure ProcessWorkItem(const workItem: IOmniWorkItem);
  public
  end;

var
  Form16: TForm16;

implementation

{$R *.dfm}

procedure TForm16.btnExceptionClick(Sender: TObject);
begin
  lbLog.ItemIndex := lbLog.Items.Add('Scheduling three work items, first will raise an exception, next two will not');
  FBackgroundWorker.Schedule(FBackgroundWorker.CreateWorkItem(true)); // raise exception
  FBackgroundWorker.Schedule(FBackgroundWorker.CreateWorkItem(false)); // no exception
  FBackgroundWorker.Schedule(FBackgroundWorker.CreateWorkItem(false)); // no exception
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
  workItem := FBackgroundWorker.CreateWorkItem(1000 + 100 * Random(10) {sleep time in ms});
  FBackgroundWorker.Schedule(workItem);
  lbLog.ItemIndex := lbLog.Items.Add(Format('Created work item %d with delay %d',
    [workItem.UniqueID, workItem.Data.AsInteger]));
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  FBackgroundWorker := Parallel.BackgroundWorker
    .NumTasks(2)
    .OnRequestDone(HandleRequestDone)
    .Execute(ProcessWorkItem);
end;

procedure TForm16.HandleRequestDone(const Sender: IOmniBackgroundWorker;
  const workItem: IOmniWorkItem);
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
  else if workItem.Result.IsString then
    lbLog.ItemIndex := lbLog.Items.Add(workItem.Result)
  else
    lbLog.ItemIndex := lbLog.Items.Add(Format('Work item %d returned %d',
      [workItem.UniqueID, workItem.Result.AsInteger]));
end;

procedure TForm16.ProcessWorkItem(const workItem: IOmniWorkItem);
begin
  if workItem.Data.IsBoolean then begin // exception test
    if workItem.Data then
      raise Exception.Create('Exception in work item')
    else
      workItem.Result := 'Processing complete';
  end
  else begin // 'sleep' test
    Sleep(workItem.Data);
    workItem.Result := - workItem.Data.AsInteger;
  end;
end;

end.
