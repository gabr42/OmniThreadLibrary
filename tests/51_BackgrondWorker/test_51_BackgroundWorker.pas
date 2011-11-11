unit test_51_BackgroundWorker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  OtlParallel, StdCtrls;

type
  TForm16 = class(TForm)
    btnWork: TButton;
    lbLog: TListBox;
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
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('Work item %d returned %d',
    [workItem.UniqueID, workItem.Result.AsInteger]));
end;

procedure TForm16.ProcessWorkItem(const workItem: IOmniWorkItem);
begin
  Sleep(workItem.Data);
  workItem.Result := - workItem.Data.AsInteger;
end;

end.
