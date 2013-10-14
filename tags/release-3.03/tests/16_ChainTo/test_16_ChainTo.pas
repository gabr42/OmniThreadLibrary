unit test_16_ChainTo;

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
  TfrmTestChainTo = class(TForm)
    btnStartTasks: TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniEventMonitor;
    procedure btnStartTasksClick(Sender: TObject);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OmniTEDTaskTerminated(const task: IOmniTaskControl);
  private
    procedure BgTask(const task: IOmniTask);
    procedure Log(const msg: string);
  public
  end;

var
  frmTestChainTo: TfrmTestChainTo;

implementation

uses
  SyncObjs,
  DSiWin32;

{$R *.dfm}

const
  MSG_INITIALIZING = 1;

{ TfrmTestOtlComm }

procedure TfrmTestChainTo.BgTask(const task: IOmniTask);
var
  a: real;
  i: integer;
begin
  task.Comm.Send(0, 'Reporting from task ' + task.Name);
  a := 42;
  for i := 1 to 5000 do
    a := Sqr(Sqrt(a));
end;

procedure TfrmTestChainTo.btnStartTasksClick(Sender: TObject);
var
  task1: IOmniTaskControl;
  task2: IOmniTaskControl;
  task3: IOmniTaskControl;
  taskA: IOmniTaskControl;
begin
  lbLog.Clear;
  task3 := CreateTask(BgTask, '3').MonitorWith(OmniTED);
  task2 := CreateTask(BgTask, '2').MonitorWith(OmniTED).ChainTo(task3);
  task1 := CreateTask(BgTask, '1').MonitorWith(OmniTED).ChainTo(task2);
  taskA := CreateTask(BgTask, 'A').MonitorWith(OmniTED).ChainTo(
           CreateTask(BgTask, 'B').MonitorWith(OmniTED).ChainTo(
           CreateTask(BgTask, 'C').MonitorWith(OmniTED)));
  task1.Run; taskA.Run;
end;

procedure TfrmTestChainTo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestChainTo.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  Log(string(msg.MsgData));
end;

procedure TfrmTestChainTo.OmniTEDTaskTerminated(const task: IOmniTaskControl);
begin
  Log(Format('Task terminated: %s', [task.Name]));
end;

end.
