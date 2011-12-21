unit test_14_TerminateWhen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlSync,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTestTerminateWhen = class(TForm)
    btnStartTasks: TButton;
    btnStopTasks : TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniEventMonitor;
    procedure btnStartTasksClick(Sender: TObject);
    procedure btnStopTasksClick(Sender: TObject);
    procedure OmniTEDTaskTerminated(const task: IOmniTaskControl);
  strict private
    FCounter  : IOmniCounter;
    FTerminate: IOmniCancellationToken;
  private
    procedure Log(const msg: string);
  public
  end;

var
  frmTestTerminateWhen: TfrmTestTerminateWhen;

implementation

uses
  SyncObjs,
  DSiWin32;

{$R *.dfm}

type
  TMyWorker = class(TOmniWorker)
  public
    procedure Cleanup; override;
  end;

{ TfrmTestOtlComm }

procedure TfrmTestTerminateWhen.btnStartTasksClick(Sender: TObject);
var
  i: integer;
begin
  if assigned(FCounter) and (FCounter.Value > 0) then
    btnStopTasksClick(Sender);
  FCounter := CreateCounter(10);
  FTerminate := CreateOmniCancellationToken;
  for i := 1 to FCounter.Value do begin
    Log(Format('Task started: %d',
      [CreateTask(TMyWorker.Create()).TerminateWhen(FTerminate).WithCounter(FCounter).
         MonitorWith(OmniTED).Run.UniqueID]));
  end;
end;

procedure TfrmTestTerminateWhen.btnStopTasksClick(Sender: TObject);
begin
  if not assigned(FTerminate) then
    Exit;
  FTerminate.Signal;
  while FCounter.Value > 0 do begin // ugly, I know
    Sleep(10);
    Application.ProcessMessages;
  end;
  Log('All stopped');
end;

procedure TfrmTestTerminateWhen.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestTerminateWhen.OmniTEDTaskTerminated(const task: IOmniTaskControl);
begin
  Log(Format('Task terminated: %d', [task.UniqueID]));
end;

{ TMyWorker }

procedure TMyWorker.Cleanup;
begin
  Task.Counter.Decrement;
end;

end.
