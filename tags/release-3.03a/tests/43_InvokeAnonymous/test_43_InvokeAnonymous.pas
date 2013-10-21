unit test_43_InvokeAnonymous;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmInvokeAnonymousDemo = class(TForm)
    lbLog: TListBox;
    btnInvoke: TButton;
    btnInvokeMonitored: TButton;
    OmniEventMonitor1: TOmniEventMonitor;
    procedure btnInvokeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg:
      TOmniMessage);
  private
    FMonitoredTask: IOmniTaskControl;
    FTask: IOmniTaskControl;
  public
  end;

var
  frmInvokeAnonymousDemo: TfrmInvokeAnonymousDemo;

implementation

{$R *.dfm}

type
  TTask = class(TOmniWorker)
  end; { TTask }

procedure TfrmInvokeAnonymousDemo.btnInvokeClick(Sender: TObject);
var
  formThreadID: DWORD;
  task        : IOmniTaskControl;
begin
  formThreadID := GetCurrentThreadID;
  if Sender = btnInvoke then
    task := FTask
  else
    task := FMonitoredTask;
  task.Invoke(
    procedure (const task: IOmniTask)
    var
      taskThreadID: DWORD;
    begin
      // this will execute in the context of the worker thread
      taskThreadID := GetCurrentThreadID;
      task.Invoke(
        procedure
        begin
          // this will execute in the context of the main thread
          frmInvokeAnonymousDemo.lbLog.Items.Add(Format(
            'Current thread ID: %d, task thread ID: %d, ' +
            ' form thread ID: %d',
            [GetCurrentThreadID, taskThreadID, formThreadID]));
        end
      );
    end
  );
end;

procedure TfrmInvokeAnonymousDemo.FormDestroy(Sender: TObject);
begin
  FTask.Terminate;
  FTask := nil;
  FMonitoredTask.Terminate;
  FMonitoredTask := nil;
end;

procedure TfrmInvokeAnonymousDemo.FormCreate(Sender: TObject);
begin
  FTask := CreateTask(TTask.Create())
    .OnMessage(Self)
    .Run;
  FMonitoredTask := CreateTask(TTask.Create())
    .MonitorWith(OmniEventMonitor1)
    .Run;
end; { TfrmInvokeAnonymousDemo.FormCreate }

procedure TfrmInvokeAnonymousDemo.OmniEventMonitor1TaskMessage(const task:
  IOmniTaskControl; const msg: TOmniMessage);
begin
  lbLog.Items.Add(Format('Received message %d', [msg.MsgID]));
end;

end.
