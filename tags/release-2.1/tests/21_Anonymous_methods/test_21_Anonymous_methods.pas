unit test_21_Anonymous_methods;

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
  TfrmAnonymousMethodsDemo = class(TForm)
    lbLog: TListBox;
    btnHello: TButton;
    OTLMonitor: TOmniEventMonitor;
    procedure btnHelloClick(Sender: TObject);
    procedure OTLMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OTLMonitorTaskTerminated(const task: IOmniTaskControl);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAnonymousMethodsDemo: TfrmAnonymousMethodsDemo;

implementation

{$R *.dfm}

procedure TfrmAnonymousMethodsDemo.btnHelloClick(Sender: TObject);
begin
  btnHello.Enabled := false;
  OTLMonitor.Monitor(CreateTask(
    procedure (const task: IOmniTask) begin
      task.Comm.Send(0, Format('Hello, world! Reporting from thread %d', [GetCurrentThreadID]));
    end,
    'HelloWorld')).Run;
end;

procedure TfrmAnonymousMethodsDemo.OTLMonitorTaskMessage(
  const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('%d:[%d/%s] %d|%s',
    [GetCurrentThreadID, task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TfrmAnonymousMethodsDemo.OTLMonitorTaskTerminated(const task:
    IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
  btnHello.Enabled := true;
end;

end.
