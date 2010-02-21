unit test_30_AnonymousEventMonitor;

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
  TfrmAnonymousEventMonitorDemo = class(TForm)
    lbLog: TListBox;
    btnHello: TButton;
    procedure btnHelloClick(Sender: TObject);
  private
    FAnonTask: IOmniTaskControl;
  public
    { Public declarations }
  end;

var
  frmAnonymousEventMonitorDemo: TfrmAnonymousEventMonitorDemo;

implementation

{$R *.dfm}

procedure TfrmAnonymousEventMonitorDemo.btnHelloClick(Sender: TObject);
begin
  btnHello.Enabled := false;
  FAnonTask := CreateTask(
    procedure (const task: IOmniTask) begin
      task.Comm.Send(0, Format('Hello, world! Reporting from thread %d',
        [GetCurrentThreadID]));
    end,
    'HelloWorld')
  .OnMessage(
    procedure(const task: IOmniTaskControl; const msg: TOmniMessage) begin
      lbLog.ItemIndex := lbLog.Items.Add(Format('%d:[%d/%s] %d|%s',
        [GetCurrentThreadID, task.UniqueID, task.Name, msg.msgID,
         msg.msgData.AsString]));
    end)
  .OnTerminated(
    procedure(const task: IOmniTaskControl) begin
      lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated',
        [task.UniqueID, task.Name]));
      btnHello.Enabled := true;
      FAnonTask := nil;
    end)
  .Run;
end;

end.
