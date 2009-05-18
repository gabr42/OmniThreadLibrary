unit test_28_Hooks;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlHooks, OtlEventMonitor;

const
  WM_NOTIFY_METH = WM_USER;
  WM_NOTIFY_PROC = WM_USER + 1;

type
  TfrmHooksDemo = class(TForm)
    lbLog: TListBox;
    btnRunTask: TButton;
    OtlMonitor: TOmniEventMonitor;
    procedure btnRunTaskClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure NotifyMeth(notifyType: TThreadNotificationType; const threadName: string);
    procedure NotifyFromMeth(var msg: TMessage); message WM_NOTIFY_METH;
    procedure NotifyFromProc(var msg: TMessage); message WM_NOTIFY_PROC;
  public
  end;

var
  frmHooksDemo: TfrmHooksDemo;

implementation

uses
  TypInfo,
  OtlTask,
  OtlTaskControl;

{$R *.dfm}

procedure NotifyProc(notifyType: TThreadNotificationType; const threadName: string);
begin
  PostMessage(frmHooksDemo.Handle, WM_NOTIFY_PROC, WParam(notifyType), 0);  
end;

procedure MyNullTask(const task: IOmniTask);
begin
  Sleep(1000);
end;

{ TfrmHooksDemo }

procedure TfrmHooksDemo.btnRunTaskClick(Sender: TObject);
begin
  CreateTask(MyNullTask).MonitorWith(OtlMonitor).Run;
end;

procedure TfrmHooksDemo.FormDestroy(Sender: TObject);
begin
  UnregisterThreadNotification(NotifyProc);
  UnregisterThreadNotification(NotifyMeth);
end;

procedure TfrmHooksDemo.FormCreate(Sender: TObject);
begin
  RegisterThreadNotification(NotifyProc);
  RegisterThreadNotification(NotifyMeth);
end;

procedure TfrmHooksDemo.NotifyFromMeth(var msg: TMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(
    'Notification from a method: ' + GetEnumName(TypeInfo(TThreadNotificationType), msg.WParam)); 
end;

procedure TfrmHooksDemo.NotifyFromProc(var msg: TMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(
    'Notification from a procedure: ' + GetEnumName(TypeInfo(TThreadNotificationType), msg.WParam)); 
end;

procedure TfrmHooksDemo.NotifyMeth(notifyType: TThreadNotificationType; const threadName:
  string);
begin
  PostMessage(frmHooksDemo.Handle, WM_NOTIFY_METH, WParam(notifyType), 0);
end;

end.
