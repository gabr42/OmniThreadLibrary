unit test_26_MultiEventMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls,
  OtlComm,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;
                
type
  TfrmMultiMonitorDemo = class(TForm)
    btnStart       : TButton;
    cbRefreshScreen: TCheckBox;
    Label1         : TLabel;
    Label2         : TLabel;
    lbFiles        : TListBox;
    seMessagesCount: TSpinEdit;
    seMonitors     : TSpinEdit;
    StatusBar      : TStatusBar;
    Timer1         : TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OTLMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OTLMonitorTaskTerminated(const task: IOmniTaskControl);
    procedure OTLRefreshTimeOut(const task: IOmniTaskControl);
    procedure Timer1Timer(Sender: TObject);
  private
    FTasks         : array[0..15] of IOmniTaskControl;
    MsgCount       : Cardinal;
    OTLMonitors    : array[0..15] of TOmniEventMonitor;
  end;

var
  ExitCounter        : cardinal;
  frmMultiMonitorDemo: TfrmMultiMonitorDemo;

implementation

uses
  GpStuff;

{$R *.dfm}

const
  MSG_STRING = 1;

procedure TaskProcedure(const task: IOmniTask);
var
  comm: IOmniCommunicationEndpoint;
  n   : cardinal;
begin
  comm := task.Comm;
  for n := 0 to frmMultiMonitorDemo.seMessagesCount.value -1 do
    while not task.Terminated and not comm.SendWait(MSG_STRING, task.Name + IntToStr(n)) do;
end;

procedure TfrmMultiMonitorDemo.btnStartClick(Sender: TObject);
var
  n: Cardinal;
begin
  ExitCounter := 0;
  lbFiles.Clear;
  btnStart.Enabled := false;
  seMonitors.Enabled := false;
  StatusBar.SimpleText := '';
  MsgCount := 0;
  for n := 0 to seMonitors.Value - 1 do
  begin
    OTLMonitors[n] := TOmniEventMonitor.create(self);
    OTLMonitors[n].OnTaskMessage := OTLMonitorTaskMessage;
    OTLMonitors[n].OnTaskTerminated := OTLMonitorTaskTerminated;
    OTLMonitors[n].OnRefreshTimeOut := OTLRefreshTimeOut;
    FTasks[n] := CreateTask(TaskProcedure, 'Task ' + Char(n + ord('A')) + ': ')
      .MonitorWith(OTLMonitors[n])
      .Run;
  end;
end;

procedure TfrmMultiMonitorDemo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  n: cardinal;
begin
  // TODO 1 -oPrimoz Gabrijelcic : testing, remove! 
  TGpTraceable($7FF99CF0).TraceReferences := true;
  //>
  for n := 0 to seMonitors.Value - 1 do
    if assigned(FTasks[n]) then begin
      FTasks[n].Terminate;
      FTasks[n] := nil;
    end;
  for n := 0 to seMonitors.Value - 1 do begin
    OTLMonitors[n].Free;
    OTLMonitors[n] := nil;
  end;
  CanClose := True;
end;

procedure TfrmMultiMonitorDemo.OTLMonitorTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  if msg.MsgID = MSG_STRING then begin
    inc(MsgCount);
    lbFiles.Items.Add('Message: ' +IntToStr(MsgCount) + ': ' + msg.MsgData);
  end;
end;

procedure TfrmMultiMonitorDemo.OTLRefreshTimeOut(const task: IOmniTaskControl);
begin
  if cbRefreshScreen.Checked then
    Application.ProcessMessages;
end;

procedure TfrmMultiMonitorDemo.OTLMonitorTaskTerminated(const task: IOmniTaskControl);
begin
  inc(ExitCounter);
  if ExitCounter >= cardinal(seMonitors.Value) then
  begin
    StatusBar.SimpleText := 'Message number: ' + IntToStr(MsgCount);
    btnStart.Enabled := true;
    seMonitors.Enabled := true;
  end;
end;

procedure TfrmMultiMonitorDemo.Timer1Timer(Sender: TObject);
begin
  StatusBar.SimpleText := 'Message number: ' + IntToStr(MsgCount);
end;

end.
