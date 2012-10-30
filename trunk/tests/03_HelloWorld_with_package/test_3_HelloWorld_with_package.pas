unit test_3_HelloWorld_with_package;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor,
  OtlComm;

type
  TfrmTestHelloWorld = class(TForm)
    btnHello        : TButton;
    lbLog           : TListBox;
    OmniEventMonitor1: TOmniEventMonitor;
    procedure btnHelloClick(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl; const msg:
      TOmniMessage);
    procedure OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
  private
    procedure RunHelloWorld(const task: IOmniTask);
  end;

var
  frmTestHelloWorld: TfrmTestHelloWorld;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestHelloWorld.btnHelloClick(Sender: TObject);
begin
  btnHello.Enabled := false;
  OmniEventMonitor1.Monitor(CreateTask(RunHelloWorld, 'HelloWorld')).Run;
end;

procedure TfrmTestHelloWorld.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.MsgID, msg.MsgData.AsString]));
end;

procedure TfrmTestHelloWorld.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
  btnHello.Enabled := true;
end;

procedure TfrmTestHelloWorld.RunHelloWorld(const task: IOmniTask);
begin
  //Executed in a background thread
  task.Comm.Send(0, 'Hello, world!');
end;

initialization
  Randomize;
end.
