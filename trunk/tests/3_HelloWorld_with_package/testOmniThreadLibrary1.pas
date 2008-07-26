unit testOmniThreadLibrary1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTestOTL = class(TForm)
    btnHello        : TButton;
    lbLog           : TListBox;
    OmniEventMonitor1: TOmniEventMonitor;
    procedure btnHelloClick(Sender: TObject);
    procedure OmniEventMonitor1TaskMessage(task: IOmniTaskControl);
    procedure OmniEventMonitor1TaskTerminated(task: IOmniTaskControl);
  strict private
  private
    procedure RunHelloWorld(task: IOmniTask);
  end;

var
  frmTestOTL: TfrmTestOTL;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestOTL.btnHelloClick(Sender: TObject);
begin
  btnHello.Enabled := false;
  OmniEventMonitor1.Monitor(CreateTask(RunHelloWorld, 'HelloWorld')).Run;
end;

procedure TfrmTestOTL.OmniEventMonitor1TaskMessage(task: IOmniTaskControl);
var
  msgID  : word;
  msgData: TOmniValue;
begin
  task.Comm.Receive(msgID, msgData);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s', [task.UniqueID, task.Name, msgID, msgData]));
end;

procedure TfrmTestOTL.OmniEventMonitor1TaskTerminated(task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
  btnHello.Enabled := true;
end;

procedure TfrmTestOTL.RunHelloWorld(task: IOmniTask);
begin
  //Executed in a background thread
  task.Comm.Send(0, 'Hello, world!');
end;

initialization
  Randomize;
end.
