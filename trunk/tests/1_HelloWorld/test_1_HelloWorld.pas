unit test_1_HelloWorld;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTestHelloWorld = class(TForm)
    btnHello        : TButton;
    lbLog           : TListBox;
    procedure btnHelloClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    FMessageDispatch: TOmniEventMonitor;
  private
    procedure HandleTaskTerminated(const task: IOmniTaskControl);
    procedure HandleTaskMessage(const task: IOmniTaskControl);
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
  FMessageDispatch.Monitor(CreateTask(RunHelloWorld, 'HelloWorld')).Run;
end;

procedure TfrmTestHelloWorld.FormCreate(Sender: TObject);
begin
  FMessageDispatch := TOmniEventMonitor.Create(Self);
  FMessageDispatch.OnTaskMessage := HandleTaskMessage;
  FMessageDispatch.OnTaskTerminated := HandleTaskTerminated;
end;

procedure TfrmTestHelloWorld.HandleTaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
  btnHello.Enabled := true;
end; { TfrmTestOTL.HandleTaskTerminated }

procedure TfrmTestHelloWorld.HandleTaskMessage(const task: IOmniTaskControl);
var
  msgID  : word;
  msgData: TOmniValue;
begin
  task.Comm.Receive(msgID, msgData);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s', [task.UniqueID, task.Name, msgID, msgData]));
end;

procedure TfrmTestHelloWorld.RunHelloWorld(const task: IOmniTask);
begin
  //Executed in a background thread
  task.Comm.Send(0, 'Hello, world!');
end;

initialization
  Randomize;
end.
