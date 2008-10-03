unit test_18_StringMsgDispatch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TAsyncHello = class(TOmniWorker)
  strict private
    aiMessage: string;
  public
    function  Initialize: boolean; override;
  published
    procedure Change(const data: TOmniValue);
    procedure SendMessage;
  end;

  TfrmTestStringMsgDispatch = class(TForm)
    actChangeMessage      : TAction;
    ActionList            : TActionList;
    actStartHello         : TAction;
    actStopHello          : TAction;
    btnChangeMessage      : TButton;
    btnStartHello         : TButton;
    btnStopHello          : TButton;
    lbLog                 : TListBox;
    OmniEventMonitor1: TOmniEventMonitor;
    btnTestInvalidMsg: TButton;
    procedure actChangeMessageExecute(Sender: TObject);
    procedure actChangeMessageUpdate(Sender: TObject);
    procedure actStartHelloExecute(Sender: TObject);
    procedure actStartHelloUpdate(Sender: TObject);
    procedure actStopHelloExecute(Sender: TObject);
    procedure actStopHelloUpdate(Sender: TObject);
    procedure btnTestInvalidMsgClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
    procedure OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
  strict private
    FHelloTask: IOmniTaskControl;
  private
  end;

var
  frmTestStringMsgDispatch: TfrmTestStringMsgDispatch;

implementation

uses
  DSiWin32;

{$R *.dfm}

{ TfrmTestOTL }

procedure TfrmTestStringMsgDispatch.actChangeMessageExecute(Sender: TObject);
begin
  FHelloTask.Comm.Send('Change', 'Random ' + IntToStr(Random(1234)));
end;

procedure TfrmTestStringMsgDispatch.actChangeMessageUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestStringMsgDispatch.actStartHelloExecute(Sender: TObject);
var
  worker: IOmniWorker;
begin
  worker := TAsyncHello.Create;
  FHelloTask :=
    OmniEventMonitor1.Monitor(CreateTask(worker, 'Hello')).
    SetTimer(1000, 'SendMessage').
    SetParameter('Delay', 1000).
    SetParameter('Message', 'Hello').
    Run;
end;

procedure TfrmTestStringMsgDispatch.actStartHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not assigned(FHelloTask);
end;

procedure TfrmTestStringMsgDispatch.actStopHelloExecute(Sender: TObject);
begin
  FHelloTask.Terminate;
  FHelloTask := nil;
end;

procedure TfrmTestStringMsgDispatch.actStopHelloUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloTask);
end;

procedure TfrmTestStringMsgDispatch.btnTestInvalidMsgClick(Sender: TObject);
begin
  FHelloTask.Comm.Send('FooBar');
end;

procedure TfrmTestStringMsgDispatch.FormCloseQuery(Sender: TObject; var CanClose:
  boolean);
begin
  if actStopHello.Enabled then
    actStopHello.Execute;
end;

procedure TfrmTestStringMsgDispatch.OmniEventMonitor1TaskMessage(const task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TfrmTestStringMsgDispatch.OmniEventMonitor1TaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated %s',
    [task.UniqueID, task.Name, task.ExitMessage]));
end;

{ TAsyncHello }

procedure TAsyncHello.Change(const data: TOmniValue);
begin
  aiMessage := data;
end;

function TAsyncHello.Initialize: boolean;
begin
  aiMessage := Task.ParamByName['Message'];
  Result := true;
end;

procedure TAsyncHello.SendMessage;
begin
  Task.Comm.Send(0, aiMessage);
end;

initialization
  Randomize;
end.
