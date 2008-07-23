unit testOtlThreadPool1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlTaskEvents,
  OtlThreadPool;

type
  TfrmTestOtlThreadPool = class(TForm)
    lbLog                 : TListBox;
    OmniTED: TOmniTaskEventDispatch;
    procedure FormCreate(Sender: TObject);
    procedure OmniTEDTaskMessage(task: IOmniTaskControl);
  private
    procedure Log(const msg: string);
  strict protected
    procedure HelloWorld(task: IOmniTask);
  end;

var
  frmTestOtlThreadPool: TfrmTestOtlThreadPool;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  MSG_HELLO = 1;

{ TfrmTestOtlComm }

procedure TfrmTestOtlThreadPool.FormCreate(Sender: TObject);
begin
  OmniTED.Monitor(CreateTask(HelloWorld, 'Hello, world!')).Schedule;
end;

procedure TfrmTestOtlThreadPool.HelloWorld(task: IOmniTask);
begin
  task.Comm.Send(MSG_HELLO, 'Hello, world!');
end;

procedure TfrmTestOtlThreadPool.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestOtlThreadPool.OmniTEDTaskMessage(task: IOmniTaskControl);
var
  msg: TOmniMessage;
begin
  task.Comm.Receive(msg);
  case msg.MsgID of
    MSG_HELLO:
      Log(msg.MsgData);
    else
      Log(Format('Unknown message %d', [msg.MsgID]));
  end; //case
end;

end.
