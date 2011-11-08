unit test_50_OmniValueArray;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl;

type
  TfrmOmniValueArray = class(TForm)
    lbLog: TListBox;
    btnSendHash: TButton;
    btnSendArray: TButton;
    procedure btnSendArrayClick(Sender: TObject);
    procedure btnSendHashClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FWorker: IOmniTaskControl;
    procedure ShowResult(const task: IOmniTaskControl; const msg: TOmniMessage);
  public
  end;

var
  frmOmniValueArray: TfrmOmniValueArray;

implementation

uses
  DSiWin32;

{$R *.dfm}

const
  MSG_ARRAY = 1;
  MSG_HASH = 2;

  MSG_RESULT = WM_USER;

procedure Worker(const task: IOmniTask);
var
  i      : integer;
  msgData: TOmniValue;
  msgID  : word;
  s      : string;
begin
  while DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent, false, INFINITE) = WAIT_OBJECT_1 do
  begin
    task.Comm.Receive(msgID, msgData);
    case msgID of
      MSG_ARRAY:
        begin
          s := '';
          for i := 0 to msgData.AsArray.Count - 1 do
            s := s + IntToStr(msgData[i]) + ' ';
          task.Comm.Send(MSG_RESULT, 'Received array with values: ' + s);
        end;
      MSG_HASH:
        begin
          s := Format('%s is %s', [msgData['Person'].AsString, msgData['Location'].AsString]);
          task.Comm.Send(MSG_RESULT, s);
        end;
    end;
  end;
end;

procedure TfrmOmniValueArray.btnSendArrayClick(Sender: TObject);
begin
  FWorker.Comm.Send(MSG_ARRAY, [0, 1, 1, 2, 3, 5, 8]);
  //equivalent code:
  //FWorker.Comm.Send(MSG_ARRAY, TOmniValue.Create([0, 1, 1, 2, 3, 5, 8]));
end;

procedure TfrmOmniValueArray.btnSendHashClick(Sender: TObject);
begin
  FWorker.Comm.Send(MSG_HASH, TOmniValue.CreateNamed([
    'Person', 'Primoz',
    'Location', 'at home']));
end;

procedure TfrmOmniValueArray.FormDestroy(Sender: TObject);
begin
  FWorker.Terminate;
  FWorker := nil;
end;

procedure TfrmOmniValueArray.FormCreate(Sender: TObject);
begin
  FWorker := CreateTask(Worker).OnMessage(MSG_RESULT, ShowResult).Run;
end;

procedure TfrmOmniValueArray.ShowResult(const task: IOmniTaskControl; const msg:
  TOmniMessage);
begin
  lbLog.Items.Add(msg.MsgData);
end;

end.
