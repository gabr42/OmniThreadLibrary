unit test_56_RunInvoke;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  OtlTaskControl,
  OtlComm;

const
  WM_LOG = WM_USER;

type
  TfrmRunInvokeTester = class(TForm)
    BtnLongWay: TButton;
    BtnShortWay: TButton;
    LBLog: TListBox;
    procedure BtnLongWayClick(Sender: TObject);
    procedure BtnShortWayClick(Sender: TObject);
  private
    FWorker: IOmniTaskControl;
    procedure LogMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure TaskTerminated;
  public
  end;

var
  frmRunInvokeTester: TfrmRunInvokeTester;

implementation

{$R *.dfm}

type
  TWorker = class(TOmniWorker)
  published
    procedure MyExecute;
  end;

{ TWorker }

procedure TWorker.MyExecute;
begin
  Task.Comm.Send(WM_LOG, 'Executing ...');

  //payload
  Sleep(3000);

  Task.Comm.Send(WM_LOG, 'Done');
  Task.Terminate;
end;

procedure TfrmRunInvokeTester.BtnLongWayClick(Sender: TObject);
begin
  BtnLongWay.Enabled := false;
  BtnShortWay.Enabled := false;

  FWorker := CreateTask(TWorker.Create())
    .OnMessage(WM_USER, LogMessage)
    .OnTerminated(TaskTerminated)
    .Run;

  FWorker.Invoke(@TWorker.MyExecute);
end;

procedure TfrmRunInvokeTester.BtnShortWayClick(Sender: TObject);
begin
  BtnLongWay.Enabled := false;
  BtnShortWay.Enabled := false;

  FWorker := CreateTask(TWorker.Create())
    .OnMessage(WM_USER, LogMessage)
    .OnTerminated(TaskTerminated)
    .Run(@TWorker.MyExecute);
end;

procedure TfrmRunInvokeTester.LogMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  LBLog.ItemIndex := LBLog.Items.Add(FormatDateTime('hh:nn:ss ', Now) + msg.MsgData.AsString);
end;

procedure TfrmRunInvokeTester.TaskTerminated;
begin
  FWorker := nil;
  BtnLongWay.Enabled := true;
  BtnShortWay.Enabled := true;
end;

end.
