unit test_56_RunInvoke;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,

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
    procedure TaskTerminated(const task: IOmniTaskControl);
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

procedure TfrmRunInvokeTester.TaskTerminated(const task: IOmniTaskControl);
begin
  FWorker := nil;
  BtnLongWay.Enabled := true;
  BtnShortWay.Enabled := true;
end;

end.
