 unit test_13_Exceptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlEventMonitor, OtlThreadPool;

type
  TfrmTestExceptions = class(TForm)
    btnAV              : TButton;
    btnCleanupException: TButton;
    btnCustomException : TButton;
    btnInitException   : TButton;
    btnRC              : TButton;
    lbLog              : TListBox;
    OmniTED: TOmniEventMonitor;
    cbThreadPool: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure RunObjectTest(Sender: TObject);
    procedure RunTest(Sender: TObject);
  private
    procedure Log(const msg: string);
  strict protected
    function FormatException(const exc: Exception): string;
  public
    procedure TestException(const task: IOmniTask);
  end;

var
  frmTestExceptions: TfrmTestExceptions;

implementation

uses
  SyncObjs,
  DSiWin32,
  OtlCommon;

{$R *.dfm}

const
  EXC_AV     = 1;
  EXC_RC     = 2;
  EXC_CUSTOM = 3;

type
  ECustomException = class(Exception);

  TExceptionTest = class(TOmniWorker)
  strict private
    etExceptInInit: boolean;
  protected
    procedure Cleanup; override;
    function  Initialize: boolean; override;
  public
    constructor Create(exceptionInInit: boolean);
  end;

{ TExceptionTest }

constructor TExceptionTest.Create(exceptionInInit: boolean);
begin
  etExceptInInit := exceptionInInit;
end;

procedure TExceptionTest.Cleanup;
begin
  if not etExceptInInit then
    raise Exception.Create('Exception in Cleanup');
end;

function TExceptionTest.Initialize: boolean;
begin
  if etExceptInInit then
    raise Exception.Create('Exception in Initialize')
  else
    Result := true;
end;

{ TfrmTestOtlComm }

function TfrmTestExceptions.FormatException(const exc: Exception): string;
begin
  Result := '';
  if assigned(exc) then
    Result := exc.ClassName + ': ' + exc.Message;
end;

procedure TfrmTestExceptions.FormCreate(Sender: TObject);
begin
  if DebugHook <> 0 then
    Log('Don''t run this program in the debugger!');
  GlobalOmniThreadPool.MonitorWith(OmniTED);
end;

procedure TfrmTestExceptions.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestExceptions.OmniTEDTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  Log(msg.MsgData);
end;

procedure TfrmTestExceptions.RunObjectTest(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  task := CreateTask(TExceptionTest.Create(Sender = btnInitException)).Enforced(true);
  if cbThreadPool.Checked then
    task.Schedule
  else
    task.Run;
  task.Terminate(3000);
  Log(Format('%d %s %s', [task.ExitCode, task.ExitMessage, FormatException(task.FatalException)]));
end;

procedure TfrmTestExceptions.RunTest(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  task := CreateTask(TestException);
  if cbThreadPool.Checked then
    task.Schedule
  else
    task.Run;
  if Sender = btnAV then
    task.Comm.Send(EXC_AV)
  else if Sender = btnRC then
    task.Comm.Send(EXC_RC)
  else
    task.Comm.Send(EXC_CUSTOM);
  task.WaitFor(30000);
  Log(Format('%d %s %s', [task.ExitCode, task.ExitMessage, FormatException(task.FatalException)]));
end;

procedure TfrmTestExceptions.TestException(const task: IOmniTask);
var
  i  : array [1..1] of integer;
  msg: TOmniMessage;
begin
  WaitForSingleObject(task.Comm.NewMessageEvent, INFINITE);
  task.Comm.Receive(msg);
  if msg.MsgID = EXC_AV then
    PChar(nil)^ := #0
  else if msg.MsgID = EXC_RC then begin
    i[1] := 42;
    i[i[1]] := 0;
  end
  else
    raise ECustomException.Create('Exception test');
end;

end.
