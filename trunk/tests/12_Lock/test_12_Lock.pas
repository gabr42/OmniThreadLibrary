unit test_12_Lock;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl,
  OtlContainers,
  OtlComm,
  OtlEventMonitor;

type
  TfrmTestLock = class(TForm)
    btnLock      : TButton;
    btnNoLock    : TButton;
    btnTestLock  : TButton;
    btnTestNoLock: TButton;
    lbLog        : TListBox;
    OmniTED      : TOmniEventMonitor;
    procedure btnNoLockClick(Sender: TObject);
    procedure btnTestLockClick(Sender: TObject);
    procedure OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  private
    procedure Log(const msg: string);
  public
    procedure TestLock(const task: IOmniTask);
  end;

var
  frmTestLock: TfrmTestLock;

implementation

uses
  SyncObjs,
  DSiWin32,
  OtlCommon;

{$R *.dfm}

const
  CTestRepetitions = 1000;

type
  TTestArray = array [1..10000] of integer;
  PTestArray = ^TTestArray;

var
  FTestArray: TTestArray;

function OneTest(lock: TSynchroObject): boolean;
var
  i         : integer;
  startValue: integer;
begin
  Result := false;
  if assigned(lock) then
    lock.Acquire;
  try
    startValue := Random(10000);
    for i := Low(FTestArray) to High(FTestArray) do
      FTestArray[i] := startValue + i;
    for i := Low(FTestArray) to High(FTestArray) do
      if FTestArray[i] <> (startValue + i) then 
        Exit;
    Result := true;
  finally
    if assigned(lock) then
      lock.Release;
  end;
end;

procedure TestArray(const task: IOmniTask);
var
  iRepeat: integer;
begin
  for iRepeat := 1 to CTestRepetitions do begin
    if WaitForSingleObject(task.TerminateEvent, 0) = WAIT_OBJECT_0 then
      break; //for
    if not OneTest(task.Lock) then
      task.Comm.Send(0, Format('Test failed at repetition %d', [iRepeat]));
  end;
end;

{ TfrmTestOtlComm }

procedure TfrmTestLock.btnNoLockClick(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  task := CreateTask(TestLock);
  if Sender = btnLock then
    task.WithLock(TCriticalSection.Create);
  task.Run.Terminate;
  Log(Format('%d %s', [task.ExitCode, task.ExitMessage]));
end;

procedure TfrmTestLock.btnTestLockClick(Sender: TObject);
var
  task: IOmniTaskControl;
  iRepeat: integer;
begin
  task := CreateTask(TestArray);
  if sender = btnTestLock then
    task.WithLock(TCriticalSection.Create);
  task.Run;
  for iRepeat := 1 to CTestRepetitions do
    if not OneTest(task.Lock) then begin
      Log(Format('Test failed at repetition %d', [iRepeat]));
      task.Terminate;
      break; //for iRepeat
    end;
  task.WaitFor(INFINITE);
  Log('Completed');
end;

procedure TfrmTestLock.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmTestLock.OmniTEDTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  Log(msg.MsgData);
end;

procedure TfrmTestLock.TestLock(const task: IOmniTask);
begin
  if assigned(task.Lock) then
    task.SetExitStatus(1, 'has lock')
  else
    task.SetExitStatus(0, 'no lock');
end;

end.
