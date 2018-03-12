unit test_24_ConnectionPool;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlThreadPool,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmConnectionPoolDemo = class(TForm)
    lbLog: TListBox;
    btnSchedule: TButton;
    btnScheduleAndWait: TButton;
    OTLMonitor: TOmniEventMonitor;
    procedure btnScheduleAndWaitClick(Sender: TObject);
    procedure btnScheduleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OTLMonitorTaskTerminated(const task: IOmniTaskControl);
  private
    FConnectionPool: IOmniThreadPool;
    function  CreateThreadData: IInterface;
    procedure Log(const msg: string);
    procedure WMMessage(var msg: TMessage); message WM_USER;
    procedure WMTaskStarted(var msg: TMessage); message WM_USER+1;
  public
  end;

var
  frmConnectionPoolDemo: TfrmConnectionPoolDemo;

implementation

uses
  OtlCommon,
  OtlTask;

{$R *.dfm}

const
  MSG_CREATING_CONNECTION  = 1;
  MSG_CREATED_CONNECTION   = 2;
  MSG_DESTROY_CONNECTION   = 3;
  MSG_DESTROYED_CONNECTION = 4;

type
  IConnectionPoolData = interface ['{F604640D-6D4E-48B4-9A8C-483CA9635C71}']
    function ConnectionID: integer;
  end;

  TConnectionPoolData = class(TInterfacedObject, IConnectionPoolData)
  strict private
    cpID: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    function ConnectionID: integer;
  end; { TConnectionPoolData }

var
  GConnPoolID: IOmniCounter;

procedure PostToForm(winMsg: cardinal; msg, data: integer);
begin
  PostMessage(frmConnectionPoolDemo.Handle, winMsg, msg, data);
end;

procedure TaskProc(const task: IOmniTask);
begin
  PostToForm(WM_USER + 1, task.UniqueID,
    (task.ThreadData as IConnectionPoolData).ConnectionID);
  Sleep(3000);
end;

{ TfrmConnectionPoolDemo }

procedure TfrmConnectionPoolDemo.btnScheduleAndWaitClick(Sender: TObject);
var
  task: IOmniTaskControl;
begin
  Screen.Cursor := crHourGlass;
  Log('Creating task');
  task := CreateTask(TaskProc).MonitorWith(OTLMonitor).Schedule(FConnectionPool);
  task.WaitFor(10000);
  task.Terminate;
  Screen.Cursor := crDefault;
  Application.ProcessMessages; //allow accumulated log messages to be processed
  Log('Awaited task termination');
end;
                    
procedure TfrmConnectionPoolDemo.btnScheduleClick(Sender: TObject);
begin
  Log('Creating task');
  CreateTask(TaskProc).MonitorWith(OTLMonitor).Schedule(FConnectionPool);
end;

function TfrmConnectionPoolDemo.CreateThreadData: IInterface;
begin
  Result := TConnectionPoolData.Create;
end;

procedure TfrmConnectionPoolDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FConnectionPool.CancelAll;
end;

procedure TfrmConnectionPoolDemo.FormCreate(Sender: TObject);
begin
  FConnectionPool := CreateThreadPool('Connection pool');
  FConnectionPool.SetThreadDataFactory(CreateThreadData);
  FConnectionPool.MaxExecuting := 3;
end;

procedure TfrmConnectionPoolDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss ', Now) + msg);
end;

procedure TfrmConnectionPoolDemo.OTLMonitorTaskTerminated(const task: IOmniTaskControl);
begin
  Log(Format('Task %d terminated', [task.UniqueID]));
end;

procedure TfrmConnectionPoolDemo.WMMessage(var msg: TMessage);
begin
  case msg.WParam of
    MSG_CREATING_CONNECTION:
      Log(Format('Creating connection in thread %d', [msg.LParam]));
    MSG_CREATED_CONNECTION:
      Log(Format('Created connection %d', [msg.LParam]));
    MSG_DESTROY_CONNECTION:
      Log(Format('Destroying connection %d', [msg.LParam]));
    MSG_DESTROYED_CONNECTION:
      Log(Format('Destroyed connection in thread %d', [msg.LParam]));
    else
      Log(Format('Invalid message %d', [msg.WParam]));
  end;
end;

procedure TfrmConnectionPoolDemo.WMTaskStarted(var msg: TMessage);
begin
  Log(Format('Task %d started using connection %d', [msg.WParam, msg.LParam]));
end;

{ TConnectionPoolData }

constructor TConnectionPoolData.Create;
begin
  PostToForm(WM_USER, MSG_CREATING_CONNECTION, integer(GetCurrentThreadID));
  cpID := GConnPoolID.Increment;
  Sleep(5000);
  PostToForm(WM_USER, MSG_CREATED_CONNECTION, cpID);
end;

destructor TConnectionPoolData.Destroy;
begin
  PostToForm(WM_USER, MSG_DESTROY_CONNECTION, cpID);
  PostToForm(WM_USER, MSG_DESTROYED_CONNECTION, integer(GetCurrentThreadID));
end;

function TConnectionPoolData.ConnectionID: integer;
begin
  Result := cpID;
end;

initialization
  GConnPoolID := CreateCounter;
end.
