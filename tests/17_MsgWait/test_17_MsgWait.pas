unit test_17_MsgWait;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmTestMsgWait = class(TForm)
    ActionList: TActionList;
    actStart  : TAction;
    actStop   : TAction;
    btnStart  : TButton;
    btnStop   : TButton;
    lbLog     : TListBox;
    oeMonitor : TOmniEventMonitor;
    procedure actStartExecute(Sender: TObject);
    procedure actStartUpdate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStopUpdate(Sender: TObject);
    procedure oeMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  strict private
    FHelloWorld: IOmniTaskControl;
  end;

var
  frmTestMsgWait: TfrmTestMsgWait;

implementation

uses
  DSiWin32,
  OtlCommon;

{$R *.dfm}

type
  THelloWorld = class(TOmniWorker)
  private
    FTimer: TDSiTimer;
  protected
    procedure Cleanup; override;
    procedure DoTimer(sender: TObject);
    function  Initialize: boolean; override;
  end;

procedure TfrmTestMsgWait.actStartExecute(Sender: TObject);
begin
  FHelloWorld := CreateTask(THelloWorld.Create(), 'Hello, World!')
                 .MonitorWith(oeMonitor)
                 .MsgWait
                 .Run;
end;

procedure TfrmTestMsgWait.actStartUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not assigned(FHelloWorld));
end;

procedure TfrmTestMsgWait.actStopExecute(Sender: TObject);
begin
  FHelloWorld.Terminate;
  FHelloWorld := nil;
end;

procedure TfrmTestMsgWait.actStopUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(FHelloWorld);
end;

procedure TfrmTestMsgWait.oeMonitorTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg.MsgData);
end;

{ THelloWorld }

procedure THelloWorld.Cleanup;
begin
  FreeAndNil(FTimer);
end;

procedure THelloWorld.DoTimer(sender: TObject);
begin
  Task.Comm.Send(0, 'Hello, world!');
end;

function THelloWorld.Initialize: boolean;
begin
  FTimer := TDSiTimer.Create(true, 1000, DoTimer);
  Result := true;
end;

end.
