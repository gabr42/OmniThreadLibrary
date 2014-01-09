unit test_31_WaitableObjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm,
  OtlTask,
  OtlTaskControl;

type
  TfrmTestWaitableObjects = class(TForm)
    btnSignal1: TButton;
    btnSignal2: TButton;
    lbLog     : TListBox;
    btnUnregister1: TButton;
    btnRegister1: TButton;
    procedure btnRegister1Click(Sender: TObject);
    procedure btnSignal1Click(Sender: TObject);
    procedure btnSignal2Click(Sender: TObject);
    procedure btnUnregister1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  strict private
    FEvent: THandle;
  private
    FSignalDemo: IOmniTaskControl;
    procedure ReportMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
  public
  end;

var
  frmTestWaitableObjects: TfrmTestWaitableObjects;

implementation

uses
  OtlCommon;

const
  MSG_SIGNAL_2        = 1;
  MSG_CHANGE_SIGNAL_1 = 2;

type
  TSignalDemo = class(TOmniWorker)
  strict private
    FSignal1: THandle;
    FSignal2: THandle;
  strict protected
    procedure HandleSignal1;
    procedure HandleSignal2;
  protected
    procedure Cleanup; override;
    function  Initialize: boolean; override;
  public
    procedure OMChangeSignal1(var msg: TOmniMessage); message MSG_CHANGE_SIGNAL_1;
    procedure OMSignal2(var msg: TOmniMessage); message MSG_SIGNAL_2;
  end; { TSignalDemo }

{$R *.dfm}

{ TfrmTestWaitableObjects }

procedure TfrmTestWaitableObjects.btnRegister1Click(Sender: TObject);
begin
  FSignalDemo.Comm.Send(MSG_CHANGE_SIGNAL_1, 1);
end; { TfrmTestWaitableObjects.btnRegister1Click }

procedure TfrmTestWaitableObjects.btnSignal1Click(Sender: TObject);
begin
  SetEvent(FEvent);
end; { TfrmTestWaitableObjects.btnSignal1Click }

procedure TfrmTestWaitableObjects.btnSignal2Click(Sender: TObject);
begin
  FSignalDemo.Comm.Send(MSG_SIGNAL_2);
end; { TfrmTestWaitableObjects.btnSignal2Click }

procedure TfrmTestWaitableObjects.btnUnregister1Click(Sender: TObject);
begin
  FSignalDemo.Comm.Send(MSG_CHANGE_SIGNAL_1, 0);
end; { TfrmTestWaitableObjects.btnUnregister1Click }

procedure TfrmTestWaitableObjects.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FSignalDemo.Terminate;
  CloseHandle(FEvent);
end; { TfrmTestWaitableObjects.FormCloseQuery }

procedure TfrmTestWaitableObjects.FormCreate(Sender: TObject);
begin
  FEvent := CreateEvent(nil, false, false, nil);
  FSignalDemo := CreateTask(TSignalDemo.Create(), 'Signal demo thread')
    .OnMessage(ReportMessage)
    .SetParameter(FEvent)
    .Run;
end; { TfrmTestWaitableObjects.FormCreate }

procedure TfrmTestWaitableObjects.ReportMessage(const task: IOmniTaskControl; const msg:
  TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg.MsgData);
end; { TfrmTestWaitableObjects.ReportMessage }

{ TSignalDemo }

procedure TSignalDemo.Cleanup;
begin
  CloseHandle(FSignal2);
  inherited;
end; { TSignalDemo.Cleanup }

procedure TSignalDemo.OMChangeSignal1(var msg: TOmniMessage);
begin
  if msg.MsgData = 1 then
    Task.RegisterWaitObject(FSignal1, HandleSignal1)
  else
    Task.UnregisterWaitObject(FSignal1);
end; { TSignalDemo.OMChangeSignal1 }

procedure TSignalDemo.OMSignal2(var msg: TOmniMessage);
begin
  SetEvent(FSignal2);
end; { TSignalDemo.HandleMessageSignal2 }

procedure TSignalDemo.HandleSignal1;
begin
  Task.Comm.Send(0, 'Received signal 1');
end; { TSignalDemo.HandleSignal1 }

procedure TSignalDemo.HandleSignal2;
begin
  Task.Comm.Send(0, 'Received signal 2');
end; { TSignalDemo.HandleSignal2 }

function TSignalDemo.Initialize: boolean;
begin
  Result := inherited Initialize;
  if not Result then
    Exit;
  FSignal1 := Task.Param[0];
  FSignal2 := CreateEvent(nil, false, false, nil);
  Task.RegisterWaitObject(FSignal1, HandleSignal1);
  Task.RegisterWaitObject(FSignal2, HandleSignal2);
end; { TSignalDemo.Initialize }

end.
