unit test_1_HelloWorld_FMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.Controls.Presentation, FMX.StdCtrls,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TForm77 = class(TForm)
    btnHello: TButton;
    lbLog: TListBox;
    procedure btnHelloClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMessageDispatch: TOmniEventMonitor;
  public
    procedure HandleTaskTerminated(const task: IOmniTaskControl);
    procedure HandleTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure RunHelloWorld(const task: IOmniTask);
  end;

var
  Form77: TForm77;

implementation

{$R *.fmx}

procedure TForm77.btnHelloClick(Sender: TObject);
begin
  btnHello.Enabled := false;
  FMessageDispatch.Monitor(CreateTask(RunHelloWorld, 'HelloWorld')).Run;
end;

procedure TForm77.FormCreate(Sender: TObject);
begin
  FMessageDispatch := TOmniEventMonitor.Create(Self);
  FMessageDispatch.OnTaskMessage := HandleTaskMessage;
  FMessageDispatch.OnTaskTerminated := HandleTaskTerminated;
end;

procedure TForm77.HandleTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] %d|%s',
    [task.UniqueID, task.Name, msg.msgID, msg.msgData.AsString]));
end;

procedure TForm77.HandleTaskTerminated(const task: IOmniTaskControl);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format('[%d/%s] Terminated', [task.UniqueID, task.Name]));
  btnHello.Enabled := true;
end;

procedure TForm77.RunHelloWorld(const task: IOmniTask);
begin
  //Executed in a background thread
  task.Comm.Send(1024, 'Hello, world!');
end;

end.
