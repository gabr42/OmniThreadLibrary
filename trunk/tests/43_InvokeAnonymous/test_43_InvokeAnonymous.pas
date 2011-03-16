unit test_43_InvokeAnonymous;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlTaskControl;

type
  TfrmInvokeAnonymousDemo = class(TForm)
    lbLog: TListBox;
    btnInvoke: TButton;
    procedure btnInvokeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTask: IOmniTaskControl;
  public
  end;

var
  frmInvokeAnonymousDemo: TfrmInvokeAnonymousDemo;

implementation

{$R *.dfm}

type
  TTask = class(TOmniWorker)
  end; { TTask }

procedure TfrmInvokeAnonymousDemo.btnInvokeClick(Sender: TObject);
var
  formThreadID: DWORD;
begin
  formThreadID := GetCurrentThreadID;
  FTask.Invoke(
    procedure (const task: IOmniTask)
    var
      taskThreadID: DWORD;
    begin
      taskThreadID := GetCurrentThreadID;
//      task.Invoke(
//        procedure
//          frmInvokeAnonymousDemo.lbLog.Items.Add(Format(
//            'Current thread ID: %d, task thread ID: %d, form thread ID: %d',
//            [GetCurrentThreadID, taskThreadID, formThreadID]));
//        begin
//      end
//      );
    end
  );
end;

procedure TfrmInvokeAnonymousDemo.FormDestroy(Sender: TObject);
begin
  FTask.Terminate;
  FTask := nil;
end;

procedure TfrmInvokeAnonymousDemo.FormCreate(Sender: TObject);
begin
  FTask := CreateTask(TTask.Create())
    .OnMessage(Self)
    .Run;
end; { TfrmInvokeAnonymousDemo.FormCreate }

end.
