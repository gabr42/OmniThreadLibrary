unit test_46_Async;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm;

const
  WM_LOG = WM_USER;

type
  TfrmDemoParallelAsync = class(TForm)
    btnAsync: TButton;
    lbLog: TListBox;
    procedure btnAsyncClick(Sender: TObject);
  private
    procedure WMLog(var msg: TOmniMessage); message WM_LOG;
  public
  end;

var
  frmDemoParallelAsync: TfrmDemoParallelAsync;

implementation

uses
  OtlTask,
  OtlParallel;

{$R *.dfm}

procedure TfrmDemoParallelAsync.btnAsyncClick(Sender: TObject);
var
  i: integer;
begin
  btnAsync.Enabled := false;

  Parallel.Async(
    procedure (const task: IOmniTask)
    var
      i: integer;
    begin
      task.Comm.Send(WM_LOG, 'Starting');
      for i := 1 to 10 do begin
        task.Comm.Send(WM_LOG, i);
        Sleep(200);
      end;
      task.Comm.Send(WM_LOG, 'Completed');
    end,

    procedure
    begin
      btnAsync.Enabled := true;
    end,

    Parallel.TaskConfig.OnMessage(Self)
  );

  for i := 1 to 10 do begin
    lbLog.ItemIndex := lbLog.Items.Add('MAIN: ' + IntToStr(i));
    Sleep(400);
    Application.ProcessMessages;
  end;
end;

procedure TfrmDemoParallelAsync.WMLog(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add('BGTASK: ' + msg.MsgData);
end;

end.
