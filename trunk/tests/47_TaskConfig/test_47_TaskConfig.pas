unit test_47_TaskConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm,
  OtlParallel;

const
  WM_LOG = WM_USER;
  WM_FUTURE_RESULT = WM_USER + 1;

type
  TfrmDemoParallelAsync = class(TForm)
    btnAsync: TButton;
    lbLog: TListBox;
    btnJoin: TButton;
    btnFuture: TButton;
    procedure btnAsyncClick(Sender: TObject);
    procedure btnFutureClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
  private
    FFuture: IOmniFuture<integer>;
    FSharedValue: integer;
    procedure WMLog(var msg: TOmniMessage); message WM_LOG;
    procedure WMFutureResult(var msg: TOmniMessage); message WM_FUTURE_RESULT;
  public
  end;

var
  frmDemoParallelAsync: TfrmDemoParallelAsync;

implementation

uses
  OtlTask,
  OtlSync;

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

procedure TfrmDemoParallelAsync.btnFutureClick(Sender: TObject);
begin
  btnFuture.Enabled := false;
  FFuture := Parallel.Future<integer>(
    function (const task: IOmniTask): integer
    begin
      Sleep(500);
      Result := 42;
      task.Comm.Send(WM_FUTURE_RESULT);
    end,
    Parallel.TaskConfig.OnMessage(Self)
  )
end;

procedure TfrmDemoParallelAsync.btnJoinClick(Sender: TObject);
begin
  FSharedValue := 42;
  Parallel.Join(
    procedure (const task: IOmniTask)
    var
      i: integer;
    begin
      for i := 1 to 1000000 do begin
        task.Lock.Acquire;
        FSharedValue := FSharedValue + 17;
        task.Lock.Release;
      end;
    end,
    procedure (const task: IOmniTask)
    var
      i: integer;
    begin
      for i := 1 to 1000000 do begin
        task.Lock.Acquire;
        FSharedValue := FSharedValue - 17;
        task.Lock.Release;
      end;
    end
    ,Parallel.TaskConfig.WithLock(CreateOmniCriticalSection)
  );
  lbLog.ItemIndex := lbLog.Items.Add(Format('JOIN: Shared value = %d (should be 42)', [FSharedValue]));
end;

procedure TfrmDemoParallelAsync.WMFutureResult(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add('FUTURE: ' + IntToStr(FFuture.Value));
  FFuture := nil;
  btnFuture.Enabled := true;
end;

procedure TfrmDemoParallelAsync.WMLog(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add('BGTASK: ' + msg.MsgData);
end;

end.
