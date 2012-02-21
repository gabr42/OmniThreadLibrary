unit test_47_TaskConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlSync,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlCollections,
  OtlParallel;

const
  WM_LOG = WM_USER;
  WM_FUTURE_RESULT = WM_USER + 1;

type
  TfrmDemoParallelTaskConfig = class(TForm)
    btnAsync: TButton;
    lbLog: TListBox;
    btnJoin: TButton;
    btnFuture: TButton;
    btnPipeline: TButton;
    btnForEach: TButton;
    btnForkJoin: TButton;
    procedure btnAsyncClick(Sender: TObject);
    procedure btnForEachClick(Sender: TObject);
    procedure btnForkJoinClick(Sender: TObject);
    procedure btnFutureClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
    procedure btnPipelineClick(Sender: TObject);
  private
    FFuture: IOmniFuture<integer>;
    FParallel: IOmniParallelLoop<integer>;
    FSharedValue: integer;
    procedure PipelineStage1(const input, output: IOmniBlockingCollection; const task:
      IOmniTask);
    procedure PipelineStage2(const input, output: IOmniBlockingCollection; const task:
      IOmniTask);
    procedure WMLog(var msg: TOmniMessage); message WM_LOG;
    procedure WMFutureResult(var msg: TOmniMessage); message WM_FUTURE_RESULT;
  public
  end;

var
  frmDemoParallelTaskConfig: TfrmDemoParallelTaskConfig;

implementation

uses
  Math;

{$R *.dfm}

{$IFNDEF OTL_HasArrayOfT}
type
  TArray<T> = array of T;
{$ENDIF ~OTL_HasArrayOfT}

procedure TfrmDemoParallelTaskConfig.btnAsyncClick(Sender: TObject);
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

    Parallel.TaskConfig.OnMessage(Self).OnTerminated(
      procedure
      begin
        btnAsync.Enabled := true;
      end
    )
  );

  for i := 1 to 10 do begin
    lbLog.ItemIndex := lbLog.Items.Add('MAIN: ' + IntToStr(i));
    Sleep(400);
    Application.ProcessMessages;
  end;
end;

procedure TfrmDemoParallelTaskConfig.btnForEachClick(Sender: TObject);
begin
  FParallel := Parallel.ForEach(1, 17)
    .TaskConfig(Parallel.TaskConfig.OnMessage(Self))
    .NoWait
    .OnStop(procedure begin FParallel := nil; end);
  FParallel
    .Execute(
      procedure (const task: IOmniTask; const value: integer)
      begin
        task.Comm.Send(WM_LOG, value);
      end);
end;

function ParallelMax(data: TArray<integer>; forkJoin: IOmniForkJoin<integer>; left, right: integer): integer;
var
  computeLeft : IOmniCompute<integer>;
  computeRight: IOmniCompute<integer>;
  mid         : integer;
begin
  if (right - left) <= 2 then
    Result := Max(data[left], data[right])
  else begin
    mid := (left + right) div 2;
    computeLeft := forkJoin.Compute(
      function: integer
      begin
        Result := ParallelMax(data, forkJoin, left, mid);
      end);
    computeRight := forkJoin.Compute(
      function: integer
      begin
        Result := ParallelMax(data, forkJoin, mid + 1, right);
      end);
    Result := Max(computeLeft.Value, computeRight.Value);
  end;
end;

procedure TfrmDemoParallelTaskConfig.btnForkJoinClick(Sender: TObject);
var
  data: TArray<integer>;
  max : integer;
begin
  //D2009 doesn't have (array of T).Create initializers
  SetLength(data, 9);
  data[0] := 1; data[1] := 17; data[2] := 4; data[3] := 99; data[4] := -250;
  data[5] := 7; data[6] := 13; data[7] := 132; data[8] := 101;
  max := ParallelMax(
    data,
    Parallel.ForkJoin<integer>.TaskConfig(Parallel.TaskConfig.OnTerminated(
      procedure (const task: IOmniTaskControl)
      begin
        lbLog.ItemIndex := lbLog.Items.Add(Format('COMPUTE: Task %d terminated', [task.UniqueID]));
      end
    )),
    Low(data),
    High(data));
  lbLog.ItemIndex := lbLog.Items.Add('FORKJOIN: ' + IntToStr(max) + ' (expected 132)');
end;

procedure TfrmDemoParallelTaskConfig.btnFutureClick(Sender: TObject);
begin
  btnFuture.Enabled := false;
  FFuture := Parallel.Future<integer>(
    function (const task: IOmniTask): integer
    begin
      Sleep(500);
      Result := 42;
//      task.Comm.Send(WM_FUTURE_RESULT);
    end,
    Parallel.TaskConfig.OnMessage(Self).OnTerminated(
      procedure
      begin
        lbLog.ItemIndex := lbLog.Items.Add('FUTURE: ' + IntToStr(FFuture.Value));
        FFuture := nil;
      end
    )
  )
end;

procedure TfrmDemoParallelTaskConfig.btnJoinClick(Sender: TObject);
begin
  FSharedValue := 42;
  Parallel.Join(
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 1000000 do begin
        joinState.Task.Lock.Acquire;
        FSharedValue := FSharedValue + 17;
        joinState.Task.Lock.Release;
      end;
    end,
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 1000000 do begin
        joinState.Task.Lock.Acquire;
        FSharedValue := FSharedValue - 17;
        joinState.Task.Lock.Release;
      end;
    end
  ).TaskConfig(Parallel.TaskConfig.WithLock(CreateOmniCriticalSection))
   .Execute;
  lbLog.ItemIndex := lbLog.Items.Add(Format('JOIN: Shared value = %d (should be 42)', [FSharedValue]));
end;

procedure TfrmDemoParallelTaskConfig.btnPipelineClick(Sender: TObject);
var
  value: TOmniValue;
begin
  for value in
    Parallel.Pipeline
     .Stages([PipelineStage1, PipelineStage2], Parallel.TaskConfig.OnMessage(Self))
     .Run
     .Output
  do
    Application.ProcessMessages;
  lbLog.ItemIndex := lbLog.Items.Add('PIPELINE: ' + IntToStr(value) + ' (should be 500500)');
end;

procedure TfrmDemoParallelTaskConfig.PipelineStage1(const input, output:
    IOmniBlockingCollection; const task: IOmniTask);
var
  i: integer;
begin
  task.Comm.Send(WM_LOG, 'Pipeline stage 1 starting');
  for i := 1 to 1000 do
    output.Add(i);
  task.Comm.Send(WM_LOG, 'Pipeline stage 1 stopped');
end;

procedure TfrmDemoParallelTaskConfig.PipelineStage2(const input, output:
    IOmniBlockingCollection; const task: IOmniTask);
var
  sum  : integer;
  value: TOmniValue;
begin
  task.Comm.Send(WM_LOG, 'Pipeline stage 2 starting');
  sum := 0;
  while input.TryTake(value) do
    Inc(sum, value);
  output.Add(sum);
  task.Comm.Send(WM_LOG, 'Pipeline stage 2 stopped');
end;

procedure TfrmDemoParallelTaskConfig.WMFutureResult(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add('FUTURE: ' + IntToStr(FFuture.Value));
//  FFuture := nil;
  btnFuture.Enabled := true;
end;

procedure TfrmDemoParallelTaskConfig.WMLog(var msg: TOmniMessage);
begin
  lbLog.ItemIndex := lbLog.Items.Add('BGTASK: ' + msg.MsgData);
end;

end.
