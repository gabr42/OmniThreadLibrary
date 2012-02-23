unit test_48_OtlParallelExceptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmOtlParallelExceptions = class(TForm)
    btnAsync: TButton;
    btnForeach: TButton;
    btnForkJoin: TButton;
    btnFuture1: TButton;
    btnFuture2: TButton;
    btnFuture3: TButton;
    btnJoin1: TButton;
    btnJoin2: TButton;
    btnPipeline1: TButton;
    lbLog: TListBox;
    btnJoin3: TButton;
    btnPipeline2: TButton;
    btnPipeline3: TButton;
    btnPipeline4: TButton;
    btnAsync2: TButton;
    btnAsync3: TButton;
    procedure btnAsync2Click(Sender: TObject);
    procedure btnAsync1Click(Sender: TObject);
    procedure btnAsync3Click(Sender: TObject);
    procedure btnFuture1Click(Sender: TObject);
    procedure btnFuture2Click(Sender: TObject);
    procedure btnFuture3Click(Sender: TObject);
    procedure btnJoin1Click(Sender: TObject);
    procedure btnJoin2Click(Sender: TObject);
    procedure btnJoin3Click(Sender: TObject);
    procedure btnPipeline1Click(Sender: TObject);
    procedure btnPipeline2Click(Sender: TObject);
    procedure btnPipeline3Click(Sender: TObject);
    procedure btnPipeline4Click(Sender: TObject);
  private
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const param: array of const); overload;
  end;

var
  frmOtlParallelExceptions: TfrmOtlParallelExceptions;

implementation

uses
  OtlTask,
  OtlTaskControl,
  OtlCommon,
  OtlCollections,
  OtlParallel;

{$R *.dfm}

type
  ETestException = class(Exception);

procedure TfrmOtlParallelExceptions.btnFuture1Click(Sender: TObject);
var
  future: IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...');
  Sleep(1000); // another long task
  try
    Log('Future retured: %d', [future.Value]);
  except
    on E: Exception do
      Log('Future raised exception %s:%s', [E.ClassName, E.Message]);
  end;
end;

procedure TfrmOtlParallelExceptions.btnFuture2Click(Sender: TObject);
var
  future: IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...');
  future.WaitFor(INFINITE);
  if assigned(future.FatalException) then
    Log('Future raised exception %s:%s', [future.FatalException.ClassName, future.FatalException.Message])
  else
    Log('Future retured: %d', [future.Value]);
end;

procedure TfrmOtlParallelExceptions.btnFuture3Click(Sender: TObject);
var
  excFuture: Exception;
  future   : IOmniFuture<integer>;
begin
  future := Parallel.Future<integer>(
    function: integer
    begin
      raise ETestException.Create('Exception in Parallel.Future');
    end
  );
  Log('Future is executing ...');
  future.WaitFor(INFINITE);
  excFuture := future.DetachException;
  try
    if assigned(excFuture) then
      Log('Future raised exception %s:%s', [excFuture.ClassName, excFuture.Message])
    else
      Log('Future retured: %d', [future.Value]);
  finally FreeAndNil(excFuture); end;
end;

procedure TfrmOtlParallelExceptions.btnJoin1Click(Sender: TObject);
var
  iInnerExc: integer;
begin
  try
    Parallel.Join([
      procedure begin
        raise ETestException.Create('Exception 1 in Parallel.Join');
      end,
      procedure begin
      end,
      procedure begin
        raise ETestException.Create('Exception 2 in Parallel.Join');
      end]).Execute;
  except
    on E: EJoinException do begin
      Log('Join raised exception %s:%s', [E.ClassName, E.Message]);
      for iInnerExc := 0 to E.Count - 1 do
        Log('  Task #%d raised exception: %s:%s', [E[iInnerExc].TaskNumber,
          E[iInnerExc].FatalException.ClassName,
          E[iInnerExc].FatalException.Message]);
    end;
  end;
end;

procedure TfrmOtlParallelExceptions.btnJoin2Click(Sender: TObject);
begin
  try
    Parallel.Join(
      procedure (const joinState: IOmniJoinState)
      begin
        Sleep(500);
        raise Exception.Create('Exception in first task');
      end,
      procedure (const joinState: IOmniJoinState)
      var
        i: integer;
      begin
        for i := 1 to 10 do begin
          Sleep(100);
          if joinState.IsExceptional then
            raise Exception.CreateFmt('Exception in second task, i = %d', [i]);
        end;
      end).Execute;
  except
    on E: Exception do
      Log('Join raised exception %s:%s', [E.ClassName, E.Message]);
  end;
end;

procedure TfrmOtlParallelExceptions.btnJoin3Click(Sender: TObject);
var
  j      : integer;
  join   : IOmniParallelJoin;
  joinExc: Exception;
begin
  join := Parallel.Join(
    procedure (const joinState: IOmniJoinState)
    begin
      Sleep(500);
      raise Exception.Create('Exception in first task');
    end,
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 10 do begin
        Sleep(100);
        if joinState.IsExceptional then
          raise Exception.CreateFmt('Exception in second task, i = %d', [i]);
      end;
    end).NoWait.Execute;
  for j := 1 to 10 do begin
    Sleep(100);
    if join.IsExceptional then begin
      Log(Format('Join raised exception, j = %d', [j]));
      break; //for j
    end;
  end;
  joinExc := join.DetachException; // includes implicit WaitFor
  if assigned(joinExc) then begin
    Log(Format('Join raised exception %s:%s', [joinExc.ClassName, joinExc.Message]));
    FreeAndNil(joinExc);
  end;
end;

procedure StageException1(const input: TOmniValue; var output: TOmniValue);
begin
  output := input.AsInteger * 42;
end;

procedure StageException2(const input, output: IOmniBlockingCollection);
var
  outVal: TOmniValue;
  value : TOmniValue;
begin
  for value in input do begin
    if value.IsException then begin
      value.AsException.Free;
      outVal.Clear;
    end
    else
      outVal := 1 / value.AsInteger;
    if not output.TryAdd(outVal) then
      break; //for
  end;
end;

procedure TfrmOtlParallelExceptions.btnAsync1Click(Sender: TObject);
begin
  Parallel.Async(
    procedure
    begin
      Sleep(1000);
      raise Exception.Create('Exception in Async');
    end);
end;

procedure TfrmOtlParallelExceptions.btnAsync2Click(Sender: TObject);
begin
  Parallel.Async(
    procedure
    begin
      Sleep(1000);
      raise Exception.Create('Exception in Async');
    end,
    Parallel.TaskConfig.OnTerminated(
      procedure (const task: IOmniTaskControl)
      var
        excp: Exception;
      begin
        if assigned(task.FatalException) then begin
          excp := task.DetachException;
          Log('Caught async exception %s:%s', [excp.ClassName, excp.Message]);
          FreeAndNil(excp);
        end;
      end
    ));
end;

procedure TfrmOtlParallelExceptions.btnAsync3Click(Sender: TObject);
begin
  // TODO -cMM: TfrmOtlParallelExceptions.btnAsync3Click default body inserted
  Parallel.Async(
    procedure
    begin
      Sleep(1000);
      raise Exception.Create('Exception in Async');
    end,
    Parallel.TaskConfig.OnTerminated(
      procedure
      begin
        Log('Async termination handler');
      end
    ));
end;

procedure TfrmOtlParallelExceptions.btnPipeline1Click(Sender: TObject);
var
  pipeline: IOmniPipeline;
  value   : TOmniValue;
begin
  Log('Should catch pipeline exception "TOmniValue cannot be converted to int64" in stage 1');
  pipeline := Parallel.Pipeline
    .Stage(StageException1)
    .Stage(StageException2);
  pipeline.Run;

  // Provide input
  with pipeline.Input do begin
    // few normal elements
    Add(1);
    Add(2);
    // then trigger the exception in the first stage
    Add('three');
    // this should never reach the pipeline output
    Add(4);
    CompleteAdding;
  end;

  // Process output
  try
    for value in pipeline.Output do
      Log(value.AsString);
  except
    on E: Exception do
      Log('Caught pipeline exception %s:%s', [E.ClassName, E.Message]);
  end;
end;

procedure TfrmOtlParallelExceptions.btnPipeline2Click(Sender: TObject);
var
  pipeline: IOmniPipeline;
  value   : TOmniValue;
begin
  Log('Should catch pipeline exception "Floating point division by zero" in stage 2');
  pipeline := Parallel.Pipeline
    .Stage(StageException1)
    .Stage(StageException2);
  pipeline.Run;

  // Provide input
  with pipeline.Input do begin
    // few normal elements
    Add(1);
    Add(2);
    // then trigger the exception in the second stage
    Add(0);
    // this should never reach the pipeline output
    Add(4);
    CompleteAdding;
  end;

  // Process output
  try
    for value in pipeline.Output do
      Log(value.AsString);
  except
    on E: Exception do
      Log('Caught pipeline exception %s:%s', [E.ClassName, E.Message]);
  end;
end;

procedure TfrmOtlParallelExceptions.btnPipeline3Click(Sender: TObject);
var
  pipeline: IOmniPipeline;
  value   : TOmniValue;
begin
  Log('Stage 2 should accept and correct stage 1 exception (third output will be empty)');
  pipeline := Parallel.Pipeline
    .Stage(StageException1)
    .Stage(StageException2)
      .HandleExceptions
    .Run;

  // Provide input
  with pipeline.Input do begin
    // few normal elements
    Add(1);
    Add(2);
    // then trigger the exception in the first stage; this exception should be 'corrected' in the second stage
    Add('three');
    Add(4);
    CompleteAdding;
  end;

  // Process output; there should be no exception in the output collection
  for value in pipeline.Output do
    Log(value.AsString);
end;

procedure TfrmOtlParallelExceptions.btnPipeline4Click(Sender: TObject);
var
  pipeline: IOmniPipeline;
  value   : TOmniValue;
begin
 Log('Should catch pipeline exception "TOmniValue cannot be converted to int64" in stage 1, then log it without reraising');
  pipeline := Parallel.Pipeline
    .Stage(StageException1)
    .Stage(StageException2);
  pipeline.Run;

  // Provide input
  with pipeline.Input do begin
    // few normal elements
    Add(1);
    Add(2);
    // then trigger the exception in the first stage
    Add('three');
    // this should never reach the pipeline output
    Add(4);
    CompleteAdding;
  end;

  // Process output; exceptions will be processed manually
  pipeline.Output.ReraiseExceptions(false);
  for value in pipeline.Output do
    if not value.IsException then
      Log(value.AsString)
    else begin
      Log('%s:%s', [value.AsException.ClassName, value.AsException.Message]);
      value.AsException.Free;
    end;
end;

procedure TfrmOtlParallelExceptions.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

procedure TfrmOtlParallelExceptions.Log(const msg: string; const param: array of const);
begin
  Log(Format(msg, param));
end;

end.

