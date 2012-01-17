(* Problem: http://otl.17slon.com/forum/index.php/topic,289.0.html

Here's my (simplified) task I would like to solve using OTL (Delphi XE) - my real-world task is somehow more complicated but can be described as:

input: a string.
output: a TStringList containing characters (one per entry) of the input string.

Example: input: "delphi"

A background thread ("master") grabs the input string and splits it into several pieces (let's say 2): "del" and "phi".
For each of the split strings a new thread ("child") is created that fills in the TStringList (output) with characters from the section of the string it receives.

At any time the "master" thread could be signaled to terminate (from the app's main thread) all child threads (and itself).

When everything is done the app's main thread processes the string list.

Preferably, the order of the characters should (when all ends) be 'd', 'e', 'l', 'p', 'h', 'i' (note that characters are actually items in the resulting string list).

Low-level solution:
- Create task (StringProcessorLL) which waits on processing requests, breaks the string
  (BreakStringLL) and processes parts in N subtasks (BreakStringLLTask). Result is returned
  asynchronouse through the messaging subsystem.
- All tasks are using same cancellation token (the one from the StringProcessorLL worker)
  so that they can be cancelled with one call.
- BreakStringLLTask tasks are scheduled from the thread pool so that they may be reused if
  multiple requests are processed in succession.
- This is an example of 'static partitioning' approach where each worker task works on
  pre-allocated part of the problem.

High-level solution:
- Uses Background Worker to implement the background worker thread and ParallelTask
  (in conjunction with a static partitioning as above) to process data.
- Uses Background Worker's cancellation mechanism.
*)

unit StringListParser1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  DSiWin32,
  OtlCommon,
  OtlComm,
  OtlSync,
  OtlTask,
  OtlTaskControl,
  OtlCollections,
  OtlParallel;

const
  WM_PROCESSING_RESULT   = WM_USER;
  WM_PROCESSING_CANCELED = WM_USER + 1;

type
  TfrmStringListParser = class(TForm)
    btnCancelHL: TButton;
    btnCancelLL: TButton;
    btnProcess: TButton;
    btnProcessHL: TButton;
    inpString: TEdit;
    lblInstructions: TLabel;
    lbLog: TListBox;
    procedure btnCancelHLClick(Sender: TObject);
    procedure btnCancelLLClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure btnProcessHLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBackgroundWorker: IOmniBackgroundWorker;
    FStringProcessor: IOmniTaskControl;
    procedure BreakStringHL(const workItem: IOmniWorkItem);
    procedure ShowResult(sl: TStringList);
    procedure ShowResultHL(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
    procedure WMProcessingCanceled(var msg: TOmniMessage); message WM_PROCESSING_CANCELED;
    procedure WMProcessingResult(var msg: TOmniMessage); message WM_PROCESSING_RESULT;
  end;

var
  frmStringListParser: TfrmStringListParser;

implementation

{$R *.dfm}

{ common worker }

procedure SplitPartialList(const input: string; output: TStringList;
  const cancel: IOmniCancellationToken);
var
  ch: char;
begin
  for ch in input do begin
    if ch = '!' then // for testing
      cancel.Signal;

    if cancel.IsSignalled then
      break; //for ch

    output.Add(ch);
    // for testing: disable the line above and enable the line below to show in output which thread processed which character
    // output.Add(ch + IntToStr(GetCurrentThreadID));

    Sleep(100); // simulate workload
  end;
end;

{ low-level approach }

procedure BreakStringLLTask(const task: IOmniTask);
var
  job   : string;
  param : TOmniValue;
  result: TStringList;
begin
  param := task.Param['Output']; result := TStringList(param.AsObject);
  param := task.Param['Job'];    job := param.AsString;
  SplitPartialList(job, result, task.CancellationToken);
end;

procedure BreakStringLL(s: string; const slOutput: TStringList; const cancellationToken:
  IOmniCancellationToken);
var
  breakTasks  : IOmniTaskGroup;
  charsPerTask: integer;
  iTask       : integer;
  numTasks    : integer;
  sPartial    : string;
  taskResults : array of TStringList;
begin
  numTasks := Environment.Process.Affinity.Count - 1;

  SetLength(taskResults, numTasks);
  for iTask := Low(taskResults) to High(taskResults) do
    taskResults[iTask] := TStringList.Create;

  breakTasks := CreateTaskGroup;
  for iTask := 1 to numTasks do begin
    // divide the remaining part in as-equal-as-possible segments
    charsPerTask := Round(Length(s) / (numTasks - iTask + 1));
    CreateTask(BreakStringLLTask)
      .SetParameter('Output', taskResults[iTask-1])
      .SetParameter('Job',    Copy(s, 1, charsPerTask))
      .CancelWith(cancellationToken)
      .Join(breakTasks)
      .Schedule;
    Delete(s, 1, charsPerTask);
  end;
  breakTasks.WaitForAll(INFINITE);

  for iTask := Low(taskResults) to High(taskResults) do begin
    for sPartial in taskResults[iTask] do
      slOutput.Add(sPartial);
    taskResults[iTask].Free;
  end;
end;

procedure StringProcessorLL(const task: IOmniTask);
var
  input   : TOmniMessage;
  slOutput: TStringList;
begin
  while DSiWaitForTwoObjects(task.TerminateEvent, task.Comm.NewMessageEvent, false, INFINITE) = WAIT_OBJECT_1 do begin
    task.Comm.Receive(input);
    slOutput := TStringList.Create;
    BreakStringLL(input.MsgData.AsString, slOutput, task.CancellationToken);
    if task.CancellationToken.IsSignalled then begin
      task.Comm.Send(WM_PROCESSING_CANCELED);
      FreeAndNil(slOutput);
    end
    else
      task.Comm.Send(WM_PROCESSING_RESULT, slOutput);
  end;
end;

{ TfrmStringListParser }

procedure TfrmStringListParser.BreakStringHL(const workItem: IOmniWorkItem);
var
  charsPerTask : integer;
  input        : string;
  iTask        : integer;
  numTasks     : integer;
  output       : TStringList;
  partialQueue : IOmniBlockingCollection;
  s            : string;
  stringBreaker: IOmniParallelTask;
  taskResults  : array of TStringList;
begin
  partialQueue := TOmniBlockingCollection.Create;
  numTasks := Environment.Process.Affinity.Count - 1;

  SetLength(taskResults, numTasks);
  for iTask := Low(taskResults) to High(taskResults) do
    taskResults[iTask] := TStringList.Create;

  stringBreaker := Parallel.ParallelTask.NumTasks(numTasks).NoWait
    .TaskConfig(Parallel.TaskConfig.CancelWith(workItem.CancellationToken))
    .Execute(
      procedure (const task: IOmniTask)
      var
        workItem: TOmniValue;
      begin
        workItem := partialQueue.Next;
        SplitPartialList(workItem[1].AsString, taskResults[workItem[0].AsInteger], task.CancellationToken);
      end
    );

  // provide input to the ForEach loop above
  input := workItem.Data;
  for iTask := 1 to numTasks do begin
    // divide the remaining part in as-equal-as-possible segments
    charsPerTask := Round(Length(input) / (numTasks - iTask + 1));
    partialQueue.Add(TOmniValue.Create([iTask-1, Copy(input, 1, charsPerTask)]));
    Delete(input, 1, charsPerTask);
  end;

  // process output
  stringBreaker.WaitFor(INFINITE);
  if not workItem.CancellationToken.IsSignalled then begin
    output := TStringList.Create;
    for iTask := Low(taskResults) to High(taskResults) do begin
      for s in taskResults[iTask] do
        output.Add(s);
    end;
    workItem.Result := output;
  end;
  for iTask := Low(taskResults) to High(taskResults) do
    taskResults[iTask].Free;
end; { TfrmStringListParser.BreakStringHL }

procedure TfrmStringListParser.btnCancelHLClick(Sender: TObject);
begin
  FBackgroundWorker.CancelAll;
end;

procedure TfrmStringListParser.btnCancelLLClick(Sender: TObject);
begin
  FStringProcessor.CancellationToken.Signal;
end;

procedure TfrmStringListParser.btnProcessClick(Sender: TObject);
begin
  FStringProcessor.CancellationToken.Clear;
  FStringProcessor.Comm.Send(0, inpString.Text);
end;

procedure TfrmStringListParser.btnProcessHLClick(Sender: TObject);
begin
  FBackgroundWorker.Schedule(FBackgroundWorker.CreateWorkItem(inpString.Text));
end;

procedure TfrmStringListParser.FormCreate(Sender: TObject);
begin
  FStringProcessor := CreateTask(StringProcessorLL).OnMessage(Self).Run;
  FBackgroundWorker := Parallel.BackgroundWorker
    .Execute(BreakStringHL)
    .OnRequestDone(ShowResultHL);
end;

procedure TfrmStringListParser.FormDestroy(Sender: TObject);
begin
  FStringProcessor.CancellationToken.Signal;
  FStringProcessor.Terminate(INFINITE);
  FStringProcessor := nil;
  FBackgroundWorker.CancelAll;
  FBackgroundWorker.Terminate(INFINITE);
  FBackgroundWorker := nil;
end;

procedure TfrmStringListParser.ShowResult(sl: TStringList);
begin
  sl.Delimiter := ',';
  lbLog.Items.Add(sl.DelimitedText);
  FreeAndNil(sl);
end;

procedure TfrmStringListParser.ShowResultHL(const Sender: IOmniBackgroundWorker;
  const workItem: IOmniWorkItem);
begin
  if workItem.CancellationToken.IsSignalled then
    lbLog.Items.Add('Canceled')
  else
    ShowResult(workItem.Result.AsObject as TStringList);
end;

procedure TfrmStringListParser.WMProcessingCanceled(var msg: TOmniMessage);
begin
  lbLog.Items.Add('Canceled');
end;

procedure TfrmStringListParser.WMProcessingResult(var msg: TOmniMessage);
begin
  ShowResult(TStringList(msg.MsgData.AsObject));
end;

end.
