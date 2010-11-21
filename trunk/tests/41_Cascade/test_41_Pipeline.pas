unit test_41_Pipeline;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlCommon,
  OtlCollections,
  OtlParallel;

type
  TfrmPipelineDemo = class(TForm)
    lbLog: TListBox;
    btnPipeline: TButton;
    btnPipelineNoWait: TButton;
    procedure btnForEachClick(Sender: TObject);
    procedure btnPipelineClick(Sender: TObject);
    procedure btnPipelineNoWaitClick(Sender: TObject);
  private
    procedure StageMod5(const input, output: IOmniBlockingCollection);
  public
  end;

var
  frmPipelineDemo: TfrmPipelineDemo;

implementation

{$R *.dfm}

procedure TfrmPipelineDemo.btnForEachClick(Sender: TObject);
var
  resValue : TOmniValue;
  stage1out: IOmniBlockingCollection;
  stage2out: IOmniBlockingCollection;
  stage3out: IOmniBlockingCollection;
  startTime: cardinal;
  sum      : integer;
begin
  stage1out := TOmniBlockingCollection.Create;
  stage2out := TOmniBlockingCollection.Create;
  stage3out := TOmniBlockingCollection.Create;

  startTime := GetTickCount;

  // (1 .. 1000000) > stage 1 -> stage1out
  Parallel.ForEach(1, 1000000).NoWait.NumTasks(1).Into(stage1out).Execute(
    procedure (const value: integer; var result: TOmniValue)
    begin
      result := value * 2;
    end
  );

  // stage1out -> stage 2 -> stage2out
  Parallel.ForEach(stage1out).NoWait.NumTasks(1).Into(stage2out).Execute(
    procedure (const value: TOmniValue; var result: TOmniValue)
    begin
      result := value.AsInteger - 3;
    end
  );

  // stage2out -> stage 3 -> stage3out
  Parallel.ForEach(stage2out).NoWait.NumTasks(1).Into(stage3out).Execute(
    procedure (const value: TOmniValue; var result: TOmniValue)
    begin
      result := value.AsInteger mod 5;
    end
  );

  sum := 0;
  for resValue in stage3out do
    Inc(sum, resValue);

  lbLog.Items.Add(Format('Sum = %d; Total time = %d ms', [sum, GetTickCount - startTime]));
end;

procedure StageMult2(const input, output: IOmniBlockingCollection);
var
  value: TOmniValue;
begin
  // This one is a global method - just for demo purposes.
  for value in input do
    output.Add(2 * value.AsInteger);
end;

procedure StageMinus3(const input, output: IOmniBlockingCollection);
var
  value: TOmniValue;
begin
  // This one is a global method - just for demo purposes.
  for value in input do
    output.Add(value.AsInteger - 3);
end;

procedure TfrmPipelineDemo.StageMod5(const input, output: IOmniBlockingCollection);
var
  value: TOmniValue;
begin
  // This one is a method - just for demo purposes.
  for value in input do
    output.Add(value.AsInteger mod 5);
end;

procedure TfrmPipelineDemo.btnPipelineClick(Sender: TObject);
var
  i             : integer;
  pipelineResult: integer;
  testResult    : integer;
begin
  Parallel
    .Pipeline
    //.Input not set - first stage will have no input
    .Stage(
      procedure (const input, output: IOmniBlockingCollection)
      var
        i: integer;
      begin
        for i := 1 to 1000000 do
          output.Add(i);
        // .CompleteAdding is called automatically
      end
    )
    .Stage(StageMult2)
    .Stages([StageMinus3, StageMod5])
      .NumTasks(2) // each stage from previous line will execute in two tasks
    //.Output not set - last stage will have no output
    .Stage(
      procedure (const input, output: IOmniBlockingCollection)
      var
        locSum: integer;
        value : TOmniValue;
      begin
        // return result via captured variable - the simplest way
        locSum := 0;
        for value in input do begin
          Inc(locSum, value);
        end;
        pipelineResult := locSum;
      end
    )
    .Run; // .NoWait is not used so .Run will block and wait for all stages to complete
  testResult := 0;
  for i := 1 to 1000000 do
    Inc(testResult, (2*i - 3) mod 5);
  lbLog.Items.Add(Format('Pipeline result: %d; expected result: %d', [pipelineResult, testResult]));
end;

procedure TfrmPipelineDemo.btnPipelineNoWaitClick(Sender: TObject);
var
  i             : integer;
  output        : IOmniBlockingCollection;
  pipelineResult: integer;
  testResult    : integer;
  value         : TOmniValue;
begin
  output := TOmniBlockingCollection.Create;
  Parallel
    .Pipeline
    //.Input not set - first stage will have no input
    .Stage(
      procedure (const input, output: IOmniBlockingCollection)
      var
        i: integer;
      begin
        for i := 1 to 1000000 do
          output.Add(i);
        // .CompleteAdding is called automatically
      end
    )
    .Stage(StageMult2)
    .Stages([StageMinus3, StageMod5])
      .NumTasks(2) // each stage from previous line will execute in two tasks
    .Output(output)
    .NoWait
    .Run;
  // Because of .NoWait, Parallel.Pipeline will not block and execution will continue immediately.
  pipelineResult := 0;
  for value in output do
    Inc(pipelineResult, value);
  testResult := 0;
  for i := 1 to 1000000 do
    Inc(testResult, (2*i - 3) mod 5);
  lbLog.Items.Add(Format('Pipeline result: %d; expected result: %d', [pipelineResult, testResult]));
end;

end.
