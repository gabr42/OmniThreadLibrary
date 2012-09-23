unit test_51_PipelineStressTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OTLParallel, OtlCollections, OtlCommon, OTLContainers, StdCtrls,
  Contnrs, OTLTask, SyncObjs;

type
  TfrmPipelineStressTest = class(TForm)
    btStartStop: TButton;
    Memo1: TMemo;
    lblStatus: TLabel;
    procedure btStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    FStopAll: Boolean;
    FPipeline: IOmniPipeline;
    FPipelineExecuting: Boolean;

    procedure TestPipeline;
  end;

var
  frmPipelineStressTest: TfrmPipelineStressTest;

implementation

{$R *.dfm}

const
  cNumTests = 5;
  cMaxStages = 8;
  cMaxTasksOnStage = 4; // Test will run from Random(1) to Random(cMaxTasksOnStage) tasks on each stage
  cMaxSleepRnd = 100;   // Stage may sleep for Random(StageNDelay), where StageNDelay = Random(cMaxSleepRnd)
  cNumItems = 250;      // # of items, going though pipeline

procedure TfrmPipelineStressTest.FormCreate(Sender: TObject);
begin
  FStopAll := True;
end;

type
  TIntList = array of Integer;

procedure TfrmPipelineStressTest.TestPipeline;

  function BuildPipeline(const aNumStages, aMaxTasksOnStage, aNumItems, aStageToStop, aItemToStop: Integer;
    const aStageMaxDelays: TIntList): IOmniPipeline;

    function GetStage(const aStageDelay: Integer; const aAllowAbort: Boolean): TPipelineStageDelegateEx;
    begin
      Result := procedure (const input, output: IOmniBlockingCollection; const task: IOmniTask)
        var
          vValue: TOmniValue;
          s: String;
          i: Integer;
          vMS: TMemoryStream;
        begin
          while True do
          begin
            Assert(FPipelineExecuting, 'Pipeline already stopped!'); // Sometimes even this fails! It can't be!

            if task.CancellationToken.IsSignalled or (not Input.Take(vValue)) then
              Exit;

            // !!!!! Aborting on aItemToStop-th item
            if aAllowAbort then
              if vValue.AsInteger >= aItemToStop then
                raise EAbort.Create('Aborting pipeline!');

            case Random(5) of
              1:
                if aStageDelay > 0 then
                  Sleep(aStageDelay);
              2:
              begin
                vMS := TMemoryStream.Create;
                try
                  vMS.Size := Random(10000);
                finally
                  vMS.Free;
                end;
              end;
            else
              begin
                s := '';
                for i := 0 to Random(1000) do
                  s := s + IntToStr(Random(1000000));
              end;
            end;

            // Even if CompleteAdding is called on output, continue iterating on input, don't exit :)
            output.TryAdd(vValue);
          end;
        end;
    end;

  var
    i: Integer;
  begin
    Assert((aNumStages > 0) and (aMaxTasksOnStage > 0), 'Wrong arguments!');
    Assert(Length(aStageMaxDelays) = aNumStages);
    Result := Parallel.Pipeline;
    for i := 0 to aNumStages-1 do
      Result.Stage
       (GetStage(aStageMaxDelays[i], i = aStageToStop)).NumTasks(Random(aMaxTasksOnStage)+1);
    try
      for i := 0 to aNumItems-1 do
        Result.Input.Add(i);
    finally
      Result.Input.CompleteAdding;
    end;
  end;

var
  vCancelling: Boolean;
  vNumItems: Integer;
  vNumStages: Integer;
  vMaxTasksOnStage: Integer;
  vItemToAbort: Integer;
  vValue: TOmniValue;
  vNumTest: Integer;
  vStageMaxDelays: TIntList;
  i: Integer;
begin
  vNumItems := cNumItems;

  for vNumTest := 1 to cNumTests do
  begin
    // Note: sometimes it is greater than vNumItems, so abort don't happen
    vItemToAbort := Random(vNumItems);

    for vNumStages := 1 to cMaxStages do
      for vMaxTasksOnStage := 1 to cMaxTasksOnStage do
      begin
        lblStatus.Caption := Format('Running test #%d: #%d stages with max #%d tasks on each stage.', [vNumTest, vNumStages, vMaxTasksOnStage]);

        FPipelineExecuting := True;
        vCancelling := False;
        try
          SetLength(vStageMaxDelays, vNumStages);
          for i := 0 to High(vStageMaxDelays) do
            vStageMaxDelays[i] := Random(cMaxSleepRnd);

          FPipeline := BuildPipeline(vNumStages, vMaxTasksOnStage, vNumItems,
            Random(vNumStages*2) {sometimes not stops, because StageToStop>MaxStages},
            vItemToAbort, vStageMaxDelays);
          FPipeline.Run;

          while True do
          try
            FPipeline.Output.TryTake(vValue, 1);
            Application.ProcessMessages;
            if FStopAll then
            begin
              FPipeline.Cancel;
              vCancelling := True;
            end;

            if FPipeline.Output.IsFinalized then
              Break;
          except
            on E: EAbort do
              if not vCancelling then
              begin
                FPipeline.Cancel;
                vCancelling := True;
              end;
            on E: Exception do
            begin
              Memo1.Lines.Add('Exception: ' + E.Message);
              Break;
            end;
          end;
          while not FPipeline.WaitFor(3) do // Wait for full completion
            Application.ProcessMessages;

          FPipelineExecuting := False;

          if not vCancelling then
            Memo1.Lines.Add('Pipeline completed!')
          else
            Memo1.Lines.Add('Pipeline aborted!');

          if FStopAll then
            raise EAbort.Create('Stop all');
          
        finally
          FPipelineExecuting := False;
          FPipeline := nil;
        end;
      end;
  end;
end;

procedure TfrmPipelineStressTest.btStartStopClick(Sender: TObject);
begin
  if not FStopAll then
  begin
    Assert(Assigned(FPipeline));
    btStartStop.Enabled := False;
    FStopAll := True;
  end
  else begin
    btStartStop.Caption := 'Stop test';
    try
      FStopAll := False;
      TestPipeline;
    finally
      FStopAll := True;
      btStartStop.Caption := 'Run test';
      btStartStop.Enabled := True;
    end;
  end;
end;

end.
