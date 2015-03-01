unit test_55_ForEachProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,

  OtlTask,
  OtlParallel;

type
  TfrmForEachWithProgressBar = class(TForm)
    btnStart : TButton;
    pbForEach: TProgressBar;
    procedure btnStartClick(Sender: TObject);
  private
    FPosition: integer;
    FProgress: integer;
    FWorker  : IOmniParallelLoop<integer>;
    procedure IncrementProgressBar;
  end;

var
  frmForEachWithProgressBar: TfrmForEachWithProgressBar;

implementation

{$R *.dfm}

const
  CNumLoop = 20000;

procedure TfrmForEachWithProgressBar.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled := false;

  pbForEach.Max := 100;
  pbForEach.Position := 0;
  pbForEach.Update;
  FProgress := 0;
  FPosition := 0;

  // reference must be kept in a global field so that the task controller is not destroyed before the processing ends
  FWorker := Parallel
    .ForEach(1, CNumLoop)
    .NoWait // important, otherwise message loop will be blocked while ForEach waits for all tasks to terminate
    .OnStop(
      procedure (const task: IOmniTask)
      begin
        // because of NoWait, OnStop delegate is invoked from the worker code; we must not destroy the worker at that point or the program will block
        task.Invoke(
          procedure begin
            FWorker := nil;
            btnStart.Enabled := true;
          end
        );
      end
    );

  FWorker.Execute(
    procedure (const task: IOmniTask; const i: integer)
    begin
      // do some work
      Sleep(1);

      // update the progress bar
      // we cannot use 'i' for progress as it does not increase sequentially
      // IncrementProgressBar uses internal counter to follow the progress
      task.Invoke(IncrementProgressBar);
    end
  );
end;

procedure TfrmForEachWithProgressBar.IncrementProgressBar;
var
  newPosition: integer;
begin
  Inc(FProgress);
  newPosition := Trunc((FProgress / CNumLoop)*pbForEach.Max);

  // make sure we don't overflow TProgressBar with messages
  if newPosition <> FPosition then begin
    pbForEach.Position := newPosition;
    FPosition := newPosition;
  end;
end;

end.
