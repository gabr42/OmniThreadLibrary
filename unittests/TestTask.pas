unit TestTask;

interface

uses
  TestFramework,
  OtlSync;

type
  // Test methods for class IOmniBlockingCollection
  TestITaskControl = class(TTestCase)
  strict private
    Synchronizer: IOmniSynchronizer<string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartTask;
    procedure TestWait;
    procedure TestTerminate;
    procedure TestTerminateWhen;
  end;

implementation

uses
  System.SysUtils,
  OtlTask, OtlTaskControl;

{ TestITaskControl }

procedure TestITaskControl.SetUp;
begin
  inherited;
  Synchronizer := TOmniSynchronizer<string>.Create;
end;

procedure TestITaskControl.TearDown;
begin
  Synchronizer := nil;
  inherited;
end;

procedure TestITaskControl.TestStartTask;
var
  didRun: boolean;
  task  : IOmniTaskControl;
begin
  didRun := false;
  task := CreateTask(
    procedure (const task: IOmniTask)
    begin
      didRun := true;
    end,
    'Test task');

  task.Run;

  CheckTrue(task.WaitFor(3000), 'Task did not terminate in 3 seconds');
  CheckTrue(didRun, 'Task did not run');

  task.Terminate;
end;

procedure TestITaskControl.TestWait;
var
  task: IOmniTaskControl;
begin
  task := CreateTask(
    procedure (const task: IOmniTask)
    begin
      Synchronizer.Signal('started');
      Synchronizer.WaitFor('stop', 5000);
    end,
    'Test task');

  task.Run;

  CheckTrue(Synchronizer.WaitFor('started', 1000), 'Task did not start in 1 second');
  CheckFalse(task.WaitFor(0), 'WaitFor(0) should not succeed');
  CheckFalse(task.WaitFor(1000), 'WaitFor(100) should not succeed');

  Synchronizer.Signal('stop');

  CheckTrue(task.WaitFor(3000), 'Task did not terminate in 3 seconds');

  task.Terminate;
end;

procedure TestITaskControl.TestTerminate;
var
  task: IOmniTaskControl;
begin
  task := CreateTask(
    procedure (const task: IOmniTask)
    begin
      Synchronizer.Signal('started');
      while not task.Terminated do
        Sleep(0);
    end,
    'Test task');

  task.Run;

  CheckTrue(Synchronizer.WaitFor('started', 1000), 'Task did not start in 1 second');
  CheckFalse(task.WaitFor(1000), 'Task has terminated prematurely');
  task.Stop;
  CheckTrue(task.WaitFor(3000), 'Task did not terminate in 3 seconds');

  task.Terminate;
end;

type
  TTerminateWhenTask = class(TOmniWorker)
  strict protected
    FSynchronizer: IOmniSynchronizer<string>;
  protected
    constructor Create(Synchronizer: IOmniSynchronizer<string>);
    function Initialize: boolean; override;
  end;

procedure TestITaskControl.TestTerminateWhen;
var
  task: IOmniTaskControl;
  event: IOmniEvent;
begin
  event := CreateOmniEvent(false, false);

  task := CreateTask(TTerminateWhenTask.Create(Synchronizer), 'Test task');
  task.TerminateWhen(event).Run;

  CheckTrue(Synchronizer.WaitFor('started', 1000), 'Task did not start in 1 second');
  CheckFalse(task.WaitFor(1000), 'Task has terminated prematurely');
  event.SetEvent;
  CheckTrue(task.WaitFor(3000), 'Task did not terminate in 3 seconds');

  task.Terminate;
end;

{ TTerminateWhenTask }

constructor TTerminateWhenTask.Create(Synchronizer: IOmniSynchronizer<string>);
begin
  inherited Create;
  FSynchronizer := Synchronizer;
end;

function TTerminateWhenTask.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then
    FSynchronizer.Signal('started');
end;

initialization
  RegisterTest(TestITaskControl.Suite);
end.
