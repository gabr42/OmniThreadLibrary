unit TestTask;

interface

uses
  TestFramework,
  OtlSync;

type
  // Test methods for class IOmniBlockingCollection
  TestITaskControl = class(TTestCase)
  strict private
    Synchronizer: TOmniSynchronizer<string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartTask;
    procedure TestWait;
    procedure TestTerminate;
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
  FreeAndNil(Synchronizer);
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

initialization
  RegisterTest(TestITaskControl.Suite);
end.
