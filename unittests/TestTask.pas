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
    procedure TestWorkerInitialized;
    procedure TestRegisterWaitObject;
    procedure TestInvoke;
  end;

implementation

uses
  System.SysUtils, System.Diagnostics,
  OtlTask, OtlTaskControl, OtlCommon;

type
  TSynchronizedOmniWorker = class(TOmniWorker)
  strict protected
    FSynchronizer: IOmniSynchronizer<string>;
  protected
    constructor Create(Synchronizer: IOmniSynchronizer<string>);
  end;

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
  sw    : TStopwatch;
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

  sw := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(sw.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

procedure TestITaskControl.TestWait;
var
  sw  : TStopwatch;
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

  sw := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(sw.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

procedure TestITaskControl.TestTerminate;
var
  sw  : TStopwatch;
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

  sw := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(sw.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

type
  TTerminateWhenTask = class(TSynchronizedOmniWorker)
  protected
    function Initialize: boolean; override;
  end;

function TTerminateWhenTask.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then
    FSynchronizer.Signal('started');
end;

procedure TestITaskControl.TestTerminateWhen;
var
  event: IOmniEvent;
  sw   : TStopwatch;
  task : IOmniTaskControl;
begin
  event := CreateOmniEvent(false, false);

  task := CreateTask(TTerminateWhenTask.Create(Synchronizer), 'Test task');
  task.TerminateWhen(event).Run;

  CheckTrue(Synchronizer.WaitFor('started', 1000), 'Task did not start in 1 second');
  CheckFalse(task.WaitFor(1000), 'Task has terminated prematurely');
  event.SetEvent;
  CheckTrue(task.WaitFor(3000), 'Task did not terminate in 3 seconds');

  sw := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(sw.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

type
  TWorkerInitializedTask = class(TSynchronizedOmniWorker)
  protected
    function Initialize: boolean; override;
  end;

function TWorkerInitializedTask.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then begin
    FSynchronizer.Signal('started');
    FSynchronizer.WaitFor('continue', 10000);
    Sleep(1000);
  end;
end;

procedure TestITaskControl.TestWorkerInitialized;
var
  await    : boolean;
  event    : IOmniEvent;
  stopwatch: TStopwatch;
  task     : IOmniTaskControl;
begin
  event := CreateOmniEvent(false, false);

  task := CreateTask(TWorkerInitializedTask.Create(Synchronizer), 'Test task');
  task.TerminateWhen(event).Run;

  CheckTrue(Synchronizer.WaitFor('started', 1000), 'Task did not start in 1 second');
  stopwatch := TStopwatch.StartNew;
  Synchronizer.Signal('continue');
  await := task.WaitForInit;
  stopwatch.Stop;
  CheckTrue(await, 'Task did not initialize correctly');
  CheckTrue(stopwatch.ElapsedMilliseconds >= 1000, 'WaitForInit has returned too soon');

  stopwatch := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(stopwatch.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

type
  TInvokeTask = class(TSynchronizedOmniWorker)
  strict private
    FInvoked: integer;
  strict protected
    procedure SignalDone;
  public
    procedure Method1;
    procedure Method2(const value: TOmniValue);
    procedure Method3(var obj: TOmniValueObj);
  end;

procedure TInvokeTask.Method1;
begin
  Inc(FInvoked, 1);
  SignalDone;
end;

procedure TInvokeTask.Method2(const value: TOmniValue);
begin
  if value = 42 then
    Inc(FInvoked, 2);
  SignalDone;
end;

procedure TInvokeTask.Method3(var obj: TOmniValueObj);
begin
  if obj.Value = '17' then
    Inc(FInvoked, 4);
  obj.Free;
  SignalDone;
end;

procedure TInvokeTask.SignalDone;
begin
  if FInvoked = 7 then
    FSynchronizer.Signal('done');
end;

procedure TestITaskControl.TestInvoke;
var
  task: IOmniTaskControl;
begin
  // Tests all the method signatures supported by Invoke (and other message dispatching functions).

  task := CreateTask(TInvokeTask.Create(Synchronizer), 'Test task');
  task.Run;

  task.Invoke('Method1');
  task.Invoke('Method2', 42);
  task.Invoke('Method3', TOmniValueObj.Create('17'));

  CheckTrue(Synchronizer.WaitFor('done', 5000));

  task.Terminate;
end;

type
  TRegisterWaitObjectTask = class(TSynchronizedOmniWorker)
  strict private
    FWaitObject1: IOmniEvent;
    FWaitObject2: IOmniEvent;
  strict protected
    procedure RespondToEvent1;
    procedure RespondToEvent2;
  protected
    function Initialize: boolean; override;
  public
    constructor Create(Synchronizer: IOmniSynchronizer<string>;
      const waitObject1, waitObject2: IOmniEvent);
  end;

constructor TRegisterWaitObjectTask.Create(Synchronizer: IOmniSynchronizer<string>;
  const waitObject1, waitObject2: IOmniEvent);
begin
  inherited Create(Synchronizer);
  FWaitObject1 := waitObject1;
  FWaitObject2 := waitObject2;
end;

function TRegisterWaitObjectTask.Initialize: boolean;
begin
  Result := inherited Initialize;
  if Result then begin
    Task.RegisterWaitObject(FWaitObject1, RespondToEvent1);
    {$IFDEF MSWINDOWS}
    Task.RegisterWaitObject(FWaitObject2.Handle, RespondToEvent2);
    {$ENDIF MSWINDOWS}
  end;
end;

procedure TRegisterWaitObjectTask.RespondToEvent1;
begin
  FSynchronizer.Signal('signal1');
end;

procedure TRegisterWaitObjectTask.RespondToEvent2;
begin
  FSynchronizer.Signal('signal2');
end;

procedure TestITaskControl.TestRegisterWaitObject;
var
  event1: IOmniEvent;
  event2: IOmniEvent;
  sw    : TStopwatch;
  task  : IOmniTaskControl;
begin
  event1 := CreateOmniEvent(false, false);
  event2 := CreateOmniEvent(false, false);

  task := CreateTask(TRegisterWaitObjectTask.Create(Synchronizer, event1, event2), 'Test task');
  task.Run;

  event1.SetEvent;
  event2.SetEvent;
  CheckTrue(Synchronizer.WaitFor('signal1', 3000), 'Wait object handler 1 was not triggered');
  {$IFDEF MSWINDOWS}
  CheckTrue(Synchronizer.WaitFor('signal2', 3000), 'Wait object handler 2 was not triggered');
  {$ENDIF MSWINDOWS}

  sw := TStopwatch.StartNew;
  task.Terminate;
  CheckTrue(sw.ElapsedMilliseconds < 500, 'Task took long time to terminate');
end;

{ TSynchronizedOmniWorker }

constructor TSynchronizedOmniWorker.Create(Synchronizer: IOmniSynchronizer<string>);
begin
  inherited Create;
  FSynchronizer := Synchronizer;
end;

initialization
  RegisterTest(TestITaskControl.Suite);
end.
