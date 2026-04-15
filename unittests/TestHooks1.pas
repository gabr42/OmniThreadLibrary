unit TestHooks1;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TestThreadNotifications = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;
    [Test] procedure TestProcCreateDestroy;
    [Test] procedure TestUnregisterStopsNotifications;
    [Test] procedure TestMultipleListeners;
    [Test] procedure TestIntegrationWithTask;
  end;

  [TestFixture]
  TestPoolNotifications = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;
    [Test] procedure TestProcCreateDestroy;
    [Test] procedure TestUnregisterStopsNotifications;
    [Test] procedure TestIntegrationWithThreadPool;
  end;

  [TestFixture]
  TestExceptionFilters = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;
    [Test] procedure TestProcFilter;
    [Test] procedure TestFilterCanReplaceException;
    [Test] procedure TestFilterChainStopProcessing;
    [Test] procedure TestUnregisterStopsFiltering;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OtlSync,
  OtlHooks,
  OtlTask,
  OtlTaskControl,
  OtlThreadPool;

{ Global state for standalone procedure hooks }

var
  GThreadNotifyLog: string;
  GPoolNotifyLog  : string;
  GFilterLog      : string;

  { Integration test state }
  GIntGotCreate    : boolean;
  GIntGotDestroy   : boolean;
  GIntThreadName   : string;
  GIntPoolCreate   : boolean;
  GIntPoolDestroy  : boolean;

procedure ThreadNotifyProc(notifyType: TThreadNotificationType;
  const threadName: string);
begin
  case notifyType of
    tntCreate:  GThreadNotifyLog := GThreadNotifyLog + '+' + threadName;
    tntDestroy: GThreadNotifyLog := GThreadNotifyLog + '-' + threadName;
  end;
end;

procedure ThreadNotifyProc2(notifyType: TThreadNotificationType;
  const threadName: string);
begin
  case notifyType of
    tntCreate:  GThreadNotifyLog := GThreadNotifyLog + '[+' + threadName + ']';
    tntDestroy: GThreadNotifyLog := GThreadNotifyLog + '[-' + threadName + ']';
  end;
end;

procedure IntegrationThreadNotifyProc(notifyType: TThreadNotificationType;
  const threadName: string);
begin
  case notifyType of
    tntCreate: begin
      GIntGotCreate := true;
      GIntThreadName := threadName;
    end;
    tntDestroy:
      GIntGotDestroy := true;
  end;
end;

procedure PoolNotifyProc(notifyType: TPoolNotificationType;
  const pool: IOmniThreadPool);
begin
  case notifyType of
    pntCreate:  GPoolNotifyLog := GPoolNotifyLog + '+pool';
    pntDestroy: GPoolNotifyLog := GPoolNotifyLog + '-pool';
  end;
end;

procedure IntegrationPoolNotifyProc(notifyType: TPoolNotificationType;
  const pool: IOmniThreadPool);
begin
  case notifyType of
    pntCreate:  GIntPoolCreate := true;
    pntDestroy: GIntPoolDestroy := true;
  end;
end;

procedure ExceptionFilterProc(var e: Exception; var continueProcessing: boolean);
begin
  GFilterLog := GFilterLog + 'F1:' + e.Message;
end;

procedure ExceptionFilterReplace(var e: Exception; var continueProcessing: boolean);
begin
  FreeAndNil(e);
  e := Exception.Create('replaced');
end;

procedure ExceptionFilterStop(var e: Exception; var continueProcessing: boolean);
begin
  GFilterLog := GFilterLog + 'STOP';
  continueProcessing := false;
end;

procedure ExceptionFilterAfterStop(var e: Exception; var continueProcessing: boolean);
begin
  GFilterLog := GFilterLog + 'SHOULD_NOT_RUN';
end;

{ TestThreadNotifications }

procedure TestThreadNotifications.SetUp;
begin
  GThreadNotifyLog := '';
end;

procedure TestThreadNotifications.TearDown;
begin
  GThreadNotifyLog := '';
end;

procedure TestThreadNotifications.TestProcCreateDestroy;
begin
  RegisterThreadNotification(ThreadNotifyProc);
  try
    SendThreadNotifications(tntCreate, 'TestThread');
    Assert.AreEqual('+TestThread', GThreadNotifyLog);

    SendThreadNotifications(tntDestroy, 'TestThread');
    Assert.AreEqual('+TestThread-TestThread', GThreadNotifyLog);
  finally
    UnregisterThreadNotification(ThreadNotifyProc);
  end;
end;

procedure TestThreadNotifications.TestUnregisterStopsNotifications;
begin
  RegisterThreadNotification(ThreadNotifyProc);
  SendThreadNotifications(tntCreate, 'A');
  Assert.AreEqual('+A', GThreadNotifyLog);

  UnregisterThreadNotification(ThreadNotifyProc);
  SendThreadNotifications(tntCreate, 'B');
  Assert.AreEqual('+A', GThreadNotifyLog, 'Should not receive after unregister');
end;

procedure TestThreadNotifications.TestMultipleListeners;
begin
  RegisterThreadNotification(ThreadNotifyProc);
  RegisterThreadNotification(ThreadNotifyProc2);
  try
    SendThreadNotifications(tntCreate, 'X');
    Assert.AreEqual('+X[+X]', GThreadNotifyLog);
  finally
    UnregisterThreadNotification(ThreadNotifyProc);
    UnregisterThreadNotification(ThreadNotifyProc2);
  end;
end;

procedure TestThreadNotifications.TestIntegrationWithTask;
var
  task    : IOmniTaskControl;
  delegate: TOmniTaskDelegate;
begin
  GIntGotCreate := false;
  GIntGotDestroy := false;
  GIntThreadName := '';

  RegisterThreadNotification(IntegrationThreadNotifyProc);
  try
    delegate :=
      procedure(const aTask: IOmniTask)
      begin
        // do nothing, just start and stop
      end;
    task := CreateTask(delegate, 'TestHookTask').Run;
    task.Terminate(5000);

    Assert.IsTrue(GIntGotCreate, 'Thread create notification should have fired');
    Assert.IsTrue(GIntGotDestroy, 'Thread destroy notification should have fired');
    Assert.AreEqual('TestHookTask', GIntThreadName);
  finally
    UnregisterThreadNotification(IntegrationThreadNotifyProc);
  end;
end;

{ TestPoolNotifications }

procedure TestPoolNotifications.SetUp;
begin
  GPoolNotifyLog := '';
end;

procedure TestPoolNotifications.TearDown;
begin
  GPoolNotifyLog := '';
end;

procedure TestPoolNotifications.TestProcCreateDestroy;
begin
  RegisterPoolNotification(PoolNotifyProc);
  try
    SendPoolNotifications(pntCreate, nil);
    Assert.AreEqual('+pool', GPoolNotifyLog);

    SendPoolNotifications(pntDestroy, nil);
    Assert.AreEqual('+pool-pool', GPoolNotifyLog);
  finally
    UnregisterPoolNotification(PoolNotifyProc);
  end;
end;

procedure TestPoolNotifications.TestUnregisterStopsNotifications;
begin
  RegisterPoolNotification(PoolNotifyProc);
  SendPoolNotifications(pntCreate, nil);
  Assert.AreEqual('+pool', GPoolNotifyLog);

  UnregisterPoolNotification(PoolNotifyProc);
  SendPoolNotifications(pntCreate, nil);
  Assert.AreEqual('+pool', GPoolNotifyLog, 'Should not receive after unregister');
end;

procedure TestPoolNotifications.TestIntegrationWithThreadPool;
begin
  GIntPoolCreate := false;
  GIntPoolDestroy := false;

  RegisterPoolNotification(IntegrationPoolNotifyProc);
  try
    var pool := CreateThreadPool('TestHookPool');
    Assert.IsTrue(GIntPoolCreate, 'Pool create notification should have fired');

    pool := nil; // release triggers destroy
    Assert.IsTrue(GIntPoolDestroy, 'Pool destroy notification should have fired');
  finally
    UnregisterPoolNotification(IntegrationPoolNotifyProc);
  end;
end;

{ TestExceptionFilters }

procedure TestExceptionFilters.SetUp;
begin
  GFilterLog := '';
end;

procedure TestExceptionFilters.TearDown;
begin
  GFilterLog := '';
end;

procedure TestExceptionFilters.TestProcFilter;
var
  e: Exception;
begin
  RegisterExceptionFilter(ExceptionFilterProc);
  try
    e := Exception.Create('test error');
    try
      FilterException(e);
      Assert.AreEqual('F1:test error', GFilterLog);
    finally
      e.Free;
    end;
  finally
    UnregisterExceptionFilter(ExceptionFilterProc);
  end;
end;

procedure TestExceptionFilters.TestFilterCanReplaceException;
var
  e: Exception;
begin
  RegisterExceptionFilter(ExceptionFilterReplace);
  try
    e := Exception.Create('original');
    FilterException(e);
    Assert.AreEqual('replaced', e.Message);
    e.Free;
  finally
    UnregisterExceptionFilter(ExceptionFilterReplace);
  end;
end;

procedure TestExceptionFilters.TestFilterChainStopProcessing;
var
  e: Exception;
begin
  RegisterExceptionFilter(ExceptionFilterStop);
  RegisterExceptionFilter(ExceptionFilterAfterStop);
  try
    e := Exception.Create('err');
    try
      FilterException(e);
      Assert.AreEqual('STOP', GFilterLog, 'Second filter should not have run');
    finally
      e.Free;
    end;
  finally
    UnregisterExceptionFilter(ExceptionFilterStop);
    UnregisterExceptionFilter(ExceptionFilterAfterStop);
  end;
end;

procedure TestExceptionFilters.TestUnregisterStopsFiltering;
var
  e: Exception;
begin
  RegisterExceptionFilter(ExceptionFilterProc);
  UnregisterExceptionFilter(ExceptionFilterProc);

  e := Exception.Create('should not log');
  try
    FilterException(e);
    Assert.AreEqual('', GFilterLog, 'Filter should not run after unregister');
  finally
    e.Free;
  end;
end;

end.
