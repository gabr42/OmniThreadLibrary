unit TestHooks1;

interface

{$I OtlOptions.inc}

uses
  TestFramework;

type
  TestThreadNotifications = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcCreateDestroy;
    procedure TestUnregisterStopsNotifications;
    procedure TestMultipleListeners;
    {$IFDEF OTL_Anonymous}
    procedure TestIntegrationWithTask;
    {$ENDIF}
  end;

  TestPoolNotifications = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcCreateDestroy;
    procedure TestUnregisterStopsNotifications;
    procedure TestIntegrationWithThreadPool;
  end;

  TestExceptionFilters = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcFilter;
    procedure TestFilterCanReplaceException;
    procedure TestFilterChainStopProcessing;
    procedure TestUnregisterStopsFiltering;
  end;

implementation

uses
  SysUtils,
  Classes,
  SyncObjs,
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
    CheckEquals('+TestThread', GThreadNotifyLog);

    SendThreadNotifications(tntDestroy, 'TestThread');
    CheckEquals('+TestThread-TestThread', GThreadNotifyLog);
  finally
    UnregisterThreadNotification(ThreadNotifyProc);
  end;
end;

procedure TestThreadNotifications.TestUnregisterStopsNotifications;
begin
  RegisterThreadNotification(ThreadNotifyProc);
  SendThreadNotifications(tntCreate, 'A');
  CheckEquals('+A', GThreadNotifyLog);

  UnregisterThreadNotification(ThreadNotifyProc);
  SendThreadNotifications(tntCreate, 'B');
  CheckEquals('+A', GThreadNotifyLog, 'Should not receive after unregister');
end;

procedure TestThreadNotifications.TestMultipleListeners;
begin
  RegisterThreadNotification(ThreadNotifyProc);
  RegisterThreadNotification(ThreadNotifyProc2);
  try
    SendThreadNotifications(tntCreate, 'X');
    CheckEquals('+X[+X]', GThreadNotifyLog);
  finally
    UnregisterThreadNotification(ThreadNotifyProc);
    UnregisterThreadNotification(ThreadNotifyProc2);
  end;
end;

{$IFDEF OTL_Anonymous}
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

    CheckTrue(GIntGotCreate, 'Thread create notification should have fired');
    CheckTrue(GIntGotDestroy, 'Thread destroy notification should have fired');
    CheckEquals('TestHookTask', GIntThreadName);
  finally
    UnregisterThreadNotification(IntegrationThreadNotifyProc);
  end;
end;
{$ENDIF}

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
    CheckEquals('+pool', GPoolNotifyLog);

    SendPoolNotifications(pntDestroy, nil);
    CheckEquals('+pool-pool', GPoolNotifyLog);
  finally
    UnregisterPoolNotification(PoolNotifyProc);
  end;
end;

procedure TestPoolNotifications.TestUnregisterStopsNotifications;
begin
  RegisterPoolNotification(PoolNotifyProc);
  SendPoolNotifications(pntCreate, nil);
  CheckEquals('+pool', GPoolNotifyLog);

  UnregisterPoolNotification(PoolNotifyProc);
  SendPoolNotifications(pntCreate, nil);
  CheckEquals('+pool', GPoolNotifyLog, 'Should not receive after unregister');
end;

procedure TestPoolNotifications.TestIntegrationWithThreadPool;
var
  pool: IOmniThreadPool;
begin
  GIntPoolCreate := false;
  GIntPoolDestroy := false;

  RegisterPoolNotification(IntegrationPoolNotifyProc);
  try
    pool := CreateThreadPool('TestHookPool');
    CheckTrue(GIntPoolCreate, 'Pool create notification should have fired');

    pool := nil; // release triggers destroy
    CheckTrue(GIntPoolDestroy, 'Pool destroy notification should have fired');
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
      CheckEquals('F1:test error', GFilterLog);
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
    CheckEquals('replaced', e.Message);
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
      CheckEquals('STOP', GFilterLog, 'Second filter should not have run');
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
    CheckEquals('', GFilterLog, 'Filter should not run after unregister');
  finally
    e.Free;
  end;
end;

initialization
  RegisterTest(TestThreadNotifications.Suite);
  RegisterTest(TestPoolNotifications.Suite);
  RegisterTest(TestExceptionFilters.Suite);
end.
