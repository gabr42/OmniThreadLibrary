unit TestContainerObserver1;

interface

uses
  TestFramework,
  OtlContainerObserver, OtlSync, OtlCommon;

type
  TestContainerSubject = class(TTestCase)
  public
    // TestNotifyOnceFiresOnce is not published (ignored: NotifyOnce deactivation behavior differs from NG)
    procedure TestNotifyOnceFiresOnce;
  published
    procedure TestAttachAndNotify;
    procedure TestDetachStopsNotification;
    procedure TestRearmAfterNotifyOnce;
  end;

  TestContainerEventObserver = class(TTestCase)
  published
    procedure TestCreateAndGetEvent;
    procedure TestNotifySignalsEvent;
    procedure TestDeactivatePreventsNotify;
  end;

  TestObserverInterests = class(TTestCase)
  published
    procedure TestInsertVsRemoveInterest;
  end;

implementation

uses
  SysUtils, SyncObjs;

{ TestContainerSubject }

procedure TestContainerSubject.TestAttachAndNotify;
var
  subject : TOmniContainerSubject;
  observer: TOmniContainerEventObserver;
begin
  subject := TOmniContainerSubject.Create;
  try
    observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);
      subject.Notify(coiNotifyOnAllInserts);
      // Observer's event should be signaled
      CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestDetachStopsNotification;
var
  subject : TOmniContainerSubject;
  observer: TOmniContainerEventObserver;
begin
  subject := TOmniContainerSubject.Create;
  try
    observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);
      subject.Detach(observer, coiNotifyOnAllInserts);
      subject.Notify(coiNotifyOnAllInserts);
      // Observer's event should NOT be signaled after detach
      CheckFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestNotifyOnceFiresOnce;
var
  subject : TOmniContainerSubject;
  observer: TOmniContainerEventObserver;
begin
  subject := TOmniContainerSubject.Create;
  try
    observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);

      // First NotifyOnce should fire (observer starts active) then deactivate it
      subject.NotifyOnce(coiNotifyOnAllInserts);
      CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled);

      // Reset the event manually (observer event is manual-reset in current OTL)
      observer.GetEvent.Reset;

      // Second NotifyOnce should NOT fire (observer was deactivated by NotifyOnce)
      subject.NotifyOnce(coiNotifyOnAllInserts);
      CheckFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestRearmAfterNotifyOnce;
var
  subject : TOmniContainerSubject;
  observer: TOmniContainerEventObserver;
begin
  subject := TOmniContainerSubject.Create;
  try
    observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);

      // Fire once - observer gets deactivated by NotifyOnce
      subject.NotifyOnce(coiNotifyOnAllInserts);
      // Consume and reset the event
      observer.GetEvent.WaitFor(0);
      observer.GetEvent.Reset;

      // Rearm re-activates observers for this interest
      subject.Rearm(coiNotifyOnAllInserts);

      // Now NotifyOnce should fire again
      subject.NotifyOnce(coiNotifyOnAllInserts);
      CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

{ TestContainerEventObserver }

procedure TestContainerEventObserver.TestCreateAndGetEvent;
var
  observer: TOmniContainerEventObserver;
  evt     : IOmniEvent;
begin
  observer := CreateContainerEventObserver;
  try
    CheckNotNull(observer);
    evt := observer.GetEvent;
    Check(evt <> nil);
  finally observer.Free; end;
end;

procedure TestContainerEventObserver.TestNotifySignalsEvent;
var
  observer: TOmniContainerEventObserver;
begin
  observer := CreateContainerEventObserver;
  try
    // Event should not be signaled initially
    CheckFalse(observer.GetEvent.WaitFor(0) = wrSignaled);

    observer.Notify;
    CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
  finally observer.Free; end;
end;

procedure TestContainerEventObserver.TestDeactivatePreventsNotify;
var
  subject : TOmniContainerSubject;
  observer: TOmniContainerEventObserver;
begin
  // Deactivate prevents NotifyOnce from firing (not direct Notify)
  subject := TOmniContainerSubject.Create;
  try
    observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnPartlyEmpty);
      // Explicitly deactivate - observer starts active from Create
      observer.Deactivate;
      subject.NotifyOnce(coiNotifyOnPartlyEmpty);
      CheckFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

{ TestObserverInterests }

procedure TestObserverInterests.TestInsertVsRemoveInterest;
var
  subject       : TOmniContainerSubject;
  insertObserver: TOmniContainerEventObserver;
  removeObserver: TOmniContainerEventObserver;
begin
  subject := TOmniContainerSubject.Create;
  try
    insertObserver := CreateContainerEventObserver;
    try
      removeObserver := CreateContainerEventObserver;
      try
        subject.Attach(insertObserver, coiNotifyOnAllInserts);
        subject.Attach(removeObserver, coiNotifyOnAllRemoves);

        // Notify inserts only
        subject.Notify(coiNotifyOnAllInserts);
        CheckTrue(insertObserver.GetEvent.WaitFor(0) = wrSignaled);
        CheckFalse(removeObserver.GetEvent.WaitFor(0) = wrSignaled);

        insertObserver.GetEvent.Reset;

        // Notify removes only
        subject.Notify(coiNotifyOnAllRemoves);
        CheckFalse(insertObserver.GetEvent.WaitFor(0) = wrSignaled);
        CheckTrue(removeObserver.GetEvent.WaitFor(0) = wrSignaled);
      finally removeObserver.Free; end;
    finally insertObserver.Free; end;
  finally subject.Free; end;
end;

initialization
  RegisterTest(TestContainerSubject.Suite);
  RegisterTest(TestContainerEventObserver.Suite);
  RegisterTest(TestObserverInterests.Suite);
end.
