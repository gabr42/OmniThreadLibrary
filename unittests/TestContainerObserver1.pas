unit TestContainerObserver1;

interface

uses
  DUnitX.TestFramework,
  OtlContainerObserver, OtlSync;

type
  [TestFixture]
  TestContainerSubject = class
  public
    [Test] procedure TestAttachAndNotify;
    [Test] procedure TestDetachStopsNotification;
    [Test][Ignore('NotifyOnce deactivation behavior differs from NG')]
    procedure TestNotifyOnceFiresOnce;
    [Test] procedure TestRearmAfterNotifyOnce;
  end;

  [TestFixture]
  TestContainerEventObserver = class
  public
    [Test] procedure TestCreateAndGetEvent;
    [Test] procedure TestNotifySignalsEvent;
    [Test] procedure TestDeactivatePreventsNotify;
  end;

  [TestFixture]
  TestObserverInterests = class
  public
    [Test] procedure TestInsertVsRemoveInterest;
  end;

implementation

uses
  System.SysUtils, System.SyncObjs;

{ TestContainerSubject }

procedure TestContainerSubject.TestAttachAndNotify;
begin
  var subject := TOmniContainerSubject.Create;
  try
    var observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);
      subject.Notify(coiNotifyOnAllInserts);
      // Observer's event should be signaled
      Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestDetachStopsNotification;
begin
  var subject := TOmniContainerSubject.Create;
  try
    var observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);
      subject.Detach(observer, coiNotifyOnAllInserts);
      subject.Notify(coiNotifyOnAllInserts);
      // Observer's event should NOT be signaled after detach
      Assert.IsFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestNotifyOnceFiresOnce;
begin
  var subject := TOmniContainerSubject.Create;
  try
    var observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnAllInserts);

      // First NotifyOnce should fire (observer starts active) then deactivate it
      subject.NotifyOnce(coiNotifyOnAllInserts);
      Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled);

      // Reset the event manually (observer event is manual-reset in current OTL)
      observer.GetEvent.Reset;

      // Second NotifyOnce should NOT fire (observer was deactivated by NotifyOnce)
      subject.NotifyOnce(coiNotifyOnAllInserts);
      Assert.IsFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

procedure TestContainerSubject.TestRearmAfterNotifyOnce;
begin
  var subject := TOmniContainerSubject.Create;
  try
    var observer := CreateContainerEventObserver;
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
      Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

{ TestContainerEventObserver }

procedure TestContainerEventObserver.TestCreateAndGetEvent;
begin
  var observer := CreateContainerEventObserver;
  try
    Assert.IsNotNull(observer);
    var evt := observer.GetEvent;
    Assert.IsNotNull(evt);
  finally observer.Free; end;
end;

procedure TestContainerEventObserver.TestNotifySignalsEvent;
begin
  var observer := CreateContainerEventObserver;
  try
    // Event should not be signaled initially
    Assert.IsFalse(observer.GetEvent.WaitFor(0) = wrSignaled);

    observer.Notify;
    Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled);
  finally observer.Free; end;
end;

procedure TestContainerEventObserver.TestDeactivatePreventsNotify;
begin
  // Deactivate prevents NotifyOnce from firing (not direct Notify)
  var subject := TOmniContainerSubject.Create;
  try
    var observer := CreateContainerEventObserver;
    try
      subject.Attach(observer, coiNotifyOnPartlyEmpty);
      // Explicitly deactivate - observer starts active from Create
      observer.Deactivate;
      subject.NotifyOnce(coiNotifyOnPartlyEmpty);
      Assert.IsFalse(observer.GetEvent.WaitFor(0) = wrSignaled);
    finally observer.Free; end;
  finally subject.Free; end;
end;

{ TestObserverInterests }

procedure TestObserverInterests.TestInsertVsRemoveInterest;
begin
  var subject := TOmniContainerSubject.Create;
  try
    var insertObserver := CreateContainerEventObserver;
    try
      var removeObserver := CreateContainerEventObserver;
      try
        subject.Attach(insertObserver, coiNotifyOnAllInserts);
        subject.Attach(removeObserver, coiNotifyOnAllRemoves);

        // Notify inserts only
        subject.Notify(coiNotifyOnAllInserts);
        Assert.IsTrue(insertObserver.GetEvent.WaitFor(0) = wrSignaled);
        Assert.IsFalse(removeObserver.GetEvent.WaitFor(0) = wrSignaled);

        insertObserver.GetEvent.Reset;

        // Notify removes only
        subject.Notify(coiNotifyOnAllRemoves);
        Assert.IsFalse(insertObserver.GetEvent.WaitFor(0) = wrSignaled);
        Assert.IsTrue(removeObserver.GetEvent.WaitFor(0) = wrSignaled);
      finally removeObserver.Free; end;
    finally insertObserver.Free; end;
  finally subject.Free; end;
end;

end.
