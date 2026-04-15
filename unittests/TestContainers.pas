/// Basic stack/queue container tests.
/// Serious testing is done as a part of the stress test.

unit TestContainers;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestContainers = class
  public
    [Test]
    procedure TestBasicQueue;
    [Test]
    procedure TestBasicStack;
    [Test]
    procedure TestOneElementQueue;
    [Test]
    procedure TestOneElementStack;
    [Test]
    procedure TestQueueObserverNotification;
    [Test]
    procedure TestStackObserverNotification;
  end;

implementation

uses
  System.SysUtils, System.SyncObjs,
  OtlContainers, OtlContainerObserver, OtlSync;

{ TestContainers }

procedure TTestContainers.TestBasicQueue;
var
  queue: TOmniBaseBoundedQueue;
  test : integer;
  value: integer;

  procedure Verify(isEmpty, isFull: boolean; tag: string);
  begin
    Assert.AreEqual(isEmpty, queue.IsEmpty, tag + '.Empty');
    Assert.AreEqual(isFull, queue.IsFull, tag + '.Full');
  end;

begin
  queue := TOmniBaseBoundedQueue.Create;
  try
    queue.Initialize(4, SizeOf(integer));
    Assert.AreEqual(SizeOf(integer), queue.ElementSize, 'ElementSize.1');
    Assert.AreEqual(4, queue.NumElements, 'NumElements.1');
    Verify(true, false, '#1');

    value := 1;
    Assert.IsTrue(queue.Enqueue(value), 'Enqueue.2');
    Verify(false, false, '#2');

    Assert.IsTrue(queue.Dequeue(value), 'Dequeue.3');
    Assert.AreEqual(1, value, 'value.3');
    Verify(true, false, '#3');

    Assert.IsFalse(queue.Dequeue(value), 'Dequeue.4');
    Verify(true, false, '#4');

    value := 1;
    Assert.IsTrue(queue.Enqueue(value), 'Enqueue.5');
    queue.Empty;
    Verify(true, false, '#5');

    for value := 1 to 4 do begin
      Assert.IsTrue(queue.Enqueue(value), 'Enqueue.6.' + value.ToString);
      Verify(false, value = 4, '#6.' + value.ToString);
    end;

    Assert.IsFalse(queue.Enqueue(value), 'Enqueue.7');
    Verify(false, true, '#7');

    for test := 1 to 4 do begin
      Assert.IsTrue(queue.Dequeue(value), 'Dequeue.8.' + test.ToString);
      Assert.AreEqual(test, value, 'value.8.' + test.ToString);
      Verify(test = 4, false, '#8.' + test.ToString);
    end;
  finally FreeAndNil(queue); end;
end;

procedure TTestContainers.TestBasicStack;
var
  stack: TOmniBaseBoundedStack;
  test : integer;
  value: integer;

  procedure Verify(isEmpty, isFull: boolean; tag: string);
  begin
    Assert.AreEqual(isEmpty, stack.IsEmpty, tag + '.Empty');
    Assert.AreEqual(isFull, stack.IsFull, tag + '.Full');
  end;

begin
  stack := TOmniBaseBoundedStack.Create;
  try
    stack.Initialize(4, SizeOf(integer));
    Assert.AreEqual(SizeOf(integer), stack.ElementSize, 'ElementSize.1');
    Assert.AreEqual(4, stack.NumElements, 'NumElements.1');
    Verify(true, false, '#1');

    value := 1;
    Assert.IsTrue(stack.Push(value), 'Push.2');
    Verify(false, false, '#2');

    Assert.IsTrue(stack.Pop(value), 'Pop.3');
    Assert.AreEqual(1, value, 'value.3');
    Verify(true, false, '#3');

    Assert.IsFalse(stack.Pop(value), 'Pop.4');
    Verify(true, false, '#4');

    value := 1;
    Assert.IsTrue(stack.Push(value), 'Push.5');
    stack.Empty;
    Verify(true, false, '#5');

    for value := 1 to 4 do begin
      Assert.IsTrue(stack.Push(value), 'Push.6.' + value.ToString);
      Verify(false, value = 4, '#6.' + value.ToString);
    end;

    Assert.IsFalse(stack.Push(value), 'Push.7');
    Verify(false, true, '#7');

    for test := 4 downto 1 do begin
      Assert.IsTrue(stack.Pop(value), 'Pop.8.' + test.ToString);
      Assert.AreEqual(test, value, 'value.8.' + test.ToString);
      Verify(test = 1, false, '#8.' + test.ToString);
    end;
  finally FreeAndNil(stack); end;
end;

procedure TTestContainers.TestOneElementQueue;
var
  value: integer;
begin
  var queue := TOmniBaseBoundedQueue.Create;
  try
    queue.Initialize(1, SizeOf(integer));
    Assert.IsTrue(queue.IsEmpty);
    Assert.IsFalse(queue.IsFull);

    value := 42;
    Assert.IsTrue(queue.Enqueue(value), 'Enqueue first');
    Assert.IsFalse(queue.IsEmpty);
    Assert.IsTrue(queue.IsFull);

    // Second enqueue should fail
    value := 99;
    Assert.IsFalse(queue.Enqueue(value), 'Enqueue second');

    Assert.IsTrue(queue.Dequeue(value), 'Dequeue');
    Assert.AreEqual(42, value);
    Assert.IsTrue(queue.IsEmpty);
  finally FreeAndNil(queue); end;
end;

procedure TTestContainers.TestOneElementStack;
var
  value: integer;
begin
  var stack := TOmniBaseBoundedStack.Create;
  try
    stack.Initialize(1, SizeOf(integer));
    Assert.IsTrue(stack.IsEmpty);
    Assert.IsFalse(stack.IsFull);

    value := 42;
    Assert.IsTrue(stack.Push(value), 'Push first');
    Assert.IsFalse(stack.IsEmpty);
    Assert.IsTrue(stack.IsFull);

    // Second push should fail
    value := 99;
    Assert.IsFalse(stack.Push(value), 'Push second');

    Assert.IsTrue(stack.Pop(value), 'Pop');
    Assert.AreEqual(42, value);
    Assert.IsTrue(stack.IsEmpty);
  finally FreeAndNil(stack); end;
end;

procedure TTestContainers.TestQueueObserverNotification;
var
  value: integer;
begin
  var queue := TOmniBoundedQueue.Create(4, SizeOf(integer));
  try
    var observer := CreateContainerEventObserver;
    try
      queue.ContainerSubject.Attach(observer, coiNotifyOnAllInserts);

      value := 1;
      queue.Enqueue(value);
      Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled,
        'Observer should be notified on enqueue');
    finally observer.Free; end;
  finally FreeAndNil(queue); end;
end;

procedure TTestContainers.TestStackObserverNotification;
var
  value: integer;
begin
  var stack := TOmniBoundedStack.Create(4, SizeOf(integer));
  try
    var observer := CreateContainerEventObserver;
    try
      stack.ContainerSubject.Attach(observer, coiNotifyOnAllInserts);

      value := 1;
      stack.Push(value);
      Assert.IsTrue(observer.GetEvent.WaitFor(0) = wrSignaled,
        'Observer should be notified on push');
    finally observer.Free; end;
  finally FreeAndNil(stack); end;
end;

end.
