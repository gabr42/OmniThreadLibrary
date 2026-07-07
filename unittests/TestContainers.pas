/// Basic stack/queue container tests.
/// Serious testing is done as a part of the stress test.

unit TestContainers;

interface

uses
  TestFramework;

type
  TTestContainers = class(TTestCase)
  published
    procedure TestBasicQueue;
    procedure TestBasicStack;
    procedure TestOneElementQueue;
    procedure TestOneElementStack;
    {$IFDEF OTL_MobileSupport}
    procedure TestQueueObserverNotification;
    procedure TestStackObserverNotification;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, SyncObjs,
  OtlContainers, OtlContainerObserver, OtlSync;

{ TestContainers }

procedure TTestContainers.TestBasicQueue;
var
  queue: TOmniBaseBoundedQueue;
  test : integer;
  value: integer;

  procedure Verify(isEmpty, isFull: boolean; tag: string);
  begin
    CheckEquals(isEmpty, queue.IsEmpty, tag + '.Empty');
    CheckEquals(isFull, queue.IsFull, tag + '.Full');
  end;

begin
  queue := TOmniBaseBoundedQueue.Create;
  try
    queue.Initialize(4, SizeOf(integer));
    CheckEquals(SizeOf(integer), queue.ElementSize, 'ElementSize.1');
    CheckEquals(4, queue.NumElements, 'NumElements.1');
    Verify(true, false, '#1');

    value := 1;
    CheckTrue(queue.Enqueue(value), 'Enqueue.2');
    Verify(false, false, '#2');

    CheckTrue(queue.Dequeue(value), 'Dequeue.3');
    CheckEquals(1, value, 'value.3');
    Verify(true, false, '#3');

    CheckFalse(queue.Dequeue(value), 'Dequeue.4');
    Verify(true, false, '#4');

    value := 1;
    CheckTrue(queue.Enqueue(value), 'Enqueue.5');
    queue.Empty;
    Verify(true, false, '#5');

    for value := 1 to 4 do begin
      CheckTrue(queue.Enqueue(value), 'Enqueue.6.' + IntToStr(value));
      Verify(false, value = 4, '#6.' + IntToStr(value));
    end;

    CheckFalse(queue.Enqueue(value), 'Enqueue.7');
    Verify(false, true, '#7');

    for test := 1 to 4 do begin
      CheckTrue(queue.Dequeue(value), 'Dequeue.8.' + IntToStr(test));
      CheckEquals(test, value, 'value.8.' + IntToStr(test));
      Verify(test = 4, false, '#8.' + IntToStr(test));
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
    CheckEquals(isEmpty, stack.IsEmpty, tag + '.Empty');
    CheckEquals(isFull, stack.IsFull, tag + '.Full');
  end;

begin
  stack := TOmniBaseBoundedStack.Create;
  try
    stack.Initialize(4, SizeOf(integer));
    CheckEquals(SizeOf(integer), stack.ElementSize, 'ElementSize.1');
    CheckEquals(4, stack.NumElements, 'NumElements.1');
    Verify(true, false, '#1');

    value := 1;
    CheckTrue(stack.Push(value), 'Push.2');
    Verify(false, false, '#2');

    CheckTrue(stack.Pop(value), 'Pop.3');
    CheckEquals(1, value, 'value.3');
    Verify(true, false, '#3');

    CheckFalse(stack.Pop(value), 'Pop.4');
    Verify(true, false, '#4');

    value := 1;
    CheckTrue(stack.Push(value), 'Push.5');
    stack.Empty;
    Verify(true, false, '#5');

    for value := 1 to 4 do begin
      CheckTrue(stack.Push(value), 'Push.6.' + IntToStr(value));
      Verify(false, value = 4, '#6.' + IntToStr(value));
    end;

    CheckFalse(stack.Push(value), 'Push.7');
    Verify(false, true, '#7');

    for test := 4 downto 1 do begin
      CheckTrue(stack.Pop(value), 'Pop.8.' + IntToStr(test));
      CheckEquals(test, value, 'value.8.' + IntToStr(test));
      Verify(test = 1, false, '#8.' + IntToStr(test));
    end;
  finally FreeAndNil(stack); end;
end;

procedure TTestContainers.TestOneElementQueue;
var
  queue: TOmniBaseBoundedQueue;
  value: integer;
begin
  queue := TOmniBaseBoundedQueue.Create;
  try
    queue.Initialize(1, SizeOf(integer));
    CheckTrue(queue.IsEmpty);
    CheckFalse(queue.IsFull);

    value := 42;
    CheckTrue(queue.Enqueue(value), 'Enqueue first');
    CheckFalse(queue.IsEmpty);
    CheckTrue(queue.IsFull);

    // Second enqueue should fail
    value := 99;
    CheckFalse(queue.Enqueue(value), 'Enqueue second');

    CheckTrue(queue.Dequeue(value), 'Dequeue');
    CheckEquals(42, value);
    CheckTrue(queue.IsEmpty);
  finally FreeAndNil(queue); end;
end;

procedure TTestContainers.TestOneElementStack;
var
  stack: TOmniBaseBoundedStack;
  value: integer;
begin
  stack := TOmniBaseBoundedStack.Create;
  try
    stack.Initialize(1, SizeOf(integer));
    CheckTrue(stack.IsEmpty);
    CheckFalse(stack.IsFull);

    value := 42;
    CheckTrue(stack.Push(value), 'Push first');
    CheckFalse(stack.IsEmpty);
    CheckTrue(stack.IsFull);

    // Second push should fail
    value := 99;
    CheckFalse(stack.Push(value), 'Push second');

    CheckTrue(stack.Pop(value), 'Pop');
    CheckEquals(42, value);
    CheckTrue(stack.IsEmpty);
  finally FreeAndNil(stack); end;
end;

{$IFDEF OTL_MobileSupport}
procedure TTestContainers.TestQueueObserverNotification;
var
  queue   : TOmniBoundedQueue;
  observer: TOmniContainerEventObserver;
  value   : integer;
begin
  queue := TOmniBoundedQueue.Create(4, SizeOf(integer));
  try
    observer := CreateContainerEventObserver;
    try
      queue.ContainerSubject.Attach(observer, coiNotifyOnAllInserts);

      value := 1;
      queue.Enqueue(value);
      CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled,
        'Observer should be notified on enqueue');
    finally observer.Free; end;
  finally FreeAndNil(queue); end;
end;

procedure TTestContainers.TestStackObserverNotification;
var
  stack   : TOmniBoundedStack;
  observer: TOmniContainerEventObserver;
  value   : integer;
begin
  stack := TOmniBoundedStack.Create(4, SizeOf(integer));
  try
    observer := CreateContainerEventObserver;
    try
      stack.ContainerSubject.Attach(observer, coiNotifyOnAllInserts);

      value := 1;
      stack.Push(value);
      CheckTrue(observer.GetEvent.WaitFor(0) = wrSignaled,
        'Observer should be notified on push');
    finally observer.Free; end;
  finally FreeAndNil(stack); end;
end;
{$ENDIF}

initialization
  RegisterTest(TTestContainers.Suite);
end.
