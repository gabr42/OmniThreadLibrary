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
  end;

implementation

uses
  SysUtils,
  OtlContainers;

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
      CheckTrue(queue.Enqueue(value), 'Enqueue.6.' + value.ToString);
      Verify(false, value = 4, '#6.' + value.ToString);
    end;

    CheckFalse(queue.Enqueue(value), 'Enqueue.7');
    Verify(false, true, '#7');

    for test := 1 to 4 do begin
      CheckTrue(queue.Dequeue(value), 'Dequeue.8.' + test.ToString);
      CheckEquals(test, value, 'value.8.' + test.ToString);
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
      CheckTrue(stack.Push(value), 'Push.6.' + value.ToString);
      Verify(false, value = 4, '#6.' + value.ToString);
    end;

    CheckFalse(stack.Push(value), 'Push.7');
    Verify(false, true, '#7');

    for test := 4 downto 1 do begin
      CheckTrue(stack.Pop(value), 'Pop.8.' + test.ToString);
      CheckEquals(test, value, 'value.8.' + test.ToString);
      Verify(test = 1, false, '#8.' + test.ToString);
    end;
  finally FreeAndNil(stack); end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestContainers.Suite);
end.
