# OTL reference: Collections

`IOmniBlockingCollection`, lock-free stack and queues, container observers.

## Contents

- IOmniBlockingCollection
- Lock-Free Bounded Stack (IOmniStack)
- Lock-Free Bounded Queue (IOmniQueue)
- Lock-Free Dynamic Queue (TOmniQueue)
- Container Observers


### IOmniBlockingCollection

Thread-safe producer/consumer queue (in `OtlCollections`):

```pascal
IOmniBlockingCollection = interface
  procedure Add(const value: TOmniValue);
  procedure CompleteAdding;
  procedure DeThrottle;                   // [3.09] switch throttling off, release blocked writers
  function  GetEnumerator: IOmniValueEnumerator;
  function  IsCompleted: boolean;
  function  IsEmpty: boolean;             // [3.07]
  function  IsFinalized: boolean;         // IsCompleted AND IsEmpty
  function  IsThrottling: boolean;        // [3.09]
  function  Next: TOmniValue;             // calls Take; raises ECollectionCompleted if done
  procedure ReraiseExceptions(enable: boolean = true);
  procedure SetThrottling(highWatermark, lowWatermark: integer);
  function  Take(var value: TOmniValue): boolean;       // blocks; returns False on CompleteAdding
  function  TryAdd(const value: TOmniValue): boolean;   // returns False if CompleteAdding called
  function  TryTake(var value: TOmniValue;
    timeout_ms: cardinal = 0): boolean;                 // timeout; INFINITE supported
  property ContainerSubject: TOmniContainerSubject;
  property Count: integer;               // [3.07]
end;
```

**Key behaviors:**
- `Add` raises exception if `CompleteAdding` was called
- `Take` blocks until data available or `CompleteAdding` called
- `TryTake(v, 0)` = non-blocking peek
- `TryTake(v, INFINITE)` = blocking wait
- For..in enumerator blocks in `MoveNext`; stops when `CompleteAdding` is called
- `ReraiseExceptions(true)`: retrieved exception `TOmniValue` is re-raised

**Throttling:**
```pascal
SetThrottling(highWatermark, lowWatermark);
// Add/TryAdd block when count >= highWatermark
// Unblock when count < lowWatermark
```

`DeThrottle` [3.09] switches throttling off at run time and unblocks all writers waiting in `Add`/`TryAdd`; callable from any thread at any time, does nothing if throttling was not enabled, and throttling cannot be enabled again afterwards. Use it to shut down a throttled collection without leaving producers blocked on consumers that will never take more data. `IsThrottling` is `True` if throttling was enabled with `SetThrottling` and not switched off by `DeThrottle`.

**Bulk import/export (D2010+):**
```pascal
class function TOmniBlockingCollection.FromArray<T>(const values: TArray<T>):
  IOmniBlockingCollection;  // [3.07.6]
class function TOmniBlockingCollection.ToArray<T>(
  const coll: IOmniBlockingCollection): TArray<T>;
procedure TOmniBlockingCollection.AddRange<T>(const values: array of T); overload;
```

### Lock-Free Bounded Stack (IOmniStack)

```pascal
IOmniStack = interface
  procedure Empty;
  procedure Initialize(numElements, elementSize: integer);
  function  IsEmpty: boolean;
  function  IsFull: boolean;
  function  Pop(var value): boolean;
  function  Push(const value): boolean;
end;

TOmniBoundedStack = class(TOmniBaseBoundedStack)
  constructor Create(numElements, elementSize: integer;
    partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
    almostFullLoadFactor: real = CAlmostFullLoadFactor);
  property ContainerSubject: TOmniContainerSubject;
end;
```

`Push` returns False if full. `Pop` returns False if empty.

### Lock-Free Bounded Queue (IOmniQueue)

```pascal
IOmniQueue = interface
  function  Dequeue(var value): boolean;
  procedure Empty;
  function  Enqueue(const value): boolean;
  procedure Initialize(numElements, elementSize: integer);
  function  IsEmpty: boolean;
  function  IsFull: boolean;
end;

TOmniBoundedQueue = class(TOmniBaseBoundedQueue)
  constructor Create(numElements, elementSize: integer;
    partlyEmptyLoadFactor: real = CPartlyEmptyLoadFactor;
    almostFullLoadFactor: real = CAlmostFullLoadFactor);
  property ContainerSubject: TOmniContainerSubject;
end;
```

`Dequeue` returns False if empty. `Enqueue` returns False if full.

### Lock-Free Dynamic Queue (TOmniQueue)

Unlimited-size queue of `TOmniValue` elements:

```pascal
TOmniBaseQueue = class
  constructor Create(blockSize: integer = 65536; numCachedBlocks: integer = 4);
  function  Dequeue: TOmniValue;           // raises exception if empty
  procedure Enqueue(const value: TOmniValue);
  function  IsEmpty: boolean;
  function  TryDequeue(var value: TOmniValue): boolean;
end;

TOmniQueue = class(TOmniBaseQueue)
  property ContainerSubject: TOmniContainerSubject;
end;
```

### Container Observers

```pascal
type
  TOmniContainerObserverInterest = (
    coiNotifyOnAllInserts,     // permanent
    coiNotifyOnAllRemoves,     // permanent
    coiNotifyOnPartlyEmpty,    // one-shot (below partlyEmptyLoadFactor, default 80%)
    coiNotifyOnAlmostFull      // one-shot (above almostFullLoadFactor, default 90%)
  );

function CreateContainerWindowsEventObserver(
  externalEvent: THandle = 0): TOmniContainerWindowsEventObserver;
function CreateContainerWindowsMessageObserver(hWindow: THandle;
  msg: cardinal; wParam, lParam: integer): TOmniContainerWindowsMessageObserver;
```

**One-shot events:** after firing, must detach and re-attach observer to receive again.

**Usage:**
```pascal
FObserver := CreateContainerWindowsEventObserver;
FCollection.ContainerSubject.Attach(FObserver, coiNotifyOnAllInserts);
FEvent := FObserver.GetEvent;  // wait on this handle
// ... when done:
FCollection.ContainerSubject.Detach(FObserver, coiNotifyOnAllInserts);
FreeAndNil(FObserver);
```
