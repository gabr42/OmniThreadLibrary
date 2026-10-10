# OTL reference: Communication and Messaging

Communication endpoints, `TOmniTwoWayChannel`, message-loop requirement.

## Contents

- IOmniCommunicationEndpoint
- TOmniTwoWayChannel
- Message Loop Requirement


### IOmniCommunicationEndpoint

Bidirectional message channel endpoint:

```pascal
TOmniMessage = record
  MsgID  : word;
  MsgData: TOmniValue;
  constructor Create(aMsgID: word; aMsgData: TOmniValue); overload;
  constructor Create(aMsgID: word); overload;
end;

IOmniCommunicationEndpoint = interface
  function  Receive(var msg: TOmniMessage): boolean; overload;
  function  Receive(var msgID: word; var msgData: TOmniValue): boolean; overload;
  function  ReceiveWait(var msg: TOmniMessage; timeout_ms: cardinal): boolean; overload;
  procedure Send(const msg: TOmniMessage); overload;
  procedure Send(msgID: word); overload;
  procedure Send(msgID: word; msgData: array of const); overload;
  procedure Send(msgID: word; msgData: TOmniValue); overload;
  function  SendWait(msgID: word; timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
  function  SendWait(msgID: word; msgData: TOmniValue;
    timeout_ms: cardinal = CMaxSendWaitTime_ms): boolean; overload;
  property NewMessageEvent: THandle;           // wait on this handle
  property OtherEndpoint: IOmniCommunicationEndpoint;
  property Reader: TOmniMessageQueue;
  property Writer: TOmniMessageQueue;
end;
```

**Notes:**
- `Receive` returns False if queue is empty (non-blocking)
- `ReceiveWait(msg, 0)` = same as `Receive`; `INFINITE` blocks until message arrives
- `Send` raises exception if queue is full. Default queue size: 1000 messages.
- `SendWait` waits up to `timeout_ms` if full; returns False if still full after timeout
- **CAUTION:** Messages sent to main thread should use message IDs >= `WM_USER`.

### TOmniTwoWayChannel

Bidirectional communication channel:

```pascal
IOmniTwoWayChannel = interface
  function Endpoint1: IOmniCommunicationEndpoint;
  function Endpoint2: IOmniCommunicationEndpoint;
end;

function CreateTwoWayChannel(numElements: integer = CDefaultQueueSize;
  taskTerminatedEvent: THandle = 0): IOmniTwoWayChannel;
```

What is sent to Endpoint1 is received on Endpoint2, and vice versa.

### Message Loop Requirement

OTL requires the owner thread to process Windows messages. Automatic in VCL/service apps; explicit handling required elsewhere:

**Console apps:**
```pascal
procedure ProcessMessages;
var Msg: TMsg;
begin
  while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

// In main loop:
while not calc.IsDone do ProcessMessages;
```

[3.09] A thread that is not the main thread now processes the pending messages of its internal monitor each time it creates the next internal monitor, so tasks created by high-level abstractions (or `Unobserved`) started from within another background task are released instead of accumulating (before 3.09 this was steady memory growth). The main thread must still process messages; a console program must pump them itself.

**OTL task starting another OTL task:**
```pascal
FOwnerTask := CreateTask(TWorker.Create(), 'OTL owner')
  .MsgWait  // CRITICAL — enables Windows message processing in this task
  .Run;
```
