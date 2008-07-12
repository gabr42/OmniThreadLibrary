unit OtlCommBufferTest;

interface

uses OtlComm;

implementation

uses SysUtils;

procedure RingBufferTest;
var
  i  : integer;
  msg: TOmniMessage;
  rb : TOmniRingBuffer;
begin
  rb := TOmniRingBuffer.Create(100);
  try
    if not rb.IsEmpty then
      raise Exception.Create('Buffer is not empty when created');
    for i := 1 to 100 do begin
      msg.MsgID := i;
      msg.MsgData := -i;
      if not rb.Enqueue(msg) then
        raise Exception.CreateFmt('Enqueue failed on element %d', [msg.MsgID]);
    end;
    for i := 1 to 100 do begin
      if rb.IsEmpty then
        raise Exception.CreateFmt('Buffer is empty on element %d', [i]);
      msg := rb.Dequeue;
      if (msg.MsgID <> i) or (msg.MsgData <> -i) then
        raise Exception.CreateFmt('Retrieved (%d, %d), expected (%d, %d)',
          [msg.MsgID, integer(msg.MsgData), i, -i]);
    end;
    if not rb.IsEmpty then
      raise Exception.Create('Buffer is not empty at the end');
  finally FreeAndNil(rb); end;
end;

initialization
  RingBufferTest;
end.
