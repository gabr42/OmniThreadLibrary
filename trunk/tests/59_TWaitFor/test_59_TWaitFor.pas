unit test_59_TWaitFor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlSync;

type
  TfrmTestTWaitFor = class(TForm)
    btnWaitForAll: TButton;
    lbLog: TListBox;
    btnWaitForAny: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnWaitForAllClick(Sender: TObject);
    procedure btnWaitForAnyClick(Sender: TObject);
  private
    FHandles: array of THandle;
    FWaiter: TWaitFor;
    procedure CheckSignalled(low, high: integer; expected: boolean); overload;
    procedure CheckSignalled(idx: integer; expected: boolean); overload;
    procedure CreateEvents(manualReset: boolean);
    procedure DestroyEvents;
    procedure Log(const msg: string);
    procedure SignalEventAsync(timeout_ms: cardinal; idx: integer);
    procedure VerifyAwaited(const awaited: array of integer);
    procedure WaitForAll(timeout_ms: cardinal; expectedResult: TWaitFor.TWaitResult;
      const msg: string);
    procedure WaitForAny(timeout_ms: cardinal; expectedResult: TWaitFor.TWaitResult;
      const msg: string; checkHandle: integer = -1);
  end;

var
  frmTestTWaitFor: TfrmTestTWaitFor;

implementation

uses
  OtlParallel;

{$R *.dfm}

const
  CNumHandles = 130;

procedure TfrmTestTWaitFor.FormCreate(Sender: TObject);
begin
  Log(Format('Testing with %d handles', [CNumHandles]));
end;

procedure TfrmTestTWaitFor.btnWaitForAllClick(Sender: TObject);
var
  i: integer;
begin
  Log('');

  CreateEvents(true);

  WaitForAll(2000, waTimeout, 'Waiting for all events, should fail in 2 seconds');

  for i := Low(FHandles) to High(FHandles) - 1 do
    SetEvent(FHandles[i]);
  Log('Signalled all events but one');

  WaitForAll(2000, waTimeout, 'Waiting for all events, should still fail in 2 seconds');

  CheckSignalled(Low(FHandles), High(FHandles) - 1, true);
  CheckSignalled(High(FHandles), false);

  Log('Will signal last event in 2 seconds');
  SignalEventAsync(2000, High(FHandles));

  WaitForAll(4000, waAwaited, 'Waiting for all events, should succeed in 2 seconds');

  DestroyEvents;

  Log('All done');
end;

procedure TfrmTestTWaitFor.btnWaitForAnyClick(Sender: TObject);
var
  awaited   : array of integer;
  i         : integer;
  info      : TWaitFor.THandleInfo;
  loopCount : integer;
  maxAwaited: integer;
begin
  Log('');
  Log('Test 1');
  Log('');

  CreateEvents(false);

  WaitForAny(2000, waTimeout, 'Waiting for any event, should fail in 2 seconds');

  CheckSignalled(Low(FHandles), High(FHandles), false);

  Log('Signalling all events, one by one');
  for i := Low(FHandles) to High(FHandles) do begin
    SignalEventAsync(50, i);
    WaitForAny(100, waAwaited, '', i);
    if i > Low(FHandles) then
      WaitForAny(0, waTimeout, '');
  end;

  CheckSignalled(Low(FHandles), High(FHandles), false);

  WaitForAny(2000, waTimeout, 'Waiting for any event, should fail in 2 seconds');

  DestroyEvents;

  Log('');
  Log('Test 2');
  Log('');

  CreateEvents(false);

  Log('Signalling events in background, waiting in foreground');
  Parallel.Async(
    procedure
    var
      i: integer;
    begin
      for i := Low(FHandles) to High(FHandles) do begin
        SetEvent(FHandles[i]);
        Sleep(3);
      end;
    end);

  SetLength(awaited, Length(FHandles));
  FillChar(awaited[0], Length(awaited) * SizeOf(awaited[0]), 0);
  loopCount := 0;
  maxAwaited := 0;
  repeat
    case FWaiter.WaitAny(1000) of
      waAwaited:
        begin
          if Length(FWaiter.Signalled) > maxAwaited then
            maxAwaited := Length(FWaiter.Signalled);
          for info in FWaiter.Signalled do
            Inc(awaited[info.Index]);
        end;
      waTimeout:
        break; //repeat
      else raise Exception.Create('Unexpected result returned from WaitAny');
    end;
    Inc(loopCount);
    Sleep(5);
  until false;
  Log(Format('  Waiting %d times, max %d returned handles', [loopCount, maxAwaited]));
  VerifyAwaited(awaited);

  DestroyEvents;

  Log('All done');
end;

procedure TfrmTestTWaitFor.CheckSignalled(low, high: integer; expected: boolean);
var
  i  : integer;
  res: cardinal;
begin
  Log(Format('Checking events %d-%d', [low, high]));
  for i := low to high do begin
    res := WaitForSingleObject(FHandles[i], 0);
    if expected then begin
      if res <> WAIT_OBJECT_0 then
        raise Exception.CreateFmt('Event %d is not signalled', [i]);
    end
    else if res <> WAIT_TIMEOUT then
      raise Exception.CreateFmt('Event %d is signalled', [i]);
  end;
  Log('  OK');
end;

procedure TfrmTestTWaitFor.CheckSignalled(idx: integer; expected: boolean);
begin
  CheckSignalled(idx, idx, expected);
end;

procedure TfrmTestTWaitFor.CreateEvents(manualReset: boolean);
var
  i: integer;
begin
  SetLength(FHandles, CNumHandles);
  for i := Low(FHandles) to High(FHandles) do
    FHandles[i] := CreateEvent(nil, manualReset, false, nil);
  FWaiter := TWaitFor.Create(FHandles);
end;

procedure TfrmTestTWaitFor.DestroyEvents;
var
  i: integer;
begin
  FreeAndNil(FWaiter);
  for i := Low(FHandles) to High(FHandles) do
    if FHandles[i] <> 0 then
      Win32Check(CloseHandle(FHandles[i]));
end;

procedure TfrmTestTWaitFor.Log(const msg: string);
begin
  if (msg = '') and (lbLog.Items.Count > 0) and (lbLog.Items[lbLog.Items.Count - 1] = '') then
    Exit;

  lbLog.ItemIndex := lbLog.Items.Add(msg);
  lbLog.Update;
end;

procedure TfrmTestTWaitFor.SignalEventAsync(timeout_ms: cardinal; idx: integer);
begin
  Parallel.Async(
    procedure begin
      Sleep(timeout_ms);
      SetEvent(FHandles[idx]);
    end);
end;

procedure TfrmTestTWaitFor.VerifyAwaited(const awaited: array of integer);
var
  i: integer;
begin
  Log('Verifying signalled handles');
  for i := Low(awaited) to High(awaited) do
    if awaited[i] <> 1 then
      raise Exception.CreateFmt('Handle %d was signalled %d times', [i, awaited[i]]);
  Log('  OK');
end;

procedure TfrmTestTWaitFor.WaitForAll(timeout_ms: cardinal;
  expectedResult: TWaitFor.TWaitResult; const msg: string);
begin
  Log(msg);
  if FWaiter.WaitAll(timeout_ms) = expectedResult then
    Log('  OK')
  else
    raise Exception.Create('WaitAll returned unexpected result');
end;

procedure TfrmTestTWaitFor.WaitForAny(timeout_ms: cardinal;
  expectedResult: TWaitFor.TWaitResult; const msg: string; checkHandle: integer);
begin
  if msg <> '' then
    Log(msg);
  if FWaiter.WaitAny(timeout_ms) = expectedResult then begin
    if (checkHandle >= 0) and
       ((Length(FWaiter.Signalled) <> 1) or
        (FWaiter.Signalled[0].Index <> checkHandle))
    then
      raise Exception.Create('WaitAny returned unexpected handle number');
    if msg <> '' then
      Log('  OK');
  end
  else
    raise Exception.Create('WaitAny returned unexpected result');
end;

end.
