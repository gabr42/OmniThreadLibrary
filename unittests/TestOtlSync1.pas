unit TestOtlSync1;

{$I OtlOptions.Inc}

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils, SyncObjs,
  Classes,
  {$IFDEF OTL_MobileSupport}Threading,{$ENDIF}
  OtlContainerObserver, OtlCollections, OtlCommon, OtlSync, OtlSync.Utils, OtlTask;

type
  ISingleton = IInterface;

  TSingleton = class(TInterfacedObject, ISingleton)
  strict private class var
    FNumSingletons: TOmniAlignedInt32;
  strict protected
    class function GetNumSingletons: integer; static;
  public
    constructor Create;
    destructor Destroy; override;
    class property NumSingletons: integer read GetNumSingletons;
  end;

  {$IFDEF OTL_MobileSupport}
  TestIEvent = class(TTestCase)
  published
    procedure TestManualReset;
    procedure TestAutoReset;
    procedure TestInitialState;
  end;
  {$ENDIF}

  TestCancellationToken = class(TTestCase)
  published
    procedure TestCreateAndSignal;
    procedure TestClear;
    procedure TestEventProperty;
  end;

  TestCountdownEvent = class(TTestCase)
  published
    procedure TestCountdown;
    procedure TestReset;
  end;

  TestLockedT = class(TTestCase)
  published
    procedure TestCreateAndValue;
    procedure TestImplicitConversion;
    procedure TestInitializeWithFactory;
    procedure TestIsInitialized;
    {$IFDEF OTL_HasLightweightMREW}
    procedure TestMREWAccess;
    {$ENDIF}
    procedure TestLockedCallback;
    procedure TestFree;
  end;

  {$IFDEF OTL_HasLightweightMREW}
  TestLightweightMREWEx = class(TTestCase)
  published
    procedure TestNestedWrite;
    {$IFDEF OTL_MobileSupport}
    procedure TestReadBlockedByWrite;
    {$ENDIF}
  end;
  {$ENDIF}

  TestLockManager = class(TTestCase)
  published
    procedure TestLockUnlockByKey;
    procedure TestLockUnlockAutoRelease;
    procedure TestLockTimeoutFailure;
    procedure TestMultipleKeysIndependent;
  end;

  TestSingleThreadUseChecker = class(TTestCase)
  published
    procedure TestSameThreadOK;
    procedure TestDifferentThreadRaises;
  end;

  // Test methods for basic synchronisation stuff
  TestOtlSync = class(TTestCase)
  strict private
    FUnalignedLock: packed record
      FFiller1   : byte;
      FSharedLock: TOmniCS;
      FFiller2   : word;
      FFiller3   : byte;
    end;
    FResourceCount: IOmniResourceCount;
    FSharedValue: int64;
    FSync: TOmniSynchronizer;
    FSystemMutex: TMutex;
  {$IFDEF OTL_Generics}
    FSingleton: TSingleton;
    FSingletonIntf: ISingleton;
  {$ENDIF OTL_Generics}
  strict protected
  {$IFDEF OTL_Generics}
    procedure Asy_AtomicInitIntf(const task: IOmniTask);
    procedure Asy_AtomicInit(const task: IOmniTask);
  {$ENDIF OTL_Generics}
    procedure Asy_LockCS(const task: IOmniTask);
    procedure Asy_ResourceCount(const task: IOmniTask);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCSInitialization;
    procedure TestCSParallel;
    procedure TestCSLock;
    procedure TestResourceCountBasic;
  {$IFDEF OTL_Generics}
    procedure TestOptimisticInitialization;
    procedure TestOptimisticInitializationIntf;
  {$ENDIF OTL_Generics}
    procedure TestMREWRead;
    procedure TestMREWReadInitalBlock;
    procedure TestMREWReadTimeout;
    procedure TestMREWReadTimeoutFail;
    procedure TestMREWWrite;
    procedure TestMREWWriteInitialBlock;
    procedure TestMREWWriteTimeout;
    procedure TestMREWWriteTimeoutFailR;
    procedure TestMREWWriteTimeoutFailW;
  end;

implementation

uses
  OtlTaskControl;

procedure TestOtlSync.TestCSInitialization;
var
  cs: TOmniCS;
  i: integer;

  procedure AcquireRelease;
  var
    cs: TOmniCS;
  begin
    cs.Acquire;
    cs.Release;
  end;

begin
  cs.Initialize;
  cs.Acquire;
  cs.Release;
  for i := 1 to 1000 do
    AcquireRelease;
  CheckTrue(true, 'ok');
end;

procedure Asy_InitializeCS(const task: IOmniTask);
var
  i: Integer;

  procedure AcquireRelease;
  var
    cs: TOmniCS;
  begin
    cs.Acquire;
    cs.Release;
  end;

begin
  for i := 1 to 1000 do
    AcquireRelease;
end;

procedure TestOtlSync.TestCSParallel;
var
  i: Integer;
  task: array [1..8] of IOmniTaskControl;
begin
  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_InitializeCS, 'Initialize CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckTrue(true, 'ok');
end;

procedure TestOtlSync.TestMREWRead;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  readers: array of IOmniTaskControl;
  time   : int64;
begin
  // Tests whether multiple readers can quire the lock at the same time

  count.Value := 0;

  SetLength(readers, 5);
  for i := Low(readers) to High(readers) do
    readers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        mrew.EnterReadLock;
        Sleep(500);
        mrew.ExitReadLock;
        if count.Increment = Length(readers) then
          FSync.Signal('done');
      end,
      Format('TestMREWRead/Reader #%d', [i])).Run;

  time := DSiTimeGetTime64;
  CheckTrue(FSync.WaitFor('done', 1000), 'Reader lock failed');
  time := DSiTimeGetTime64 - time;

  CheckTrue(time < 1000, 'Readers did not execute in parallel');
end;

procedure TestOtlSync.TestMREWReadTimeout;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  readers: array of IOmniTaskControl;
  time   : int64;
begin
  // Tests whether multiple readers can quire the lock at the same time

  count.Value := 0;

  SetLength(readers, 5);
  for i := Low(readers) to High(readers) do
    readers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        if not mrew.TryEnterReadLock(100) then
          Exit;
        Sleep(500);
        mrew.ExitReadLock;
        if count.Increment = Length(readers) then
          FSync.Signal('done');
      end,
      Format('TestMREWReadTimeout/Reader #%d', [i])).Run;

  time := DSiTimeGetTime64;
  CheckTrue(FSync.WaitFor('done', 1000), 'Reader lock failed');
  time := DSiTimeGetTime64 - time;

  CheckTrue(time < 1000, 'Readers did not execute in parallel');
end;

procedure TestOtlSync.TestMREWReadInitalBlock;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  readers: array of IOmniTaskControl;
begin
  // Tests whether a reader will acquire a lock if it is initially blocked

  count.Value := 0;

  mrew.EnterWriteLock;

  SetLength(readers, 5);
  for i := Low(readers) to High(readers) do
    readers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        if count.Increment = Length(readers) then
          FSync.Signal('go')
        else
          FSync.WaitFor('go');
        try
          if not mrew.TryEnterReadLock(2000) then begin
            FSync.Signal('fault');
            Exit;
          end;
          mrew.ExitReadLock;
        finally
          if count.Decrement = 0 then
            FSync.Signal('done');
        end;
      end,
      Format('TestMREWReadInitialBlock/Reader #%d', [i])).Run;

  FSync.WaitFor('go');
  Sleep(500);
  mrew.ExitWriteLock;

  CheckTrue(FSync.WaitFor('done', 1000), 'Reader lock failed');
  CheckFalse(FSync.WaitFor('fault', 0), 'At least one reader failed to acquire the lock');
end;

procedure TestOtlSync.TestMREWReadTimeoutFail;
const
  CTImeout = 100;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  readers: array of IOmniTaskControl;
  times  : array of int64;

  function MakeTask(idx: integer): TOmniTaskDelegate;
  begin
    Result :=
      procedure (const task: IOmniTask)
      var
        time: int64;
      begin
        FSync.WaitFor('go');
        try
          time := DSiTimeGetTime64;
          if not mrew.TryEnterReadLock(CTimeout) then begin
            times[idx] := DSiTimeGetTime64 - time;
            Exit;
          end;

          times[idx] := -1;
          mrew.ExitReadLock;
        finally
          if count.Increment = Length(readers) then
            FSync.Signal('done');
        end;
      end;
  end;

begin
  // Tests whether MREW read timeout fails when a writer is acquired and whether both kind of locks can be acquired after that

  count.Value := 0;

  SetLength(times, 5);
  SetLength(readers, 5);
  for i := Low(readers) to High(readers) do
    readers[i] := CreateTask(MakeTask(i), Format('TestMREWReadTimeoutFail/Reader #%d', [i])).Run;

  mrew.EnterWriteLock;
  try
    FSync.Signal('go');
    CheckTrue(FSync.WaitFor('done', CTimeout * 10), 'Reader lock failed');
  finally mrew.ExitWriteLock; end;

  for i := Low(readers) to High(readers) do
    CheckTrue((times[i] > (CTimeout * 0.8)) and (times[i] < (CTimeout * 3)),
      Format('Reader #%d waited %d ms instead of %d ms', [i, times[i], CTimeout]));

  if not mrew.TryEnterReadLock(0) then
    Fail('Failed to acquire read lock after timeouts')
  else
    mrew.ExitReadLock;
  if not mrew.TryEnterWriteLock(0) then
    Fail('Failed to acquire write lock after timeouts')
  else
    mrew.ExitWriteLock;
end;

procedure TestOtlSync.TestMREWWrite;
var
  count  : TOmniAlignedInt32;
  hwm    : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  writers: array of IOmniTaskControl;
begin
  // Tests whether multiple writers cannot quire the lock at the same time

  count.Value := 0;
  hwm.Value := 0;

  SetLength(writers, 5);
  for i := Low(writers) to High(writers) do
    writers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        mrew.EnterWriteLock;
        if hwm.Increment > 1 then
          FSync.Signal('overflow');
        Sleep(500);
        hwm.Decrement;
        mrew.ExitWriteLock;
        if count.Increment = Length(writers) then
          FSync.Signal('done');
      end,
      Format('TestMREWWrite/Writer #%d', [i])).Run;

  CheckTrue(FSync.WaitFor('done', Length(writers) * 1000), 'Writer lock failed');
  CheckFalse(FSync.WaitFor('overflow', 0), 'More than one writer executed in parallel');
end;

procedure TestOtlSync.TestMREWWriteInitialBlock;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  writers: array of IOmniTaskControl;
begin
  // Tests whether a writer will acquire a lock if it is initially blocked

  count.Value := 0;

  mrew.EnterReadLock;

  SetLength(writers, 5);
  for i := Low(writers) to High(writers) do
    writers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        if count.Increment = Length(writers) then
          FSync.Signal('go')
        else
          FSync.WaitFor('go');
        try
          if not mrew.TryEnterWriteLock(2000) then begin
            FSync.Signal('fault');
            Exit;
          end;
          mrew.ExitWriteLock;
        finally
          if count.Decrement = 0 then
            FSync.Signal('done');
        end;
      end,
      Format('TestMREWWriteInitialBlock/Writer #%d', [i])).Run;

  FSync.WaitFor('go');
  Sleep(500);
  mrew.ExitReadLock;

  CheckTrue(FSync.WaitFor('done', 1000), 'Writer lock failed');
  CheckFalse(FSync.WaitFor('fault', 0), 'At least one writer failed to acquire the lock');
end;

procedure TestOtlSync.TestMREWWriteTimeout;
var
  count  : TOmniAlignedInt32;
  hwm    : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  writers: array of IOmniTaskControl;
begin
  // Tests whether multiple writers cannot quire the lock at the same time

  count.Value := 0;
  hwm.Value := 0;

  SetLength(writers, 5);
  for i := Low(writers) to High(writers) do
    writers[i] := CreateTask(
      procedure (const task: IOmniTask)
      begin
        if not mrew.TryEnterWriteLock(Length(writers) * 1000) then begin
          FSync.Signal('failed');
          Exit;
        end;
        if hwm.Increment > 1 then
          FSync.Signal('overflow');
        Sleep(500);
        hwm.Decrement;
        mrew.ExitWriteLock;
        if count.Increment = Length(writers) then
          FSync.Signal('done');
      end,
      Format('TestMREWWriteTimeout/Writer #%d', [i])).Run;

  CheckTrue(FSync.WaitFor('done', Length(writers) * 1000), 'Writer lock failed');
  CheckFalse(FSync.WaitFor('failed', 0), 'At least one writer failed to acquire lock');
  CheckFalse(FSync.WaitFor('overflow', 0), 'More than one writer executed in parallel');
end;

procedure TestOtlSync.TestMREWWriteTimeoutFailR;
const
  CTImeout = 100;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  writers: array of IOmniTaskControl;
  times  : array of int64;

  function MakeTask(idx: integer): TOmniTaskDelegate;
  begin
    Result :=
      procedure (const task: IOmniTask)
      var
        time: int64;
      begin
        FSync.WaitFor('go');
        try
          time := DSiTimeGetTime64;
          if not mrew.TryEnterWriteLock(CTimeout) then begin
            times[idx] := DSiTimeGetTime64 - time;
            Exit;
          end;

          times[idx] := -1;
          mrew.ExitWriteLock;
        finally
          if count.Increment = Length(writers) then
            FSync.Signal('done');
        end;
      end;
  end;

begin
  // Tests whether MREW write timeout fails when a reader is acquired and whether both kind of locks can be acquired after that

  count.Value := 0;

  SetLength(times, 5);
  SetLength(writers, 5);
  for i := Low(writers) to High(writers) do
    writers[i] := CreateTask(MakeTask(i), Format('TestMREWWriteTimeoutFail/Writer #%d', [i])).Run;

  mrew.EnterReadLock;
  try
    FSync.Signal('go');
    CheckTrue(FSync.WaitFor('done', CTimeout * 10), 'Writer lock failed');
  finally mrew.ExitReadLock; end;

  for i := Low(writers) to High(writers) do
    CheckTrue((times[i] > (CTimeout * 0.8)) and (times[i] < (CTimeout * 3)),
      Format('Writer #%d waited %d ms instead of %d ms', [i, times[i], CTimeout]));

  if not mrew.TryEnterReadLock(0) then
    Fail('Failed to acquire read lock after timeouts')
  else
    mrew.ExitReadLock;
  if not mrew.TryEnterWriteLock(0) then
    Fail('Failed to acquire write lock after timeouts')
  else
    mrew.ExitWriteLock;
end;

procedure TestOtlSync.TestMREWWriteTimeoutFailW;
const
  CTImeout = 100;
var
  count  : TOmniAlignedInt32;
  i      : integer;
  mrew   : TOmniMREW;
  writers: array of IOmniTaskControl;
  times  : array of int64;

  function MakeTask(idx: integer): TOmniTaskDelegate;
  begin
    Result :=
      procedure (const task: IOmniTask)
      var
        time: int64;
      begin
        FSync.WaitFor('go');
        try
          time := DSiTimeGetTime64;
          if not mrew.TryEnterWriteLock(CTimeout) then begin
            times[idx] := DSiTimeGetTime64 - time;
            Exit;
          end;

          times[idx] := -1;
          mrew.ExitWriteLock;
        finally
          if count.Increment = Length(writers) then
            FSync.Signal('done');
        end;
      end;
  end;

begin
  // Tests whether MREW write timeout fails when a writer is acquired and whether both kind of locks can be acquired after that

  count.Value := 0;

  SetLength(times, 5);
  SetLength(writers, 5);
  for i := Low(writers) to High(writers) do
    writers[i] := CreateTask(MakeTask(i), Format('TestMREWWriteTimeoutFail/Writer #%d', [i])).Run;

  mrew.EnterWriteLock;
  try
    FSync.Signal('go');
    CheckTrue(FSync.WaitFor('done', CTimeout * 10), 'Writer lock failed');
  finally mrew.ExitWriteLock; end;

  for i := Low(writers) to High(writers) do
    CheckTrue((times[i] > (CTimeout * 0.8)) and (times[i] < (CTimeout * 3)),
      Format('Writer #%d waited %d ms instead of %d ms', [i, times[i], CTimeout]));

  if not mrew.TryEnterReadLock(0) then
    Fail('Failed to acquire read lock after timeouts')
  else
    mrew.ExitReadLock;
  if not mrew.TryEnterWriteLock(0) then
    Fail('Failed to acquire write lock after timeouts')
  else
    mrew.ExitWriteLock;
end;

procedure TestOtlSync.Asy_LockCS(const task: IOmniTask);
var
  i: Integer;
begin
  for i := 1 to 10000 do begin
    FUnalignedLock.FSharedLock.Acquire;
    Inc(FSharedValue);
    FUnalignedLock.FSharedLock.Release;
    FUnalignedLock.FSharedLock.Acquire;
    Dec(FSharedValue);
    FUnalignedLock.FSharedLock.Release;
  end;
end;

procedure TestOtlSync.TestCSLock;
var
  i: Integer;
  task: array [1..8] of IOmniTaskControl;
begin
  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_LockCS, 'Lock CS #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckEquals(int64(0), FSharedValue);
end;

{$IFDEF OTL_Generics}
procedure TestOtlSync.Asy_AtomicInit(const task: IOmniTask);
begin
  WaitForSingleObject(Task.CancellationToken.Handle, INFINITE);
  Atomic<TSingleton>.Initialize(FSingleton,
    function: TSingleton begin Result := TSingleton.Create; end);
end;

procedure TestOtlSync.TestOptimisticInitialization;
var
  i      : integer;
  iRepeat: integer;
  task   : array [1..8] of IOmniTaskControl;
  token  : IOmniCancellationToken;
begin
  for iRepeat := 1 to 100 do begin
    FreeAndNil(FSingleton);

    token := CreateOmniCancellationToken;
    for i := Low(task) to High(task) do
      task[i] := CreateTask(Asy_AtomicInit, 'AtomicInit #' + IntToStr(i)).CancelWith(token).Run;

    token.Signal;

    for i := Low(task) to High(task) do
      task[i].Terminate;

    CheckTrue(assigned(FSingleton), 'There is no singleton');
  end;
  CheckEquals(1, TSingleton.NumSingletons);
  FreeAndNil(FSingleton);
end;

procedure TestOtlSync.Asy_AtomicInitIntf(const task: IOmniTask);
begin
  WaitForSingleObject(Task.CancellationToken.Handle, INFINITE);
  Atomic<ISingleton>.Initialize(FSingletonIntf,
    function: ISingleton begin Result := TSingleton.Create; end);
end;

procedure TestOtlSync.TestOptimisticInitializationIntf;
var
  i      : integer;
  iRepeat: integer;
  task   : array [1..8] of IOmniTaskControl;
  token  : IOmniCancellationToken;
begin
  for iRepeat := 1 to 100 do begin
    FSingletonIntf := nil;

    token := CreateOmniCancellationToken;
    for i := Low(task) to High(task) do
      task[i] := CreateTask(Asy_AtomicInitIntf, 'AtomicInitIntf #' + IntToStr(i)).CancelWith(token).Run;

    token.Signal;

    for i := Low(task) to High(task) do
      task[i].Terminate;

    CheckTrue(assigned(FSingletonIntf), 'There is no singleton');
  end;
  CheckEquals(1, TSingleton.NumSingletons);
  FSingletonIntf := nil;
end;
{$ENDIF OTL_Generics}

procedure TestOtlSync.Asy_ResourceCount(const task: IOmniTask);
begin
  FResourceCount.Allocate;
  FResourceCount.Release;
end;

procedure TestOtlSync.SetUp;
begin
  FSync := TOmniSynchronizer.Create;
  FSystemMutex := TMutex.Create(nil, false, '/OmniThreadLibrary/TestOtlSync/A4EDD8C0-88D0-46A9-890B-8EAEF466C44A');
  FSystemMutex.Acquire
end;

procedure TestOtlSync.TearDown;
begin
  FSystemMutex.Release;
  FreeAndNil(FSystemMutex);
  FreeAndNil(FSync);
end;

procedure TestOtlSync.TestResourceCountBasic;
var
  i   : integer;
  task: array [1..8] of IOmniTaskControl;
begin
  FResourceCount := CreateResourceCount(4);

  for i := Low(task) to High(task) do
    task[i] := CreateTask(Asy_ResourceCount, 'ResourceCount #' + IntToStr(i));

  for i := Low(task) to High(task) do
    task[i].Run;

  for i := Low(task) to High(task) do
    task[i].Terminate;

  CheckEquals(3, FResourceCount.Allocate);
end;

constructor TSingleton.Create;
begin
  inherited Create;
  FNumSingletons.Increment;
end;

destructor TSingleton.Destroy;
begin
  FNumSingletons.Decrement;
  inherited;
end;

class function TSingleton.GetNumSingletons: integer;
begin
  Result := FNumSingletons;
end;

{ TestIEvent }

{$IFDEF OTL_MobileSupport}
procedure TestIEvent.TestAutoReset;
var
  event: IOmniEvent;
begin
  event := CreateOmniEvent(false, false);
  CheckTrue(wrTimeout = event.WaitFor(0));
  CheckTrue(wrTimeout = event.WaitFor(100));
  event.SetEvent;
  CheckTrue(wrSignaled = event.WaitFor(0));
  CheckTrue(wrTimeout = event.WaitFor(0));
  event.SetEvent;
  event.Reset;
  CheckTrue(wrTimeout = event.WaitFor(0));
end;

procedure TestIEvent.TestInitialState;
var
  event: IOmniEvent;
begin
  event := CreateOmniEvent(false, true);
  CheckTrue(wrSignaled = event.WaitFor(0));
end;

procedure TestIEvent.TestManualReset;
var
  event: IOmniEvent;
begin
  event := CreateOmniEvent(true, false);
  CheckTrue(wrTimeout = event.WaitFor(0));
  CheckTrue(wrTimeout = event.WaitFor(100));
  event.SetEvent;
  CheckTrue(wrSignaled = event.WaitFor(0));
  CheckTrue(wrSignaled = event.WaitFor(100));
  event.Reset;
  CheckTrue(wrTimeout = event.WaitFor(0));
end;
{$ENDIF}

{ TestCancellationToken }

procedure TestCancellationToken.TestCreateAndSignal;
var
  ct: IOmniCancellationToken;
begin
  ct := CreateOmniCancellationToken;
  CheckFalse(ct.IsSignalled, 'initially not signalled');
  ct.Signal;
  CheckTrue(ct.IsSignalled, 'signalled after Signal');
end;

procedure TestCancellationToken.TestClear;
var
  ct: IOmniCancellationToken;
begin
  ct := CreateOmniCancellationToken;
  ct.Signal;
  CheckTrue(ct.IsSignalled, 'signalled');
  ct.Clear;
  CheckFalse(ct.IsSignalled, 'cleared');
  ct.Signal;
  CheckTrue(ct.IsSignalled, 're-signalled after clear');
end;

procedure TestCancellationToken.TestEventProperty;
var
  ct: IOmniCancellationToken;
begin
  ct := CreateOmniCancellationToken;
  CheckFalse(ct.IsSignalled, 'not signalled initially');
  ct.Signal;
  CheckTrue(ct.IsSignalled, 'signalled after Signal');
  CheckTrue(WaitForSingleObject(ct.Handle, 0) = WAIT_OBJECT_0, 'handle set after signal');
  ct.Clear;
  CheckTrue(WaitForSingleObject(ct.Handle, 0) = WAIT_TIMEOUT, 'handle cleared');
end;

{ TestCountdownEvent }

procedure TestCountdownEvent.TestCountdown;
var
  cde: IOmniCountdownEvent;
begin
  cde := CreateOmniCountdownEvent(3, 0);
  CheckTrue(wrTimeout = cde.WaitFor(0), 'not signalled at count=3');
  cde.BaseCountdown.Signal;
  CheckTrue(wrTimeout = cde.WaitFor(0), 'not signalled at count=2');
  cde.BaseCountdown.Signal;
  CheckTrue(wrTimeout = cde.WaitFor(0), 'not signalled at count=1');
  cde.BaseCountdown.Signal;
  CheckTrue(cde.IsSignalled, 'signalled at count=0');
end;

procedure TestCountdownEvent.TestReset;
var
  cde: IOmniCountdownEvent;
begin
  cde := CreateOmniCountdownEvent(1, 0);
  cde.BaseCountdown.Signal;
  CheckTrue(cde.IsSignalled, 'signalled');
  cde.Reset;
  CheckFalse(cde.IsSignalled, 'not signalled after reset');
  cde.BaseCountdown.Signal;
  CheckTrue(cde.IsSignalled, 'signalled again');
end;

{ TestLockedT }

procedure TestLockedT.TestCreateAndValue;
var
  li: Locked<integer>;
begin
  li := Locked<integer>.Create(42);
  CheckEquals(42, li.Value);
end;

procedure TestLockedT.TestImplicitConversion;
var
  li: Locked<integer>;
  v: integer;
begin
  li := Locked<integer>.Create(17);
  v := li;
  CheckEquals(17, v);
end;

procedure TestLockedT.TestInitializeWithFactory;
var
  factory: Locked<integer>.TFactory;
  li     : Locked<integer>;
  v      : integer;
begin
  FillChar(li, SizeOf(li), 0);
  factory := function: integer begin Result := 99; end;
  v := li.Initialize(factory);
  CheckEquals(99, v);
  CheckEquals(99, li.Value);
  // Second call returns same value without calling factory again
  factory := function: integer begin Result := 200; end;
  v := li.Initialize(factory);
  CheckEquals(99, v, 'factory not called on second Initialize');
end;

procedure TestLockedT.TestIsInitialized;
var
  li: Locked<integer>;
begin
  FillChar(li, SizeOf(li), 0);
  CheckFalse(li.IsInitialized, 'not initialized initially');
  li := Locked<integer>.Create(1);
  CheckTrue(li.IsInitialized, 'initialized after Create');
end;

{$IFDEF OTL_HasLightweightMREW}
procedure TestLockedT.TestMREWAccess;
var
  li: Locked<integer>;
  v: integer;
begin
  li := Locked<integer>.Create(10);
  v := li.BeginRead;
  CheckEquals(10, v);
  li.EndRead;
  v := li.BeginWrite;
  CheckEquals(10, v);
  li.EndWrite;
end;
{$ENDIF}

procedure TestLockedT.TestLockedCallback;
var
  li  : Locked<integer>;
  proc: Locked<integer>.TProcT;
  sum : integer;
begin
  li := Locked<integer>.Create(5);
  sum := 0;
  proc := procedure(const value: integer) begin sum := value + 10; end;
  li.Locked(proc);
  CheckEquals(15, sum);
end;

procedure TestLockedT.TestFree;
var
  li: Locked<TStringList>;
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('test');
  li := Locked<TStringList>.Create(sl, true);
  CheckEquals(1, li.Value.Count);
  li.Free;
  // After Free, value should be nil
  CheckTrue(li.Value = nil, 'value nil after Free');
end;

{ TestLightweightMREWEx }

{$IFDEF OTL_HasLightweightMREW}
procedure TestLightweightMREWEx.TestNestedWrite;
var
  mrew: TLightweightMREWEx;
begin
  mrew.BeginWrite;
  // Nested write from same thread should succeed
  mrew.BeginWrite;
  mrew.EndWrite;
  mrew.EndWrite;
  CheckTrue(true, 'nested write succeeded');
end;

{$IFDEF OTL_MobileSupport}
procedure TestLightweightMREWEx.TestReadBlockedByWrite;
var
  mrew   : ILightweightMREWEx;
  synch  : IOmniSynchronizer;
  blocked: TOmniAlignedInt32;
  proc   : TProc;
begin
  mrew := TLightweightMREWExImpl.Create;
  synch := TOmniSynchronizer.Create;
  blocked.Value := 0;

  mrew.BeginWrite;
  proc :=
    procedure
    begin
      synch.Signal('started');
      blocked.Value := 1;
      mrew.BeginRead;
      blocked.Value := 2;
      mrew.EndRead;
    end;
  System.Threading.TTask.Run(proc);

  synch.WaitFor('started');
  Sleep(200);
  CheckEquals(1, blocked.Value, 'reader is blocked');
  mrew.EndWrite;
  Sleep(200);
  CheckEquals(2, blocked.Value, 'reader unblocked after EndWrite');
end;
{$ENDIF OTL_MobileSupport}
{$ENDIF}

{ TestLockManager }

procedure TestLockManager.TestLockUnlockByKey;
var
  lm: IOmniLockManager<string>;
begin
  lm := TOmniLockManager<string>.CreateInterface;
  CheckTrue(lm.Lock('key1', 0), 'lock key1');
  lm.Unlock('key1');
  CheckTrue(lm.Lock('key1', 0), 're-lock key1 after unlock');
  lm.Unlock('key1');
end;

procedure TestLockManager.TestLockUnlockAutoRelease;
var
  lm        : IOmniLockManager<string>;
  autoUnlock: IOmniLockManagerAutoUnlock;
begin
  lm := TOmniLockManager<string>.CreateInterface;
  begin
    autoUnlock := lm.LockUnlock('key1', 1000);
    Check(autoUnlock <> nil, 'auto-unlock acquired');
  end;
  // After autoUnlock goes out of scope, lock should be released
  CheckTrue(lm.Lock('key1', 0), 'lock available after auto-unlock');
  lm.Unlock('key1');
end;

procedure TestLockManager.TestLockTimeoutFailure;
var
  lm   : IOmniLockManager<string>;
  synch: IOmniSynchronizer;
  proc : TProc;
begin
  lm := TOmniLockManager<string>.CreateInterface;
  synch := TOmniSynchronizer.Create;

  lm.Lock('key1', 0);

  proc :=
    procedure
    begin
      if not lm.Lock('key1', 100) then
        synch.Signal('done');
    end;
  System.Threading.TTask.Run(proc);

  synch.WaitFor('done');
  lm.Unlock('key1');
end;

procedure TestLockManager.TestMultipleKeysIndependent;
var
  lm: IOmniLockManager<string>;
begin
  lm := TOmniLockManager<string>.CreateInterface;
  CheckTrue(lm.Lock('a', 0), 'lock a');
  CheckTrue(lm.Lock('b', 0), 'lock b while a locked');
  lm.Unlock('a');
  lm.Unlock('b');
end;

{ TestSingleThreadUseChecker }

procedure TestSingleThreadUseChecker.TestSameThreadOK;
var
  checker: TOmniSingleThreadUseChecker;
begin
  checker.AttachToCurrentThread;
  checker.Check;
  CheckTrue(true, 'Check from same thread OK');
end;

procedure TestSingleThreadUseChecker.TestDifferentThreadRaises;
var
  checker: TOmniSingleThreadUseChecker;
  synch  : IOmniSynchronizer;
  raised : TOmniAlignedInt32;
  proc   : TProc;
begin
  synch := TOmniSynchronizer.Create;
  raised.Value := 0;
  checker.AttachToCurrentThread;

  proc :=
    procedure
    begin
      try
        checker.Check;
      except
        raised.Value := 1;
      end;
      synch.Signal('done');
    end;
  System.Threading.TTask.Run(proc);

  synch.WaitFor('done');
  CheckEquals(1, raised.Value, 'Check from different thread raised exception');
end;

initialization
  {$IFDEF OTL_MobileSupport}
  RegisterTest(TestIEvent.Suite);
  {$ENDIF}
  RegisterTest(TestCancellationToken.Suite);
  RegisterTest(TestCountdownEvent.Suite);
  RegisterTest(TestLockedT.Suite);
  {$IFDEF OTL_HasLightweightMREW}
  RegisterTest(TestLightweightMREWEx.Suite);
  {$ENDIF}
  RegisterTest(TestLockManager.Suite);
  RegisterTest(TestSingleThreadUseChecker.Suite);
  RegisterTest(TestOtlSync.Suite);
end.
