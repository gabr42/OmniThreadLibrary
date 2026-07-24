unit TestOtlSync1;

{$I OtlOptions.Inc}

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils, SyncObjs,
  Classes,
  {$IFDEF OTL_HasSystemThreading}Threading,{$ENDIF}
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

  TestCancellationToken = class(TTestCase)
  published
    procedure TestCreateAndSignal;
    procedure TestClear;
    procedure TestEventProperty;
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
    procedure TestEndReadWithoutBeginReadRaises;
    procedure TestUpgradeBeginWriteRaises;
    procedure TestUpgradeTryBeginWriteRaises;
    procedure TestReadInsideWriteRaisesByDefault;
    procedure TestTryReadInsideWriteRaisesByDefault;
    procedure TestAllowReadInsideWriteAfterUseRaises;
    {$IFDEF OTL_HasSystemThreading}
    procedure TestReadBlockedByWrite;
    procedure TestNestedTryWrite;
    procedure TestNestedWriteContention;
    procedure TestEndWriteNotOwnerRaises;
    procedure TestReadInsideWriteGrantedWhenAllowed;
    procedure TestTryReadInsideWriteGrantedWhenAllowed;
    procedure TestNestedReadDepth3;
    procedure TestEndWriteWithNestedReadRaises;
    procedure TestTwoLocksInterleavedRelease;
    procedure TestInnerEndWriteWithNestedReadSucceeds;
    procedure TestRecursiveReadWithPendingWriter;
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

{ TestLockedT }

procedure TestLockedT.TestCreateAndValue;
var
  li: Locked<integer>;
begin
  li := Locked<integer>.Create(42);
  li.Acquire;
  try
    CheckEquals(42, li.Value);
  finally li.Release; end;
end;

procedure TestLockedT.TestImplicitConversion;
var
  li: Locked<integer>;
  v: integer;
begin
  li := Locked<integer>.Create(17);
  li.Acquire;
  try
    v := li;
  finally li.Release; end;
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
  li.Acquire;
  try
    CheckEquals(99, li.Value);
  finally li.Release; end;
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
  li.Acquire;
  try
    CheckEquals(1, li.Value.Count);
  finally li.Release; end;
  li.Free;
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

procedure TestLightweightMREWEx.TestEndReadWithoutBeginReadRaises;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  raised := '<no exception>';
  try
    mrew.EndRead;
  except
    on E: Exception do
      raised := E.Message;
  end;
  CheckTrue(Pos('TLightweightMREWEx.EndRead', raised) > 0,
    'unmatched EndRead raises with class/method context, got: ' + raised);
end;

procedure TestLightweightMREWEx.TestUpgradeBeginWriteRaises;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  // Without upgrade detection this deadlocks (exclusive waits for our own
  // shared) - see TLightweightMREWEx.BeginWrite.
  mrew.BeginRead;
  try
    raised := '<no exception>';
    try
      mrew.BeginWrite;
      mrew.EndWrite;
    except
      on E: Exception do
        raised := E.Message;
    end;
    CheckTrue(Pos('TLightweightMREWEx.BeginWrite', raised) > 0,
      'BeginWrite while holding a read lock raises, got: ' + raised);
  finally mrew.EndRead; end;
end;

procedure TestLightweightMREWEx.TestUpgradeTryBeginWriteRaises;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  mrew.BeginRead;
  try
    raised := '<no exception>';
    try
      if mrew.TryBeginWrite then
        mrew.EndWrite;
    except
      on E: Exception do
        raised := E.Message;
    end;
    CheckTrue(Pos('TLightweightMREWEx.TryBeginWrite', raised) > 0,
      'TryBeginWrite while holding a read lock raises, got: ' + raised);
  finally mrew.EndRead; end;
end;

procedure TestLightweightMREWEx.TestReadInsideWriteRaisesByDefault;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  mrew.BeginWrite;
  try
    raised := '<no exception>';
    try
      mrew.BeginRead;
      mrew.EndRead;
    except
      on E: Exception do
        raised := E.Message;
    end;
    CheckTrue(Pos('TLightweightMREWEx.BeginRead', raised) > 0,
      'BeginRead inside write lock raises by default, got: ' + raised);
  finally mrew.EndWrite; end;
end;

procedure TestLightweightMREWEx.TestTryReadInsideWriteRaisesByDefault;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  mrew.BeginWrite;
  try
    raised := '<no exception>';
    try
      if mrew.TryBeginRead then
        mrew.EndRead;
    except
      on E: Exception do
        raised := E.Message;
    end;
    CheckTrue(Pos('TLightweightMREWEx.TryBeginRead', raised) > 0,
      'TryBeginRead inside write lock raises by default, got: ' + raised);
  finally mrew.EndWrite; end;
end;

procedure TestLightweightMREWEx.TestAllowReadInsideWriteAfterUseRaises;
var
  mrew  : TLightweightMREWEx;
  raised: string;
begin
  mrew.BeginWrite;
  mrew.EndWrite;
  raised := '<no exception>';
  try
    mrew.AllowReadInsideWrite := true;
  except
    on E: Exception do
      raised := E.Message;
  end;
  CheckTrue(Pos('TLightweightMREWEx.SetAllowReadInsideWrite', raised) > 0,
    'setting AllowReadInsideWrite after first use raises, got: ' + raised);
end;

{$IFDEF OTL_HasSystemThreading}
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

procedure TestLightweightMREWEx.TestNestedTryWrite;
var
  entered: TOmniAlignedInt32;
  mrew   : ILightweightMREWEx;
begin
  mrew := TLightweightMREWExImpl.Create;
  entered.Value := 0;

  mrew.BeginWrite;
  CheckTrue(mrew.TryBeginWrite, 'nested TryBeginWrite succeeds for the owner');
  mrew.EndWrite;
  mrew.EndWrite;

  // All nested locks are released - another thread must be able to acquire.
  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrew.TryBeginWrite then begin
          entered.Value := 1;
          mrew.EndWrite;
        end;
      end).Wait(5000),
    'verification task completed');
  CheckEquals(1, entered.Value, 'lock is free after all nested EndWrite calls');
end;

procedure TestLightweightMREWEx.TestNestedWriteContention;
var
  mrew : ILightweightMREWEx;
  state: TOmniAlignedInt32;
  synch: IOmniSynchronizer;
begin
  mrew := TLightweightMREWExImpl.Create;
  synch := TOmniSynchronizer.Create;
  state.Value := 0;

  mrew.BeginWrite;
  mrew.BeginWrite;
  System.Threading.TTask.Run(
    procedure
    begin
      synch.Signal('started');
      state.Value := 1;
      mrew.BeginWrite;
      state.Value := 2;
      mrew.EndWrite;
      synch.Signal('done');
    end);

  synch.WaitFor('started');
  Sleep(200);
  CheckEquals(1, state.Value, 'second writer is blocked');
  mrew.EndWrite; // releases the nested lock, owner still holds the outer one
  Sleep(200);
  CheckEquals(1, state.Value, 'second writer is still blocked after inner EndWrite');
  mrew.EndWrite;
  CheckTrue(synch.WaitFor('done', 5000), 'second writer acquired the lock after outer EndWrite');
  CheckEquals(2, state.Value, 'second writer completed');
end;

procedure TestLightweightMREWEx.TestEndWriteNotOwnerRaises;
var
  mrew  : ILightweightMREWEx;
  raised: string;
begin
  mrew := TLightweightMREWExImpl.Create;
  raised := '<no exception>';

  mrew.BeginWrite;
  try
    CheckTrue(
      System.Threading.TTask.Run(
        procedure
        begin
          try
            mrew.EndWrite;
          except
            on E: Exception do
              raised := E.Message;
          end;
        end).Wait(5000),
      'verification task completed');
  finally mrew.EndWrite; end;

  CheckTrue(Pos('TLightweightMREWEx.EndWrite', raised) > 0,
    'EndWrite from a non-owner thread raises with class/method context, got: ' + raised);
end;

procedure TestLightweightMREWEx.TestReadInsideWriteGrantedWhenAllowed;
var
  entered: TOmniAlignedInt32;
  mrew   : ILightweightMREWEx;
begin
  mrew := TLightweightMREWExImpl.Create;
  mrew.AllowReadInsideWrite := true; // must be set before first use
  entered.Value := 0;

  mrew.BeginWrite;
  mrew.BeginRead; // granted as nested: exclusive access implies read rights
  mrew.EndRead;
  mrew.EndWrite;

  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrew.TryBeginWrite then begin
          entered.Value := 1;
          mrew.EndWrite;
        end;
      end).Wait(5000),
    'verification task completed');
  CheckEquals(1, entered.Value, 'lock fully released after read-under-write');
end;

procedure TestLightweightMREWEx.TestTryReadInsideWriteGrantedWhenAllowed;
var
  entered: TOmniAlignedInt32;
  mrew   : ILightweightMREWEx;
begin
  mrew := TLightweightMREWExImpl.Create;
  mrew.AllowReadInsideWrite := true; // must be set before first use
  entered.Value := 0;

  mrew.BeginWrite;
  CheckTrue(mrew.TryBeginRead, 'TryBeginRead granted under owned write lock');
  mrew.EndRead;
  mrew.EndWrite;

  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrew.TryBeginWrite then begin
          entered.Value := 1;
          mrew.EndWrite;
        end;
      end).Wait(5000),
    'verification task completed');
  CheckEquals(1, entered.Value, 'lock fully released after tryread-under-write');
end;

procedure TestLightweightMREWEx.TestNestedReadDepth3;
var
  mrew : ILightweightMREWEx;
  state: TOmniAlignedInt32;
  synch: IOmniSynchronizer;
begin
  mrew := TLightweightMREWExImpl.Create;
  synch := TOmniSynchronizer.Create;
  state.Value := 0;

  mrew.BeginRead;
  mrew.BeginRead;
  mrew.BeginRead;
  System.Threading.TTask.Run(
    procedure
    begin
      synch.Signal('started');
      state.Value := 1;
      mrew.BeginWrite;
      state.Value := 2;
      mrew.EndWrite;
      synch.Signal('done');
    end);

  synch.WaitFor('started');
  Sleep(200);
  CheckEquals(1, state.Value, 'writer blocked at depth 3');
  mrew.EndRead;
  Sleep(200);
  CheckEquals(1, state.Value, 'writer blocked at depth 2');
  mrew.EndRead;
  Sleep(200);
  CheckEquals(1, state.Value, 'writer blocked at depth 1');
  mrew.EndRead;
  CheckTrue(synch.WaitFor('done', 5000), 'writer acquired after last EndRead');
  CheckEquals(2, state.Value, 'writer completed');
end;

procedure TestLightweightMREWEx.TestEndWriteWithNestedReadRaises;
var
  entered: TOmniAlignedInt32;
  mrew   : ILightweightMREWEx;
  raised : string;
begin
  mrew := TLightweightMREWExImpl.Create;
  mrew.AllowReadInsideWrite := true; // must be set before first use
  entered.Value := 0;

  mrew.BeginWrite;
  mrew.BeginRead; // read-under-write is granted
  raised := '<no exception>';
  try
    mrew.EndWrite; // outermost write release with the nested read still held
  except
    on E: Exception do
      raised := E.Message;
  end;
  CheckTrue(Pos('TLightweightMREWEx.EndWrite', raised) > 0,
    'EndWrite with outstanding nested read raises, got: ' + raised);

  // The raise must leave the lock intact: clean up in the correct order.
  mrew.EndRead;
  mrew.EndWrite;
  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrew.TryBeginWrite then begin
          entered.Value := 1;
          mrew.EndWrite;
        end;
      end).Wait(5000),
    'verification task completed');
  CheckEquals(1, entered.Value, 'lock fully released after correct-order cleanup');
end;

procedure TestLightweightMREWEx.TestTwoLocksInterleavedRelease;
var
  enteredA: TOmniAlignedInt32;
  enteredB: TOmniAlignedInt32;
  mrewA   : ILightweightMREWEx;
  mrewB   : ILightweightMREWEx;
begin
  mrewA := TLightweightMREWExImpl.Create;
  mrewB := TLightweightMREWExImpl.Create;
  enteredA.Value := 0;
  enteredB.Value := 0;

  mrewA.BeginRead;
  mrewB.BeginRead; // A's per-thread node is now non-head (B was linked in front of it)
  CheckTrue(mrewA.TryBeginRead, 'nested TryBeginRead bumps the count on a non-head node');
  mrewA.EndRead;
  mrewA.EndRead; // removes A's node from a non-head position in the per-thread list
  mrewB.EndRead; // removes B's node from the head position

  // Both locks must be fully released - another thread must acquire each as a writer.
  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrewA.TryBeginWrite then begin
          enteredA.Value := 1;
          mrewA.EndWrite;
        end;
      end).Wait(5000),
    'verification task A completed');
  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrewB.TryBeginWrite then begin
          enteredB.Value := 1;
          mrewB.EndWrite;
        end;
      end).Wait(5000),
    'verification task B completed');
  CheckEquals(1, enteredA.Value, 'lock A fully released');
  CheckEquals(1, enteredB.Value, 'lock B fully released');
end;

procedure TestLightweightMREWEx.TestInnerEndWriteWithNestedReadSucceeds;
var
  entered: TOmniAlignedInt32;
  mrew   : ILightweightMREWEx;
begin
  mrew := TLightweightMREWExImpl.Create;
  mrew.AllowReadInsideWrite := true; // must be set before first use
  entered.Value := 0;

  mrew.BeginWrite;
  mrew.BeginWrite;
  mrew.BeginRead;
  // Inner EndWrite must not raise - the nested-read guard applies only to the
  // outermost write release (FWriteLockCount.Value = 1), not to this one.
  mrew.EndWrite;
  mrew.EndRead;
  mrew.EndWrite;

  CheckTrue(
    System.Threading.TTask.Run(
      procedure
      begin
        if mrew.TryBeginWrite then begin
          entered.Value := 1;
          mrew.EndWrite;
        end;
      end).Wait(5000),
    'verification task completed');
  CheckEquals(1, entered.Value, 'lock fully released');
end;

procedure TestLightweightMREWEx.TestRecursiveReadWithPendingWriter;
var
  mrew : ILightweightMREWEx;
  state: TOmniAlignedInt32;
  synch: IOmniSynchronizer;
begin
  mrew := TLightweightMREWExImpl.Create;
  synch := TOmniSynchronizer.Create;
  state.Value := 0;

  mrew.BeginRead;
  System.Threading.TTask.Run(
    procedure
    begin
      synch.Signal('started');
      state.Value := 1;
      mrew.BeginWrite;
      state.Value := 2;
      mrew.EndWrite;
      synch.Signal('done');
    end);

  synch.WaitFor('started');
  Sleep(200); // let the writer become a pending exclusive waiter

  // Raw SRWLOCK deadlocks here: a nested shared acquire queues behind the
  // pending exclusive waiter. Recursion tracking must grant it immediately.
  mrew.BeginRead;
  CheckEquals(1, state.Value, 'writer still blocked during nested read');
  mrew.EndRead;
  Sleep(200);
  CheckEquals(1, state.Value, 'writer still blocked - outer read still held');
  mrew.EndRead;
  CheckTrue(synch.WaitFor('done', 5000), 'writer acquired after last EndRead');
  CheckEquals(2, state.Value, 'writer completed');
end;
{$ENDIF OTL_HasSystemThreading}
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
  {$IFDEF OTL_HasSystemThreading}
  {$ENDIF}
  RegisterTest(TestCancellationToken.Suite);
  RegisterTest(TestLockedT.Suite);
  {$IFDEF OTL_HasLightweightMREW}
  RegisterTest(TestLightweightMREWEx.Suite);
  {$ENDIF}
  RegisterTest(TestLockManager.Suite);
  RegisterTest(TestSingleThreadUseChecker.Suite);
  RegisterTest(TestOtlSync.Suite);
end.
