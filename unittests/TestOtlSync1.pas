unit TestOtlSync1;

{$I OtlOptions.Inc}

interface

uses
  TestFramework, GpStuff, Windows, DSiWin32, OtlContainers, SysUtils, SyncObjs,
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
  readers: TArray<IOmniTaskControl>;
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
  readers: TArray<IOmniTaskControl>;
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
  readers: TArray<IOmniTaskControl>;
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
  readers: TArray<IOmniTaskControl>;
  times  : TArray<int64>;

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
    CheckTrue((times[i] > (CTimeout * 0.5)) and (times[i] < (CTimeout * 1.5)),
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
  writers: TArray<IOmniTaskControl>;
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
  writers: TArray<IOmniTaskControl>;
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
  writers: TArray<IOmniTaskControl>;
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
  writers: TArray<IOmniTaskControl>;
  times  : TArray<int64>;

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
    CheckTrue((times[i] > (CTimeout * 0.5)) and (times[i] < (CTimeout * 1.5)),
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
  writers: TArray<IOmniTaskControl>;
  times  : TArray<int64>;

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
    CheckTrue((times[i] > (CTimeout * 0.5)) and (times[i] < (CTimeout * 1.5)),
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

  CheckEquals(0, FSharedValue);
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
  for iRepeat := 1 to {$IFDEF CONSOLE_TESTRUNNER}100{$ELSE}10{$ENDIF} do begin
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
  for iRepeat := 1 to {$IFDEF CONSOLE_TESTRUNNER}100{$ELSE}10{$ENDIF} do begin
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
  inherited;
  FSync := TOmniSynchronizer.Create;
end;

procedure TestOtlSync.TearDown;
begin
  FreeAndNil(FSync);
  inherited;
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

initialization
  // Register any test cases with the test runner
  RegisterTest(TestOtlSync.Suite);
end.
