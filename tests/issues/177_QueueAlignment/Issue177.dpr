program Issue177;

{ Repro for GitHub issue #177: assertions in TOmniBaseQueue.Initialize fail when
  the memory manager does not return 16-byte aligned blocks (e.g. madExcept with
  resource leak tracking on x64).

  TOmniBaseQueue.Initialize allocates its head/tail TOmniTaggedPointer records with
  plain AllocMem and then asserts that the result is aligned to 2*SizeOf(pointer),
  which is what CMPXCHG16B (x64) requires. This holds only if the memory manager
  happens to return suitably aligned blocks. Unlike TOmniBaseBoundedStack and
  TOmniBaseBoundedQueue, which align their buffers with RoundUpTo, nothing in the
  queue guarantees it.

  The test installs a memory manager that returns blocks aligned to SizeOf(pointer)
  but deliberately NOT to 2*SizeOf(pointer) - a stand-in for any allocator with a
  per-block header - and create and destroy a queue. No madExcept needed.

  Exit code: 0 = pass, 1 = fail. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  OtlCommon,
  OtlContainers;

var
  GOrigMM: TMemoryManagerEx;

// Block layout: [base ptr and size header][padding]<result>. result mod 2*SizeOf(pointer) = SizeOf(pointer).

function MisGetMem(size: NativeInt): pointer;
var
  a   : NativeUInt;
  base: NativeUInt;
begin
  base := NativeUInt(GOrigMM.GetMem(size + 48));
  a := (base + 32) and not NativeUInt(15);
  PNativeUInt(a - SizeOf(pointer))^ := NativeUInt(size);
  PNativeUInt(a)^ := base;
  Result := pointer(a + SizeOf(pointer));
end; { MisGetMem }

function MisFreeMem(p: pointer): integer;
begin
  Result := GOrigMM.FreeMem(pointer(PNativeUInt(NativeUInt(p) - SizeOf(pointer))^));
end; { MisFreeMem }

function MisReallocMem(p: pointer; size: NativeInt): pointer;
var
  oldSize: NativeInt;
begin
  oldSize := NativeInt(PNativeUInt(NativeUInt(p) - 2*SizeOf(pointer))^);
  Result := MisGetMem(size);
  if oldSize < size then
    size := oldSize;
  Move(p^, Result^, size);
  MisFreeMem(p);
end; { MisReallocMem }

function MisAllocMem(size: NativeInt): pointer;
begin
  Result := MisGetMem(size);
  FillChar(Result^, size, 0);
end; { MisAllocMem }

function MisRegisterLeak(p: pointer): boolean;
begin
  Result := false;
end; { MisRegisterLeak }

var
  failures: integer = 0;
  lastLine: integer = 0;

// Records the failure instead of raising, so that no exception has to unwind through
// the deliberately odd allocator (and so the queue can still be destroyed cleanly).
procedure RecordAssert(const message, filename: string; lineNumber: integer; errorAddr: pointer);
begin
  Inc(failures);
  lastLine := lineNumber;
end; { RecordAssert }

var
  mm   : TMemoryManagerEx;
  i    : integer;
  queue: TOmniBaseQueue;
  value: TOmniValue;

begin
  SetErrorMode(SEM_NOGPFAULTERRORBOX or SEM_FAILCRITICALERRORS);
  AssertErrorProc := RecordAssert;
  GetMemoryManager(GOrigMM);
  mm := GOrigMM;
  mm.GetMem := MisGetMem;
  mm.FreeMem := MisFreeMem;
  mm.ReallocMem := MisReallocMem;
  mm.AllocMem := MisAllocMem;
  mm.RegisterExpectedMemoryLeak := MisRegisterLeak;
  mm.UnregisterExpectedMemoryLeak := MisRegisterLeak;
  // Nothing may be allocated before the original manager is restored and
  // freed afterwards (or vice versa), hence no I/O while the hook is active.
  SetMemoryManager(mm);
  try
    queue := TOmniBaseQueue.Create; // Initialize asserts on head/tail pointer alignment
    try
      // Exercise the CAS operations on head/tail as well. If the pointers were misaligned
      // in a build without assertions, this would fault (CMPXCHG16B on x64).
      for i := 1 to 100 do
        queue.Enqueue(i);
      for i := 1 to 100 do
        if (not queue.TryDequeue(value)) or (value.AsInteger <> i) then
          Inc(failures);
    finally FreeAndNil(queue); end;
  finally SetMemoryManager(GOrigMM); end;
  if failures > 0 then begin
    Writeln('FAIL: ', failures, ' failure(s), last assertion in OtlContainers.pas line ', lastLine);
    ExitCode := 1;
  end
  else
    Writeln('PASS: queue created and destroyed with a SizeOf(pointer)-aligned allocator');
end.
