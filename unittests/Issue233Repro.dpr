program Issue233Repro;

{
  Reproduction for issue #233 (TOmniBaseBoundedQueue: signed pointer comparison breaks ring
  buffers that cross 0x80000000 in large-address-aware 32-bit processes).

  A memory manager hook places the queue's blocks in a region that straddles 0x80000000.
  The start of the allocation is swept so that, in turn, the public and the recycle ring
  buffer cross the 2 GB line. For each position the queue is exercised and checked.

    dcc32 -B -NSSystem;System.Win;Winapi -U..;..\src Issue233Repro.dpr

  Exit code 0 = all positions OK, 1 = at least one position fails.
}

{$APPTYPE CONSOLE}

{$SetPEFlags $20}  // IMAGE_FILE_LARGE_ADDRESS_AWARE

uses
  Winapi.Windows,
  System.SysUtils,
  OtlContainers;

const
  CRegionStart: NativeUInt = $7FFF0000;
  CRegionSize : NativeUInt = $20000;
  CLineAddress: NativeUInt = $80000000;
  CNumElements = 20;

var
  GOldMM    : TMemoryManagerEx;
  GCapture  : boolean;
  GCursor   : NativeUInt;
  GRegionEnd: NativeUInt;

function IsOwned(p: pointer): boolean;
begin
  Result := (NativeUInt(p) >= CRegionStart) and (NativeUInt(p) < CRegionStart + CRegionSize);
end;

function HookGetMem(size: NativeInt): pointer;
begin
  if GCapture then begin
    Result := pointer(GCursor);
    GCursor := (GCursor + NativeUInt(size) + 15) and not NativeUInt(15);
    if GCursor > GRegionEnd then
      raise Exception.Create('Issue233Repro: out of region memory');
  end
  else
    Result := GOldMM.GetMem(size);
end;

function HookFreeMem(p: pointer): integer;
begin
  if IsOwned(p) then
    Result := 0
  else
    Result := GOldMM.FreeMem(p);
end;

function HookReallocMem(p: pointer; size: NativeInt): pointer;
begin
  if IsOwned(p) then
    raise Exception.Create('Issue233Repro: ReallocMem of an owned block')
  else
    Result := GOldMM.ReallocMem(p, size);
end;

function HookAllocMem(size: NativeInt): pointer;
begin
  if GCapture then begin
    Result := HookGetMem(size);
    FillChar(Result^, size, 0);
  end
  else
    Result := GOldMM.AllocMem(size);
end;

procedure InstallHook;
var
  mm: TMemoryManagerEx;
begin
  GetMemoryManager(GOldMM);
  mm := GOldMM;
  mm.GetMem := HookGetMem;
  mm.FreeMem := HookFreeMem;
  mm.ReallocMem := HookReallocMem;
  mm.AllocMem := HookAllocMem;
  SetMemoryManager(mm);
end;

// returns '' if the queue behaves, otherwise a description of the first problem
function ExerciseQueue(queue: TOmniBoundedQueue): string;
var
  cycle: integer;
  i    : integer;
  value: integer;
begin
  for cycle := 1 to 3 do begin
    if queue.IsFull then
      Exit(Format('cycle %d: IsFull on empty queue', [cycle]));
    for i := 1 to CNumElements-1 do begin
      value := i + cycle * 1000;
      if not queue.Enqueue(value) then
        Exit(Format('cycle %d: Enqueue(%d) failed', [cycle, i]));
    end;
    for i := 1 to CNumElements-1 do begin
      value := 0;
      if not queue.Dequeue(value) then
        Exit(Format('cycle %d: Dequeue #%d returned nothing (message lost)', [cycle, i]));
      if value <> i + cycle * 1000 then
        Exit(Format('cycle %d: Dequeue #%d returned %d, expected %d', [cycle, i, value, i + cycle * 1000]));
    end;
    if not queue.IsEmpty then
      Exit(Format('cycle %d: queue not empty after draining', [cycle]));
  end;
  Result := '';
end;

var
  failed : integer;
  offset : integer;
  problem: string;
  queue  : TOmniBoundedQueue;
  runs   : integer;
begin
  if VirtualAlloc(pointer(CRegionStart), CRegionSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE) = nil then begin
    Writeln('VirtualAlloc at ', IntToHex(CRegionStart, 8), ' failed: ', GetLastError, ' ', SysErrorMessage(GetLastError));
    Halt(2);
  end;
  GRegionEnd := CRegionStart + CRegionSize;
  InstallHook;
  failed := 0;
  runs := 0;
  offset := 1600;
  while offset >= 0 do begin
    GCursor := CLineAddress - NativeUInt(offset);
    GCapture := true;
    try
      queue := TOmniBoundedQueue.Create(CNumElements, SizeOf(integer));
    finally GCapture := false; end;
    try
      problem := ExerciseQueue(queue);
    finally FreeAndNil(queue); end;
    Inc(runs);
    if problem <> '' then begin
      Inc(failed);
      Writeln(Format('start %s (%4d below 2GB): FAIL - %s', [IntToHex(CLineAddress - NativeUInt(offset), 8), offset, problem]));
    end;
    Dec(offset, 16);
  end;
  Writeln(Format('%d of %d positions failed', [failed, runs]));
  if failed > 0 then Halt(1);
end.
