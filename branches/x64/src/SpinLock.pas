unit SpinLock;
{
#===============================================================================

# Name:        SpinLock.pas
# Author:      Istvan Agoston <Lee_Nover@delphi-si.com>
# Created:     2007-03-25
# Last Change: 2008-10-07
# Version:     1.1.11

# Description:

  A scalable atomic lock

  http://www.intel.com/cd/ids/developer/asmo-na/eng/dc/threading/333935.htm


# Warnings and/or special considerations:

  Currently the SharedSpinLock does not work across user(logon) sessions.
  Vista does not allow \Global objects to be created in sessions other than 0 (Services),
  but can be consumed by them. A workaround is in progress using \Session\ mapping.

  Source code in this file is subject to the license specified below.

#===============================================================================

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is 'SpinLock.pas'.

  The Initial Developer of the Original Code is 'Istvan Agoston'.

  Contributor(s):
    Primoz Gabrijelcic, primoz@gabrijelcic.org, http://17slon.com/blogs/gabr/blogger.html
      Grouping and indexing shared SpinLocks idea
      Disabling sleepless spin on single CPU process affinity idea


#===============================================================================


# History:
  2008-10-07
    ! fixed a critical bug in TicketSpinLock.Acquire; did not set a local Owner before jumping

  2008-08-29
    * moved constants out of classes for D7 compatibility

  2008-07-11
    ! fixed a critical bug that in some cases let the SpinLock through
    * also a small speed optimization; using only _mov instead of _cmpxchg

  2008-07-09
    + TTicketSpinLock: a fair lock (FIFO); loops until it is next in line
    * changed "add reg, 1" to "inc reg"

  2007-04-05
    * removed a conditional jump in Release; using Two-Byte Near-Return RET

  2007-04-04
    * optimizations; restructured loops, replaced jumps with conditional ops
    ! updated Pascal implementation
    + disabling sleepless spin on single CPU process affinity 1) (tnx Primoz)
      1) TSpinLock checks process affinity, TSharedSpinLock checks available CPUs

  2007-04-03
    ! fixed: did not support recursive locks correctly
    + Added NoSleepCount - how many retries without using Sleep
    + Added DynamicSleep - nonlinear SleepAmount increase

  2007-03-27
    ! fixed: if TryCount was 0, the function would violate bounds (tnx GJ)

  2007-03-26
    + Added a shared spin lock - spin lock working accross processes
      using memory mapped files (tnx for ideas GJ and Primoz)

        constructor parameters:
          GroupName: app defined group name of locks (a group can have max 16383 locks)
          Index: app defined index of a lock
          SleepAmount: how much to sleep on a failed lock (in ms)


    ! fixed: in ASM, SleepAmount was pushed as an offset and not the value
        (tnx GJ for finding it)

  2007-03-25
    First release

}

{$DEFINE SpinLock}
{$DEFINE ASM}
{$DEFINE DynamicSleep}

{$IFDEF CPUX64}
{$DEFINE PUREPASCAL}
{$ENDIF CPUX64}

{$IFDEF PUREPASCAL}
  {$UNDEF ASM}
{$ENDIF}

interface

uses Windows, SyncObjs;

type
  PLockMap = ^TLockMap;
  TLockMap = record
    Lock: Cardinal;
    LockCount: Cardinal;
  end;

const
  PageSize = MAXWORD + 1;
  MaxLocks = PageSize div SizeOf(TLockMap);
  TSLOwnerOffset = SizeOf(TLockMap);
  TSLNextOffset  = TSLOwnerOffset + SizeOf(Word);

type
  TSpinLock = class(TSynchroObject)
  protected
    FNoSleepCount: Cardinal;
    FOwner: PLockMap;
    FSleepAmount: Cardinal;
    procedure InitLockMap; virtual;
    procedure InitNoSleepCount(const NoSleepCount: Cardinal); virtual;
    procedure ReleaseLockMap; virtual;
  public
    constructor Create(const SleepAmount: Cardinal = 0;
      const NoSleepCount: Cardinal = 0); virtual;
    destructor Destroy; override;
    procedure Acquire; overload; override;
    function Acquire(TryCount: Cardinal): Boolean; reintroduce; overload; virtual;
    procedure Release; override;
  end;

  TSpinIndex = 0..MaxLocks-1;

  TSharedSpinLock = class(TSpinLock)
  protected
    FAttributes: TSecurityAttributes;
    FDescriptor: TSecurityDescriptor;
    FGroupName: WideString;
    FHandle: THandle;
    FLockIndex: TSpinIndex;
    FMapping: Pointer;
    procedure InitLockMap; override;
    procedure InitNoSleepCount(const NoSleepCount: Cardinal); override;
    procedure ReleaseLockMap; override;
  public
    constructor Create(const GroupName: WideString; const Index: TSpinIndex;
      const SleepAmount: Cardinal = 0; const NoSleepCount: Cardinal = 0); reintroduce;
  end;

  TTicketInfo = packed record
    case Boolean of
      False: (
        Owner: Word;
        Next: Word;
      );

      True: (
        Combined: Cardinal
      );
  end;

  PLockMap2 = ^TLockMap2;
  TLockMap2 = record
    Lock: Cardinal;
    LockCount: Cardinal;
    Ticket: TTicketInfo;
  end;

  TTicketSpinLock = class(TSpinLock)
  protected
    procedure InitLockMap; override;
    procedure ReleaseLockMap; override;
  public
    procedure Acquire; override;
    function Acquire(TryCount: Cardinal): Boolean; override;
    procedure Release; override;
  end;

function ProcessIdToSessionId(const dwProcessId: DWORD; out pSessionId: DWORD): BOOL; stdcall; external kernel32;


implementation

uses SysUtils;

const
  SGlobalSharedSpinLockMapping = '';//'\Session\%d\$$SharedSpinLockMapping$$'; // DO NOT LOCALIZE

resourcestring
  STicketSpinLockNoTryAcquire = 'TicketSpinLock does not support TryAcquire';
  SCantIntializeSecurityDescriptor = 'Can''t intialize security descriptor';
  SCantSetSecurityDescriptorDacl = 'Can''t set security descriptor dacl';
  SErrorCreatingMMF = 'Error creating MMF: ';
  SErrorMappingMMF = 'Error mapping MMF: ';

type
  PLockMapArray = ^TLockMapArray;
  TLockMapArray = array[0..MaxLocks-1] of TLockMap;

{ compares CompareVal and Target and returns Target's old value
  if they're equal, Target is set to NewVal }
function LockCmpxchg(CompareVal, NewVal: Cardinal; var Target: Cardinal): Cardinal;
asm
  lock cmpxchg [ecx], edx
end;

{ increments Target by Source and returns Target's old value }
function LockXadd(const Source: Cardinal; var Target: Cardinal): Cardinal;
asm
  lock xadd [edx], eax
end;

constructor TSpinLock.Create(const SleepAmount: Cardinal = 0;
  const NoSleepCount: Cardinal = 0);
begin
  inherited Create;
  FSleepAmount := SleepAmount;

  InitNoSleepCount(NoSleepCount);
  InitLockMap;
end;

destructor TSpinLock.Destroy;
begin
  ReleaseLockMap;
  FOwner := nil;
  inherited;
end;

procedure TSpinLock.Acquire;
{$IFNDEF ASM}
var
  LOwner: Cardinal;
  LLock: Cardinal;
  LSleepCounter: Cardinal;
  LSleepAmount: Cardinal;
begin
  LLock := GetCurrentThreadId;
  if FNoSleepCount > 0 then
  begin
    LSleepCounter := 0;
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    while (LOwner <> 0) and (LOwner <> LLock) and (LSleepCounter < FNoSleepCount) do
    begin
      Inc(LSleepCounter);
      LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    end;
  end;

  LSleepAmount := 0;
  LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
  while (LOwner <> 0) and (LOwner <> LLock) do
  begin
    Sleep(LSleepAmount);
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    {$IFDEF DynamicSleep}
    if LSleepAmount < FSleepAmount then
      Inc(LSleepAmount, LSleepAmount + 1);
    {$ENDIF}
  end;
  Inc(FOwner.LockCount);
{$ELSE}
asm
  {
    EBX - Pointer to Self
    ECX - ThreadID
    EDX - FLock
    ESI - FNoSleepCount to 0 (decrementing), 0 to FSleepAmount (incrementing)
  }

  mov ecx, fs:[$00000018]       // get thread information block
  mov edx, [eax].FOwner         // move owner to edx
  mov ecx, [ecx+$24]            // get thread id into ecx (moved here for instruction pairing)
  push ebx                      // save ebx and store self in it
  mov ebx, eax
  xor eax, eax                  // clear eax for comparison to 0
  lock cmpxchg [edx], ecx       // if no owner, then set our thread id
  je @@exitImmediate            // if wasn't owned then we've locked and exit
  cmp eax, ecx                  // if we own it then exit (recursive locks)
  je @@exitImmediate
  push esi
  mov esi, [ebx].FNoSleepCount  // set nosleep counter
  test esi, esi                 // if nosleep is 0 then jump to waitLoop
  jz @@waitLoop
  inc esi
  xor eax, eax

@@noSleepLoop:
  dec esi                       // decrement local NoSleepCount
  jz @@waitLoop                 // if we reached 0 then go to waitable loop
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@noSleepLoop             // if it's owned then repeat the loop
  lock cmpxchg [edx], ecx       // try to obtain lock
  jz @@exitNoSleep              // if obtained we exit else enter waitable loop

@@waitLoop:
  push ecx                      // save ecx and edx coz Sleep modifies them
  push edx
{$IFDEF DynamicSleep}
  push esi                      // esi has our sleep amount
  xor eax, eax
  cmp esi, [ebx].FSleepAmount   // compare local SleepAmount counter
  cmovc eax, esi                // if less than SleepAmount then copy to eax
  adc esi, eax                  // if compare was less we double esi and add 1
{$ELSE}
  push [ebx].FSleepAmount
{$ENDIF}
  call Windows.Sleep
  pop edx                       // restore edx and ecx
  pop ecx
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@waitLoop                // if it's owned then repeat the loop
  xor eax, eax
  lock cmpxchg [edx], ecx       // try to obtain lock
  jnz @@waitLoop                // if lock failed then reenter the loop

@@exitNoSleep:
  pop esi

@@exitImmediate:
  inc [edx+$04]                 // increment lock count
  pop ebx
{$ENDIF}
end;

function TSpinLock.Acquire(TryCount: Cardinal): Boolean;
{$IFNDEF ASM}
var
  LOwner: Cardinal;
  LLock: Cardinal;
  LSleepCounter: Cardinal;
  LSleepAmount: Cardinal;
begin
  if TryCount = 0 then
    Inc(TryCount);

  LLock := GetCurrentThreadId;
  // if not owned, set ourself as owner
  if FNoSleepCount > 0 then
  begin
    LSleepCounter := 0;
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    while (LOwner <> 0) and (LOwner <> LLock)
      and (LSleepCounter < FNoSleepCount) and (TryCount > 0) do
    begin
      Inc(LSleepCounter);
      Dec(TryCount);
      LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    end;
  end;

  LSleepAmount := 0;
  LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
  while (LOwner <> 0) and (LOwner <> LLock) and (TryCount > 0) do
  begin
    Sleep(LSleepAmount);
    Dec(TryCount);
    LOwner := LockCmpxchg(0, LLock, FOwner.Lock);
    {$IFDEF DynamicSleep}
    if LSleepAmount < FSleepAmount then
      Inc(LSleepAmount, LSleepAmount + 1);
    {$ENDIF}
  end;

  Result := TryCount > 0;
  if Result then
    Inc(FOwner.LockCount);
{$ELSE}
asm
  {
    EBX - Pointer to Self
    ECX - ThreadID
    EDX - FLock
    ESI - FNoSleepCount to 0 (decrementing), 0 to FSleepAmount (incrementing)
    EDI - TryCount
  }

  mov ecx, 1                    // set ecx to 1 for comparing TryCount
  push edi                      // save edi
  mov edi, edx                  // copy TryCount to edi
  cmp edi, ecx                  // check if TryCount is less than 1
  adc edi, 1                    // increment TryCount and add 1 if it was 0

  mov ecx, fs:[$00000018]       // get thread id into ecx
  mov edx, [eax].FOwner         // move owner to edx
  mov ecx, [ecx+$24]            // moved here for instruction pairing
  push ebx                      // save ebx and store self in it
  mov ebx, eax
  xor eax, eax                  // clear eax for comparison to 0
  lock cmpxchg [edx], ecx       // if no owner, then set our thread id
  je @@exitImmediate            // if wasn't owned then we've locked and exit
  cmp eax, ecx                  // if we own it then exit (recursive locks)
  je @@exitImmediate
  push esi
  mov esi, [ebx].FNoSleepCount  // set nosleep counter
  test esi, esi                 // if nosleep is 0 then jump to waitLoop
  jz @@waitLoop
  inc esi
  xor eax, eax

@@noSleepLoop:
  dec esi                       // decrement local NoSleepCount
  jz @@waitLoop                 // if we reached 0 then go to waitable loop
  dec edi                       // decrement TryCount
  jz @@exitNoSleep              // if we reached 0 then exit
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@noSleepLoop             // if it's owned then repeat the loop
  lock cmpxchg [edx], ecx       // try to obtain lock
  jz @@exitNoSleep              // if obtained we exit else enter waitable loop

@@waitLoop:
  dec edi                       // decrement TryCount
  jz @@exit                     // if we reached 0 then exit
  push ecx                      // save ecx and edx coz Sleep modifies them
  push edx
{$IFDEF DynamicSleep}
  push esi                      // esi has our sleep amount
  xor eax, eax
  cmp esi, [ebx].FSleepAmount   // compare local SleepAmount counter
  cmovc eax, esi                // if less than SleepAmount then copy to eax
  adc esi, eax                  // if compare was less we double esi and add 1
{$ELSE}
  push [ebx].FSleepAmount
{$ENDIF}
  call Windows.Sleep
  pop edx                       // restore edx and ecx
  pop ecx
  pause
  cmp [edx], 0                  // check the owner (volatile read)
  jne @@waitLoop                // if it's owned then repeat the loop
  xor eax, eax
  lock cmpxchg [edx], ecx       // try to obtain lock
  jnz @@waitLoop                // if lock failed then reenter the loop

@@exit:
@@exitNoSleep:
  pop esi

@@exitImmediate:
  xor eax, eax
  cmp eax, edi                  // check TryCount
  adc [edx+$04], 0              // if it's above 0 then increment lock count
  mov eax, edi                  // store result
  pop ebx
  pop edi
{$ENDIF}
end;

procedure TSpinLock.InitLockMap;
begin
  FOwner := AllocMem(SizeOf(TLockMap));
end;

procedure TSpinLock.InitNoSleepCount(const NoSleepCount: Cardinal);
var
  LPAM: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  LSAM: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  LCPUCount: Cardinal;
  I: Integer;
begin
  // check on how many CPUs our process can run
  // if it's only 1 then there's no use in looping without sleep
  if GetProcessAffinityMask(GetCurrentProcess, LPAM, LSAM) then
  begin
    LCPUCount := 0;
    for I := 0 to 31 do
      if (LPAM and (1 shl I)) <> 0 then
        Inc(LCPUCount);
  end
  else
    LCPUCount := 1;

  if LCPUCount > 1 then
    FNoSleepCount := NoSleepCount
  else
    FNoSleepCount := 0;
end;

procedure TSpinLock.Release;
{$IFNDEF ASM}
var
  LLock: Cardinal;
begin
  LLock := GetCurrentThreadId;
  if (FOwner.Lock <> LLock) then
    Exit;

  if (FOwner.LockCount > 0) then
    Dec(FOwner.LockCount);

  if FOwner.LockCount = 0 then
    FOwner.Lock := 0;
{$ELSE}
asm
  mov edx, [eax].FOwner         // store self in edx
  mov eax, fs:[$00000018]       // get thread id
  mov eax, [eax+$24]            // store thread id in eax
  cmp [edx], eax                // test if we own the lock, exit if not
  jne @@exit
  xor ecx, ecx                  // clear ecx for reseting the lock
  dec [edx+$04]                 // decrement lock count
  cmovz eax, ecx                // if reached 0, copy ecx to eax
  mov [edx], eax                // update the lock with threadId or 0
@@exit:
  db $F3, $C3                   // Two-Byte Near-Return RET
{$ENDIF}
end;

procedure TSpinLock.ReleaseLockMap;
begin
  FreeMem(FOwner);
end;

constructor TSharedSpinLock.Create(const GroupName: WideString;
  const Index: TSpinIndex; const SleepAmount: Cardinal = 0;
  const NoSleepCount: Cardinal = 0);
begin
  Assert(Index < MaxLocks);
  FGroupName := GroupName;
  FLockIndex := Index;
  inherited Create(SleepAmount, NoSleepCount);
end;

procedure TSharedSpinLock.InitLockMap;
var
  LLockClassName: WideString;
  LSessionId: Cardinal;
begin
  // null SA needed to access from processes' running as different users
  if not InitializeSecurityDescriptor(@FDescriptor, SECURITY_DESCRIPTOR_REVISION) then
    raise Exception.Create(SCantIntializeSecurityDescriptor);
  if not SetSecurityDescriptorDacl(@FDescriptor, True, nil, False) then
    raise Exception.Create(SCantSetSecurityDescriptorDacl);
  FAttributes.nLength := SizeOf(FAttributes);
  FAttributes.bInheritHandle := True;
  FAttributes.lpSecurityDescriptor := @FDescriptor;

  ProcessIdToSessionId(GetCurrentProcessId, LSessionId);
  LLockClassName := WideFormat(SGlobalSharedSpinLockMapping, [LSessionId]) + FGroupName;

  FHandle := CreateFileMappingW($FFFFFFFF, @FAttributes, PAGE_READWRITE, 0, PageSize, PWideChar(LLockClassName));
  if FHandle = 0 then
    raise Exception.Create(SErrorCreatingMMF + SysErrorMessage(GetLastError));

  FMapping := MapViewOfFile(FHandle, FILE_MAP_WRITE, 0, 0, 0);
  if Assigned(FMapping) then
  begin
    FOwner := @PLockMapArray(FMapping)[FLockIndex];
  end
  else
  begin
    CloseHandle(FHandle);
    raise Exception.Create(SErrorMappingMMF + SysErrorMessage(GetLastError));
  end;
end;

procedure TSharedSpinLock.InitNoSleepCount(const NoSleepCount: Cardinal);
var
  LPAM: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  LSAM: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  LCPUCount: Cardinal;
  I: Integer;
begin
  // check how many CPUs we can use
  // if it's only 1 then there's no use in looping without sleep
  if GetProcessAffinityMask(GetCurrentProcess, LPAM, LSAM) then
  begin
    LCPUCount := 0;
    for I := 0 to 31 do
      if (LSAM and (1 shl I)) <> 0 then
        Inc(LCPUCount);
  end
  else
    LCPUCount := 1;

  if LCPUCount > 1 then
    FNoSleepCount := NoSleepCount
  else
    FNoSleepCount := 0;
end;

procedure TSharedSpinLock.ReleaseLockMap;
begin
  if FHandle > 0 then
  begin
    if Assigned(FMapping) then
      UnmapViewOfFile(FMapping);

    CloseHandle(FHandle);
  end;
end;

{ TTicketSpinLock }

procedure TTicketSpinLock.Acquire;
const
  CInc = $00010000;
var
  LOwnerPtr: PLockMap2;
  LTicket: Word;
  LOwner: Cardinal;
  LLock: Cardinal;
  LSleepCounter: Cardinal;
  LSleepAmount: Cardinal;
{$IFNDEF ASM}
begin
  LLock := GetCurrentThreadId;
  LOwner := FOwner.Lock;
  if LOwner <> LLock then
  begin
    LOwnerPtr := PLockMap2(FOwner);
    LOwner := LockXadd(CInc, LOwnerPtr.Ticket.Combined);
    LTicket := TTicketInfo(LOwner).Next;
    if TTicketInfo(LOwner).Owner <> LTicket then
    begin
      if FNoSleepCount > 0 then
      begin
        LSleepCounter := FNoSleepCount;
        while (LOwner <> LTicket) and (LSleepCounter > 0) do
        begin
          Dec(LSleepCounter);
          LOwner := LOwnerPtr.Ticket.Owner;
        end;
      end;
  
      LSleepAmount := 0;
      while (LOwner <> LTicket) do
      begin
        Sleep(LSleepAmount);
        LOwner := LOwnerPtr.Ticket.Owner;
        {$IFDEF DynamicSleep}
        if LSleepAmount < FSleepAmount then
          Inc(LSleepAmount, LSleepAmount + 1);
        {$ENDIF}
      end;
    end;

    FOWner.Lock := LLock;
  end;
  Inc(FOwner.LockCount);
{$ELSE}
asm
  mov ecx, fs:[$00000018]       // get thread information block
  mov edx, [eax].FOwner         // move owner to edx
  mov LOwnerPtr, edx            // store Owner record in local variable
  mov ecx, [ecx+$24]            // get thread id into ecx
  cmp ecx, [edx]                // compare the owner
  je @@incLockCount             // if we own the lock just exit and increment lock count

  mov LLock, ecx                // store ThreadID in local variable
  mov ecx, CInc
  lock xadd [edx+TSLOwnerOffset], ecx // increment the Ticket number and get the owner|ticket in ecx
  movzx edx, cx                 // copy the owner to edx
  shr ecx, 16                   // shift to get only the ticket number
  cmp cx, dx                    // compare ticket and owner
  je @@setOwnerThreadId         // exit if equal (we've obtained the lock)

  // copy parameters to local variables
  mov LTicket, cx
  mov ecx, [eax].FNoSleepCount
  mov LSleepCounter, ecx
  mov edx, [eax].FSleepAmount
  mov LSleepAmount, edx
  inc LSleepCounter


@@noSleepLoop:
  pause
  mov eax, LOwnerPtr
  mov eax, [eax + TSLOwnerOffset] // copy the owner
  cmp LTicket, ax               // compare ticket and owner
  je @@setOwnerThreadId         // if it's owned then set owner and exit
  dec LSleepCounter             // decrement NoSleepCount
  jnz @@noSleepLoop             // if not 0 repeat the loop


@@waitLoop:
{$IFDEF DynamicSleep}
  xor eax, eax
  mov ecx, LSleepCounter        // starts from 0
  cmp ecx, LSleepAmount         // compare local SleepAmount counter
  cmovc eax, LSleepCounter      // if less than SleepAmount then copy to eax
  adc LSleepCounter, eax        // if compare was less we double esi and add 1
  push LSleepCounter
{$ELSE}
  push LSleepAmount
{$ENDIF}
  call Windows.Sleep
  mov eax, LOwnerPtr
  mov eax, [eax + TSLOwnerOffset] // copy the owner
  cmp LTicket, ax               // compare ticket and owner
  jne @@waitLoop                // if not equal we repeat the loop


@@setOwnerThreadId:
  mov eax, LLock
  mov ecx, LOwnerPtr
  mov [ecx], eax          // set the owning ThreadID

@@incLockCount:
  mov edx, LOwnerPtr
  inc [edx + $04]               // increment lock count
{$ENDIF}
end;


function TTicketSpinLock.Acquire(TryCount: Cardinal): Boolean;
begin
  raise Exception.Create(STicketSpinLockNoTryAcquire);
end;

procedure TTicketSpinLock.InitLockMap;
begin
  FOwner := AllocMem(SizeOf(TLockMap2));
end;

procedure TTicketSpinLock.Release;
{$IFNDEF ASM}
var
  LLock: Cardinal;
begin
  LLock := GetCurrentThreadId;
  if (FOwner.Lock = LLock) then
  begin
    if (FOwner.LockCount > 0) then
      Dec(FOwner.LockCount);

    if FOwner.LockCount = 0 then
    begin
      FOwner.Lock := 0;
      Inc(PLockMap2(FOwner).Ticket.Owner);
    end;
  end;
{$ELSE}
asm
  mov edx, [eax].FOwner         // store self in edx
  mov eax, fs:[$00000018]       // get thread id
  mov eax, [eax+$24]
  cmp [edx], eax                // test if we own the lock, exit if not
  jne @@exit

  xor eax, eax
  xor ecx, ecx
  dec [edx+$04]                 // decrement lock count
  cmovnz ecx, [edx]             // if not reached 0 we copy the old owner
  setz al                       // if reached 0, set eax to 1
  mov [edx], ecx                // set the new owner threadId
  lock add [edx + TSLOwnerOffset], ax   // increment Owner and thus release the lock
@@exit:
  db $F3, $C3                   // Two-Byte Near-Return RET
{$ENDIF}
end;


procedure TTicketSpinLock.ReleaseLockMap;
begin
  FreeMem(FOwner);
end;

end.
