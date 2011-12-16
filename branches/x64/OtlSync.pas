///<summary>Synchronisation primitives. Part of the OmniThreadLibrary project.</summary>
///<remarks>Move* family of functions require Pentium 4 processor (or newer).</remarks>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Home              : http://otl.17slon.com
///   Support           : http://otl.17slon.com/forum/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///     Web             : http://gp.17slon.com
///   Contributors      : GJ, Lee_Nover, dottor_jeckill
///
///   Creation date     : 2009-03-30
///   Last modification : 2011-12-16
///   Version           : 1.12
///</para><para>
///   History:
///     1.12: 2011-12-16
///       - [GJ] Converted low-level primitives to work in 64-bit platform and added few
///         platform-independent versions (CAS, MoveDPtr).
///     1.11: 2011-12-14
///       - Implemented simplified versions of Atomic<T:class,constructor>.Initialize and
///         Locked<T:class,constructor>.Initialize that work on D2010 and newer.
///     1.10a: 2011-12-09
///       - TOmniCS reuses LockCount from owned TOmniCriticalSection.
///     1.10: 2011-12-02
///       - Locked<class> by default takes ownership of the object and frees it when
///         Locked<> goes out of scope. You can change this by calling
///         Locked<T>.Create(obj, false). To free the object manually, call Locked<T>.Free.
///       - Atomic<class>.Initialize was broken.
///       - Implemented Atomic<class>.Initialize(object) and Locked<class>.Initialize.
///       - Implemented Mfence.
///       - Locked<T>.Initialize creates memory barrier after storing newly created
///         resource into shared variable.
///     1.09: 2011-12-01
///       - IOmniCriticalSection implements TFixedCriticalSection (as suggested by Eric
///         Grange in http://delphitools.info/2011/11/30/fixing-tcriticalsection/).
///       - Implemented IOmniCriticalSection.LockCount and TOmniCS.LockCount.
///       - Locked<T>.GetValue raises exception if critical section's LockCount is 0.
///     1.08: 2011-11-29
///       - Implements Locked<T> class.
///     1.07a: 2011-11-29
///       - Compiles with D2007.
///     1.07: 2011-11-25
///       - Implemented Atomic<T> class for atomic interface initialization.
///     1.06: 2011-03-01
///       - [dottor_jeckill] Bug fix: TOmniResourceCount.TryAllocate always returned False.
///     1.05: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.04a: 2010-03-30
///       - Prevent race condition in a rather specialized usage of TOmniResourceCount.
///     1.04: 2010-02-04
///       - Implemented CAS8 and CAS16.
///     1.03: 2010-02-03
///       - IOmniCancellationToken extended with the Clear method.
///     1.02: 2010-02-02
///       - Implemented IOmniCancellationToken.
///     1.01a: 2010-01-07
///       - "Wait when no resources" state in TOmniResourceCount was not properly
///         implemented.
///     1.01: 2009-12-30
///       - Implemented resource counter with empty state signalling - TOmniResourceCount.
///     1.0: 2008-08-26
///       - TOmniCS and IOmniCriticalSection imported from the OtlCommon unit.
///       - [GJ] Added very simple (and very fast) multi-reader-exclusive-writer TOmniMREW.
///       - First official release.
///</para></remarks>

unit OtlSync;

{$I OtlOptions.inc}

interface

uses
  SyncObjs,
  DSiWin32,
  {$IFDEF OTL_ERTTI}
  RTTI,
  {$ENDIF OTL_ERTTI}
  GpStuff;

type
{$IFNDEF WIN64}
  NativeInt = Integer;
  PNativeInt = PInteger;
{$ENDIF WIN64}

   TFixedCriticalSection = class(TCriticalSection)
   strict protected
     FDummy: array [0..95] of byte;
   end; { TFixedCriticalSection }

  IOmniCriticalSection = interface ['{AA92906B-B92E-4C54-922C-7B87C23DABA9}']
    function  GetLockCount: integer;
    //
    procedure Acquire;
    procedure Release;
    function  GetSyncObj: TSynchroObject;
    property LockCount: integer read GetLockCount;
  end; { IOmniCriticalSection }

  ///<summary>Simple critical section wrapper. Critical section is automatically
  ///    initialised on first use.</summary>
  TOmniCS = record
  strict private
    ocsSync: IOmniCriticalSection;
  private
    function  GetLockCount: integer; inline;
    function  GetSyncObj: TSynchroObject; inline;
  public
    procedure Initialize;
    procedure Acquire; inline;
    procedure Release; inline;
    property LockCount: integer read GetLockCount;
    property SyncObj: TSynchroObject read GetSyncObj;
  end; { TOmniCS }

  ///<summary>Very lightweight multiple-readers-exclusive-writer lock.</summary>
  TOmniMREW = record
  strict private
    omrewReference: NativeInt;      //Reference.Bit0 is 'writing in progress' flag
  public
    procedure EnterReadLock; inline;
    procedure EnterWriteLock; inline;
    procedure ExitReadLock; inline;
    procedure ExitWriteLock; inline;
  end; { TOmniMREW }

  IOmniResourceCount = interface ['{F5281539-1DA4-45E9-8565-4BEA689A23AD}']
    function  GetHandle: THandle;
    //
    function  Allocate: cardinal; 
    function  Release: cardinal;
    function  TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal = 0): boolean;
    property Handle: THandle read GetHandle;
  end; { IOmniResourceCount }

  ///<summary>Kind of an inverse semaphore. Gets signalled when count drops to 0.
  ///   Allocate decrements the count (and blocks if initial count is 0), Release
  ///   increments the count.</summary>
  ///<since>2009-12-30</since>
  TOmniResourceCount = class(TInterfacedObject, IOmniResourceCount)
  strict private
    orcAvailable   : TDSiEventHandle;
    orcHandle      : TDSiEventHandle;
    orcLock        : TOmniCS;
    orcNumResources: TGp4AlignedInt;
  protected
    function GetHandle: THandle;
  public
    constructor Create(initialCount: cardinal);
    destructor  Destroy; override;
    function  Allocate: cardinal; inline;
    function  Release: cardinal;
    function  TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal = 0): boolean;
    property Handle: THandle read GetHandle;
  end; { TOmniResourceCount }

  IOmniCancellationToken = interface ['{5946F4E8-45C0-4E44-96AB-DBE2BE66A701}']
    function  GetHandle: THandle;
  //
    procedure Clear;
    function  IsSignalled: boolean;
    procedure Signal;
    property Handle: THandle read GetHandle;
  end; { IOmniCancellationToken }

  {$IFDEF OTL_Generics}
  Atomic<T> = class
    type TFactory = reference to function: T;
    class function Initialize(var storage: T; factory: TFactory): T; overload;
    {$IFDEF OTL_ERTTI}
    class function Initialize(var storage: T): T; overload;
    {$ENDIF OTL_ERTTI}
  end; { Atomic<T> }

  Locked<T> = record
  strict private // keep those aligned!
    FLock     : TOmniCS;
    FValue    : T;
  strict private
    FInitialized: boolean;
    FLifecycle  : IInterface;
    FLockCount  : integer;
    FOwnsObject : boolean;
    procedure Clear; inline;
    function  GetValue: T; inline;
  public
    type TFactory = reference to function: T;
    constructor Create(const value: T; ownsObject: boolean = true);
    class operator Implicit(const value: Locked<T>): T; inline;
    class operator Implicit(const value: T): Locked<T>; inline;
    function  Initialize(factory: TFactory): T; overload;
    {$IFDEF OTL_ERTTI}
    function  Initialize: T; overload;
    {$ENDIF OTL_ERTTI}
    procedure Acquire; inline;
    procedure Release; inline;
    procedure Free; inline;
    property Value: T read GetValue;
  end; { Locked<T> }
  {$ENDIF OTL_Generics}

function CreateOmniCriticalSection: IOmniCriticalSection;
function CreateOmniCancellationToken: IOmniCancellationToken;
function CreateResourceCount(initialCount: integer): IOmniResourceCount;

function NInterlockedExchangeAdd(var addend; value: NativeInt): NativeInt;

function CAS8(const oldValue, newValue: byte; var destination): boolean;
function CAS16(const oldValue, newValue: word; var destination): boolean;
function CAS32(const oldValue, newValue: cardinal; var destination): boolean; overload;
{$IFNDEF CPUX64}
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean; overload;
{$ENDIF ~CPUX64}
function CAS64(const oldData, newData: int64; var destination): boolean; overload;

function CAS(const oldValue, newValue: NativeInt; var destination): boolean; overload;
function CAS(const oldValue, newValue: pointer; var destination): boolean; overload;
function CAS(const oldData: pointer; oldReference: NativeInt; newData: pointer;
  newReference: NativeInt; var destination): boolean; overload;

{$IFNDEF CPUX64}
procedure Move64(var Source, Destination); overload;
procedure Move64(newData: pointer; newReference: cardinal; var Destination); overload;
{$ENDIF ~CPUX64}

procedure Move128(var Source, Destination);
procedure MoveDPtr(var Source, Destination); overload;
procedure MoveDPtr(newData: pointer; newReference: NativeInt; var Destination); overload;

function GetThreadId: NativeInt;
function GetCPUTimeStamp: int64;

var
  GOmniCancellationToken: IOmniCancellationToken;

implementation

uses
  Windows,
  SysUtils,
  TypInfo;

type
  TOmniCriticalSection = class(TInterfacedObject, IOmniCriticalSection)
  strict private
    ocsCritSect : TSynchroObject;
    ocsLockCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Acquire; inline;
    function  GetLockCount: integer;
    function  GetSyncObj: TSynchroObject;
    procedure Release; inline;
  end; { TOmniCriticalSection }

  TOmniCancellationToken = class(TInterfacedObject, IOmniCancellationToken)
  private
    octEvent     : TDSiEventHandle;
    octIsSignalled: boolean;
  protected
    function  GetHandle: THandle; inline;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear; inline;
    function  IsSignalled: boolean; inline;
    procedure Signal; inline;
    property Handle: THandle read GetHandle;
  end; { TOmniCancellationToken }

{ exports }

function CreateOmniCriticalSection: IOmniCriticalSection;
begin
  Result := TOmniCriticalSection.Create;
end; { CreateOmniCriticalSection }

function CreateOmniCancellationToken: IOmniCancellationToken;
begin
  Result := TOmniCancellationToken.Create;
end; { CreateOmniCancellationToken }

function CreateResourceCount(initialCount: integer): IOmniResourceCount;
begin
  Result := TOmniResourceCount.Create(initialCount);
end; { CreateResourceCount }

function CAS8(const oldValue, newValue: byte; var destination): boolean;
asm
{$IFDEF CPUX64}
  .noframe
  mov   al, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], dl
  setz  al
end; { CAS8 }

function CAS16(const oldValue, newValue: word; var destination): boolean;
asm
{$IFDEF CPUX64}
  .noframe
  mov     ax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], dx
  setz  al
end; { CAS16 }

function CAS32(const oldValue, newValue: cardinal; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
  .noframe
  // TODO 1 : Gorazd, tole spodaj ne dela!
//  mov   rax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], edx
  setz  al
end; { CAS32 }

{$IFNDEF CPUX64}
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean; overload;
asm
//{$IFDEF CPUX64}
//  .noframe
//  mov    rax, oldValue
//{$ENDIF CPUX64}
  lock cmpxchg [destination], edx
  setz  al
end; { CAS32 }
{$ENDIF ~CPUX64}

function CAS64(const oldData, newData: int64; var destination): boolean; overload;
asm
{$IFNDEF CPUX64}
  push  edi
  push  ebx
  mov   edi, destination
  mov   ebx, low newData
  mov   ecx, high newData
  mov   eax, low oldData
  mov   edx, high oldData
  lock cmpxchg8b [edi]
  pop   ebx
  pop   edi
{$ELSE CPUX64}
  .noframe
  mov   rax, oldData
  lock cmpxchg [destination], newData
{$ENDIF ~CPUX64}
  setz  al
end; { CAS64 }

function CAS(const oldValue, newValue: NativeInt; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
  .noframe
  mov   rax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], newValue
  setz  al
end; { CAS }

function CAS(const oldValue, newValue: pointer; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
  .noframe
  mov   rax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], newValue
  setz  al
end; { CAS }

//eighter 8-byte or 16-byte CAS, depending on the platform; destination must be propely aligned (8- or 16-byte)
function CAS(const oldData: pointer; oldReference: NativeInt; newData: pointer;
  newReference: NativeInt; var destination): boolean; overload;
asm
{$IFNDEF CPUX64}
  push  edi
  push  ebx
  mov   ebx, newData
  mov   ecx, newReference
  mov   edi, destination
  lock cmpxchg8b qword ptr [edi]
  pop   ebx
  pop   edi
{$ELSE CPUX64}
  .noframe
  push  rbx
  mov   rax, oldData
  mov   rbx, newData
  mov   rcx, newReference
  mov   r8, [rsp + $30]
  lock cmpxchg16b [r8]
  pop   rbx
{$ENDIF CPUX64}
  setz  al
end; { CAS }

{$IFNDEF CPUX64}
procedure Move64(var Source, Destination); overload;
//Move 8 bytes atomicly from Source 8-byte aligned to Destination!
asm
  movq  xmm0, qword [Source]
  movq  qword [Destination], xmm0
end;

procedure Move64(newData: pointer; newReference: cardinal; var Destination); overload;
//Move 8 bytes atomically into 8-byte Destination!
asm
  movd  xmm0, eax
  movd  xmm1, edx
  punpckldq xmm0, xmm1
  movq  qword [Destination], xmm0
end; { Move64 }
{$ENDIF ~CPUX64}

procedure Move128(var Source, Destination);
//Move 16 bytes atomically from Source to 16-byte aligned to Destination!
asm
{$IFNDEF CPUX64}
  movdqa  xmm0, dqword [Source]
  movdqa  dqword [Destination], xmm0
{$ELSE CPUX64}
  .noframe
//Move 16 bytes atomically into 16-byte Destination!
  push  rbx
  mov   r8, destination
  mov   rbx, [Source]
  mov   rcx, [Source + 8]
  mov   rax, [r8]
  mov   rdx, [r8 + 8]
@repeat:
  lock  cmpxchg16B [r8]
  jnz   @repeat
  pop   rbx
{$ENDIF CPUX64}
end; { Move128 }

//eighter 8-byte or 16-byte atomic Move, depending on the platform; destination must be propely aligned (8- or 16-byte)
procedure MoveDPtr(newData: pointer; newReference: NativeInt; var Destination); overload;
asm
{$IFNDEF CPUX64}
  movd  xmm0, eax
  movd  xmm1, edx
  punpckldq xmm0, xmm1
  movq  qword [Destination], xmm0
  mov   eax, [eax + $24]
{$ELSE CPUX64}
  .noframe
//Move 16 bytes atomically into 16-byte Destination!
  push  rbx
  mov   r8, destination
  mov   rbx, newData
  mov   rcx, newReference
  mov   rax, [r8]
  mov   rdx, [r8 + 8]
@repeat:
  lock  cmpxchg16B [r8]
  jnz   @repeat
  pop   rbx
{$ENDIF CPUX64}
end; { MoveDPtr }

//eighter 8-byte or 16-byte atomic Move, depending on the platform; destination must be propely aligned (8- or 16-byte)
procedure MoveDPtr(var Source, Destination);
asm
{$IFNDEF CPUX64}
//Move 8 bytes atomically from Source 8-byte aligned to Destination!
  movq  xmm0, qword [Source]
  movq  qword [Destination], xmm0
{$ELSE CPUX64}
  .noframe
//Move 16 bytes atomically into 16-byte Destination!
  push  rbx
  mov   r8, destination
  mov   rbx, [Source]
  mov   rcx, [Source + 8]
  mov   rax, [r8]
  mov   rdx, [r8 + 8]
@repeat:
  lock  cmpxchg16B [r8]
  jnz   @repeat
  pop   rbx
{$ENDIF CPUX64}
end;

function GetThreadId: NativeInt;
//result := GetCurrentThreadId;
asm
{$IFNDEF CPUX64}
  mov   eax, fs:[$18]      //eax := thread information block
  mov   eax, [eax + $24]   //eax := thread id
{$ELSE CPUX64}
  mov   rax, gs:[abs $30]
  mov   eax, [rax + $48]
{$ENDIF CPUX64}
end; { GetThreadId }

function GetCPUTimeStamp: int64;
asm
{$IFDEF CPUX64}
  .noframe
{$ENDIF CPUX64}
  rdtsc
{$IFDEF CPUX64}
  shl   rdx, 32
  or    rax, rdx
{$ENDIF CPUX64}
end; { GetCPUTimeStamp }

function NInterlockedExchangeAdd(var addend; value: NativeInt): NativeInt;
asm
{$IFNDEF CPUX64}
  lock  xadd [addend], value
{$ELSE CPUX64}
  .noframe
  lock  xadd [addend], value
  mov   rax, value
{$ENDIF CPUX64}
end; { NInterlockedExchangeAdd }

{$IFNDEF OTL_HasInterlockedCompareExchangePointer}
function InterlockedCompareExchangePointer(var destination: pointer; exchange: pointer;
  comparand: pointer): pointer;
begin
  Result := pointer(InterlockedCompareExchange(integer(destination), integer(exchange),
    integer(comparand)));
end; { InterlockedCompareExchangePointer }

{$ENDIF OTL_HasInterlockedCompareExchangePointer}

procedure MFence; assembler;
asm
  mfence
end; { MFence }

{ TOmniCS }

procedure TOmniCS.Acquire;
begin
  Initialize;
  ocsSync.Acquire;
end; { TOmniCS.Acquire }

function TOmniCS.GetLockCount: integer;
begin
  Result := ocsSync.LockCount;
end; { TOmniCS.GetLockCount }

function TOmniCS.GetSyncObj: TSynchroObject;
begin
  Initialize;
  Result := ocsSync.GetSyncObj;
end; { TOmniCS.GetSyncObj }

procedure TOmniCS.Initialize;
var
  syncIntf: IOmniCriticalSection;
begin
  Assert(cardinal(@ocsSync) mod SizeOf(pointer) = 0, 'TOmniCS.Initialize: ocsSync is not properly aligned!');
  Assert(cardinal(@syncIntf) mod SizeOf(pointer) = 0, 'TOmniCS.Initialize: syncIntf is not properly aligned!');
  if not assigned(ocsSync) then begin
    syncIntf := CreateOmniCriticalSection;
    if CAS(nil, pointer(syncIntf), ocsSync) then
      pointer(syncIntf) := nil;
  end;
end; { TOmniCS.Initialize }

procedure TOmniCS.Release;
begin
  ocsSync.Release;
end; { TOmniCS.Release }

{ TOmniCriticalSection }

constructor TOmniCriticalSection.Create;
begin
  ocsCritSect := TFixedCriticalSection.Create;
end; { TOmniCriticalSection.Create }

destructor TOmniCriticalSection.Destroy;
begin
  FreeAndNil(ocsCritSect);
end; { TOmniCriticalSection.Destroy }

procedure TOmniCriticalSection.Acquire;
begin
  ocsCritSect.Acquire;
  Inc(ocsLockCount);
end; { TOmniCriticalSection.Acquire }

function TOmniCriticalSection.GetLockCount: integer;
begin
  Result := ocsLockCount;
end; { TOmniCriticalSection.GetLockCount }

function TOmniCriticalSection.GetSyncObj: TSynchroObject;
begin
  Result := ocsCritSect;
end; { TOmniCriticalSection.GetSyncObj }

procedure TOmniCriticalSection.Release;
begin
  ocsCritSect.Release;
  Dec(ocsLockCount);
end; { TOmniCriticalSection.Release }

{ TOmniCancellationToken }

constructor TOmniCancellationToken.Create;
begin
  octEvent := CreateEvent(nil, true, false, nil);
end; { TOmniCancellationToken.Create }

destructor TOmniCancellationToken.Destroy;
begin
  DSiCloseHandleAndNull(octEvent);
end; { TOmniCancellationToken.Destroy }

procedure TOmniCancellationToken.Clear;
begin
  octIsSignalled := false;
  ResetEvent(octEvent);
end; { TOmniCancellationToken.Clear }

function TOmniCancellationToken.GetHandle: THandle;
begin
  Result := octEvent;
end; { TOmniCancellationToken.GetHandle }

function TOmniCancellationToken.IsSignalled: boolean;
begin
  Result := octIsSignalled;
end; { TOmniCancellationToken.IsSignalled }

procedure TOmniCancellationToken.Signal;
begin
  octIsSignalled := true;
  SetEvent(octEvent);
end; { TOmniCancellationToken.Signal }

{ TOmniMREW }

procedure TOmniMREW.EnterReadLock;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := omrewReference AND NOT 1;
  until CAS(currentReference, currentReference + 2, omrewReference);
end; { TOmniMREW.EnterReadLock }

procedure TOmniMREW.EnterWriteLock;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := omrewReference AND NOT 1;
  until CAS(currentReference, currentReference + 1, omrewReference);
  //Now wait on all readers
  repeat
  until omrewReference = 1;
end; { TOmniMREW.EnterWriteLock }

procedure TOmniMREW.ExitReadLock;
begin
  //Decrease omrewReference
  NInterlockedExchangeAdd(omrewReference, -2);
end; { TOmniMREW.ExitReadLock }

procedure TOmniMREW.ExitWriteLock;
begin
  omrewReference := 0;
end; { TOmniMREW.ExitWriteLock }

{ TOmniResourceCount }

constructor TOmniResourceCount.Create(initialCount: cardinal);
begin
  inherited Create;
  orcHandle := CreateEvent(nil, true, (initialCount = 0), nil);
  orcAvailable := CreateEvent(nil, true, (initialCount <> 0), nil);
  orcNumResources.Value := initialCount;
end; { TOmniResourceCount.Create }

destructor TOmniResourceCount.Destroy;
begin
  DSiCloseHandleAndNull(orcHandle);
  DSiCloseHandleAndNull(orcAvailable);
  inherited;
end; { TOmniResourceCount.Destroy }

///<summary>Allocates resource and returns number of remaining resources.
///  If the initial number of resources is 0, then the call will block until a resource
///  becomes available.
///  If there are no remaining resources (Result is 0), sets externally visible event.
///</summary>
function TOmniResourceCount.Allocate: cardinal;
begin
  TryAllocate(Result, INFINITE);
end; { TOmniResourceCount.Allocate }

function TOmniResourceCount.GetHandle: THandle;
begin
  Result := orcHandle;
end; { TOmniResourceCount.GetHandle }

///<summary>Releases resource and returns number of remaining resources.
///  Resets the externally visible event if necessary.
///</summary>
function TOmniResourceCount.Release: cardinal;
begin
  orcLock.Acquire;
  try
    Result := cardinal(orcNumResources.Increment);
    if Result = 1 then begin
      ResetEvent(orcHandle);
      SetEvent(orcAvailable);
    end;
  finally orcLock.Release; end;
end; { TOmniResourceCount.Release }

///<summary>Like Allocate, but with a timeout.</summary>
function TOmniResourceCount.TryAllocate(var resourceCount: cardinal;
  timeout_ms: cardinal): boolean;
var
  startTime_ms: int64;
  waitTime_ms : int64;
begin
  Result := false;
  startTime_ms := DSiTimeGetTime64;
  orcLock.Acquire;
  repeat
    if orcNumResources.Value = 0 then begin
      orcLock.Release;
      if timeout_ms <= 0 then
        Exit;
      if timeout_ms = INFINITE then
        waitTime_ms := INFINITE
      else begin
        waitTime_ms := startTime_ms + timeout_ms - DSiTimeGetTime64;
        if waitTime_ms <= 0 then
          Exit;
      end;
      if WaitForSingleObject(orcAvailable, waitTime_ms) <> WAIT_OBJECT_0 then
        Exit; // skip final Release
      orcLock.Acquire;
    end;
    if orcNumResources.Value > 0 then begin
      Result := true;
      resourceCount := cardinal(orcNumResources.Decrement);
      if resourceCount = 0 then begin
        orcLock.Release; //prevent race condition - another thread may wait on orcHandle and destroy this instance
        ResetEvent(orcAvailable);
        SetEvent(orcHandle);
        Exit; // skip final Release
      end;
      break; //repeat
    end;
  until false;
  orcLock.Release; 
end; { TOmniResourceCount.TryAllocate }

{$IFDEF OTL_Generics}
{ Atomic<T> }

class function Atomic<T>.Initialize(var storage: T; factory: TFactory): T;
var
  interlockRes: pointer;
  tmpT        : T;
begin
  if not assigned(PPointer(@storage)^) then begin
    Assert(cardinal(@storage) mod SizeOf(pointer) = 0, 'Atomic<T>.Initialize: storage is not properly aligned!');
    Assert(cardinal(@tmpT) mod SizeOf(pointer) = 0, 'Atomic<T>.Initialize: tmpT is not properly aligned!');
    tmpT := factory();
    interlockRes := InterlockedCompareExchangePointer(PPointer(@storage)^, PPointer(@tmpT)^, nil);
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkInterface:
        if interlockRes = nil then
          PPointer(@tmpT)^ := nil;
      tkClass:
        if interlockRes <> nil then
          TObject(PPointer(@tmpT)^).Free;
      else
        raise Exception.Create('Atomic<T>.Initialize: Unsupported type');
    end; //case
  end;
  Result := storage;
end; { Atomic<T>.Initialize }

{$IFDEF OTL_ERTTI}
class function Atomic<T>.Initialize(var storage: T): T;
begin
  if not assigned(PPointer(@storage)^) then begin
    if PTypeInfo(TypeInfo(T))^.Kind  <> tkClass then
      raise Exception.Create('Atomic<T>.Initialize: Unsupported type');
    Result := Atomic<T>.Initialize(storage,
      function: T
      var
        aMethCreate : TRttiMethod;
        instanceType: TRttiInstanceType;
        ctx         : TRttiContext;
        resValue    : TValue;
        rType       : TRttiType;
      begin
        ctx := TRttiContext.Create;
        rType := ctx.GetType(TypeInfo(T));
        for aMethCreate in rType.GetMethods do begin
          if (aMethCreate.IsConstructor) and (Length(aMethCreate.GetParameters) = 0) then begin
            instanceType := rType.AsInstance;
            resValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
            Result := resValue.AsType<T>;
            break; //for
          end;
        end; //for
      end);
  end;
end; { Atomic<T>.Initialize }
{$ENDIF OTL_ERTTI}

{ Locked<T> }

constructor Locked<T>.Create(const value: T; ownsObject: boolean);
begin
  Clear;
  FValue := value;
  if ownsObject and (PTypeInfo(TypeInfo(T))^.Kind = tkClass) then
    FLifecycle := AutoDestroyObject(TObject(PPointer(@value)^));
  FInitialized := true;
  FLockCount := 0;
end; { Locked<T>.Create }

class operator Locked<T>.Implicit(const value: Locked<T>): T;
begin
  Result := value.Value;
end; { Locked<T>.Implicit }

class operator Locked<T>.Implicit(const value: T): Locked<T>;
begin
  Result := Locked<T>.Create(value);
end; { Locked<T>.Implicit }

procedure Locked<T>.Acquire;
begin
  FLock.Acquire;
end; { Locked<T>.Acquire }

procedure Locked<T>.Clear;
begin
  FLifecycle := nil;
  FInitialized := false;
  FValue := Default(T);
  FOwnsObject := false;
end; { Locked }

procedure Locked<T>.Free;
begin
  if (PTypeInfo(TypeInfo(T))^.Kind = tkClass) then
    TObject(PPointer(@FValue)^).Free;
  Clear;
end; { Locked<T>.Free }

function Locked<T>.GetValue: T;
begin
  Assert(FLock.LockCount > 0, 'Locked<T>.GetValue: Not locked');
  Result := FValue;
end; { Locked<T>.GetValue }

function Locked<T>.Initialize(factory: TFactory): T;
begin
  if not FInitialized then begin
    Acquire;
    try
      if not FInitialized then begin
        FValue := factory();
        //MFence; // not needed on x86 and x64, see comments to http://www.thedelphigeek.com/2011/12/on-optimistic-and-pessimistic.html
        FInitialized := true;
      end;
    finally Release; end;
  end;
  Result := FValue;
end; { Locked<T>.Initialize }

{$IFDEF OTL_ERTTI}
function Locked<T>.Initialize: T;
begin
  if not FInitialized then begin
    if PTypeInfo(TypeInfo(T))^.Kind  <> tkClass then
      raise Exception.Create('Locked<T>.Initialize: Unsupported type');
    Result := Initialize(
      function: T
      var
        aMethCreate : TRttiMethod;
        instanceType: TRttiInstanceType;
        ctx         : TRttiContext;
        resValue    : TValue;
        rType       : TRttiType;
      begin
        ctx := TRttiContext.Create;
        rType := ctx.GetType(TypeInfo(T));
        for aMethCreate in rType.GetMethods do begin
          if (aMethCreate.IsConstructor) and (Length(aMethCreate.GetParameters) = 0) then begin
            instanceType := rType.AsInstance;
            resValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
            Result := resValue.AsType<T>;
            break; //for
          end;
        end; //for
      end);
  end;
end; { Locked<T>.Initialize }
{$ENDIF OTL_ERTTI}

procedure Locked<T>.Release;
begin
  FLock.Release;
end; { Locked<T>.Release }

{$ENDIF OTL_Generics}

initialization
  GOmniCancellationToken := CreateOmniCancellationToken;
end.
