///<summary>Synchronisation primitives. Part of the OmniThreadLibrary project.</summary>
///<remarks>Move* family of functions require Pentium 4 processor (or newer).</remarks>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2014, Primoz Gabrijelcic
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
///   Last modification : 2014-16-11
///   Version           : 1.18
///</para><para>
///   History:
///     1.18: 2014-16-11
///       - Finalized TWaitFor implementation.
///     1.19: 2014-11-04
///       - TWaitForAll renamed to TWaitFor.
///       - TWaitFor.Wait renamed to TWaitFor.WaitAll.
///       - Implemented TWaitFor.MsgWaitAny and .WaitAny.
///       - Implemented WaitForAnyObject.
///     1.18: 2014-11-03
///       - Implemented WaitForAllObjects and TWaitForAll class.
///     1.17: 2014-01-11
///       - Implemented TOmniMREW.TryEnterReadLock and TryEnterWriteLock.
///     1.16: 2014-01-09
///       - Locked<T>.Free can be called if Locked<T> owns its Value.
///     1.15: 2013-03-05
///       - TOmniLockManager<K> is reentrant.
///     1.14: 2013-02-27
///       - Implemented TOmniLockManager<K> and IOmniLockManager<K>.
///     1.13a: 2013-01-08
///       - Locked<T>.Free must execute in locked context.
///     1.13: 2012-02-21
///       - Implemented Locked<T>.Locked.
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
  SysUtils,
  SyncObjs,
  Classes,
  {$IFDEF OTL_Generics}
  Generics.Defaults,
  Generics.Collections,
  {$ENDIF OTL_Generics}
  DSiWin32,
  {$IFDEF OTL_ERTTI}
  RTTI,
  {$ENDIF OTL_ERTTI}
  GpStuff,
  GpLists;

type
{$IF CompilerVersion < 23} //pre-XE2
  NativeInt = integer;
  PNativeInt = PInteger;
{$IFEND}

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
    //Treated as an integer, IInterface is only used to provide automatic initialization to 0.
    //Bit0 is 'writing in progress' flag.
    omrewReference: IInterface;
  public
    procedure EnterReadLock; inline;
    procedure EnterWriteLock; inline;
    procedure ExitReadLock; inline;
    procedure ExitWriteLock; inline;
    function  TryEnterReadLock: boolean; inline;
    function  TryEnterWriteLock: boolean; inline;
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
    FOwnsObject : boolean;
    procedure Clear; inline;
    function  GetValue: T; inline;
  public
    type TFactory = reference to function: T;
    type TProcT = reference to procedure(const value: T);
    constructor Create(const value: T; ownsObject: boolean = true);
    class operator Implicit(const value: Locked<T>): T; inline;
    class operator Implicit(const value: T): Locked<T>; inline;
    function  Initialize(factory: TFactory): T; overload;
    {$IFDEF OTL_ERTTI}
    function  Initialize: T; overload;
    {$ENDIF OTL_ERTTI}
    procedure Acquire; inline;
    procedure Locked(proc: TProc); overload; inline;
    procedure Locked(proc: TProcT); overload; inline;
    procedure Release; inline;
    procedure Free; inline;
    property Value: T read GetValue;
  end; { Locked<T> }

  IOmniLockManagerAutoUnlock = interface
    procedure Unlock;
  end; { IOmniLockManagerAutoUnlock }

  IOmniLockManager<K> = interface
    function  Lock(const key: K; timeout_ms: cardinal): boolean;
    function  LockUnlock(const key: K; timeout_ms: cardinal): IOmniLockManagerAutoUnlock;
    function  Unlock(const key: K): boolean;
  end; { IOmniLockManager<K> }

  TOmniLockManager<K> = class(TInterfacedObject, IOmniLockManager<K>)
  strict private type
    TNotifyPair = class(TGpDoublyLinkedListObject)
      Key   : K;
      Notify: TDSiEventHandle;
      constructor Create(const aKey: K; aNotify: TDSiEventHandle);
    end;
    TLockValue = record
      LockCount: integer;
      ThreadID : cardinal;
      constructor Create(aThreadID: cardinal; aLockCount: integer);
    end;
  strict private
    FComparer  : IEqualityComparer<K>;
    FLock      : TOmniCS;
    FLockList  : TDictionary<K,TLockValue>;
    FNotifyList: TGpDoublyLinkedList;
  strict private type
    TAutoUnlock = class(TInterfacedObject, IOmniLockManagerAutoUnlock)
    strict private
      FUnlockProc: TProc;
    public
      constructor Create(unlockProc: TProc);
      destructor  Destroy; override;
      procedure Unlock;
    end;
  public
    class function CreateInterface(capacity: integer = 0): IOmniLockManager<K>; overload;
    class function CreateInterface(comparer: IEqualityComparer<K>; capacity: integer = 0):
      IOmniLockManager<K>; overload;
    constructor Create(capacity: integer = 0); overload;
    constructor Create(const comparer: IEqualityComparer<K>; capacity: integer = 0); overload;
    destructor  Destroy; override;
    function  Lock(const key: K; timeout_ms: cardinal): boolean;
    function  LockUnlock(const key: K; timeout_ms: cardinal): IOmniLockManagerAutoUnlock;
    function  Unlock(const key: K): boolean;
  end; { TOmniLockManager<K> }
  {$ENDIF OTL_Generics}

  ///<summary>Waits on any/all from any number of handles.</summary>
  ///  Don't use it to wait on mutexes!
  ///  http://joeduffyblog.com/2007/05/13/registerwaitforsingleobject-and-mutexes-dont-mix/
  TWaitFor = class
  protected type //must be visible from the callback
    TWaiter = class
    strict private
      FIdxHandle: integer;
      FOwner    : TWaitFor;
      FSignalled: boolean;
    public
      constructor Create(owner: TWaitFor; idxHandle: integer);
      procedure Awaited;
      property Index: integer read FIdxHandle;
      property Signalled: boolean read FSignalled write FSignalled;
    end;
  public type
    TWaitResult = (
      waAwaited,      // WAIT_OBJECT_0 .. WAIT_OBJECT_n
      waTimeout,      // WAIT_TIMEOUT
      waFailed,       // WAIT_FAILED
      waIOCompletion  // WAIT_IO_COMPLETION
    );
    THandleInfo = record
      Index: integer;
    end;
    THandles = TArray<THandleInfo>;
  strict private
    FAwaitedLock     : TOmniCS;
    FHandles         : array of THandle;
    FIdxSignalled    : integer;
    FResourceCount   : IOmniResourceCount;
    FSignal          : TDSiEventHandle;
    FSignalledHandles: THandles;
    FWaitHandles     : TGpInt64ObjectList;
  strict protected
    function  MapToResult(winResult: cardinal): TWaitResult;
    procedure RegisterWaitHandles(extraFlags: cardinal);
    procedure UnregisterWaitHandles;
  protected //must be visible from the callback
    procedure Awaited_Asy(idxHandle: integer);
  public
    constructor Create; overload;
    constructor Create(const handles: array of THandle); overload;
    destructor  Destroy; override;
    function  MsgWaitAny(timeout_ms, wakeMask, flags: cardinal): TWaitResult;
    procedure SetHandles(const handles: array of THandle);
    function  WaitAll(timeout_ms: cardinal): TWaitResult;
    function  WaitAny(timeout_ms: cardinal; alertable: boolean = false): TWaitResult;
    property Signalled: THandles read FSignalledHandles;
  end; { TWaitForAll }

function CreateOmniCriticalSection: IOmniCriticalSection;
function CreateOmniCancellationToken: IOmniCancellationToken;
function CreateResourceCount(initialCount: integer): IOmniResourceCount;

procedure NInterlockedExchangeAdd(var addend; value: NativeInt);

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

///<summary>Waits on any number of handles.</summary>
///<returns>True on success, False on timeout.</returns>
function WaitForAllObjects(const handles: array of THandle; timeout_ms: cardinal): boolean;

function GetThreadId: NativeInt;
function GetCPUTimeStamp: int64;

var
  GOmniCancellationToken: IOmniCancellationToken;
  CASAlignment: integer; //required alignment for the CAS function - 8 or 16, depending on the platform

implementation

uses
  Windows,
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
    octEvent      : TDSiEventHandle;
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
  mov   al, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], dl
  setz  al
end; { CAS8 }

function CAS16(const oldValue, newValue: word; var destination): boolean;
asm
{$IFDEF CPUX64}
  mov     ax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], dx
  setz  al
end; { CAS16 }

function CAS32(const oldValue, newValue: cardinal; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
  mov   eax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], edx
  setz  al
end; { CAS32 }

{$IFNDEF CPUX64}
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean; overload;
asm
//{$IFDEF CPUX64}
  mov    eax, oldValue
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
  mov   rax, oldData
  lock cmpxchg [destination], newData
{$ENDIF ~CPUX64}
  setz  al
end; { CAS64 }

function CAS(const oldValue, newValue: NativeInt; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
  mov   rax, oldValue
{$ENDIF CPUX64}
  lock cmpxchg [destination], newValue
  setz  al
end; { CAS }

function CAS(const oldValue, newValue: pointer; var destination): boolean; overload;
asm
{$IFDEF CPUX64}
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
  push  rbx                     //rsp := rsp - 8 !
  mov   rax, oldData
  mov   rbx, newData
  mov   rcx, newReference
  mov   r8, [destination + 8]   //+8 with respect to .noframe
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
  rdtsc
{$IFDEF CPUX64}
  shl   rdx, 32
  or    rax, rdx
{$ENDIF CPUX64}
end; { GetCPUTimeStamp }

procedure NInterlockedExchangeAdd(var addend; value: NativeInt);
asm
  lock  xadd [addend], value
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

function WaitForAllObjects(const handles: array of THandle; timeout_ms: cardinal): boolean;
var
  waiter: TWaitFor;
begin
  waiter := TWaitFor.Create(handles);
  try
    Result := (waiter.WaitAll(timeout_ms) = waAwaited);
  finally FreeAndNil(waiter); end;
end; { WaitForAllObjects }

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
    currentReference := NativeInt(omrewReference) AND NOT 1;
  until CAS(currentReference, currentReference + 2, NativeInt(omrewReference));
end; { TOmniMREW.EnterReadLock }

procedure TOmniMREW.EnterWriteLock;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := NativeInt(omrewReference) AND NOT 1;
  until CAS(currentReference, currentReference + 1, NativeInt(omrewReference));
  //Now wait on all readers
  repeat
  until NativeInt(omrewReference) = 1;
end; { TOmniMREW.EnterWriteLock }

procedure TOmniMREW.ExitReadLock;
begin
  //Decrease omrewReference
  NInterlockedExchangeAdd(NativeInt(omrewReference), -2);
end; { TOmniMREW.ExitReadLock }

procedure TOmniMREW.ExitWriteLock;
begin
  NativeInt(omrewReference) := 0;
end; { TOmniMREW.ExitWriteLock }

function TOmniMREW.TryEnterReadLock: boolean;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  currentReference := NativeInt(omrewReference) AND NOT 1;
  Result := CAS(currentReference, currentReference + 2, NativeInt(omrewReference));
end; { TOmniMREW.TryEnterReadLock }

function TOmniMREW.TryEnterWriteLock: boolean;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  currentReference := NativeInt(omrewReference) AND NOT 1;
  Result := CAS(currentReference, currentReference + 1, NativeInt(omrewReference));
  if Result then
    //Now wait on all readers
    repeat
    until NativeInt(omrewReference) = 1;
end; { TOmniMREW.TryEnterWriteLock }

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
  FLock.Initialize;
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
  if FInitialized then begin
    Acquire;
    try
      if assigned(FLifecycle) then
        Clear
      else if FInitialized then begin
        if (PTypeInfo(TypeInfo(T))^.Kind = tkClass) then
          TObject(PPointer(@FValue)^).Free;
        Clear;
      end;
    finally Release; end;
  end;
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
        params      : TArray<TRttiParameter>;
        resValue    : TValue;
        rType       : TRttiType;
      begin
        ctx := TRttiContext.Create;
        rType := ctx.GetType(TypeInfo(T));
        for aMethCreate in rType.GetMethods do begin
          if aMethCreate.IsConstructor then begin
            params := aMethCreate.GetParameters;
            if Length(params) = 0 then begin
              instanceType := rType.AsInstance;
              resValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
              Result := resValue.AsType<T>;
              break; //for
            end;
          end;
        end; //for
      end);
  end;
end; { Locked<T>.Initialize }

{$ENDIF OTL_ERTTI}

procedure Locked<T>.Locked(proc: TProc);
begin
  Acquire;
  try
    proc;
  finally Release; end;
end; { Locked<T>.Locked }

procedure Locked<T>.Locked(proc: TProcT);
begin
  Acquire;
  try
    proc(Value);
  finally Release; end;
end; { Locked<T>.Locked }

procedure Locked<T>.Release;
begin
  FLock.Release;
end; { Locked<T>.Release }

{ TOmniLockManager<K>.TNotifyPair<K> }

constructor TOmniLockManager<K>.TNotifyPair.Create(const aKey: K; aNotify: TDSiEventHandle);
begin
  inherited Create;
  Key := aKey;
  Notify := aNotify;
end; { TOmniLockManager<K>.TNotifyPair.Create }

{ TOmniLockManager<K>.TAutoUnlock }

constructor TOmniLockManager<K>.TAutoUnlock.Create(unlockProc: TProc);
begin
  inherited Create;
  FUnlockProc := unlockProc;
end; { TOmniLockManager<K>.TAutoUnlock.Create }

destructor TOmniLockManager<K>.TAutoUnlock.Destroy;
begin
  Unlock;
  inherited;
end; { TOmniLockManager<K>.TAutoUnlock.Destroy }

procedure TOmniLockManager<K>.TAutoUnlock.Unlock;
begin
  if assigned(FUnlockProc) then
    FUnlockProc;
  FUnlockProc := nil;
end; { TOmniLockManager<K>.TAutoUnlock.Unlock }

{ TOmniLockManager<K>.TLockValue }

constructor TOmniLockManager<K>.TLockValue.Create(aThreadID: cardinal; aLockCount: integer);
begin
  ThreadID := aThreadID;
  LockCount := aLockCount;
end; { TOmniLockManager<K>.TLockValue }

{ TOmniLockManager<K> }

class function TOmniLockManager<K>.CreateInterface(comparer: IEqualityComparer<K>;
  capacity: integer): IOmniLockManager<K>;
begin
  Result := TOmniLockManager<K>.Create(comparer, capacity);
end; { TOmniLockManager<K>.CreateInterface }

class function TOmniLockManager<K>.CreateInterface(capacity: integer):
  IOmniLockManager<K>;
begin
  Result := TOmniLockManager<K>.Create(capacity);
end; { TOmniLockManager<K>.CreateInterface }

constructor TOmniLockManager<K>.Create(const comparer: IEqualityComparer<K>;
  capacity: integer);
begin
  inherited Create;
  FComparer := comparer;
  if not assigned(FComparer) then
    FComparer := TEqualityComparer<K>.Default;
  FLockList := TDictionary<K,TLockValue>.Create(capacity, FComparer);
  FNotifyList := TGpDoublyLinkedList.Create;
end; { TOmniLockManager }

constructor TOmniLockManager<K>.Create(capacity: integer);
begin
  Create(nil, capacity);
end; { TOmniLockManager }

destructor TOmniLockManager<K>.Destroy;
begin
  FreeAndNil(FLockList);
  FreeAndNil(FNotifyList);
  inherited;
end; { TOmniLockManager }

function TOmniLockManager<K>.Lock(const key: K; timeout_ms: cardinal): boolean;
var
  lockData  : TLockValue;
  lockThread: integer;
  notifyItem: TGpDoublyLinkedListObject;
  startWait : int64;
  waitEvent : TDSiEventHandle;
  wait_ms   : integer;
begin
  Result := false;
  waitEvent := 0;
  startWait := DSiTimeGetTime64;

  repeat
    FLock.Acquire;
    try
      if not FLockList.TryGetValue(key, lockData) then begin
        // Unlocked
        FLockList.Add(key, TLockValue.Create(GetCurrentThreadID, 1));
        Result := true;
        break; //repeat
      end
      else if lockData.ThreadID = GetCurrentThreadID then begin
        // Already locked by this thread, increase the lock count
        Inc(lockData.LockCount);
        FLockList.AddOrSetValue(key, lockData);
        Result := true;
        break; //repeat
      end
      else if waitEvent = 0 then begin
        waitEvent := CreateEvent(nil, false, false, nil);
        FNotifyList.InsertAtTail(TNotifyPair.Create(key, waitEvent));
      end;
    finally FLock.Release; end;
    wait_ms := integer(timeout_ms) - integer(DSiTimeGetTime64 - startWait);
  until ((timeout_ms <> INFINITE) and (wait_ms <= 0)) or
        (WaitForSingleObject(waitEvent, cardinal(wait_ms)) = WAIT_TIMEOUT);

  if waitEvent <> 0 then begin
    FLock.Acquire;
    try
      for notifyItem in FNotifyList do
        if TNotifyPair(notifyItem).Notify = waitEvent then begin
          notifyItem.Free;
          break; //for notifyItem
        end;
      DSiCloseHandleAndNull(waitEvent);
    finally FLock.Release; end;
  end;
end; { TOmniLockManager<K>.Lock }

function TOmniLockManager<K>.LockUnlock(const key: K; timeout_ms: cardinal): IOmniLockManagerAutoUnlock;
begin
  if not Lock(key, timeout_ms) then
    Result := nil
  else
    Result := TAutoUnlock.Create(
      procedure
      begin
        Unlock(key);
      end
    );
end; { TOmniLockManager<K>.LockUnlock }

function TOmniLockManager<K>.Unlock(const key: K): boolean;
var
  lockData  : TLockValue;
  notifyItem: TGpDoublyLinkedListObject;
begin
  FLock.Acquire;
  try
    if not FLockList.TryGetValue(key, lockData) then
      raise Exception.Create('TOmniLockManager<K>.Unlock: Key not locked');
    if lockData.ThreadID <> GetCurrentThreadID then
      raise Exception.Create('TOmniLockManager<K>.Unlock: Key was not locked by the current thread');
    if lockData.LockCount > 1 then begin
      Dec(lockData.LockCount);
      FLockList.AddOrSetValue(key, lockData);
    end
    else begin
      FLockList.Remove(key);
      for notifyItem in FNotifyList do
        if FComparer.Equals(TNotifyPair(notifyItem).Key, key) then begin
          SetEvent(TNotifyPair(notifyItem).Notify);
          break; //for notifyItem
        end;
    end;
  finally FLock.Release; end;
end; { TOmniLockManager<K>.Unlock }

{$ENDIF OTL_Generics}

{ TWaitFor.TWaiter }

constructor TWaitFor.TWaiter.Create(owner: TWaitFor; idxHandle: integer);
begin
  inherited Create;
  FOwner := owner;
  FIdxHandle := idxHandle;
end; { TWaitFor.TWaiter.Create }

procedure TWaitFor.TWaiter.Awaited;
begin
  FOwner.Awaited_Asy(FIdxHandle);
end; { TWaitFor.TWaiter.Awaited }

{ TWaitForAll }

constructor TWaitFor.Create(const handles: array of THandle);
begin
  Create;
  SetHandles(handles);
end; { TWaitFor.Create }

constructor TWaitFor.Create;
begin
  inherited;
  FSignal := CreateEvent(nil, false, false, nil);
  FWaitHandles := TGpInt64ObjectList.Create;
end; { TWaitFor.Create }

destructor TWaitFor.Destroy;
begin
  UnregisterWaitHandles;
  FreeAndNil(FWaitHandles);
  DSiCloseHandleAndNull(FSignal);
  inherited;
end; { TWaitFor.Destroy }

procedure TWaitFor.Awaited_Asy(idxHandle: integer);
var
  waiter: TWaiter;
begin
  FAwaitedLock.Acquire;
  try
    waiter := TWaiter(FWaitHandles.Objects[idxHandle]);
    waiter.Signalled := true;
    if assigned(FResourceCount) then
      FResourceCount.Allocate
    else
      SetEvent(FSignal);
  finally FAwaitedLock.Release; end;
end; { TWaitFor.Awaited_Asy }

function TWaitFor.MsgWaitAny(timeout_ms, wakeMask, flags: cardinal): TWaitResult;
var
  winResult: cardinal;
begin
  FIdxSignalled := -1;
  RegisterWaitHandles(WT_EXECUTEONLYONCE);
  try
    winResult := MsgWaitForMultipleObjectsEx(1, FSignal, timeout_ms, wakeMask, flags);
  finally UnregisterWaitHandles; end;
  Result := MapToResult(winResult);
end; { TWaitFor.MsgWaitAny }

procedure WaitForCallback(Context: Pointer; TimerOrWaitFired: Boolean); stdcall;
begin
  if not TimerOrWaitFired then
    TWaitFor.TWaiter(Context).Awaited;
end; { WaitForCallback }

function TWaitFor.MapToResult(winResult: cardinal): TWaitResult;
begin
  if winResult = WAIT_OBJECT_0 then
    Result := waAwaited
  else if winResult = WAIT_TIMEOUT then
    Result := waTimeout
  else if winResult = WAIT_IO_COMPLETION then
    Result := waIOCompletion
  else
    Result := waFailed;
end; { TWaitFor.MapToResult }

procedure TWaitFor.RegisterWaitHandles(extraFlags: cardinal);
var
  idxWait      : integer;
  iHandle      : integer;
  newWaitObject: THandle;
  waiter       : TWaiter;
begin
  for iHandle := Low(FHandles) to High(FHandles) do begin
    waiter := TWaiter.Create(Self, iHandle);
    idxWait := FWaitHandles.AddObject(0 {placeholder}, waiter);
    Win32Check(RegisterWaitForSingleObject(newWaitObject, FHandles[iHandle], WaitForCallback,
                                           pointer(waiter), INFINITE,
                                           extraFlags OR WT_EXECUTEINPERSISTENTTHREAD));
    FWaitHandles[idxWait] := newWaitObject;
  end;
  SetLength(FSignalledHandles, 0);
end; { TWaitFor.RegisterWaitHandles }

procedure TWaitFor.SetHandles(const handles: array of THandle);
var
  iHandle: integer;
begin
  SetLength(FHandles, Length(handles));
  for iHandle := Low(handles) to High(handles) do
    FHandles[iHandle] := handles[iHandle];
end; { TWaitFor.SetHandles }

procedure TWaitFor.UnregisterWaitHandles;
var
  countSignalled: integer;
  i             : integer;
  waiter        : TWaiter;
begin
  for i := 0 to FWaitHandles.Count - 1 do
    UnregisterWaitEx(THandle(FWaitHandles[i]), INVALID_HANDLE_VALUE);

  countSignalled := 0;
  for i := 0 to FWaitHandles.Count - 1 do begin
    waiter := TWaiter(FWaitHandles.Objects[i]);
    if waiter.Signalled then
      Inc(countSignalled);
  end;

  SetLength(FSignalledHandles, countSignalled);
  countSignalled := 0;
  for i := 0 to FWaitHandles.Count - 1 do begin
    waiter := TWaiter(FWaitHandles.Objects[i]);
    if waiter.Signalled then begin
      FSignalledHandles[countSignalled].Index := waiter.Index;
      Inc(countSignalled);
    end;
  end;
  FWaitHandles.Clear;
end; { TWaitFor.UnregisterWaitHandles }

function TWaitFor.WaitAll(timeout_ms: cardinal): TWaitResult;
var
  winResult: cardinal;
begin
  FResourceCount := CreateResourceCount(Length(FHandles));
  try
    RegisterWaitHandles(WT_EXECUTEONLYONCE);
    try
      winResult := WaitForSingleObject(FResourceCount.Handle, timeout_ms);
    finally UnregisterWaitHandles; end;
    Result := MapToResult(winResult);
  finally FResourceCount := nil; end;
end; { TWaitFor.WaitAll }

function TWaitFor.WaitAny(timeout_ms: cardinal; alertable: boolean): TWaitResult;
var
  winResult: cardinal;
begin
  FIdxSignalled := -1;
  RegisterWaitHandles(WT_EXECUTEONLYONCE);
  try
    winResult := WaitForMultipleObjectsEx(1, @FSignal, false, timeout_ms, alertable);
  finally UnregisterWaitHandles; end;
  Result := MapToResult(winResult);
end; { TWaitFor.WaitAny }

initialization
  GOmniCancellationToken := CreateOmniCancellationToken;
  {$IFDEF CPUX64}
  CASAlignment := 16;
  {$ELSE}
  CASAlignment := 8;
  {$ENDIF CPUX64}
end.
