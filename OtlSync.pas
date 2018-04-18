///<summary>Synchronisation primitives. Part of the OmniThreadLibrary project.</summary>
///<remarks>Move* family of functions require Pentium 4 processor (or newer).</remarks>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017, Primoz Gabrijelcic
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
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://plus.google.com/communities/112307748950248514961
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : GJ, Lee_Nover, dottor_jeckill, Sean B. Durkin, VyPu
///   Creation date     : 2009-03-30
///   Last modification : 2018-04-06
///   Version           : 1.27
///</para><para>
///   History:
///	    1.27: 2018-04-06
///	      - Added timeout parameter to TOmniMREW.TryEnterReadLock and TOmniMREW.TryExitReadLock.
///     1.26: 2017-11-09
///       - [VyPu] Fixed: TOmniCriticalSection.Release decremented ocsLockCount after releasing the critical section.
///     1.25: 2017-09-28
///       - [VyPu] Locked<T>.Value is now both readable and writable property.
///     1.24: 2017-06-14
///       - TOmniCS.Initialize uses global lock to synchronize initialization instead of
///         a CAS operation. This fixes all reasons for the infamous error
///         "TOmniCS.Initialize: XXX is not properly aligned!".
///     1.23: 2016-10-24
///       - Implemented two-parameter version of Atomic initializer which intializes
///         an interface type from a class type.
///     1.22c: 2015-09-10
///       - Fixed unsafe 64-bit pointer-to-integer casts.
///     1.22b: 2015-09-07
///       - TWaitFor.MsgWaitAny now uses RegisterWaitForSingleObject approach when
///         waiting on 64 handles. Previously, MsgWaitForMultipleObjectsEx was called,
///         which can only handle up to 63 handles.
///     1.22a: 2015-09-04
///       - Fixed a bug in TWaitFor: When the code was waiting on less than 64 handles
///         and timeout occurred, the Signalled[] property was not always empty.
///       - Fixed: TWaitFor was not working correctly with more than 64 handles if
///         it was created with the parameter-less constructor.
///     1.22: 2015-07-27
///       - Implemented TOmniSingleThreadUseChecker.AttachToThread which forcibly
///         attaches thread checker to the current thread even if it was used
///         from another thread before.
///     1.21: 2015-07-10
///       - Implemented TOmniSingleThreadUseChecker, a record which checks that the
///         owner is only used from one thread. See OtlComm/TOmniCommunicationEndpoint
///         for an example.
///     1.20: 2015-04-17
///       - TOmniCS.GetLockCount won't crash if Initialize was not called yet.
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
  {$IFDEF OTL_ERTTI}
  RTTI,
  {$ENDIF OTL_ERTTI}
  TypInfo,
  {$IFDEF MSWINDOWS}
  Windows,
  DSiWin32,
  GpStuff,
  GpLists,
  {$ENDIF}
  {$IFDEF OTL_MobileSupport}
  {$IFDEF POSIX}
  Posix.Pthread,
  {$ENDIF}
  System.Diagnostics,
  {$ENDIF OTL_MobileSupport}
  OtlCommon;

type
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

  {$IFDEF OTL_MobileSupport}
  IOmniSynchroObserver = interface ['{03330A74-3C3D-4D2F-9A21-89663DE7FD10}']
    procedure EnterGate;
    procedure LeaveGate;
    /// <param name="SynchObj">SynchObj must support IOmniSynchroObject.</param>
    procedure DereferenceSynchObj(const SynchObj: TObject; AllowInterface: boolean);
    /// <param name="Subtractend">Signaller must support IOmniSynchroObject.</param>
    procedure BeforeSignal(const Signaller: TObject; var Data: TObject);
    /// <param name="Subtractend">Signaller must support IOmniSynchroObject.</param>
    procedure AfterSignal(const Signaller: TObject; var Data: TObject);
  end; { IOmniSynchroObserver }

  IOmniSynchro = interface ['{2C4F0CF8-A722-45EC-BFCA-AA512E58B54D}']
    function  EnterSpinLock: IInterface;
    procedure Signal;
    /// <remarks>
    ///  If this event is attached to IOmniSynchroObserver,
    //    such as TWaitFor (acting as a condition variable)
    ///   a thread must not invoke WaitFor() directly on this event, but
    ///   rather through the containing TWaitFor, or as otherwise defined by
    //    the attached observer.
    /// </remarks>
    function  WaitFor(Timeout: LongWord = INFINITE): TWaitResult; overload;
    procedure ConsumeSignalFromObserver( const Observer: IOmniSynchroObserver);
    /// <remarks>
    ///  IsSignaled() is only valid when all the Signal()/ Reset()
    ///   invocations are done whilst attached to an IOmniEventObserver.
    ///   Otherwise this returned value must not be relied upon.
    /// </remarks>
    function  IsSignalled: boolean;
    procedure AddObserver(const Observer: IOmniSynchroObserver);
    procedure RemoveObserver(const Observer: IOmniSynchroObserver);
    function  Base: TSynchroObject;
    {$IFDEF MSWINDOWS}
    function  Handle: THandle;
    {$ENDIF}
  end; { IOmniSynchro }

  IOmniSynchroObject = interface ['{A8B95978-87BF-4031-94B2-8EDC351F47BE}']
    function  GetSynchro: IOmniSynchro;
  //
    property Synchro: IOmniSynchro read GetSynchro;
  end; { IOmniSynchroObject }

  /// <remarks>
  ///   IOmniEvent is a wrapper around a TEvent object.
  ///   It can co-operate with condition variables through the use of an
  ///   attached IOmniEventObserver. IOmniEvent objects can be enrolled
  ///   in TWaitFor objects on non-windows platforms.
  /// </remarks>
  IOmniEvent = interface(IOmniSynchro) ['{3403D24B-3CBE-4A83-9F4C-FA4719AA23C5}']
    procedure SetEvent;
    procedure Reset;
    function  BaseEvent: TEvent;
  end; { IOmniEvent }

  IOmniCountdownEvent = interface(IOmniSynchro) ['{40557184-B610-46E8-B186-D5B431D1B1A4}']
    function  BaseCountdown: TCountdownEvent;
    procedure Reset;
  end; { IOmniCountdownEvent }
  {$ENDIF OTL_MobileSupport}

  //At some point this type will be dropped and all the codebase will use
  //IOmniEvent or something similar.
  TOmniTransitionEvent = {$IFDEF MSWINDOWS}THandle{$ELSE}IOmniEvent{$ENDIF};

  {$IFDEF MSWINDOWS}
  IOmniHandleObject = interface ['{80B85D03-8E1F-4812-8782-38A04BA52076}']
    function  GetHandle: THandle;
  //
    property Handle: THandle read GetHandle;
  end; { IOmniHandleObject }
  {$ENDIF MSWINDOWS}

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
    function  TryEnterReadLock(timeout_ms: integer = 0): boolean;
    function  TryEnterWriteLock(timeout_ms: integer = 0): boolean;
  end; { TOmniMREW }

  IOmniResourceCount = interface({$IFDEF MSWINDOWS}
                                 IOmniHandleObject
                                 {$ELSE}{$IFDEF OTL_MobileSupport}
                                 IOmniSynchroObject
                                 {$ENDIF}{$ENDIF})
  ['{F5281539-1DA4-45E9-8565-4BEA689A23AD}']
    function  Allocate: cardinal;
    function  Release: cardinal;
    function  TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal = 0): boolean;
  end; { IOmniResourceCount }

  ///<summary>Kind of an inverse semaphore. Gets signalled when count drops to 0.
  ///   Allocate decrements the count (and blocks if initial count is 0), Release
  ///   increments the count.
  ///   Threadsafe.
  ///</summary>
  {$IFDEF MSWINDOWS}
  TOmniResourceCount = class(TInterfacedObject, IOmniResourceCount, IOmniHandleObject)
  strict private
    orcAvailable   : TDSiEventHandle;
    orcHandle      : TDSiEventHandle;
    orcLock        : TOmniCS;
    orcNumResources: TOmniAlignedInt32;
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
  {$ELSE}{$IFDEF OTL_MobileSupport}
  TOmniResourceCount = class abstract(TInterfacedObject, IOmniResourceCount, IOmniSynchroObject)
  strict protected
    function  GetSynchro: IOmniSynchro;
  public
    constructor Create(initialCount: cardinal);
    destructor  Destroy; override;
    function  Allocate: cardinal;
    function  Release: cardinal;
    function  TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal = 0): boolean;
    property Synchro: IOmniSynchro read GetSynchro;
  end; { TOmniResourceCount }
  {$ENDIF OTL_MobileSupport}{$ENDIF MSWINDOWS}

  IOmniCancellationToken = interface ['{5946F4E8-45C0-4E44-96AB-DBE2BE66A701}']
    {$IFDEF MSWINDOWS}
    function  GetHandle: THandle;
    {$ELSE}
    function  GetEvent: IOmniEvent;
    {$ENDIF MSWINDOWS}
  //
    procedure Clear;
    function  IsSignalled: boolean;
    procedure Signal;
    {$IFDEF MSWINDOWS}
    property Handle: THandle read GetHandle;
    {$ELSE}
    property Event: IOmniEvent read GetEvent;
    {$ENDIF MSWINDOWS}
  end; { IOmniCancellationToken }

  {$IFDEF OTL_Generics}
  Atomic<T> = class
    type TFactory = reference to function: T;
    class function Initialize(var storage: T; factory: TFactory): T; overload;
    {$IFDEF OTL_ERTTI}
    class function Initialize(var storage: T): T; overload;
    {$ENDIF OTL_ERTTI}
  end; { Atomic<T> }

  {$IFDEF OTL_ERTTI}
  Atomic<I; T:constructor> = class
    class function Initialize(var storage: I): I;
  end; { Atomic<I,T> }
  {$ENDIF OTL_ERTTI}

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
    procedure SetValue(const value: T); inline;
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
    property Value: T read GetValue write SetValue;
  end; { Locked<T> }

  IOmniLockManagerAutoUnlock = interface
    procedure Unlock;
  end; { IOmniLockManagerAutoUnlock }

  IOmniLockManager<K> = interface
    function  Lock(const key: K; timeout_ms: cardinal): boolean;
    function  LockUnlock(const key: K; timeout_ms: cardinal): IOmniLockManagerAutoUnlock;
    procedure Unlock(const key: K);
  end; { IOmniLockManager<K> }

  {$IFDEF MSWINDOWS} // mobile version does not implement doubly linked list (yet)
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
    procedure Unlock(const key: K);
  end; { TOmniLockManager<K> }
  {$ENDIF MSWINDOWS}
  {$ENDIF OTL_Generics}

  {$IFDEF MSWINDOWS}
  ///<summary>Waits on any/all from any number of handles.</summary>
  ///  Don't use it to wait on mutexes!
  ///  http://joeduffyblog.com/2007/05/13/registerwaitforsingleobject-and-mutexes-dont-mix/
  TWaitFor = class
  private type
    TWaitMode = (wmSmart, wmForceWFM, wmForceRWFS);
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
    TWaitForResult = (
      waAwaited,      // WAIT_OBJECT_0 .. WAIT_OBJECT_n
      waTimeout,      // WAIT_TIMEOUT
      waFailed,       // WAIT_FAILED
      waIOCompletion, // WAIT_IO_COMPLETION
      waMessage       // message or wake event (WAIT_OBJECT_n+1)
    );
    THandleInfo = record
      Index: integer;
    end;
    THandles = array of THandleInfo;
    THandleArr = array of THandle;
  strict private
    FAwaitedLock     : TOmniCS;
    FHandles         : array of THandle;
    FIdxSignalled    : integer;
    FResourceCount   : IOmniResourceCount;
    FSignal          : TDSiEventHandle;
    FSignalledHandles: THandles;
    FWaitHandles     : TGpInt64ObjectList;
    FWaitMode        : TWaitMode; // for testing
  strict protected
    function  GetWaitHandles: THandleArr;
    function  MapToHandle(winResult: cardinal; isMsgWait: boolean): cardinal;
    function  MapToResult(winResult: cardinal): TWaitForResult;
    procedure RegisterWaitHandles(extraFlags: cardinal);
    procedure UnregisterWaitHandles;
  protected //must be visible from the callback
    procedure Awaited_Asy(idxHandle: integer);
  public
    constructor Create; overload;
    constructor Create(const handles: array of THandle); overload;
    destructor  Destroy; override;
    function  MsgWaitAny(timeout_ms, wakeMask, flags: cardinal): TWaitForResult;
    procedure SetHandles(const handles: array of THandle);
    function  WaitAll(timeout_ms: cardinal): TWaitForResult;
    function  WaitAny(timeout_ms: cardinal; alertable: boolean = false): TWaitForResult;
    property Signalled: THandles read FSignalledHandles;
    property WaitHandles: THandleArr read GetWaitHandles;
  end; { TWaitFor }
  {$ELSE ~MSWINDOWS}
  {$IFDEF OTL_MobileSupport}
  ///<summary>Waits on any/all from any number of synchroobjects such as Events and CountDownEvents.</summary>
  TSynchroWaitFor = class
  public type //TODO: not integrated yet (maybe will even be removed at the end but currently OtlTaskControl expects it)
    TWaitForResult = (
      waAwaited,      // WAIT_OBJECT_0 .. WAIT_OBJECT_n
      waTimeout,      // WAIT_TIMEOUT
      waFailed,       // WAIT_FAILED
      waIOCompletion, // WAIT_IO_COMPLETION
      waMessage       // message or wake event (WAIT_OBJECT_n+1)
    );
    THandleInfo = record //TODO: not integrated yet (maybe will even be removed at the end but currently OtlTaskControl expects it)
      Index: integer;
    end;
  strict private type
    TSynchroList = class(TList<IOmniSynchro>) end;
    ISynchroClientEx = interface ['{A4D963B3-88CD-466A-9885-3C66E605E32E}']
      procedure Deref;
    end; { ISyncroClientEx }
    TSynchroClient = class(TInterfacedObject, IOmniSynchroObserver, ISynchroClientEx)
    strict private
      FController: TSynchroWaitFor;
      procedure EnterGate;
      procedure LeaveGate;
      procedure DereferenceSynchObj(const SynchObj: TObject; AllowInterface: boolean);
      procedure BeforeSignal(const Signaller: TObject; var Data: TObject);
      procedure AfterSignal(const Signaller: TObject; var Data: TObject);
      procedure Deref;
    public
      constructor Create(AController: TSynchroWaitFor);
    end; { TSynchroClient }
  protected type
    TCondition = class
    protected
      FCondVar   : TConditionVariableCS;
      FController: TSynchroWaitFor;
    public
      constructor Create(AController: TSynchroWaitFor);
      destructor  Destroy; override;
      function  Wait(timeout_ms: cardinal; var Signaller: IOmniSynchro): TWaitResult;
      function  Test(var Signaller: IOmniSynchro): boolean; virtual; abstract;
      function  WaitAll: boolean; virtual; abstract;
    end;
  strict private
    FAllSignalled: TCondition;
    FGate        : IOmniCriticalSection;
    FOneSignalled: TCondition;
    FSynchObjects: TSynchroList;
    FSynchClient : IOmniSynchroObserver;
  protected
    property Gate: IOmniCriticalSection read FGate;
    property SynchObjects: TSynchroList read FSynchObjects;
  public
    constructor Create(const SynchObjects: array of IOmniSynchro; const AShareLock: IOmniCriticalSection = nil);
    destructor  Destroy; override;
    function  WaitAll(timeout_ms: cardinal): TWaitResult;
    function  WaitAny(timeout_ms: cardinal; var Signaller: IOmniSynchro): TWaitResult;
  end; { TWaitForAll }

  TWaitFor = TSynchroWaitFor;
  {$ENDIF OTL_MobileSupport}
  {$ENDIF ~MSWINDOWS}

  TOmniSingleThreadUseChecker = record
  private
    FLock    : TOmniCS;
    FThreadID: cardinal;
  public
    procedure AttachToCurrentThread; inline;
    procedure Check; inline;
    procedure DebugCheck; inline;
  end; { TOmniSingleThreadUseChecker }

  // Compatibility layer for interlocked operations.
  TInterlockedEx = class
  public
    class function Add(var Target: NativeInt; Increment: NativeInt): NativeInt; overload; static; inline;
    class function CAS(const oldValue, newValue: NativeInt; var destination): boolean; overload; static; inline;
    class function CAS(const oldValue, newValue: pointer; var destination): boolean; overload; static; inline;
    class function CompareExchange(var Target: NativeInt; Value: NativeInt; Comparand: NativeInt): NativeInt; static; inline;
    class function Increment(var Target: Integer): Integer; overload; static; inline;
    class function Decrement(var Target: Integer): Integer; overload; static; inline;
  end; { TInterlockedEx }

{$IFDEF OTL_NeedsWindowsAPIs}
  TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean) stdcall;
  BOOL = LongBool;
  ULONG = Cardinal;

const
  WT_EXECUTEONLYONCE           = ULONG($00000008);
  WT_EXECUTEINPERSISTENTTHREAD = ULONG($00000080);

function RegisterWaitForSingleObject(out phNewWaitObject: THandle; hObject: THandle;
  CallBack: TWaitOrTimerCallback; Context: Pointer; dwMilliseconds: ULONG;
  dwFlags: ULONG): BOOL; stdcall;
  external 'kernel32.dll' name 'RegisterWaitForSingleObject';
function RegisterWaitForSingleObjectEx(hObject: THandle;
  CallBack: TWaitOrTimerCallback; Context: Pointer; dwMilliseconds: ULONG;
  dwFlags: ULONG): THandle; stdcall;
  external 'kernel32.dll' name 'RegisterWaitForSingleObjectEx';
function UnregisterWait(WaitHandle: THandle): BOOL; stdcall;
  external 'kernel32.dll' name 'UnregisterWait';
function UnregisterWaitEx(WaitHandle: THandle; CompletionEvent: THandle): BOOL; stdcall;
  external 'kernel32.dll' name 'UnregisterWaitEx';
{$ENDIF OTL_NeedsWindowsAPIs}

function CreateOmniCriticalSection: IOmniCriticalSection;
function CreateOmniCancellationToken: IOmniCancellationToken;
function CreateResourceCount(initialCount: integer): IOmniResourceCount;

{$IFDEF OTL_MobileSupport}
function CreateOmniCountdownEvent(Count: Integer; SpinCount: Integer; const AShareLock: IOmniCriticalSection = nil): IOmniCountdownEvent;
function CreateOmniEvent(AManualReset, InitialState: boolean; const AShareLock: IOmniCriticalSection = nil): IOmniEvent;
{$ENDIF OTL_MobileSupport}

{$IFDEF MSWINDOWS}
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
{$ENDIF MSWINDOWS}

function GetThreadId: NativeInt;
function GetCPUTimeStamp: int64;

function SetEvent(event: TOmniTransitionEvent): boolean;

var
  GOmniCancellationToken: IOmniCancellationToken;
  CASAlignment: integer; //required alignment for the CAS function - 8 or 16, depending on the platform

implementation

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
  {$IFDEF MSWINDOWS}
  private
    FEvent      : TDSiEventHandle;
    FIsSignalled: boolean;
  protected
    function  GetHandle: THandle; inline;
  {$ELSE}
  private
    FEvent: IOmniEvent;
  protected
    function  GetEvent: IOmniEvent; inline;
  {$ENDIF MSWINDOWS}
  public
    constructor Create;
    procedure Clear; inline;
    function  IsSignalled: boolean; inline;
    procedure Signal; inline;
  {$IFDEF MSWINDOWS}
    destructor  Destroy; override;
    property Handle: THandle read GetHandle;
  {$ELSE}
    property Event: IOmniEvent read GetEvent;
  {$ENDIF MSWINDOWS}
  end; { TOmniCancellationToken }

  {$IFDEF OTL_MobileSupport}
  TOmniSynchroObject = class abstract(TSynchroObject, IInterface, IOmniSynchro)
  private
    procedure PerformObservableAction(Action: TProc; DoLock: boolean);
    function  Base: TSynchroObject;
    {$IFDEF MSWINDOWS}
    function  Handle: THandle;
    {$ENDIF}
  strict protected
    FBase               : TSynchroObject;
    FOwnsBase           : boolean;
    FLock               : TSpinLock;
    FObservers          : TList<IOmniSynchroObserver>;
    FData               : TArray<TObject>;
    [Volatile] FRefCount: integer;
    FShareLock          : IOmniCriticalSection;
  private
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    property Lock: TSpinLock read FLock;
    property ShareLock: IOmniCriticalSection read FShareLock;
  public
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
  public
    constructor Create(ABase: TSynchroObject; OwnsIt: boolean; const AShareLock: IOmniCriticalSection = nil);
    destructor  Destroy; override;
    function  EnterSpinLock: IInterface;
    procedure Acquire; override;
    procedure Release; override;
    procedure Signal;
    function  WaitFor(Timeout: LongWord = INFINITE): TWaitResult; override;
    procedure ConsumeSignalFromObserver(const Observer: IOmniSynchroObserver); virtual; abstract;
    function  IsSignalled: boolean; virtual; abstract;
    procedure AddObserver(const Observer: IOmniSynchroObserver);
    procedure RemoveObserver(const Observer: IOmniSynchroObserver);
  end; { TOmniSynchroObject }

  TSynchroSpin = class(TInterfacedObject)
  private
    FController: TOmniSynchroObject;
  public
    constructor Create(AController: TOmniSynchroObject);
    destructor Destroy; override;
  end; { TSynchroSpin }

  TOmniCountdownEvent = class(TOmniSynchroObject, IOmniCountdownEvent)
  strict protected
    FCountdown: TCountdownEvent;
  public
    constructor Create(Count: Integer; SpinCount: Integer; const AShareLock: IOmniCriticalSection = nil);
    procedure Reset;
    procedure ConsumeSignalFromObserver(const Observer: IOmniSynchroObserver);  override;
    function  IsSignalled: boolean; override;
    function  BaseCountdown: TCountdownEvent;
  end; { TOmniCountdownEvent }

  TOmniEvent = class(TOmniSynchroObject, IOmniEvent)
  strict protected
    FEvent: TEvent;
    [Volatile] FState: boolean;
    FManualReset: boolean;
  public
    constructor Create(AManualReset, InitialState: boolean; const AShareLock: IOmniCriticalSection = nil);
    procedure Reset;
    procedure SetEvent;
    function  BaseEvent: TEvent;
    procedure ConsumeSignalFromObserver(const Observer: IOmniSynchroObserver);  override;
    function  WaitFor(Timeout: LongWord = INFINITE): TWaitResult; override;
    function  IsSignalled: boolean; override;
  end; { TOmniEvent }

  {$IFNDEF MSWINDOWS}
  TOneCondition = class(TSynchroWaitFor.TCondition)
  public
    function  Test(var Signaller: IOmniSynchro): boolean; override;
    function  WaitAll: boolean; override;
  end; { TOneCondition }

  TAllCondition = class( TSynchroWaitFor.TCondition)
  public
    function  Test(var Signaller: IOmniSynchro): boolean; override;
    function  WaitAll: boolean; override;
  end; { TAllCondition }

  TPreSignalData = class
  public
    OneSignalled: boolean;
    AllSignalled: boolean;
    constructor Create(AOneSignalled, AllSignalled: boolean);
  end; { TPreSignaData }
  {$ENDIF ~MSWINDOWS}
  {$ENDIF OTL_MobileSupport}

var
  GOmniCSInitializer: TOmniCriticalSection;

{ transitional }

function SetEvent(event: TOmniTransitionEvent): boolean;
begin
  Result := true;
  {$IFDEF MSWINDOWS}
  if event <> 0 then
    Result := Windows.SetEvent(event);
  {$ELSE}
  if assigned(event) then
    event.SetEvent;
  {$ENDIF ~MSWINDOWS}
end; { SetEvent }

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

{$IFDEF OTL_MobileSupport}
function CreateOmniCountdownEvent(Count: Integer; SpinCount: Integer; const AShareLock: IOmniCriticalSection = nil): IOmniCountdownEvent;
begin
  Result := TOmniCountdownEvent.Create(Count, SpinCount, AShareLock);
end; { CreateOmniCountdownEvent }

function CreateOmniEvent(AManualReset, InitialState: boolean; const AShareLock: IOmniCriticalSection = nil): IOmniEvent;
begin
  Result := TOmniEvent.Create(AManualReset, InitialState, AShareLock);
end; { CreateOmniEvent }
{$ENDIF OTL_MobileSupport}

{$IFDEF MSWINDOWS}
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
{$ENDIF MSWINDOWS}

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

{$IFDEF MSWINDOWS}
function WaitForAllObjects(const handles: array of THandle; timeout_ms: cardinal):
  boolean;
var
  waiter: TWaitFor;
begin
  waiter := TWaitFor.Create(handles);
  try
    Result := (waiter.WaitAll(timeout_ms) = waAwaited);
  finally FreeAndNil(waiter); end;
end; { WaitForAllObjects }
{$ENDIF MSWINDOWS}

{ TOmniCS }

procedure TOmniCS.Acquire;
begin
  Initialize;
  ocsSync.Acquire;
end; { TOmniCS.Acquire }

function TOmniCS.GetLockCount: integer;
begin
  Result := 0;
  if Assigned(ocsSync) then
    Result := ocsSync.LockCount;
end; { TOmniCS.GetLockCount }

function TOmniCS.GetSyncObj: TSynchroObject;
begin
  Initialize;
  Result := ocsSync.GetSyncObj;
end; { TOmniCS.GetSyncObj }

procedure TOmniCS.Initialize;
begin
  if not assigned(ocsSync) then begin
    GOmniCSInitializer.Acquire;
    try
      if not assigned(ocsSync) then
        ocsSync := CreateOmniCriticalSection;
    finally GOmniCSInitializer.Release; end;
  end;
end; { TOmniCS.Initialize }

procedure TOmniCS.Release;
begin
  ocsSync.Release;
end; { TOmniCS.Release }

{ TOmniCriticalSection }

constructor TOmniCriticalSection.Create;
begin
  inherited Create;
  ocsCritSect := TFixedCriticalSection.Create;
end; { TOmniCriticalSection.Create }

destructor TOmniCriticalSection.Destroy;
begin
  FreeAndNil(ocsCritSect);
  inherited;
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
  Dec(ocsLockCount);
  ocsCritSect.Release;
end; { TOmniCriticalSection.Release }

{ TOmniCancellationToken }

constructor TOmniCancellationToken.Create;
begin
  {$IFDEF MSWINDOWS}
  FEvent := CreateEvent(nil, true, false, nil);
  {$ELSE}
  FEvent := CreateOmniEvent(True, False);
  {$ENDIF ~MSWINDOWS}
end; { TOmniCancellationToken.Create }

{$IFDEF MSWINDOWS}
destructor TOmniCancellationToken.Destroy;
begin
  DSiCloseHandleAndNull(FEvent);
  inherited;
end; { TOmniCancellationToken.Destroy }
{$ENDIF MSWINDOWS}

procedure TOmniCancellationToken.Clear;
begin
  {$IFDEF MSWINDOWS}
  FIsSignalled := false;
  ResetEvent(FEvent);
  {$ELSE}
  FEvent.Reset;
  {$ENDIF ~MSWINDOWS}
end; { TOmniCancellationToken.Clear }

{$IFDEF MSWINDOWS}
function TOmniCancellationToken.GetHandle: THandle;
begin
  Result := FEvent;
end; { TOmniCancellationToken.GetHandle }
{$ELSE}
function TOmniCancellationToken.GetEvent: IOmniEvent;
begin
  Result := FEvent;
end; { TOmniCancellationToken.GetEvent }
{$ENDIF MSWINDOWS}

function TOmniCancellationToken.IsSignalled: boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := FIsSignalled;
  {$ELSE}
  Result := FEvent.IsSignalled;
  {$ENDIF ~MSWINDOWS}
end; { TOmniCancellationToken.IsSignalled }

procedure TOmniCancellationToken.Signal;
begin
  {$IFDEF MSWINDOWS}
  FIsSignalled := true;
  SetEvent(FEvent);
  {$ELSE}
  FEvent.Signal;
  {$ENDIF ~MSWINDOWS}
end; { TOmniCancellationToken.Signal }

{ TOmniMREW }

procedure TOmniMREW.EnterReadLock;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := NativeInt(omrewReference) AND NOT 1;
  {$IFDEF MSWINDOWS}
  until CAS(currentReference, currentReference + 2, NativeInt(omrewReference));
  {$ELSE}
  until TInterlockedEx.CompareExchange(NativeInt(omrewReference), currentReference + 2, currentReference) = currentReference;
  {$ENDIF}
end; { TOmniMREW.EnterReadLock }

procedure TOmniMREW.EnterWriteLock;
var
  currentReference: NativeInt;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := NativeInt(omrewReference) AND NOT 1;
  {$IFDEF MSWINDOWS}
  until CAS(currentReference, currentReference + 1, NativeInt(omrewReference));
  {$ELSE}
  until TInterlockedEx.CompareExchange(NativeInt(omrewReference), currentReference + 1, currentReference) = currentReference;
  {$ENDIF}
  //Now wait on all readers
  repeat
  until NativeInt(omrewReference) = 1;
end; { TOmniMREW.EnterWriteLock }

procedure TOmniMREW.ExitReadLock;
begin
  //Decrease omrewReference
  {$IFDEF MSWINDOWS}
  NInterlockedExchangeAdd(NativeInt(omrewReference), -2);
  {$ELSE}
  TInterlockedEx.Add(NativeInt(omrewReference), -2)
  {$ENDIF}
end; { TOmniMREW.ExitReadLock }

procedure TOmniMREW.ExitWriteLock;
begin
  NativeInt(omrewReference) := 0;
end; { TOmniMREW.ExitWriteLock }

function TOmniMREW.TryEnterReadLock(timeout_ms: integer): boolean;
var
  currentReference: NativeInt;
  startWait_ms: int64;

  function Timeout(var returnFalse: boolean): boolean;
  begin
    Result := (timeout_ms <= 0) or DSiHasElapsed64(startWait_ms, timeout_ms);
    if Result then
      returnFalse := true;
  end; { Timeout }

begin
  Result := true;
  startWait_ms := DSiTimeGetTime64; //TODO: Rewrite this with a faster, non-locking clock
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := NativeInt(omrewReference) AND NOT 1;
  {$IFDEF MSWINDOWS}
  until CAS(currentReference, currentReference + 2, NativeInt(omrewReference)) or Timeout(Result);
  {$ELSE}
  until (TInterlockedEx.CompareExchange(NativeInt(omrewReference), currentReference + 2, currentReference) = currentReference) or Timeout(Result);
  {$ENDIF}
end; { TOmniMREW.TryEnterReadLock }

function TOmniMREW.TryEnterWriteLock(timeout_ms: integer): boolean;
var
  currentReference: NativeInt;
  startWait_ms: int64;

  function Timeout(var returnFalse: boolean): boolean;
  begin
    Result := (timeout_ms <= 0) or DSiHasElapsed64(startWait_ms, timeout_ms);
    if Result then
      returnFalse := true;
  end; { Timeout }

begin
  Result := true;
  startWait_ms := DSiTimeGetTime64; //TODO: Rewrite this with a faster, non-locking clock

  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := NativeInt(omrewReference) AND NOT 1;
  {$IFDEF MSWINDOWS}
  until CAS(currentReference, currentReference + 1, NativeInt(omrewReference)) or Timeout(Result);
  {$ELSE}
  until (TInterlockedEx.CompareExchange(NativeInt(omrewReference), currentReference + 1, currentReference) = currentReference) or Timeout(Result);
  {$ENDIF}
  if Result then begin
    //Now wait on all readers
    repeat
    until (NativeInt(omrewReference) = 1) or Timeout(Result);
    if not Result then
      ExitWriteLock;
  end;
end; { TOmniMREW.TryEnterWriteLock }

{$IFDEF MSWINDOWS}

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
  startTime_ms := DSiTimeGetTime64; //TODO: Rewrite this with a faster, non-locking clock
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

{$ELSE ~MSWINDOWS}

constructor TOmniResourceCount.Create(initialCount: cardinal);
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.Create }

destructor TOmniResourceCount.Destroy;
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.Destroy }

function TOmniResourceCount.Allocate: cardinal;
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.Allocate }

function TOmniResourceCount.GetSynchro: IOmniSynchro;
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.GetSynchro }

function TOmniResourceCount.Release: cardinal;
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.Release }

function TOmniResourceCount.TryAllocate(var resourceCount: cardinal; timeout_ms: cardinal): boolean;
begin
  { TODO : Not implemented! }
  raise Exception.Create('Not implemented!');
end; { TOmniResourceCount.TryAllocate }

{$ENDIF ~MSWINDOWS}
{$IFDEF OTL_Generics}

{ Atomic<T> }

class function Atomic<T>.Initialize(var storage: T; factory: TFactory): T;
var
  interlockRes: pointer;
  tmpT        : T;
begin
  if not assigned(PPointer(@storage)^) then begin
    Assert(NativeUInt(@storage) mod SizeOf(pointer) = 0, 'Atomic<T>.Initialize: storage is not properly aligned!');
    Assert(NativeUInt(@tmpT) mod SizeOf(pointer) = 0, 'Atomic<T>.Initialize: tmpT is not properly aligned!');
    tmpT := factory();
    {$IFDEF MSWINDOWS}
    interlockRes := InterlockedCompareExchangePointer(PPointer(@storage)^, PPointer(@tmpT)^, nil);
    {$ELSE}
    interlockRes := TInterlocked.CompareExchange(PPointer(@storage)^, PPointer(@tmpT)^, nil);
    {$ENDIF}
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

{ ATomic<I,T> }

class function Atomic<I,T>.Initialize(var storage: I): I;
begin
  Result := Atomic<I>.Initialize(storage,
    function: I
    begin
      Result := TValue.From<T>(T.Create).AsType<I>;
    end);
end; { Atomic<I,T>.Initialize }

{$ENDIF OTL_ERTTI}

{ Locked<T> }

constructor Locked<T>.Create(const value: T; ownsObject: boolean);
begin
  Clear;
  FValue := value;
  if ownsObject and (PTypeInfo(TypeInfo(T))^.Kind = tkClass) then
    FLifecycle := CreateAutoDestroyObject(TObject(PPointer(@value)^));
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

procedure Locked<T>.SetValue(const value: T);
begin
  Assert(FLock.LockCount > 0, 'Locked<T>.SetValue: Not locked');
  FValue := value;
end; { Locked<T>.SetValue }

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

{$IFDEF MSWINDOWS}

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
  startWait := DSiTimeGetTime64; //TODO: Rewrite this with a faster, non-locking clock

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

procedure TOmniLockManager<K>.Unlock(const key: K);
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

{$ENDIF MSWINDOWS}
{$ENDIF OTL_Generics}

{$IFDEF MSWINDOWS}

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

{ TWaitFor }

constructor TWaitFor.Create(const handles: array of THandle);
begin
  Create;
  SetHandles(handles);
end; { TWaitFor.Create }

constructor TWaitFor.Create;
begin
  inherited;
  FSignal := CreateEvent(nil, false, false, nil);
  FWaitMode := wmSmart;
  FWaitHandles := TGpInt64ObjectList.Create;
end; { TWaitFor.Create }

destructor TWaitFor.Destroy;
begin
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

function TWaitFor.MsgWaitAny(timeout_ms, wakeMask, flags: cardinal): TWaitForResult;
var
  winResult: cardinal;
begin
  if (FWaitMode = wmForceWFM) or ((FWaitMode = wmSmart) and (Length(FHandles) < 64)) then
    winResult := MapToHandle(MsgWaitForMultipleObjectsEx(Length(FHandles), FHandles[0], timeout_ms, wakeMask, flags), true)
  else begin
    FIdxSignalled := -1;
    RegisterWaitHandles(0);
    try
      winResult := MsgWaitForMultipleObjectsEx(1, FSignal, timeout_ms, wakeMask, flags);
    finally UnregisterWaitHandles; end;
  end;
  Result := MapToResult(winResult);
end; { TWaitFor.MsgWaitAny }

procedure WaitForCallback(Context: Pointer; TimerOrWaitFired: Boolean); stdcall;
begin
  if not TimerOrWaitFired then
    TWaitFor.TWaiter(Context).Awaited;
end; { WaitForCallback }

function TWaitFor.GetWaitHandles: THandleArr;
begin
  SetLength(Result, Length(FHandles));
  if Length(Result) > 0 then
    Move(FHandles[Low(FHandles)], Result[Low(Result)], Length(Result) * SizeOf(Result[Low(Result)]));
end; { TWaitFor.GetWaitHandles }

function TWaitFor.MapToHandle(winResult: cardinal; isMsgWait: boolean): cardinal;
begin
  if isMsgWait and (winResult = (WAIT_OBJECT_0 + cardinal(Length(FHandles)))) then begin
    SetLength(FSignalledHandles, 0);
    Result := WAIT_OBJECT_0 + 64; //unused value in Windows API
  end
  else if {(winResult >= WAIT_OBJECT_0) and }
     (winResult < (WAIT_OBJECT_0 + cardinal(Length(FHandles)))) then
  begin
    SetLength(FSignalledHandles, 1);
    FSignalledHandles[0].Index := winResult - WAIT_OBJECT_0;
    Result := WAIT_OBJECT_0;
  end
  else begin
    SetLength(FSignalledHandles, 0);
    Result := winResult;
  end;
end; { TWaitFor.MapToHandle }

function TWaitFor.MapToResult(winResult: cardinal): TWaitForResult;
begin
  if winResult = WAIT_OBJECT_0 then
    Result := waAwaited
  else if winResult = WAIT_TIMEOUT then
    Result := waTimeout
  else if winResult = WAIT_IO_COMPLETION then
    Result := waIOCompletion
  else if winResult = (WAIT_OBJECT_0 + 64) then
    Result := waMessage
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
  FWaitHandles.Clear;
  for iHandle := Low(FHandles) to High(FHandles) do begin
    waiter := TWaiter.Create(Self, iHandle);
    idxWait := FWaitHandles.AddObject(0 {placeholder}, waiter);
    if iHandle <> idxWait then
      raise Exception.Create('TWaitFor.RegisterWaitHandles: Indexes out of sync');
{$WARN SYMBOL_PLATFORM OFF}
    Win32Check(RegisterWaitForSingleObject(newWaitObject, FHandles[iHandle], WaitForCallback,
                                           pointer(waiter), INFINITE,
                                           extraFlags OR WT_EXECUTEINPERSISTENTTHREAD));
{$WARN SYMBOL_PLATFORM ON}
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
    UnregisterWait(THandle(FWaitHandles[i]));

  SetLength(FSignalledHandles, FWaitHandles.Count);
  countSignalled := 0;
  for i := 0 to FWaitHandles.Count - 1 do begin
    waiter := TWaiter(FWaitHandles.Objects[i]);
    if waiter.Signalled then begin
      FSignalledHandles[countSignalled].Index := waiter.Index;
      Inc(countSignalled);
    end;
  end;
  SetLength(FSignalledHandles, countSignalled);
  FWaitHandles.Clear;
end; { TWaitFor.UnregisterWaitHandles }

function TWaitFor.WaitAll(timeout_ms: cardinal): TWaitForResult;
var
  winResult: cardinal;
begin
  if (FWaitMode = wmForceWFM) or ((FWaitMode = wmSmart) and (Length(FHandles) <= 64)) then
    winResult := MapToHandle(WaitForMultipleObjects(Length(FHandles), @(FHandles[0]), true, timeout_ms), false)
  else begin
    FResourceCount := CreateResourceCount(Length(FHandles));
    try
      RegisterWaitHandles(WT_EXECUTEONLYONCE);
      try
        winResult := WaitForSingleObject(FResourceCount.Handle, timeout_ms);
      finally UnregisterWaitHandles; end;
    finally FResourceCount := nil; end;
  end;
  Result := MapToResult(winResult);
end; { TWaitFor.WaitAll }

function TWaitFor.WaitAny(timeout_ms: cardinal; alertable: boolean = false):
  TWaitForResult;
var
  winResult: cardinal;
begin
  if (FWaitMode = wmForceWFM) or ((FWaitMode = wmSmart) and (Length(FHandles) <= 64)) then
    winResult := MapToHandle(WaitForMultipleObjectsEx(Length(FHandles), @(FHandles[0]), false, timeout_ms, alertable), false)
  else begin
    FIdxSignalled := -1;
    RegisterWaitHandles(0);
    try
      winResult := WaitForMultipleObjectsEx(1, @FSignal, false, timeout_ms, alertable);
    finally UnregisterWaitHandles; end;
  end;
  Result := MapToResult(winResult);
end; { TWaitFor.WaitAny }

{$ELSE ~MSWINDOWS}
{$IFDEF OTL_MobileSupport}

{ TSynchroWaitFor.TSynchroClient }

constructor TSynchroWaitFor.TSynchroClient.Create(AController: TSynchroWaitFor);
begin
  FController := AController;
  FController.FSynchClient := Self;
end; { TSynchroWaitFor.TSynchroClient.Create }

procedure TSynchroWaitFor.TSynchroClient.EnterGate;
begin
  if assigned( FController) then
    FController.FGate.Acquire;
end; { TSynchroWaitFor.TSynchroClient.EnterGate }

procedure TSynchroWaitFor.TSynchroClient.LeaveGate;
begin
  if assigned( FController) then
    FController.FGate.Release;
end; { TSynchroWaitFor.TSynchroClient.LeaveGate }

procedure TSynchroWaitFor.TSynchroClient.Deref;
begin
  FController := nil;
end; { TSynchroWaitFor.TSynchroClient.Deref }

procedure TSynchroWaitFor.TSynchroClient.DereferenceSynchObj(const SynchObj: TObject;
  AllowInterface: boolean);
begin
  if not assigned(FController) then
    Exit;
  { TODO : Is there something mising? }
end; { TSynchroWaitFor.TSynchroClient.DereferenceSynchObj }

procedure TSynchroWaitFor.TSynchroClient.BeforeSignal(const Signaller: TObject; var Data: TObject);
var
  Dummy: IOmniSynchro;
begin
  if assigned(FController) then
    Data := TPreSignalData.Create(
      FController.FOneSignalled.Test(Dummy),
      FController.FAllSignalled.Test(Dummy));
end; { TSynchroWaitFor.TSynchroClient.BeforeSignal }

procedure TSynchroWaitFor.TSynchroClient.AfterSignal(const Signaller: TObject; var Data: TObject);
var
  Dummy: IOmniSynchro;
begin
  try
    if not assigned(FController) then
      Exit;
    if (not (Data as TPreSignalData).OneSignalled)
       and FController.FOneSignalled.Test(Dummy)
    then
      FController.FOneSignalled.FCondVar.Release;
    if (not (Data as TPreSignalData).AllSignalled)
       and FController.FAllSignalled.Test(Dummy)
    then
      FController.FAllSignalled.FCondVar.Release;
  finally FreeAndNil(Data); end;
end; { TSynchroWaitFor.TSynchroClient.AfterSignal }

{ TSynchroWaitFor.TCondition }

constructor TSynchroWaitFor.TCondition.Create(AController: TSynchroWaitFor);
begin
  inherited Create;
  FCondVar := TConditionVariableCS.Create;
  FController := AController;
end; { TSynchroWaitFor.TCondition.Create }

destructor TSynchroWaitFor.TCondition.Destroy;
begin
  FreeAndNil(FCondVar);
  inherited;
end; { TSynchroWaitFor.TCondition.Destroy }

function TSynchroWaitFor.TCondition.Wait(timeout_ms: cardinal; var Signaller: IOmniSynchro): TWaitResult;
var
  Elapsed   : int64;
  Signaller1: IOmniSynchro;
  Timer     : TStopWatch;
  WaitTime  : cardinal;
begin
  Result := wrError;
  WaitTime := timeout_ms;
  if WaitTime > 0 then
    Timer := TStopWatch.StartNew;
  FController.FGate.Acquire;
  try
    repeat
      if WaitTime > 0 then begin
        Elapsed := Timer.ElapsedMilliseconds;
        if timeout_ms <= Elapsed then
          WaitTime := 0
        else
          WaitTime := timeout_ms - Elapsed;
      end;
      if Test(Signaller1) then
        Result := wrSignaled
      else  if WaitTime = 0 then
        Result := wrTimeout
      else begin
        case FCondVar.WaitFor(TCriticalSection(FController.FGate.GetSyncObj), WaitTime) of
          wrSignaled:
            begin
              if Test( Signaller1) then
                Result := wrSignaled
              else if WaitTime = 0 then
                Result := wrTimeout
              else
                Result := wrIOCompletion
            end;
          wrTimeout:
            Result := wrTimeout;
          wrAbandoned,
          wrError,
          wrIOCompletion:
            Result := wrError;
        end; // case
      end;
      if Result = wrSignaled then begin
        if assigned(Signaller1) then
          Signaller1.ConsumeSignalFromObserver(FController.FSynchClient);
        Signaller := Signaller1;
      end
    until Result <> wrIOCompletion;
  finally FController.FGate.Release; end;
end; { TSynchroWaitFor.TCondition.Wait }

{ TSynchroWaitFor }

constructor TSynchroWaitFor.Create(const SynchObjects: array of IOmniSynchro;
  const AShareLock: IOmniCriticalSection = nil);
var
  Member: IOmniSynchro;
begin
  if assigned( AShareLock) then
    FGate := AShareLock
  else
    FGate := CreateOmniCriticalSection;
  Assert(FGate.GetSyncObj is TCriticalSection);
  FSynchObjects := TSynchroList.Create;
  FOneSignalled := TOneCondition.Create(self);
  FAllSignalled := TAllCondition.Create(self);
  TSynchroClient.Create(self);
  for Member in SynchObjects do
    FSynchObjects.Add(Member);
end; { TSynchroWaitFor.Create }

destructor TSynchroWaitFor.Destroy;
var
  SynchClientEx: ISynchroClientEx;
begin
  FSynchObjects.Clear;
  FGate := nil;
  FreeAndNil(FSynchObjects);
  FreeAndNil(FOneSignalled);
  FreeAndNil(FAllSignalled);
  if Supports(FSynchClient, ISynchroClientEx, SynchClientEx) then
    SynchClientEx.Deref;
  FSynchClient := nil;
  inherited;
end; { TSynchroWaitFor.Destroy }

function TSynchroWaitFor.WaitAll(timeout_ms: cardinal): TWaitResult;
var
  Signaller: IOmniSynchro;
begin
  Result := FAllSignalled.Wait(timeout_ms, Signaller);
end; { TSynchroWaitFor.WaitAll }

function TSynchroWaitFor.WaitAny(timeout_ms: cardinal; var Signaller: IOmniSynchro): TWaitResult;
begin
  result := FAllSignalled.Wait(timeout_ms, Signaller);
end; { TSynchroWaitFor.WaitAny }

{ TOneCondition }

function TOneCondition.Test(var Signaller: IOmniSynchro): boolean;
var
  member: IOmniSynchro;
begin
  Result := False;
  FController.Gate.Acquire;
  try
    for member in FController.SynchObjects do begin
      Result := member.IsSignalled;
      if Result then
        continue; //for
      Signaller := member;
      break; //for
    end; //for
  finally FController.Gate.Release; end
end; { TOneCondition.Test }

function TOneCondition.WaitAll: boolean;
begin
  Result := False;
end; { TOneCondition.WaitAll }

{ TAllCondition }

function TAllCondition.Test(var Signaller: IOmniSynchro): boolean;
var
  member: IOmniSynchro;
begin
  Result := True;
  Signaller := nil;
  FController.Gate.Acquire;
  try
    for member in FController.SynchObjects do begin
      Result := member.IsSignalled;
      if not Result then
        break; //for
      if not assigned(Signaller) then
        Signaller := member;
    end; //for
  finally FController.Gate.Release; end;
end; { TAllCondition.Test }

function TAllCondition.WaitAll: boolean;
begin
  Result := True;
end; { TAllCondition.WaitAll }
{$ENDIF OTL_MobileSupport}
{$ENDIF ~MSWINDOWS}

{ TOmniSingleThreadUseChecker }

procedure TOmniSingleThreadUseChecker.AttachToCurrentThread;
begin
  FLock.Acquire;
  try
    FThreadID := cardinal(GetCurrentThreadID);
  finally FLock.Release; end;
end; { TOmniSingleThreadUseChecker.AttachToCurrentThread }

procedure TOmniSingleThreadUseChecker.Check;
{$IFDEF MSWINDOWS}
var
  thID: cardinal;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  FLock.Acquire;
  try
    thID := cardinal(GetCurrentThreadID);
    if (FThreadID <> 0) and (FThreadID <> thID) then
      raise Exception.CreateFmt(
        'Unsafe use: Current thread ID: %d, previous thread ID: %d',
        [thID, FThreadID]);
    FThreadID := thId;
  finally FLock.Release; end;
  {$ENDIF MSWINDOWS}
end; { TOmniSingleThreadUseChecker.Check }

procedure TOmniSingleThreadUseChecker.DebugCheck;
{$IFDEF MSWINDOWS}
{$IFDEF OTL_CheckThreadSafety}
var
  thID: cardinal;
{$ENDIF OTL_CheckThreadSafety}
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF OTL_CheckThreadSafety}
  FLock.Acquire;
  try
    thID := cardinal(GetCurrentThreadID);
    if (FThreadID <> 0) and (FThreadID <> thID) then
      raise Exception.CreateFmt(
        'Unsafe use: Current thread ID: %d, previous thread ID: %d',
        [thID, FThreadID]);
    FThreadID := thId;
  finally FLock.Release; end;
  {$ENDIF OTL_CheckThreadSafety}
  {$ENDIF MSWINDOWS}
end; { TOmniSingleThreadUseChecker.DebugCheck }

{$IFDEF OTL_MobileSupport}

{ TOmniSynchroObject }

constructor TOmniSynchroObject.Create(ABase: TSynchroObject; OwnsIt: boolean;
  const AShareLock: IOmniCriticalSection);
begin
  FBase := ABase;
  FOwnsBase := OwnsIt;
  if assigned(AShareLock) then
    FShareLock := AShareLock
  else
    FLock.Create(False);
  FObservers := TList<IOmniSynchroObserver>.Create
end; { TOmniSynchroObject.Create }

destructor TOmniSynchroObject.Destroy;
var
  Obs: IOmniSynchroObserver;
begin
  if FRefCount <> 0 then
    raise Exception.Create('TOmniSynchroObject.Destroy RefCount not zero.');
  with EnterSpinLock do begin
    for Obs in FObservers do
      Obs.DereferenceSynchObj(self, False);
    if FOwnsBase then
      FreeAndNil(FBase);
    FObservers.Free;
    inherited;
  end;
end; { TOmniSynchroObject.Destroy }

class function TOmniSynchroObject.NewInstance: TObject;
var
  Inst: TOmniSynchroObject;
begin
  Inst := TOmniSynchroObject(inherited NewInstance);
  Inst.FrefCount := 1;
  Result := Inst;
end; { TOmniSynchroObject.NewInstance }

procedure TOmniSynchroObject.AfterConstruction;
begin
  inherited;
  TInterlocked.Decrement(FRefCount);
end; { TOmniSynchroObject.AfterConstruction }

function TOmniSynchroObject._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount)
end; { TOmniSynchroObject._AddRef }

function TOmniSynchroObject._Release: Integer;
begin
  result := TInterlocked.Decrement(FRefCount);
  if result = 0 then
    Destroy;
end; { TOmniSynchroObject._Release }

function TOmniSynchroObject.Base: TSynchroObject;
begin
  Result := FBase;
end; { TOmniSynchroObject.Base }

function TOmniSynchroObject.EnterSpinLock: IInterface;
begin
  Result := TSynchroSpin.Create(Self)
end; { TOmniSynchroObject.EnterSpinLock }

function TOmniSynchroObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TOmniSynchroObject.QueryInterface }

procedure TOmniSynchroObject.PerformObservableAction(Action: TProc; DoLock: boolean);
var
  iObserver: integer;
  observer : IOmniSynchroObserver;
begin
  if DoLock then
    EnterSpinLock; //until end of method

  if FObservers.Count = 0 then
    Action
  else begin
    for observer in FObservers do
      observer.EnterGate;
    try
      for iObserver := 0 to FObservers.Count - 1 do
        observer.BeforeSignal(self, FData[iObserver]);
      Action;
      for iObserver := 0 to FObservers.Count - 1 do
        observer.AfterSignal(self, FData[iObserver]);
    finally
      for observer in FObservers do
        observer.LeaveGate;
    end // try
  end;
end; { TOmniSynchroObject.PerformObservableAction }

procedure TOmniSynchroObject.Release;
begin
  PerformObservableAction(procedure begin FBase.Release; end, True);
end; { TOmniSynchroObject.Release }

procedure TOmniSynchroObject.Signal;
begin
  Release;
end; { TOmniSynchroObject.Signal }

function TOmniSynchroObject.WaitFor(Timeout: LongWord): TWaitResult;
begin
  if FObservers.Count > 0 then
    raise Exception.Create('Cannot wait directly on TOmniSynchroObject whilst it is enrolled in a compound syncro object.')
  else
    Result := FBase.WaitFor(Timeout);
end; { TOmniSynchroObject.WaitFor }

{$IFDEF MSWINDOWS}
function TOmniSynchroObject.Handle: THandle;
begin
  if FBase is THandleObject then
    Result := THandleObject(FBase).Handle
  else
    raise Exception.Create('TOmniSynchroObject.Handle: Handle is not available!');
end; { TOmniSynchroObject.Handle }
{$ENDIF}

procedure TOmniSynchroObject.Acquire;
begin
  WaitFor(INFINITE)
end; { TOmniSynchroObject.Acquire }

procedure TOmniSynchroObject.AddObserver(const Observer: IOmniSynchroObserver);
begin
  with EnterSpinLock do begin
    if FObservers.IndexOf(Observer) = -1 then
      FObservers.Add(Observer);
    SetLength(FData, FObservers.Count);
  end;
end; { TOmniSynchroObject.AddObserver }

procedure TOmniSynchroObject.RemoveObserver(const Observer: IOmniSynchroObserver);
begin
  with EnterSpinLock do begin
    if FObservers.Count = 0 then
      Exit;
    FObservers.Remove(Observer);
    Observer.DereferenceSynchObj(self, FRefCount > 0);
    SetLength(FData, FObservers.Count)
  end;
end; { TOmniSynchroObject.RemoveObserver }

{ TSynchroSpin }

constructor TSynchroSpin.Create(AController: TOmniSynchroObject);
begin
  FController := AController;
  if assigned(FController.ShareLock) then
    FController.ShareLock.Acquire
  else
    FController.Lock.Enter;
end; { TSynchroSpin.Create }

destructor TSynchroSpin.Destroy;
begin
  if assigned(FController.ShareLock) then
    FController.ShareLock.Release
  else
    FController.Lock.Exit(True);
  inherited;
end; { TSynchroSpin.Destroy }

{ TOmniCountdownEvent }

constructor TOmniCountdownEvent.Create(Count, SpinCount: Integer; const AShareLock: IOmniCriticalSection);
begin
  FCountdown := TCountdownEvent.Create(Count, SpinCount);
  inherited Create(FCountdown, True, AShareLock)
end; { TOmniCountdownEvent.Create }

function TOmniCountdownEvent.IsSignalled: boolean;
begin
  Result := FCountdown.IsSet;
end; { TOmniCountdownEvent.IsSignalled }

procedure TOmniCountdownEvent.Reset;
begin
  PerformObservableAction(procedure begin FCountdown.Reset; end, True);
end; { TOmniCountdownEvent.Reset }

function TOmniCountdownEvent.BaseCountdown: TCountdownEvent;
begin
  Result := FCountdown;
end; { TOmniCountdownEvent.BaseCountdown }

procedure TOmniCountdownEvent.ConsumeSignalFromObserver(const Observer: IOmniSynchroObserver);
begin
end; { TOmniCountdownEvent.ConsumeSignalFromObserver }

{ TOmniEvent }

constructor TOmniEvent.Create(AManualReset, InitialState: boolean; const AShareLock: IOmniCriticalSection);
begin
  FEvent := TEvent.Create(nil, AManualReset, InitialState, '', False);
  FState := InitialState;
  FManualReset := AManualReset;
  inherited Create(FEvent, True, AShareLock);
end; { TOmniEvent.Create }

function TOmniEvent.BaseEvent: TEvent;
begin
  Result := FEvent;
end; { TOmniEvent.BaseEvent }

procedure TOmniEvent.ConsumeSignalFromObserver(const Observer: IOmniSynchroObserver);
begin
  // Here we are already inside the lock.
  if not FManualReset then begin
    FEvent.ResetEvent;
    FState := False;
  end
end; { TOmniEvent.ConsumeSignalFromObserver }

function TOmniEvent.IsSignalled: boolean;
begin
  Result := FState;
end; { TOmniEvent.IsSignalled }

procedure TOmniEvent.Reset;
begin
  PerformObservableAction(
    procedure
    begin
      FEvent.ResetEvent;
      FState := False;
    end,
    True);
end; { TOmniEvent.Reset }

procedure TOmniEvent.SetEvent;
begin
  PerformObservableAction(
    procedure
    begin
      FEvent.SetEvent;
      FState := True;
    end,
    True);
end; { TOmniEvent.SetEvent }

function TOmniEvent.WaitFor(Timeout: LongWord): TWaitResult;
begin
  Result := inherited WaitFor(Timeout);
  if (Result = wrSignaled) and (not FManualReset) then
    FState := False;
end; { TOmniEvent.WaitFor }

{$IFNDEF MSWINDOWS}

{ TPreSignalData }

constructor TPreSignalData.Create(AOneSignalled, AllSignalled: boolean);
begin
  OneSignalled := AOneSignalled;
  AllSignalled := AllSignalled;
end; { TPreSignalData.Create }

{$ENDIF ~MSWINDOWS}
{$ENDIF OTL_MobileSupport}

{ TInterlockedEx }

class function TInterlockedEx.Add(var Target: NativeInt; Increment: NativeInt): NativeInt;
begin
  {$IFDEF CPUX64}
  Result := TInterlocked.Add(Int64(Target), Int64(Increment));
  {$ELSE}{$IFDEF OTL_HasTInterlocked}
  Result := TInterlocked.Add(Integer(Target), Integer(Increment));
  {$ELSE}
  Result := InterlockedExchangeAdd(Target, Increment);
  {$ENDIF}{$ENDIF}
end; { TInterlockedEx.Add }

class function TInterlockedEx.CAS(const oldValue, newValue: pointer; var destination): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := OtlSync.CAS(oldValue, newValue, destination);
  {$ELSE}
  Result := CompareExchange(NativeInt(destination), NativeInt(newValue), NativeInt(oldValue)) = NativeInt(newValue);
  {$ENDIF}
end; { TInterlockedEx.CAS }

class function TInterlockedEx.CAS(const oldValue, newValue: NativeInt;
  var destination): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := OtlSync.CAS(oldValue, newValue, destination);
  {$ELSE}
  Result := CompareExchange(NativeInt(destination), newValue, oldValue) = NativeInt(newValue);
  {$ENDIF}
end; { TInterlockedEx.CAS }

class function TInterlockedEx.CompareExchange(var Target: NativeInt; Value: NativeInt; Comparand: NativeInt): NativeInt;
begin
  {$IFDEF CPUX64}
  Result := TInterlocked.CompareExchange(Int64(Target), Int64(Value), Int64(Comparand));
  {$ELSE}{$IFDEF OTL_HasTInterlocked}
  Result := TInterlocked.CompareExchange(Integer(Target), Integer(Value), Integer(Comparand));
  {$ELSE}
  Result := InterlockedCompareExchange(Target, Value, Comparand);
  {$ENDIF}{$ENDIF}
end; { TInterlockedEx.CompareExchange }

class function TInterlockedEx.Decrement(var Target: Integer): Integer;
begin
  {$IFDEF OTL_HasTInterlocked}
  Result := TInterlocked.Decrement(Target);
  {$ELSE}
  Result := InterlockedDecrement(Target);
  {$ENDIF OTL_HasTInterlocked}
end; { TInterlockedEx.Decrement }

class function TInterlockedEx.Increment(var Target: Integer): Integer;
begin
  {$IFDEF OTL_HasTInterlocked}
  Result := TInterlocked.Increment(Target);
  {$ELSE}
  Result := InterlockedIncrement(Target);
  {$ENDIF OTL_HasTInterlocked}
end; { TInterlockedEx.Increment }

initialization
  GOmniCancellationToken := CreateOmniCancellationToken;
  GOmniCSInitializer := TOmniCriticalSection.Create;
  {$IFDEF CPUX64}
  CASAlignment := 16;
  {$ELSE}
  CASAlignment := 8;
  {$ENDIF CPUX64}
finalization
  FreeAndNil(GOmniCSInitializer);
end.
