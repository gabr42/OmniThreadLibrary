///<summary>Interfaces for platform independant synchronization primitives.
///    Part of the OmniThreadLibrary project. Requires Delphi XE7.</summary>
///<author>Sean B. Durkin</author>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017 Sean B. Durkin, Primoz Gabrijelcic
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
///   Authors           : Seam B. Durkin, Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Creation date     : 2017-02-11
///   Last modification : 2017-02-11
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2017-02-11
///       - Imported from mobile/Otl.Parallel.SynchroPrimitives.InterfaceLevel.pas.

unit OtlSync.Platform.Interfaced;

// IMPORTANT!
//  READ THE COMMENTS IN UNIT OtlSync.Platform.

{$I OtlOptions.inc}

interface

uses
  System.SyncObjs,
  System.Generics.Collections,
  System.SysUtils,
  OtlSync.Platform,
  OtlSync.Platform.Atomic;

const
  FOREVER = INFINITE;
  MaxCardinal: cardinal = cardinal(-1);

type
  TOmniLockingMechanism = (KernelLocking, BusLocking);

  TOmniSignalState = (esSignalled, esNotSignalled, esUnknown);

  TOmniSynchroCapability = (
    //ISynchro
    scModular, scSupportsSignalled,
    //ISynchrom, IOmniLock
    scPoolManaged,
    //IEvent
    scManualEvent, scLight,
    //IOmniSemaphore
    scSupportsValueTesting,
    //IOmniLock
    scKernelMode
  );
  TOmniSynchroCapabilities = set of TOmniSynchroCapability;

  IOmniSynchro = interface ['{EF480FC2-7BBB-40B9-A9F7-246A7834CFA9}']
    function GetCapabilities: TOmniSynchroCapabilities;
  {$IFDEF MSWINDOWS}
    function GetHandle: THandle;
  {$ENDIF}
  //
    procedure Signal;
    /// <remarks> IOmniSynchro.isSignalled() is not supported for auto-reset events and semaphores.</remarks>
    function  SignalState: TOmniSignalState;
    function  WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
    function  AsObject: TObject;
    function  AsMWObject: TObject;
    function  IsSignalled: boolean;

    {$IFDEF MSWINDOWS}
      /// <remarks> IOmniSynchro.Handle is not supported for:
      ///   Non-windows platforms;
      ///   Light events;
      ///   Testable semaphores;
      ///   CountDowns/Ups  nor
      ///   Functional events.
      ///  Basically, it is only available for heavy events and native semaphores,
      ///  all on a Windows platform.
      /// </remarks>
      property Handle: THandle read GetHandle;
    {$ENDIF}
    property Capabilities: TOmniSynchroCapabilities read GetCapabilities;
  end; { IOmniSynchro }

  TOmniSynchroArray = TArray<IOmniSynchro>;

  IOmniEvent = interface(IOmniSynchro) ['{3E6E1606-88E5-4898-9F79-613085CAE090}']
    procedure SetEvent;
    procedure ResetEvent;
  end; { IOmniEvent }

  IOmniSemaphore = interface(IOmniSynchro)
  ['{4ACCD111-3E22-4902-B397-3309D84BBDD9}']
    function InitialValue: cardinal;
    /// <remarks> IOmniSemaphore.Value() is not supported for native semaphores.</remarks>
    function Value: cardinal;
    /// <remarks> IOmniSemaphore.Reset() is not supported for native semaphores.</remarks>
    procedure Reset;
  end; { IOmniSemaphore }

  IOmniLock = interface
  ['{38665699-5A91-4930-8AB8-AC4AD36B7182}']
    function GetCapabilities: TOmniSynchroCapabilities;
  //
    procedure Enter;
    procedure Leave;
    function  LockCount: integer;
    function  AsObject: TObject;
    function  AsSpinLock: POmniAtomicSpinLock;
    function  AsCriticalSection: TCriticalSection;
    property Capabilities: TOmniSynchroCapabilities read GetCapabilities;
  end; { IOmniLock }

  IOmniCountDown = interface(IOmniSynchro)
  ['{23D82B7E-9670-4B24-835C-D1E879CF36C9}']
    function  Allocate: cardinal; // Signal and return the value.
    procedure CounterSignal;
    function  InitialValue: cardinal;
    function  Value: cardinal;
    function  SignalHit: boolean;
  end; { IOmniCountDown }

  IOmniCountUp = interface(IOmniSynchro)
  ['{C3AD39CC-7918-44E3-AE08-B5E8F2F9EDB5}']
    function  InitialValue: cardinal;
    function  MaxValue: cardinal;
    function  Value: cardinal;
    function  SignalHit: boolean;
  end; { IOmniCountUp }

  IOmniPlatformMREW = interface
  ['{4929F170-F5DC-41F8-852E-400D99B441BC}']
    function  EnterRead(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitRead;
    function  EnterWrite(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitWrite;
    function  ReaderCount: integer; // if +ve, this is the number of entered readers,
                                    // if -ve, there is an entered writer.
    function  AsReadLock: IOmniLock;
    function  AsWriteLock: IOmniLock;
  end; { IOmniPlatformMREW }

function CreateOmniPlatformMREW: IOmniPlatformMREW;

type
  TOmniSynchroFactory = class
  public type
    TObjectList = class(TObjectList<TObject>) end;
    TObjectQueue = class(TQueue<TObject>) end;
  private
    FManualKernelEvents: TObjectQueue;
    FAutoKernelEvents  : TObjectQueue;
    FLightEvents       : TObjectQueue;
    FSemaphores        : TObjectQueue; // TSimulatedSemaphores only. Native semaphores cannot be pooled.
    FCrits             : TObjectQueue;
    FCountDowns        : TObjectQueue;
    FCountUps          : TObjectQueue;
    FFunctionals       : TObjectQueue;
    [Volatile]
    FAquisitionCount   : TOmniVolatileUint32;
    FPoolMax           : cardinal;
    //FSemaphoreCount    : cardinal;
  private
    [Volatile] FLock: TOmniAtomicSpinLock;
    function Queues: TArray<TObjectQueue>;
  public
    constructor Create( APoolMax: cardinal);
    destructor  Destroy; override;
    function  AcquireKernelEvent(AManual, AInitialState, AReUse: boolean): IOmniEvent;
    function  AcquireLightEvent(AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IOmniEvent;
    function  AcquireNativeSemaphore(AInitialCount: cardinal): IOmniSemaphore; // Native semaphores are never pooled.
    function  AcquireTestableSemaphore(AInitialCount: cardinal; AReUse: boolean): IOmniSemaphore;
    /// <remarks> When re-using critical sections, Enter/Leave calls must be balanced before releasing the last reference. </remarks>
    function  AcquireCriticalSection(AReUse: boolean): IOmniLock;  overload;
    class function AcquireCriticalSection: IOmniLock; overload;
    function  AcquireSpinLock: IOmniLock; // Spinlocks are never pooled.
    function  AcquireCountDown(InitialValue: cardinal; AReUse: boolean): IOmniCountDown;
    function  AcquireCountUp(InitialValue, MaxValue: cardinal; AReUse: boolean): IOmniCountUp;
    /// <remarks> To pulse a functional event, call Signal() on it. </remarks>
    function  AcquireFunctionalEvent(ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock; AReUse: boolean): IOmniSynchro;
    procedure PurgeAllPools;
    function  CanDestroy: boolean;
  end; { TOmniSynchroFactory }

{$REGION 'For internal use only.'}
/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountDownIntf(const APool: TOmniSynchroFactory; InitialValue: cardinal): IOmniCountDown;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountUpIntf(const APool: TOmniSynchroFactory; InitialValue, MaxValue: cardinal): IOmniCountUp;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateKernelEventIntf(const APool: TOmniSynchroFactory; ManualReset, InitialState: boolean): IOmniEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateLightEventIntf(const APool: TOmniSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IOmniEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCritLockIntf(const APool: TOmniSynchroFactory): IOmniLock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateSpinLockIntf: IOmniLock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateTestableSemaphoreIntf(const APool: TOmniSynchroFactory; AInitialCount: cardinal): IOmniSemaphore;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateNativeSemaphoreIntf(AInitialCount: cardinal): IOmniSemaphore;
{$ENDREGION}

implementation

uses
  OtlSync.Platform.Basic;

type
  TOmniLightEventFriend = class(TLightEvent)
  end;

  TOmniCountDownFriend = class(TCountDown)
  end;

  TOmniCountUpFriend = class(TCountUp)
  end;

  TOmniFunctionalEventFriend = class(TFunctionalEvent)
  end;

  TOmniRecycleObject = class abstract(TObject, IInterface)
  private const
    CObjPooledFlag = integer($80000000);
  var
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FPool: TOmniSynchroFactory;
    [Volatile] FRefCount: TOmniVolatileInt32;
    FAcquired: boolean;
  strict protected
    function  QueryInterface( const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  AsObject: TObject;
    function  GetRefCount: integer; inline;
  protected
    function  Pool: TOmniSynchroFactory.TObjectQueue; virtual; abstract;
    procedure ReleaseConfiguration; virtual;
  public
    constructor Create(const APool: TOmniSynchroFactory);
    procedure AfterConstruction; override;
    function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    class function NewInstance: TObject; override;
    function  SignalState: TOmniSignalState; virtual; abstract;
    property RefCount: integer read GetRefCount;
  end; { TOmniRecycleObject }

  TOmniRecycleSynchro = class(TOmniRecycleObject, IOmniSynchro)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; virtual;
    {$ENDIF}
    function  GetCapabilities: TOmniSynchroCapabilities; override;
  public
    function  AsMWObject: TObject; virtual;
    function  IsSignalled: boolean; virtual; abstract;
    procedure Signal; virtual; abstract;
    function  SignalState: TOmniSignalState; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; virtual; abstract;
  end; { TOmniRecycleSynchro }

  TOmniRecycleEvent = class(TOmniRecycleSynchro, IOmniEvent)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; override;
    {$ENDIF}
    function  Pool: TOmniSynchroFactory.TObjectQueue; override;
  private
    FAsyncClear     : boolean;
    FBase           : TOtlEvent;
    FIsLight        : boolean;
    [Volatile] FLock: TOmniAtomicSpinLock; // Only used for manual events.
    FManual         : boolean;
    FShadow         : TOmniSignalState;
    FWaiters        : cardinal;
  protected
    procedure Reconfigure(manual: boolean; value: cardinal); virtual;
  public
    constructor CreateAsKernel(const APool: TOmniSynchroFactory; AManual, AInitialState: boolean);
    constructor CreateAsLight(const APool: TOmniSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  IsSignalled: boolean; override;
    procedure ResetEvent;
    procedure Signal; override;
    function  SignalState: TOmniSignalState; override;
    procedure SetEvent;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniRecycleEvent }

  TOmniRecycleSemaphore = class(TOmniRecycleSynchro, IOmniSemaphore)
  private type
    TSimulatedSemaphore = class(TFunctionalEvent)
    protected
      [Volatile] FCurrentValue: TOmniVolatileUint32;
      FInitialValue           : cardinal;
      [Volatile] FLock        : TOmniAtomicSpinLock;
      procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean;
        var isInSignalledState: boolean); override;
    public
      constructor Create(AInitialValue: cardinal);
      destructor  Destroy; override;
      procedure Reset;
      procedure ReconfigureInitial(value: cardinal);
      function  SignalState: TOmniSignalState;
    end; { TSimulatedSemaphore }
  var
    FBase: TSimulatedSemaphore;
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; override;
    {$ENDIF}
    function  Pool: TOmniSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(value: cardinal); virtual;
  public
    constructor Create(const APool: TOmniSynchroFactory; AInitialCount: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    procedure Reset;
    procedure Signal; override;
    function  SignalState: TOmniSignalState;override;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniRecycleSemaphore }

  TOmniNativeSemaphore = class(TInterfacedObject, IOmniSemaphore)
  private
    FBase           : TOtlSemaphore;
    FInitial        : cardinal;
    [Volatile] FLock: TOmniAtomicSpinLock;
    FShadow         : cardinal;
    FWaiters        : cardinal;
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle;
    {$ENDIF}
  public
    constructor Create( AInitialCount: cardinal);
    destructor Destroy; override;
    function  AsObject: TObject;
    function  AsMWObject: TObject;
    function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean;
    procedure Reset;
    procedure Signal;
    function  SignalState: TOmniSignalState;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult;
  end; { TOmniNativeSemaphore }

  TOmniRecycleCriticalSection = class(TOmniRecycleObject, IOmniLock)
  strict private
    FCrit                : TCriticalSection;
    [Volatile] FLockCount: TOmniVolatileInt32;
  strict protected
    procedure Enter;
    procedure Leave;
    function  LockCount: integer;
    function Pool: TOmniSynchroFactory.TObjectQueue; override;
  public
    constructor Create(const APool: TOmniSynchroFactory);
    destructor  Destroy; override;
    function  AsCriticalSection: TCriticalSection;
    function  AsSpinLock: POmniAtomicSpinLock;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  SignalState: TOmniSignalState; override;
  end; { TOmniRecycleCriticalSection }

  TOmniSpinLock = class(TInterfacedObject, IOmniLock)
  strict private
    [Volatile] FLock: TOmniAtomicSpinLock;
  strict protected
    procedure Enter;
    procedure Leave;
  public
    constructor Create;
    destructor  Destroy; override;
    function  AsCriticalSection: TCriticalSection;
    function  AsObject: TObject;
    function  AsSpinLock: POmniAtomicSpinLock;
    function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    function  LockCount: integer;
  end; { TOmniSpinLock }

  TOmniRecycleCountDown = class(TOmniRecycleSynchro, IOmniCountDown)
  strict private
    FBase: TCountDown;
  protected
    function  Pool: TOmniSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(AInitial: cardinal);
  public
    constructor Create( const APool: TOmniSynchroFactory; AInitialValue: cardinal);
    destructor  Destroy; override;
    function  Allocate: cardinal; // Signal and return the value.
    procedure CounterSignal;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    procedure Signal; override;
    function  SignalHit: boolean;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniRecycleCountDown }

  TOmniRecycleCountUp = class(TOmniRecycleSynchro, IOmniCountUp)
  strict private
    FBase: TCountUp;
  protected
    function  Pool: TOmniSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(AInitial, AMaxValue: cardinal);
  public
    constructor Create( const APool: TOmniSynchroFactory; AInitial, AMaxValue: cardinal);
    destructor  Destroy; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    function  MaxValue: cardinal;
    procedure Signal; override;
    function  SignalHit: boolean;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniRecycleCountUp }

  TOmniRecycleFunctional = class(TOmniRecycleSynchro)
  strict private
    FBase: TFunctionalEvent;
  protected
    function  Pool: TOmniSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
    procedure ReleaseConfiguration; override;
  public
    constructor Create(const APool: TOmniSynchroFactory; ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
    destructor  Destroy; override;
    function  GetCapabilities: TOmniSynchroCapabilities; override;
    function  IsSignalled: boolean; override;
    procedure Signal; override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TOmniRecycleFunctional }

  TOmniPlatformMREW = class(TInterfacedObject, IOmniPlatformMREW)
  strict private type
    TFacade = class(TInterfacedObject, IInterface, IOmniLock)
    strict protected
      procedure Enter; virtual; abstract;
      procedure Leave; virtual; abstract;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      function  LockCount: integer; virtual;
    protected
      FController: TOmniPlatformMREW;
    public
      function  AsCriticalSection: TCriticalSection;
      function  AsObject: TObject;
      function  AsSpinLock: POmniAtomicSpinLock;
      function  GetCapabilities: TOmniSynchroCapabilities; virtual;
    end; { TFacade }

    TReaderLock = class(TFacade)
    strict protected
      procedure Enter; override;
      procedure Leave; override;
    public
      function  LockCount: integer; override;
    end; { TReaderLock }

    TWriterLock = class(TFacade)
    strict protected
      procedure Enter; override;
      procedure Leave; override;
      function  LockCount: integer; override;
    end; { TWriterLock }

  var
    FMREW       : TOtlMREW;
    FReadFacade : TReaderLock;
    FWriteFacade: TWriterLock;
  public
    constructor Create;
    destructor  Destroy; override;
    function  EnterRead(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitRead;
    function  EnterWrite(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitWrite;
    function  ReaderCount: integer;
    function  AsReadLock: IOmniLock;
    function  AsWriteLock: IOmniLock;
  end; { TOmniPlatformMREW }

{ exports }

function CreateOmniPlatformMREW: IOmniPlatformMREW;
begin
  Result := TOmniPlatformMREW.Create;
end; { CreateOmniPlatformMREW }

{ internal exports }

function _CreateCountDownIntf(const APool: TOmniSynchroFactory; InitialValue: cardinal): IOmniCountDown;
begin
  Result := TOmniRecycleCountDown.Create(APool, InitialValue);
end; { _CreateCountDownIntf }

function _CreateCountUpIntf(const APool: TOmniSynchroFactory; InitialValue, MaxValue: cardinal): IOmniCountUp;
begin
  Result := TOmniRecycleCountUp.Create(APool, InitialValue, MaxValue);
end; { _CreateCountUpIntf }

function _CreateKernelEventIntf(const APool: TOmniSynchroFactory; ManualReset, InitialState: boolean): IOmniEvent;
begin
  Result := TOmniRecycleEvent.CreateAsKernel(APool, ManualReset, InitialState);
end; { _CreateKernelEventIntf }

function _CreateLightEventIntf(const APool: TOmniSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IOmniEvent;
begin
  Result := TOmniRecycleEvent.CreateAsLight(APool, ManualReset, InitialState, SpinMax);
end; { _CreateLightEventIntf }

function _CreateCritLockIntf(const APool: TOmniSynchroFactory): IOmniLock;
begin
  Result := TOmniRecycleCriticalSection.Create(APool);
end; { _CreateCritLockIntf }

function _CreateSpinLockIntf: IOmniLock;
begin
  Result := TOmniSpinLock.Create;
end; { _CreateSpinLockIntf }

function _CreateTestableSemaphoreIntf(const APool: TOmniSynchroFactory; AInitialCount: cardinal): IOmniSemaphore;
begin
  Result := TOmniRecycleSemaphore.Create(APool, AInitialCount);
end; { _CreateTestableSemaphoreIntf }

function _CreateNativeSemaphoreIntf(AInitialCount: cardinal): IOmniSemaphore;
begin
  Result := TOmniNativeSemaphore.Create(AInitialCount);
end; { _CreateNativeSemaphoreIntf }

{ TOmniSynchroFactory }

constructor TOmniSynchroFactory.Create(APoolMax: cardinal);
begin
  FLock.Initialize;
  FPoolMax := APoolMax;
  FManualKernelEvents := TObjectQueue.Create;
  FAutoKernelEvents   := TObjectQueue.Create;
  FLightEvents        := TObjectQueue.Create;
  FSemaphores         := TObjectQueue.Create;
  FCrits              := TObjectQueue.Create;
  FCountDowns         := TObjectQueue.Create;
  FCountUps           := TObjectQueue.Create;
  FFunctionals        := TObjectQueue.Create;
end; { TOmniSynchroFactory.Create }

destructor TOmniSynchroFactory.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  Queue: TObjectQueue;
{$ENDIF}
begin
  if FAquisitionCount.Read <> 0 then
    raise TOmniSynchroException.Create(ECannotDestroySynchroFactory);
  PurgeAllPools;
  {$IFNDEF AUTOREFCOUNT}
  for Queue in Queues do
    Queue.Free;
  {$ENDIF}
  inherited;
  FLock.Finalize;
end; { TOmniSynchroFactory.Destroy }

function TOmniSynchroFactory.AcquireCountDown(
  InitialValue: cardinal; AReUse: boolean): IOmniCountDown;
var
  Addend: TOmniRecycleCountDown;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue  := FCountDowns;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleCountDown;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(InitialValue);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleCountDown.Create(Pool, InitialValue);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TOmniSynchroFactory.AcquireCountDown }

function TOmniSynchroFactory.AcquireCountUp(
  InitialValue, MaxValue: cardinal; AReUse: boolean): IOmniCountUp;
var
  Addend: TOmniRecycleCountUp;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue  := FCountUps;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleCountUp;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(InitialValue, MaxValue);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleCountUp.Create(Pool, InitialValue, MaxValue);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TOmniSynchroFactory.AcquireCountUp }

class function TOmniSynchroFactory.AcquireCriticalSection: IOmniLock;
begin
  Result := TOmniRecycleCriticalSection.Create(nil);
end; { TOmniSynchroFactory.AcquireCriticalSection }

function TOmniSynchroFactory.AcquireCriticalSection(AReUse: boolean): IOmniLock;
var
  Addend: TOmniRecycleCriticalSection;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FCrits;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleCriticalSection;
    Addend.FRefCount.Write(0);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleCriticalSection.Create(Pool);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TOmniSynchroFactory.AcquireCriticalSection }

function TOmniSynchroFactory.AcquireFunctionalEvent(
  ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock; AReUse: boolean): IOmniSynchro;
var
  Addend : TOmniRecycleFunctional;
  DoPulse: boolean;
  Pool   : TOmniSynchroFactory;
  Queue  : TObjectQueue;
begin
  DoPulse := False;
  Queue := FFunctionals;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleFunctional;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(ASignalTest, APlock);
    DoPulse := True;
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleFunctional.Create(Pool, ASignalTest, APLock);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
  if DoPulse then
    Addend.Signal;
end; { TOmniSynchroFactory.AcquireFunctionalEvent }

function TOmniSynchroFactory.AcquireKernelEvent(
  AManual, AInitialState, AReUse: boolean): IOmniEvent;
var
  Addend: TOmniRecycleEvent;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  if AManual then
    Queue := FManualKernelEvents
  else
    Queue := FAutoKernelEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleEvent;
    Addend.FRefCount.Write(0);
    if (not (scSupportsSignalled in Addend.GetCapabilities))
       or (Addend.IsSignalled <> AInitialState) then
    begin
      if AInitialState then
        Addend.SetEvent
      else
        Addend.ResetEvent;
    end
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleEvent.CreateAsKernel(Pool, AManual, AInitialState);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TOmniSynchroFactory.AcquireKernelEvent }

function TOmniSynchroFactory.AcquireLightEvent(
  AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IOmniEvent;
var
  Addend: TOmniRecycleEvent;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FLightEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleEvent;
    Addend.Reconfigure(AManual, SpinMax);
    Addend.FRefCount.Write(0);
    if (not (scSupportsSignalled in Addend.GetCapabilities))
       or (Addend.IsSignalled <> AInitialState) then
    begin
      if AInitialState then
        Addend.SetEvent
      else
        Addend.ResetEvent;
    end
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleEvent.CreateAsLight(Pool, AManual, AInitialState, SpinMax);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave
end; { TOmniSynchroFactory.AcquireLightEvent }

function TOmniSynchroFactory.AcquireNativeSemaphore(AInitialCount: cardinal): IOmniSemaphore;
begin
  Result := TOmniNativeSemaphore.Create(AInitialCount);
end; { TOmniSynchroFactory.AcquireNativeSemaphore }

function TOmniSynchroFactory.AcquireTestableSemaphore(AInitialCount: cardinal; AReUse: boolean): IOmniSemaphore;
var
  Addend: TOmniRecycleSemaphore;
  Pool  : TOmniSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FSemaphores;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TOmniRecycleSemaphore;
    Addend.Reconfigure(AInitialCount);
    Addend.FRefCount.Write(0);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TOmniRecycleSemaphore.Create(Pool, AInitialCount);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TOmniSynchroFactory.AcquireTestableSemaphore }

function TOmniSynchroFactory.AcquireSpinLock: IOmniLock;
begin
  Result := TOmniSpinLock.Create;
end; { TOmniSynchroFactory.AcquireSpinLock }

function TOmniSynchroFactory.CanDestroy: boolean;
begin
  Result := FAquisitionCount.Read = 0;
end; { TOmniSynchroFactory.CanDestroy }

procedure TOmniSynchroFactory.PurgeAllPools;
begin
  FLock.WithinLock(
    procedure
    var
      Queue: TObjectQueue;
    begin
      for Queue in Queues do
        while Queue.Count > 0 do
        {$IFDEF AUTOREFCOUNT}
          Queue.Dequeue
        {$ELSE}
          Queue.Dequeue.Free
        {$ENDIF}
    end);
end; {  }

function TOmniSynchroFactory.Queues: TArray<TObjectQueue>;
begin
  Result := [FManualKernelEvents, FAutoKernelEvents, FLightEvents, FSemaphores, FCrits, FCountDowns, FCountUps, FFunctionals];
end; { TOmniSynchroFactory.Queues }

{ TOmniRecycleObject }

constructor TOmniRecycleObject.Create(const APool: TOmniSynchroFactory);
begin
  FPool := APool;
end; { TOmniRecycleObject.Create }

procedure TOmniRecycleObject.AfterConstruction;
begin
  FRefCount.Decrement;
end; { TOmniRecycleObject.AfterConstruction }

function TOmniRecycleObject.AsObject: TObject;
begin
  Result := Self;
end; { TOmniRecycleObject.AsObject }

function TOmniRecycleObject.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [];
  if assigned(FPool) then
    Include(Result, scPoolManaged);
end; { TOmniRecycleObject.GetCapabilities }

function TOmniRecycleObject.GetRefCount: integer;
begin
  Result := FRefCount.Read;
  if ((Result and CObjPooledFlag) <> 0) or (Result <= -2) then
    Result := -1;
end; { TOmniRecycleObject.GetRefCount }

class function TOmniRecycleObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TOmniRecycleObject(Result).FRefCount.Initialize(1);
end; { TOmniRecycleObject.NewInstance }

function TOmniRecycleObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TOmniRecycleObject.QueryInterface }

procedure TOmniRecycleObject.ReleaseConfiguration;
begin
  // do nothing
end; { TOmniRecycleObject.ReleaseConfiguration }

function TOmniRecycleObject._AddRef: integer;
begin
  Result := FRefCount.Increment;
end; { TOmniRecycleObject._AddRef }

function TOmniRecycleObject._Release: integer;
var
  OldValue     : integer;
  Pl           : TOmniSynchroFactory.TObjectQueue;
  PooledByOther: boolean;
begin
  Result := FRefCount.Decrement;
  if Result <> 0 then
    Exit;
  repeat
    OldValue := FRefCount.Read;
    PooledByOther := (OldValue and CObjPooledFlag) = CObjPooledFlag;
  until PooledByOther or FRefCount.CompareAndExchange(OldValue, OldValue or CObjPooledFlag);
  if PooledByOther then
    Exit;
  ReleaseConfiguration;
  if assigned(FPool) then begin
    if FAcquired then begin
      FAcquired := False;
      FPool.FAquisitionCount.Decrement;
    end;
    Pl := Pool;
    if cardinal(Pl.Count) < FPool.FPoolMax then
      Pool.Enqueue(Self)
    else begin
      FPool := nil;
      Destroy;
    end
  end
  else
    Destroy;
end; { TOmniRecycleObject._Release }

{ TOmniRecycleSynchro }

function TOmniRecycleSynchro.AsMWObject: TObject;
begin
  Result := nil
end; { TOmniRecycleSynchro.AsMWObject }

function TOmniRecycleSynchro.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TOmniRecycleSynchro.GetCapabilities }

{$IFDEF MSWINDOWS}
function TOmniRecycleSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TOmniRecycleSynchro.GetHandle }
{$ENDIF}

function TOmniRecycleSynchro.SignalState: TOmniSignalState;
begin
  if IsSignalled then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TOmniRecycleSynchro.SignalState }

{ TOmniRecycleEvent }

constructor TOmniRecycleEvent.CreateAsKernel(const APool: TOmniSynchroFactory; AManual, AInitialState: boolean);
begin
  FLock.Initialize;
  inherited Create(APool);
  FManual := AManual;
  FisLight := False;
  if AInitialState then
    FShadow := esSignalled
  else
    FShadow := esNotSignalled;
  FWaiters := 0;
  FAsyncClear := False;
  FBase := TKernelEvent.Create(AManual, AInitialState);
end; { TOmniRecycleEvent.CreateAsKernel }

constructor TOmniRecycleEvent.CreateAsLight(
  const APool: TOmniSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
begin
  FLock.Initialize;
  inherited Create(APool);
  FManual := AManual;
  FisLight := True;
  if AInitialState then
    FShadow := esSignalled
  else
    FShadow := esNotSignalled;
  FWaiters := 0;
  FAsyncClear := False;
  FBase  := TLightEvent.Create(AManual, AInitialState, SpinMax);
end; { TOmniRecycleEvent.CreateAsLight }

destructor TOmniRecycleEvent.Destroy;
begin
  FBase.Free;
  FLock.Finalize;
  inherited;
end; { TOmniRecycleEvent.Destroy }

function TOmniRecycleEvent.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TOmniRecycleEvent.AsMWObject }

function TOmniRecycleEvent.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  if FIsLight then
    Include(Result, scLight);
  if FManual then
    Include(Result, scManualEvent);
  if FIsLight or FManual then
    Include(Result, scSupportsSignalled);
end; { TOmniRecycleEvent.GetCapabilities }

{$IFDEF MSWINDOWS}
function TOmniRecycleEvent.GetHandle: THandle;
begin
  if FisLight then
      Result := 0
    else
      Result := FBase.Handle;
end; { TOmniRecycleEvent.GetHandle }
{$ENDIF}

function TOmniRecycleEvent.IsSignalled: boolean;
var
  ok: boolean;
begin
  if FIsLight then
    Result := FBase.IsSignalled
  else if FManual then
    Result := FShadow = esSignalled
  else begin
    FLock.Enter;
    ok := FShadow <> esUnknown;
    Result := FShadow = esSignalled;
    FLock.Leave;
    if not ok then
      raise TOmniSynchroException.Create(EIsSignalledNotSupported);
  end
end; { TOmniRecycleEvent.IsSignalled }

function TOmniRecycleEvent.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if not assigned(FPool) then
    Result := nil
  else if FIsLight then
    Result := FPool.FLightEvents
  else if FManual then
    Result := FPool.FManualKernelEvents
  else
    Result := FPool.FAutoKernelEvents;
end; { TOmniRecycleEvent.Pool }

procedure TOmniRecycleEvent.ResetEvent;
begin
  if not FIsLight then
    FLock.Enter;
  FBase.ResetEvent;
  if not FIsLight then begin
    if FManual or (FWaiters = 0) then
      FShadow := esNotSignalled
    else
      FAsyncClear := True;
    FLock.Leave
  end;
end; { TOmniRecycleEvent.ResetEvent }

procedure TOmniRecycleEvent.SetEvent;
begin
  if not FisLight then
    FLock.Enter;
  FBase.SetEvent;
  if not FIsLight then begin
    if FManual or (FWaiters = 0) then
      FShadow := esSignalled
    else
      FAsyncClear := False;
    FLock.Leave;
  end
end; { TOmniRecycleEvent.SetEvent }

procedure TOmniRecycleEvent.Reconfigure(manual: boolean; value: cardinal);
begin
  if FBase is TLightEvent then
    TOmniLightEventFriend(FBase).Reconfigure(Manual, Value);
end; { TOmniRecycleEvent.Reconfigure }

procedure TOmniRecycleEvent.Signal;
begin
  SetEvent;
end; { TOmniRecycleEvent.Signal }

function TOmniRecycleEvent.SignalState: TOmniSignalState;
begin
  if IsSignalled then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TOmniRecycleEvent.SignalState }

function TOmniRecycleEvent.WaitFor(timeout: cardinal): TWaitResult;
begin
  if (not FIsLight) and (not FManual) then begin
    FLock.Enter;
    if FWaiters = 0 then
      FAsyncClear := False;
    Inc(FWaiters);
    FShadow := esUnknown;
    FLock.Leave
  end;
  Result := FBase.WaitFor(timeout);
  if (not FIsLight) and (not FManual) then begin
    FLock.Enter;
    Dec(FWaiters);
    if (FWaiters = 0) and FAsyncClear then begin
      FAsyncClear := False;
      FShadow     := esNotSignalled;
    end;
    FLock.Leave;
  end
end; { TOmniRecycleEvent.WaitFor }

{ TOmniRecycleSemaphore.TSimulatedSemaphore }

constructor TOmniRecycleSemaphore.TSimulatedSemaphore.Create(AInitialValue: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitialValue;
  FCurrentValue.Initialize(FInitialValue);
  inherited Create(nil, @FLock);
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.Create }

destructor TOmniRecycleSemaphore.TSimulatedSemaphore.Destroy;
begin
  FLock.Finalize;
  FCurrentValue.Finalize;
  inherited;
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.Destroy }

procedure TOmniRecycleSemaphore.TSimulatedSemaphore.ReconfigureInitial(value: cardinal);
begin
  FInitialValue := value;
  Reset;
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.ReconfigureInitial }

procedure TOmniRecycleSemaphore.TSimulatedSemaphore.Reset;
begin
  FLock.WithinLock(
    procedure
    begin
      FCurrentValue.Write(FInitialValue);
      if FInitialValue > 0 then
        Pulse;
    end)
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.Reset }

function TOmniRecycleSemaphore.TSimulatedSemaphore.SignalState: TOmniSignalState;
begin
  if FCurrentValue.Read > 0 then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.SignalState }

procedure TOmniRecycleSemaphore.TSimulatedSemaphore.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  wasSuccessfullyAcquired := doAcquire and FCurrentValue.DecrementIfAboveZero;
  isInSignalledState := FCurrentValue.Read > 0;
end; { TOmniRecycleSemaphore.TSimulatedSemaphore.SignalTest }

{ TOmniRecycleSemaphore }

constructor TOmniRecycleSemaphore.Create(
  const APool: TOmniSynchroFactory; AInitialCount: cardinal);
begin
  inherited Create(APool);
  FBase := TSimulatedSemaphore.Create(AInitialCount);
end; { TOmniRecycleSemaphore.Create }

destructor TOmniRecycleSemaphore.Destroy;
begin
  FBase.Free;
  inherited;
end; { TOmniRecycleSemaphore.Destroy }

function TOmniRecycleSemaphore.AsMWObject: TObject;
begin
  Result := nil;
end; { TOmniRecycleSemaphore.AsMWObject }

function TOmniRecycleSemaphore.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
  Include(Result, scSupportsValueTesting);
end; { TOmniRecycleSemaphore.GetCapabilities }

{$IFDEF MSWINDOWS}
function TOmniRecycleSemaphore.GetHandle: THandle;
begin
  Result := 0;
end; { TOmniRecycleSemaphore.GetHandle }
{$ENDIF}

function TOmniRecycleSemaphore.InitialValue: cardinal;
begin
  Result := FBase.FInitialValue;
end; { TOmniRecycleSemaphore.InitialValue }

function TOmniRecycleSemaphore.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TOmniRecycleSemaphore.IsSignalled }

function TOmniRecycleSemaphore.SignalState: TOmniSignalState;
begin
  Result := FBase.SignalState;
end; { TOmniRecycleSemaphore.SignalState }

function TOmniRecycleSemaphore.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if not assigned(FPool) then
    Result := nil
  else
    Result := FPool.FSemaphores;
end; { TOmniRecycleSemaphore.Pool }

procedure TOmniRecycleSemaphore.Reconfigure(value: cardinal);
begin
  FBase.ReconfigureInitial(value);
end; { TOmniRecycleSemaphore.Reconfigure }

procedure TOmniRecycleSemaphore.Reset;
begin
  FBase.Reset;
end; { TOmniRecycleSemaphore.Reset }

procedure TOmniRecycleSemaphore.Signal;
begin
  FBase.FLock.Enter;
  FBase.FCurrentValue.Increment;
  FBase.FLock.Leave;
  FBase.Pulse;
end; { TOmniRecycleSemaphore.Signal }

function TOmniRecycleSemaphore.Value: cardinal;
begin
  FBase.FLock.Enter;
  Result := FBase.FCurrentValue.Read;
  FBase.FLock.Leave;
end; { TOmniRecycleSemaphore.Value }

function TOmniRecycleSemaphore.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TOmniRecycleSemaphore.WaitFor }

{ TOmniNativeSemaphore }

constructor TOmniNativeSemaphore.Create(AInitialCount: cardinal);
begin
  FInitial := AInitialCount;
  FBase    := TOtlSemaphore.Create(FInitial);
  FWaiters := 0;
  FShadow  := FInitial;
  FLock.Initialize;
end; { TOmniNativeSemaphore.Create }

destructor TOmniNativeSemaphore.Destroy;
begin
  FLock.Finalize;
  FBase.Free;
  inherited;
end; { TOmniNativeSemaphore.Destroy }

function TOmniNativeSemaphore.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TOmniNativeSemaphore.AsMWObject }

function TOmniNativeSemaphore.AsObject: TObject;
begin
  Result := Self;
end; { TOmniNativeSemaphore.AsObject }

function TOmniNativeSemaphore.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [];
{$IFDEF MSWINDOWS}
  Include(Result, scModular);
{$ENDIF}
end; { TOmniNativeSemaphore.GetCapabilities }

{$IFDEF MSWINDOWS}
function TOmniNativeSemaphore.GetHandle: THandle;
begin
  Result := FBase.Handle;
end; { TOmniNativeSemaphore.GetHandle }
{$ENDIF}

function TOmniNativeSemaphore.InitialValue: cardinal;
begin
  Result := FInitial;
end; { TOmniNativeSemaphore.InitialValue }

function TOmniNativeSemaphore.IsSignalled: boolean;
var
  state: TOmniSignalState;
begin
  state := SignalState;
  Result := state = esSignalled;
  if state = esUnknown then
    raise TOmniSynchroException.Create(EIsSignalledNotSupported);
end; { TOmniNativeSemaphore.IsSignalled }

function TOmniNativeSemaphore.SignalState: TOmniSignalState;
begin
  FLock.Enter;
  if (FWaiters = 0) or (FShadow = 0) then begin
    if FShadow > 0 then
      Result := esSignalled
    else
      Result := esNotSignalled;
  end
  else
    Result := esUnknown;
  FLock.Leave;
end; { TOmniNativeSemaphore.SignalState }

procedure TOmniNativeSemaphore.Reset;
begin
  raise TOmniSynchroException.Create(EIsSignalledNotSupported);
end; { TOmniNativeSemaphore.Reset }

procedure TOmniNativeSemaphore.Signal;
begin
  FLock.Enter;
  FBase.Signal;
  Inc(FShadow);
  FLock.Leave;
end; { TOmniNativeSemaphore.Signal }

function TOmniNativeSemaphore.Value: cardinal;
var
  ok: boolean;
begin
  Result := 0;
  FLock.Enter;
  ok := FWaiters = 0;
  if ok then
    Result := FShadow;
  FLock.Leave;
  if not ok then
    raise TOmniSynchroException.Create(EIsSignalledNotSupported);
end; { TOmniNativeSemaphore.Value }

function TOmniNativeSemaphore.WaitFor(Timeout: cardinal): TWaitResult;
begin
  FLock.Enter;
  Inc(FWaiters);
  FLock.Leave;
  Result := FBase.WaitFor(Timeout);
  FLock.Enter;
  if result = wrSignaled then
    Dec(FShadow);
  Dec(FWaiters);
  FLock.Leave;
end; { TOmniNativeSemaphore.WaitFor }

{ TOmniRecycleCriticalSection }

constructor TOmniRecycleCriticalSection.Create(const APool: TOmniSynchroFactory);
begin
  inherited Create(APool);
  FCrit := TFixedCriticalSection.Create;
  FLockCount.Initialize(0);
end; { TOmniRecycleCriticalSection.Create }

destructor TOmniRecycleCriticalSection.Destroy;
begin
  FCrit.Free;
  FLockCount.Finalize;
  inherited;
end; { TOmniRecycleCriticalSection.Destroy }

function TOmniRecycleCriticalSection.AsCriticalSection: TCriticalSection;
begin
  Result := FCrit;
end; { TOmniRecycleCriticalSection.AsCriticalSection }

function TOmniRecycleCriticalSection.AsSpinLock: POmniAtomicSpinLock;
begin
  Result := nil;
end; { TOmniRecycleCriticalSection.AsSpinLock }

procedure TOmniRecycleCriticalSection.Enter;
begin
  FCrit.Enter;
  FLockCount.Increment;
end; { TOmniRecycleCriticalSection.Enter }

function TOmniRecycleCriticalSection.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scKernelMode);
end; { TOmniRecycleCriticalSection.GetCapabilities }

procedure TOmniRecycleCriticalSection.Leave;
begin
  FLockCount.Decrement;
  FCrit.Leave;
end; { TOmniRecycleCriticalSection.Leave }

function TOmniRecycleCriticalSection.LockCount: integer;
begin
  Result := FLockCount.Read;
end; { TOmniRecycleCriticalSection.LockCount }

function TOmniRecycleCriticalSection.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCrits
  else
    Result := nil;
end; { TOmniRecycleCriticalSection.Pool }

function TOmniRecycleCriticalSection.SignalState: TOmniSignalState;
begin
  Result := esUnknown;
end; { TOmniRecycleCriticalSection.SignalState }

{ TOmniSpinLock }

constructor TOmniSpinLock.Create;
begin
  FLock.Initialize;
end; { TOmniSpinLock.Create }

destructor TOmniSpinLock.Destroy;
begin
  FLock.Finalize;
  inherited;
end; { TOmniSpinLock.Destroy }

function TOmniSpinLock.AsCriticalSection: TCriticalSection;
begin
  Result := nil;
end; { TOmniSpinLock.AsCriticalSection }

function TOmniSpinLock.AsObject: TObject;
begin
  Result := Self;
end; { TOmniSpinLock.AsObject }

function TOmniSpinLock.AsSpinLock: POmniAtomicSpinLock;
begin
  Result := @FLock;
end; { TOmniSpinLock.AsSpinLock }

function TOmniSpinLock.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [];
end; { TOmniSpinLock.GetCapabilities }

procedure TOmniSpinLock.Enter;
begin
  FLock.Enter;
end; { TOmniSpinLock.Enter }

procedure TOmniSpinLock.Leave;
begin
  FLock.Leave;
end; { TOmniSpinLock.Leave }

function TOmniSpinLock.LockCount: integer;
begin
  Result := FLock.FEntryCount.Read;
end; { TOmniSpinLock.LockCount }

{ TOmniRecycleCountDown }

constructor TOmniRecycleCountDown.Create(const APool: TOmniSynchroFactory; AInitialValue: cardinal);
begin
  inherited Create(APool);
  FBase := TCountDown.Create(AInitialValue);
end; { TOmniRecycleCountDown.Create }

destructor TOmniRecycleCountDown.Destroy;
begin
  FBase.Free;
  inherited;
end; { TOmniRecycleCountDown.Destroy }

function TOmniRecycleCountDown.Allocate: cardinal;
begin
  Result := FBase.Allocate;
end; { TOmniRecycleCountDown.Allocate }

procedure TOmniRecycleCountDown.CounterSignal;
begin
  FBase.CounterSignal;
end; { TOmniRecycleCountDown.CounterSignal }

function TOmniRecycleCountDown.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TOmniRecycleCountDown.GetCapabilities }

function TOmniRecycleCountDown.InitialValue: cardinal;
begin
  Result := FBase.FullCount;
end; { TOmniRecycleCountDown.InitialValue }

function TOmniRecycleCountDown.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TOmniRecycleCountDown.IsSignalled }

function TOmniRecycleCountDown.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCountDowns
  else
    Result := nil;
end; { TOmniRecycleCountDown.Pool }

procedure TOmniRecycleCountDown.Reconfigure(AInitial: cardinal);
begin
  TOmniCountDownFriend(FBase).Reconfigure(AInitial);
end; { TOmniRecycleCountDown.Reconfigure }

procedure TOmniRecycleCountDown.Signal;
begin
  FBase.Signal;
end; { TOmniRecycleCountDown.Signal }

function TOmniRecycleCountDown.SignalHit: boolean;
begin
  Result := FBase.SignalHit;
end; { TOmniRecycleCountDown.SignalHit }

function TOmniRecycleCountDown.Value: cardinal;
begin
  Result := FBase.Value;
end; { TOmniRecycleCountDown.Value }

function TOmniRecycleCountDown.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TOmniRecycleCountDown.WaitFor }

{ TOmniRecycleCountUp }

constructor TOmniRecycleCountUp.Create(
  const APool: TOmniSynchroFactory; AInitial, AMaxValue: cardinal);
begin
  inherited Create(APool);
  FBase := TCountUp.Create(AInitial, AMaxValue);
end; { TOmniRecycleCountUp.Create }

destructor TOmniRecycleCountUp.Destroy;
begin
  FBase.Free;
  inherited;
end; { TOmniRecycleCountUp.Destroy }

function TOmniRecycleCountUp.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TOmniRecycleCountUp.GetCapabilities }

function TOmniRecycleCountUp.InitialValue: cardinal;
begin
  Result := FBase.InitialValue;
end; { TOmniRecycleCountUp.InitialValue }

function TOmniRecycleCountUp.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TOmniRecycleCountUp.IsSignalled }

function TOmniRecycleCountUp.MaxValue: cardinal;
begin
  Result := FBase.MaxValue;
end; { TOmniRecycleCountUp.MaxValue }

function TOmniRecycleCountUp.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCountUps
  else
    Result := nil;
end; { TOmniRecycleCountUp.Pool }

procedure TOmniRecycleCountUp.Reconfigure(AInitial, AMaxValue: cardinal);
begin
  TOmniCountUpFriend(FBase).Reconfigure(AInitial, AMaxValue);
end; { TOmniRecycleCountUp.Reconfigure }

procedure TOmniRecycleCountUp.Signal;
begin
  FBase.Signal;
end; { TOmniRecycleCountUp.Signal }

function TOmniRecycleCountUp.SignalHit: boolean;
begin
  Result := FBase.SignalHit;
end; { TOmniRecycleCountUp.SignalHit }

function TOmniRecycleCountUp.Value: cardinal;
begin
  Result := FBase.Value;
end; { TOmniRecycleCountUp.Value }

function TOmniRecycleCountUp.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TOmniRecycleCountUp.WaitFor }

{ TOmniRecycleFunctional }

constructor TOmniRecycleFunctional.Create(
  const APool: TOmniSynchroFactory; ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
begin
  inherited Create(APool);
  FBase := TFunctionalEvent.Create(ASignalTest, APLock);
end; { TOmniRecycleFunctional.Create }

destructor TOmniRecycleFunctional.Destroy;
begin
  FBase.Free;
  inherited
end; { TOmniRecycleFunctional.Destroy }

function TOmniRecycleFunctional.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TOmniRecycleFunctional.GetCapabilities }

function TOmniRecycleFunctional.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TOmniRecycleFunctional.IsSignalled }

function TOmniRecycleFunctional.Pool: TOmniSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FFunctionals
  else
    Result := nil;
end; { TOmniRecycleFunctional.Pool }

procedure TOmniRecycleFunctional.Reconfigure(
  ASignalTest: TOmniEventFunction; APLock: POmniAtomicSpinLock);
begin
  TOmniFunctionalEventFriend(FBase).Reconfigure(ASignalTest, APLock);
end; { TOmniRecycleFunctional.Reconfigure }

procedure TOmniRecycleFunctional.ReleaseConfiguration;
begin
  TOmniFunctionalEventFriend(FBase).Reconfigure(nil, nil);
end; { TOmniRecycleFunctional.ReleaseConfiguration }

procedure TOmniRecycleFunctional.Signal;
begin
  FBase.Pulse;
end; { TOmniRecycleFunctional.Signal }

function TOmniRecycleFunctional.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TOmniRecycleFunctional.WaitFor }

{ TOmniPlatformMREW.TFacade }

function TOmniPlatformMREW.TFacade.AsCriticalSection: TCriticalSection;
begin
  Result := nil;
end; { TOmniPlatformMREW.TFacade.AsCriticalSection }

function TOmniPlatformMREW.TFacade.AsObject: TObject;
begin
  Result := Self;
end; { TOmniPlatformMREW.TFacade.AsObject }

function TOmniPlatformMREW.TFacade.AsSpinLock: POmniAtomicSpinLock;
begin
  Result := nil;
end; { TOmniPlatformMREW.TFacade.AsSpinLock }

function TOmniPlatformMREW.TFacade.GetCapabilities: TOmniSynchroCapabilities;
begin
  Result := [];
end; { TOmniPlatformMREW.TFacade.GetCapabilities }

function TOmniPlatformMREW.TFacade.LockCount: integer;
begin
  Result := FController.FMREW.ReaderCount;
end; { TOmniPlatformMREW.TFacade.LockCount }

function TOmniPlatformMREW.TFacade._AddRef: Integer;
begin
  Result := FController._AddRef;
end; { TOmniPlatformMREW.TFacade._AddRef }

function TOmniPlatformMREW.TFacade._Release: Integer;
begin
  Result := FController._Release
end; { TOmniPlatformMREW.TFacade._Release }

{ TOmniPlatformMREW.TReaderLock }

procedure TOmniPlatformMREW.TReaderLock.Enter;
begin
  FController.FMREW.EnterRead(INFINITE);
end; { TOmniPlatformMREW.TReaderLock.Enter }

procedure TOmniPlatformMREW.TReaderLock.Leave;
begin
  FController.FMREW.ExitRead;
end; { TOmniPlatformMREW.TReaderLock.Leave }

function TOmniPlatformMREW.TReaderLock.LockCount: integer;
begin
  Result := inherited LockCount;
  if result < 0 then
    Result := 1;
end; { TOmniPlatformMREW.TReaderLock.LockCount }

{ TOmniPlatformMREW.TWriterLock }

procedure TOmniPlatformMREW.TWriterLock.Enter;
begin
  FController.FMREW.EnterWrite(INFINITE);
end; { TWriterLock.Enter }

procedure TOmniPlatformMREW.TWriterLock.Leave;
begin
  FController.FMREW.ExitWrite;
end; { TWriterLock.Leave }

function TOmniPlatformMREW.TWriterLock.LockCount: integer;
begin
  Result := - inherited LockCount;
  if result < 0 then
    Result := 1;
end; { TWriterLock.LockCount }

{ TOmniPlatformMREW }

constructor TOmniPlatformMREW.Create;
begin
  FMREW := TOtlMREW.Create;
  FReadFacade := TOmniPlatformMREW.TReaderLock.Create;
  FReadFacade.FController := Self;
  FWriteFacade := TOmniPlatformMREW.TWriterLock.Create;
  FWriteFacade.FController := Self;
end; { TOmniPlatformMREW.Create }

destructor TOmniPlatformMREW.Destroy;
begin
  FReadFacade.FController := nil;
  FreeAndNil(FReadFacade);
  FreeAndNil(FWriteFacade);
  FreeAndNil(FMREW);
  inherited;
end; { TOmniPlatformMREW.Destroy }

function TOmniPlatformMREW.AsReadLock: IOmniLock;
begin
  Result := FReadFacade as IOmniLock;
end; { TOmniPlatformMREW.AsReadLock }

function TOmniPlatformMREW.AsWriteLock: IOmniLock;
begin
  Result := FWriteFacade as IOmniLock;
end; { TOmniPlatformMREW.AsWriteLock }

function TOmniPlatformMREW.EnterRead(timeout: cardinal): TWaitResult;
begin
  Result := FMREW.EnterRead(timeout);
end; { TOmniPlatformMREW.EnterRead }

function TOmniPlatformMREW.EnterWrite(timeout: cardinal): TWaitResult;
begin
  Result := FMREW.EnterWrite(timeout);
end; { TOmniPlatformMREW.EnterWrite }

procedure TOmniPlatformMREW.ExitRead;
begin
  FMREW.ExitRead;
end; { TOmniPlatformMREW.ExitRead }

procedure TOmniPlatformMREW.ExitWrite;
begin
  FMREW.ExitWrite;
end; { TOmniPlatformMREW.ExitWrite }

function TOmniPlatformMREW.ReaderCount: integer;
begin
  Result := FMREW.ReaderCount;
end; { TOmniPlatformMREW.ReaderCount }

end.
