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
//  READ THE COMMENTS IN UNIT OtlPlatform.SynchroPrimitives.

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
  TLockingMechanism = (KernelLocking, BusLocking);

  TSignalState = (esSignalled, esNotSignalled, esUnknown);

  TSynchroCapability = (
    //ISynchro
    scModular, scSupportsSignalled,
    //ISynchrom, ILock
    scPoolManaged,
    //IEvent
    scManualEvent, scLight,
    //ISemaphore
    scSupportsValueTesting,
    //ILock
    scKernelMode
  );
  TSynchroCapabilities = set of TSynchroCapability;

  ISynchro = interface ['{EF480FC2-7BBB-40B9-A9F7-246A7834CFA9}']
    function GetCapabilities: TSynchroCapabilities;
  {$IFDEF MSWINDOWS}
    function GetHandle: THandle;
  {$ENDIF}
  //
    procedure Signal;
    /// <remarks> ISynchro.isSignalled() is not supported for auto-reset events and semaphores.</remarks>
    function  SignalState: TSignalState;
    function  WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
    function  AsObject: TObject;
    function  AsMWObject: TObject;
    function  IsSignalled: boolean;

    {$IFDEF MSWINDOWS}
      /// <remarks> ISynchro.Handle is not supported for:
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
    property Capabilities: TSynchroCapabilities read GetCapabilities;
  end; { ISynchro }

  TSynchroArray = TArray<ISynchro>;

  IEvent = interface(ISynchro) ['{3E6E1606-88E5-4898-9F79-613085CAE090}']
    procedure SetEvent;
    procedure ResetEvent;
  end; { IEvent }

  ISemaphore = interface(ISynchro) ['{4ACCD111-3E22-4902-B397-3309D84BBDD9}']
    function InitialValue: cardinal;
    /// <remarks> ISemaphore.Value() is not supported for native semaphores.</remarks>
    function Value: cardinal;
    /// <remarks> ISemaphore.Reset() is not supported for native semaphores.</remarks>
    procedure Reset;
  end; { ISemaphore }

  ILock = interface ['{38665699-5A91-4930-8AB8-AC4AD36B7182}']
    function GetCapabilities: TSynchroCapabilities;
  //
    procedure Enter;
    procedure Leave;
    function  LockCount: integer;
    function  AsObject: TObject;
    function  AsSpinLock: PSBDSpinLock;
    function  AsCriticalSection: TCriticalSection;
    property Capabilities: TSynchroCapabilities read GetCapabilities;
  end;

  ICountDown = interface(ISynchro) ['{23D82B7E-9670-4B24-835C-D1E879CF36C9}']
    function  Allocate: cardinal; // Signal and return the value.
    procedure CounterSignal;
    function  InitialValue: cardinal;
    function  Value: cardinal;
    function  SignalHit: boolean;
  end; { ICountDown }

  ICountUp = interface(ISynchro) ['{C3AD39CC-7918-44E3-AE08-B5E8F2F9EDB5}']
    function  InitialValue: cardinal;
    function  MaxValue: cardinal;
    function  Value: cardinal;
    function  SignalHit: boolean;
  end; { ICountUp }

  IOtlMREW = interface ['{4929F170-F5DC-41F8-852E-400D99B441BC}']
    function  EnterRead(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitRead;
    function  EnterWrite(timeout: cardinal = INFINITE): TWaitResult;
    procedure ExitWrite;
    function  ReaderCount: integer; // if +ve, this is the number of entered readers,
                                    // if -ve, there is an entered writer.
    function  AsReadLock: ILock;
    function  AsWriteLock: ILock;
  end; { IOtlMREW }

function CreateMREW: IOtlMREW;

type
  TSynchroFactory = class
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
    FAquisitionCount   : TVolatileUint32;
    FPoolMax           : cardinal;
    //FSemaphoreCount    : cardinal;
  private
    [Volatile] FLock: TSBDSpinLock;
    function Queues: TArray<TObjectQueue>;
  public
    constructor Create( APoolMax: cardinal);
    destructor  Destroy; override;
    function  AcquireKernelEvent(AManual, AInitialState, AReUse: boolean): IEvent;
    function  AcquireLightEvent(AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IEvent;
    function  AcquireNativeSemaphore(AInitialCount: cardinal): ISemaphore; // Native semaphores are never pooled.
    function  AcquireTestableSemaphore(AInitialCount: cardinal; AReUse: boolean): ISemaphore;
    /// <remarks> When re-using critical sections, Enter/Leave calls must be balanced before releasing the last reference. </remarks>
    function  AcquireCriticalSection(AReUse: boolean): ILock;  overload;
    class function AcquireCriticalSection: ILock; overload;
    function  AcquireSpinLock: ILock; // Spinlocks are never pooled.
    function  AcquireCountDown(InitialValue: cardinal; AReUse: boolean): ICountDown;
    function  AcquireCountUp(InitialValue, MaxValue: cardinal; AReUse: boolean): ICountUp;
    /// <remarks> To pulse a functional event, call Signal() on it. </remarks>
    function  AcquireFunctionalEvent(ASignalTest: TEventFunction; APLock: PSBDSpinLock; AReUse: boolean): ISynchro;
    procedure PurgeAllPools;
    function  CanDestroy: boolean;
  end; { TSynchroFactory }

{$REGION 'For internal use only.'}
/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountDownIntf(const APool: TSynchroFactory; InitialValue: cardinal): ICountDown;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCountUpIntf(const APool: TSynchroFactory; InitialValue, MaxValue: cardinal): ICountUp;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateKernelEventIntf(const APool: TSynchroFactory; ManualReset, InitialState: boolean): IEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateLightEventIntf(const APool: TSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateCritLockIntf(const APool: TSynchroFactory): ILock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateSpinLockIntf: ILock;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateTestableSemaphoreIntf(const APool: TSynchroFactory; AInitialCount: cardinal): ISemaphore;

/// <remarks>DO NOT USE. For internal use only.</remarks>
function _CreateNativeSemaphoreIntf(AInitialCount: cardinal): ISemaphore;
{$ENDREGION}

implementation

uses
  OtlSync.Platform.Errors,
  OtlSync.Platform.Basic;

type
  TLightEventFriend = class(TLightEvent)
  end;

  TCountDownFriend = class(TCountDown)
  end;

  TCountUpFriend = class(TCountUp)
  end;

  TFunctionalEventFriend = class(TFunctionalEvent)
  end;

  TRecycleObject = class abstract(TObject, IInterface)
  private const
    CObjPooledFlag = integer($80000000);
  var
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FPool: TSynchroFactory;
    [Volatile] FRefCount: TVolatileInt32;
    FAcquired: boolean;
  strict protected
    function  QueryInterface( const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  AsObject: TObject;
    function  GetRefCount: integer; inline;
  protected
    function  Pool: TSynchroFactory.TObjectQueue; virtual; abstract;
    procedure ReleaseConfiguration; virtual;
  public
    constructor Create(const APool: TSynchroFactory);
    procedure AfterConstruction; override;
    function  GetCapabilities: TSynchroCapabilities; virtual;
    class function NewInstance: TObject; override;
    function  SignalState: TSignalState; virtual; abstract;
    property RefCount: integer read GetRefCount;
  end; { TRecycleObject }

  TRecycleSynchro = class(TRecycleObject, ISynchro)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; virtual;
    {$ENDIF}
    function  GetCapabilities: TSynchroCapabilities; override;
  public
    function  AsMWObject: TObject; virtual;
    function  IsSignalled: boolean; virtual; abstract;
    procedure Signal; virtual; abstract;
    function  SignalState: TSignalState; override;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; virtual; abstract;
  end; { TRecycleSynchro }

  TRecycleEvent = class(TRecycleSynchro, IEvent)
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; override;
    {$ENDIF}
    function  Pool: TSynchroFactory.TObjectQueue; override;
  private
    FAsyncClear     : boolean;
    FBase           : TOtlEvent;
    FIsLight        : boolean;
    [Volatile] FLock: TSBDSpinLock; // Only used for manual events.
    FManual         : boolean;
    FShadow         : TSignalState;
    FWaiters        : cardinal;
  protected
    procedure Reconfigure(manual: boolean; value: cardinal); virtual;
  public
    constructor CreateAsKernel(const APool: TSynchroFactory; AManual, AInitialState: boolean);
    constructor CreateAsLight(const APool: TSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  IsSignalled: boolean; override;
    procedure ResetEvent;
    procedure Signal; override;
    function  SignalState: TSignalState; override;
    procedure SetEvent;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TRecycleEvent }

  TRecycleSemaphore = class(TRecycleSynchro, ISemaphore)
  private type
    TSimulatedSemaphore = class(TFunctionalEvent)
    protected
      [Volatile] FCurrentValue: TVolatileUint32;
      FInitialValue           : cardinal;
      [Volatile] FLock        : TSBDSpinLock;
      procedure SignalTest(doAcquire: boolean; var wasSuccessfullyAcquired: boolean;
        var isInSignalledState: boolean); override;
    public
      constructor Create(AInitialValue: cardinal);
      destructor  Destroy; override;
      procedure Reset;
      procedure ReconfigureInitial(value: cardinal);
      function  SignalState: TSignalState;
    end; { TSimulatedSemaphore }
  var
    FBase: TSimulatedSemaphore;
  protected
    {$IFDEF MSWINDOWS}
      function GetHandle: THandle; override;
    {$ENDIF}
    function  Pool: TSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(value: cardinal); virtual;
  public
    constructor Create(const APool: TSynchroFactory; AInitialCount: cardinal);
    destructor  Destroy; override;
    function  AsMWObject: TObject; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    procedure Reset;
    procedure Signal; override;
    function  SignalState: TSignalState;override;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TRecycleSemaphore }

  TNativeSemaphore = class(TInterfacedObject, ISemaphore)
  private
    FBase           : TOtlSemaphore;
    FInitial        : cardinal;
    [Volatile] FLock: TSBDSpinLock;
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
    function  GetCapabilities: TSynchroCapabilities; virtual;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean;
    procedure Reset;
    procedure Signal;
    function  SignalState: TSignalState;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult;
  end; { TNativeSemaphore }

  TRecycleCrit = class(TRecycleObject, ILock)
  strict private
    FCrit                : TCriticalSection;
    [Volatile] FLockCount: TVolatileInt32;
  strict protected
    procedure Enter;
    procedure Leave;
    function  LockCount: integer;
    function Pool: TSynchroFactory.TObjectQueue; override;
  public
    constructor Create(const APool: TSynchroFactory);
    destructor  Destroy; override;
    function  AsCriticalSection: TCriticalSection;
    function  AsSpinLock: PSBDSpinLock;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  SignalState: TSignalState; override;
  end; { TRecycleCrit }

  TSpinIntf = class(TInterfacedObject, ILock)
  strict private
    [Volatile] FLock: TSBDSpinLock;
  strict protected
    procedure Enter;
    procedure Leave;
  public
    constructor Create;
    destructor  Destroy; override;
    function  AsCriticalSection: TCriticalSection;
    function  AsObject: TObject;
    function  AsSpinLock: PSBDSpinLock;
    function  GetCapabilities: TSynchroCapabilities; virtual;
    function  LockCount: integer;
  end; { TSpinIntf }

  TRecycleCountDown = class(TRecycleSynchro, ICountDown)
  strict private
    FBase: TCountDown;
  protected
    function  Pool: TSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(AInitial: cardinal);
  public
    constructor Create( const APool: TSynchroFactory; AInitialValue: cardinal);
    destructor  Destroy; override;
    function  Allocate: cardinal; // Signal and return the value.
    procedure CounterSignal;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    procedure Signal; override;
    function  SignalHit: boolean;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TRecycleCountDown }

  TRecycleCountUp = class(TRecycleSynchro, ICountUp)
  strict private
    FBase: TCountUp;
  protected
    function  Pool: TSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(AInitial, AMaxValue: cardinal);
  public
    constructor Create( const APool: TSynchroFactory; AInitial, AMaxValue: cardinal);
    destructor  Destroy; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  InitialValue: cardinal;
    function  IsSignalled: boolean; override;
    function  MaxValue: cardinal;
    procedure Signal; override;
    function  SignalHit: boolean;
    function  Value: cardinal;
    function  WaitFor(timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TRecycleCountUp }

  TRecycleFunctional = class(TRecycleSynchro)
  strict private
    FBase: TFunctionalEvent;
  protected
    function  Pool: TSynchroFactory.TObjectQueue; override;
    procedure Reconfigure(ASignalTest: TEventFunction; APLock: PSBDSpinLock);
    procedure ReleaseConfiguration; override;
  public
    constructor Create(const APool: TSynchroFactory; ASignalTest: TEventFunction; APLock: PSBDSpinLock);
    destructor  Destroy; override;
    function  GetCapabilities: TSynchroCapabilities; override;
    function  IsSignalled: boolean; override;
    procedure Signal; override;
    function  WaitFor( Timeout: cardinal = INFINITE): TWaitResult; override;
  end; { TRecycleFunctional }

  TIntfMREW = class(TInterfacedObject, IOtlMREW)
  strict private type
    TFacade = class(TInterfacedObject, IInterface, ILock)
    strict protected
      procedure Enter; virtual; abstract;
      procedure Leave; virtual; abstract;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      function  LockCount: integer; virtual;
    protected
      FController: TIntfMREW;
    public
      function  AsCriticalSection: TCriticalSection;
      function  AsObject: TObject;
      function  AsSpinLock: PSBDSpinLock;
      function  GetCapabilities: TSynchroCapabilities; virtual;
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
    function  AsReadLock: ILock;
    function  AsWriteLock: ILock;
  end; { TIntfMREW }

{ exports }

function CreateMREW: IOtlMREW;
begin
  Result := TIntfMREW.Create;
end; { CreateMREW }

{ internal exports }

function _CreateCountDownIntf(const APool: TSynchroFactory; InitialValue: cardinal): ICountDown;
begin
  Result := TRecycleCountDown.Create(APool, InitialValue);
end; { _CreateCountDownIntf }

function _CreateCountUpIntf(const APool: TSynchroFactory; InitialValue, MaxValue: cardinal): ICountUp;
begin
  Result := TRecycleCountUp.Create(APool, InitialValue, MaxValue);
end; { _CreateCountUpIntf }

function _CreateKernelEventIntf(const APool: TSynchroFactory; ManualReset, InitialState: boolean): IEvent;
begin
  Result := TRecycleEvent.CreateAsKernel(APool, ManualReset, InitialState);
end; { _CreateKernelEventIntf }

function _CreateLightEventIntf(const APool: TSynchroFactory; ManualReset, InitialState: boolean; SpinMax: cardinal): IEvent;
begin
  Result := TRecycleEvent.CreateAsLight(APool, ManualReset, InitialState, SpinMax);
end; { _CreateLightEventIntf }

function _CreateCritLockIntf(const APool: TSynchroFactory): ILock;
begin
  Result := TRecycleCrit.Create(APool);
end; { _CreateCritLockIntf }

function _CreateSpinLockIntf: ILock;
begin
  Result := TSpinIntf.Create;
end; { _CreateSpinLockIntf }

function _CreateTestableSemaphoreIntf(const APool: TSynchroFactory; AInitialCount: cardinal): ISemaphore;
begin
  Result := TRecycleSemaphore.Create(APool, AInitialCount);
end; { _CreateTestableSemaphoreIntf }

function _CreateNativeSemaphoreIntf(AInitialCount: cardinal): ISemaphore;
begin
  Result := TNativeSemaphore.Create(AInitialCount);
end; { _CreateNativeSemaphoreIntf }

{ TSynchroFactory }

constructor TSynchroFactory.Create(APoolMax: cardinal);
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
end; { TSynchroFactory.Create }

destructor TSynchroFactory.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  Queue: TObjectQueue;
{$ENDIF}
begin
  if FAquisitionCount.Read <> 0 then
    raise TParallelException.Create(ECannotDestroySynchroFactory);
  PurgeAllPools;
  {$IFNDEF AUTOREFCOUNT}
  for Queue in Queues do
    Queue.Free;
  {$ENDIF}
  inherited;
  FLock.Finalize;
end; { TSynchroFactory.Destroy }

function TSynchroFactory.AcquireCountDown(
  InitialValue: cardinal; AReUse: boolean): ICountDown;
var
  Addend: TRecycleCountDown;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue  := FCountDowns;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleCountDown;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(InitialValue);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TRecycleCountDown.Create(Pool, InitialValue);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TSynchroFactory.AcquireCountDown }

function TSynchroFactory.AcquireCountUp(
  InitialValue, MaxValue: cardinal; AReUse: boolean): ICountUp;
var
  Addend: TRecycleCountUp;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue  := FCountUps;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleCountUp;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(InitialValue, MaxValue);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TRecycleCountUp.Create(Pool, InitialValue, MaxValue);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TSynchroFactory.AcquireCountUp }

class function TSynchroFactory.AcquireCriticalSection: ILock;
begin
  Result := TRecycleCrit.Create(nil);
end; { TSynchroFactory.AcquireCriticalSection }

function TSynchroFactory.AcquireCriticalSection(AReUse: boolean): ILock;
var
  Addend: TRecycleCrit;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FCrits;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleCrit;
    Addend.FRefCount.Write(0);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TRecycleCrit.Create(Pool);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TSynchroFactory.AcquireCriticalSection }

function TSynchroFactory.AcquireFunctionalEvent(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock; AReUse: boolean): ISynchro;
var
  Addend : TRecycleFunctional;
  DoPulse: boolean;
  Pool   : TSynchroFactory;
  Queue  : TObjectQueue;
begin
  DoPulse := False;
  Queue := FFunctionals;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleFunctional;
    Addend.FRefCount.Write(0);
    Addend.Reconfigure(ASignalTest, APlock);
    DoPulse := True;
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TRecycleFunctional.Create(Pool, ASignalTest, APLock);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
  if DoPulse then
    Addend.Signal;
end; { TSynchroFactory.AcquireFunctionalEvent }

function TSynchroFactory.AcquireKernelEvent(
  AManual, AInitialState, AReUse: boolean): IEvent;
var
  Addend: TRecycleEvent;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  if AManual then
    Queue := FManualKernelEvents
  else
    Queue := FAutoKernelEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleEvent;
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
    Addend := TRecycleEvent.CreateAsKernel(Pool, AManual, AInitialState);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TSynchroFactory.AcquireKernelEvent }

function TSynchroFactory.AcquireLightEvent(
  AManual, AInitialState: boolean; SpinMax: integer; AReUse: boolean): IEvent;
var
  Addend: TRecycleEvent;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FLightEvents;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleEvent;
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
    Addend := TRecycleEvent.CreateAsLight(Pool, AManual, AInitialState, SpinMax);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave
end; { TSynchroFactory.AcquireLightEvent }

function TSynchroFactory.AcquireNativeSemaphore(AInitialCount: cardinal): ISemaphore;
begin
  Result := TNativeSemaphore.Create(AInitialCount);
end; { TSynchroFactory.AcquireNativeSemaphore }

function TSynchroFactory.AcquireTestableSemaphore(AInitialCount: cardinal; AReUse: boolean): ISemaphore;
var
  Addend: TRecycleSemaphore;
  Pool  : TSynchroFactory;
  Queue : TObjectQueue;
begin
  Queue := FSemaphores;
  FLock.Enter;
  if AReUse and (Queue.Count > 0) then begin
    Addend := Queue.Dequeue as TRecycleSemaphore;
    Addend.Reconfigure(AInitialCount);
    Addend.FRefCount.Write(0);
  end
  else begin
    if AReUse then
      Pool := self
    else
      Pool := nil;
    Addend := TRecycleSemaphore.Create(Pool, AInitialCount);
  end;
  Result := Addend;
  if AReUse then begin
    Addend.FAcquired := True;
    FAquisitionCount.Increment;
  end;
  FLock.Leave;
end; { TSynchroFactory.AcquireTestableSemaphore }

function TSynchroFactory.AcquireSpinLock: ILock;
begin
  Result := TSpinIntf.Create;
end; { TSynchroFactory.AcquireSpinLock }

function TSynchroFactory.CanDestroy: boolean;
begin
  Result := FAquisitionCount.Read = 0;
end; { TSynchroFactory.CanDestroy }

procedure TSynchroFactory.PurgeAllPools;
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

function TSynchroFactory.Queues: TArray<TObjectQueue>;
begin
  Result := [FManualKernelEvents, FAutoKernelEvents, FLightEvents, FSemaphores, FCrits, FCountDowns, FCountUps, FFunctionals];
end; { TSynchroFactory.Queues }

{ TRecycleObject }

constructor TRecycleObject.Create(const APool: TSynchroFactory);
begin
  FPool := APool;
end; { TRecycleObject.Create }

procedure TRecycleObject.AfterConstruction;
begin
  FRefCount.Decrement;
end; { TRecycleObject.AfterConstruction }

function TRecycleObject.AsObject: TObject;
begin
  Result := Self;
end; { TRecycleObject.AsObject }

function TRecycleObject.GetCapabilities: TSynchroCapabilities;
begin
  Result := [];
  if assigned(FPool) then
    Include(Result, scPoolManaged);
end; { TRecycleObject.GetCapabilities }

function TRecycleObject.GetRefCount: integer;
begin
  Result := FRefCount.Read;
  if ((Result and CObjPooledFlag) <> 0) or (Result <= -2) then
    Result := -1;
end; { TRecycleObject.GetRefCount }

class function TRecycleObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TRecycleObject(Result).FRefCount.Initialize(1);
end; { TRecycleObject.NewInstance }

function TRecycleObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TRecycleObject.QueryInterface }

procedure TRecycleObject.ReleaseConfiguration;
begin
  // do nothing
end; { TRecycleObject.ReleaseConfiguration }

function TRecycleObject._AddRef: integer;
begin
  Result := FRefCount.Increment;
end; { TRecycleObject._AddRef }

function TRecycleObject._Release: integer;
var
  OldValue     : integer;
  Pl           : TSynchroFactory.TObjectQueue;
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
end; { TRecycleObject._Release }

{ TRecycleSynchro }

function TRecycleSynchro.AsMWObject: TObject;
begin
  Result := nil
end; { TRecycleSynchro.AsMWObject }

function TRecycleSynchro.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TRecycleSynchro.GetCapabilities }

{$IFDEF MSWINDOWS}
function TRecycleSynchro.GetHandle: THandle;
begin
  Result := 0;
end; { TRecycleSynchro.GetHandle }
{$ENDIF}

function TRecycleSynchro.SignalState: TSignalState;
begin
  if IsSignalled then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TRecycleSynchro.SignalState }

{ TRecycleEvent }

constructor TRecycleEvent.CreateAsKernel(const APool: TSynchroFactory; AManual, AInitialState: boolean);
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
end; { TRecycleEvent.CreateAsKernel }

constructor TRecycleEvent.CreateAsLight(
  const APool: TSynchroFactory; AManual, AInitialState: boolean; SpinMax: cardinal);
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
end; { TRecycleEvent.CreateAsLight }

destructor TRecycleEvent.Destroy;
begin
  FBase.Free;
  FLock.Finalize;
  inherited;
end; { TRecycleEvent.Destroy }

function TRecycleEvent.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TRecycleEvent.AsMWObject }

function TRecycleEvent.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  if FIsLight then
    Include(Result, scLight);
  if FManual then
    Include(Result, scManualEvent);
  if FIsLight or FManual then
    Include(Result, scSupportsSignalled);
end; { TRecycleEvent.GetCapabilities }

{$IFDEF MSWINDOWS}
function TRecycleEvent.GetHandle: THandle;
begin
  if FisLight then
      Result := 0
    else
      Result := FBase.Handle;
end; { TRecycleEvent.GetHandle }
{$ENDIF}

function TRecycleEvent.IsSignalled: boolean;
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
      raise TParallelException.Create(EIsSignalledNotSupported);
  end
end; { TRecycleEvent.IsSignalled }

function TRecycleEvent.Pool: TSynchroFactory.TObjectQueue;
begin
  if not assigned(FPool) then
    Result := nil
  else if FIsLight then
    Result := FPool.FLightEvents
  else if FManual then
    Result := FPool.FManualKernelEvents
  else
    Result := FPool.FAutoKernelEvents;
end; { TRecycleEvent.Pool }

procedure TRecycleEvent.ResetEvent;
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
end; { TRecycleEvent.ResetEvent }

procedure TRecycleEvent.SetEvent;
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
end; { TRecycleEvent.SetEvent }

procedure TRecycleEvent.Reconfigure(manual: boolean; value: cardinal);
begin
  if FBase is TLightEvent then
    TLightEventFriend(FBase).Reconfigure(Manual, Value);
end; { TRecycleEvent.Reconfigure }

procedure TRecycleEvent.Signal;
begin
  SetEvent;
end; { TRecycleEvent.Signal }

function TRecycleEvent.SignalState: TSignalState;
begin
  if IsSignalled then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TRecycleEvent.SignalState }

function TRecycleEvent.WaitFor(timeout: cardinal): TWaitResult;
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
end; { TRecycleEvent.WaitFor }

{ TRecycleSemaphore.TSimulatedSemaphore }

constructor TRecycleSemaphore.TSimulatedSemaphore.Create(AInitialValue: cardinal);
begin
  FLock.Initialize;
  FInitialValue := AInitialValue;
  FCurrentValue.Initialize(FInitialValue);
  inherited Create(nil, @FLock);
end; { TRecycleSemaphore.TSimulatedSemaphore.Create }

destructor TRecycleSemaphore.TSimulatedSemaphore.Destroy;
begin
  FLock.Finalize;
  FCurrentValue.Finalize;
  inherited;
end; { TRecycleSemaphore.TSimulatedSemaphore.Destroy }

procedure TRecycleSemaphore.TSimulatedSemaphore.ReconfigureInitial(value: cardinal);
begin
  FInitialValue := value;
  Reset;
end; { TRecycleSemaphore.TSimulatedSemaphore.ReconfigureInitial }

procedure TRecycleSemaphore.TSimulatedSemaphore.Reset;
begin
  FLock.WithinLock(
    procedure
    begin
      FCurrentValue.Write(FInitialValue);
      if FInitialValue > 0 then
        Pulse;
    end)
end; { TRecycleSemaphore.TSimulatedSemaphore.Reset }

function TRecycleSemaphore.TSimulatedSemaphore.SignalState: TSignalState;
begin
  if FCurrentValue.Read > 0 then
    Result := esSignalled
  else
    Result := esNotSignalled;
end; { TRecycleSemaphore.TSimulatedSemaphore.SignalState }

procedure TRecycleSemaphore.TSimulatedSemaphore.SignalTest(
  doAcquire: boolean; var wasSuccessfullyAcquired, isInSignalledState: boolean);
begin
  wasSuccessfullyAcquired := doAcquire and FCurrentValue.DecrementIfAboveZero;
  isInSignalledState := FCurrentValue.Read > 0;
end; { TRecycleSemaphore.TSimulatedSemaphore.SignalTest }

{ TRecycleSemaphore }

constructor TRecycleSemaphore.Create(
  const APool: TSynchroFactory; AInitialCount: cardinal);
begin
  inherited Create(APool);
  FBase := TSimulatedSemaphore.Create(AInitialCount);
end; { TRecycleSemaphore.Create }

destructor TRecycleSemaphore.Destroy;
begin
  FBase.Free;
  inherited;
end; { TRecycleSemaphore.Destroy }

function TRecycleSemaphore.AsMWObject: TObject;
begin
  Result := nil;
end; { TRecycleSemaphore.AsMWObject }

function TRecycleSemaphore.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
  Include(Result, scSupportsValueTesting);
end; { TRecycleSemaphore.GetCapabilities }

{$IFDEF MSWINDOWS}
function TRecycleSemaphore.GetHandle: THandle;
begin
  Result := 0;
end; { TRecycleSemaphore.GetHandle }
{$ENDIF}

function TRecycleSemaphore.InitialValue: cardinal;
begin
  Result := FBase.FInitialValue;
end; { TRecycleSemaphore.InitialValue }

function TRecycleSemaphore.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TRecycleSemaphore.IsSignalled }

function TRecycleSemaphore.SignalState: TSignalState;
begin
  Result := FBase.SignalState;
end; { TRecycleSemaphore.SignalState }

function TRecycleSemaphore.Pool: TSynchroFactory.TObjectQueue;
begin
  if not assigned(FPool) then
    Result := nil
  else
    Result := FPool.FSemaphores;
end; { TRecycleSemaphore.Pool }

procedure TRecycleSemaphore.Reconfigure(value: cardinal);
begin
  FBase.ReconfigureInitial(value);
end; { TRecycleSemaphore.Reconfigure }

procedure TRecycleSemaphore.Reset;
begin
  FBase.Reset;
end; { TRecycleSemaphore.Reset }

procedure TRecycleSemaphore.Signal;
begin
  FBase.FLock.Enter;
  FBase.FCurrentValue.Increment;
  FBase.FLock.Leave;
  FBase.Pulse;
end; { TRecycleSemaphore.Signal }

function TRecycleSemaphore.Value: cardinal;
begin
  FBase.FLock.Enter;
  Result := FBase.FCurrentValue.Read;
  FBase.FLock.Leave;
end; { TRecycleSemaphore.Value }

function TRecycleSemaphore.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TRecycleSemaphore.WaitFor }

{ TNativeSemaphore }

constructor TNativeSemaphore.Create(AInitialCount: cardinal);
begin
  FInitial := AInitialCount;
  FBase    := TOtlSemaphore.Create(FInitial);
  FWaiters := 0;
  FShadow  := FInitial;
  FLock.Initialize;
end; { TNativeSemaphore.Create }

destructor TNativeSemaphore.Destroy;
begin
  FLock.Finalize;
  FBase.Free;
  inherited;
end; { TNativeSemaphore.Destroy }

function TNativeSemaphore.AsMWObject: TObject;
begin
  Result := FBase.AsMWObject;
end; { TNativeSemaphore.AsMWObject }

function TNativeSemaphore.AsObject: TObject;
begin
  Result := Self;
end; { TNativeSemaphore.AsObject }

function TNativeSemaphore.GetCapabilities: TSynchroCapabilities;
begin
  Result := [];
{$IFDEF MSWINDOWS}
  Include(Result, scModular);
{$ENDIF}
end; { TNativeSemaphore.GetCapabilities }

{$IFDEF MSWINDOWS}
function TNativeSemaphore.GetHandle: THandle;
begin
  Result := FBase.Handle;
end; { TNativeSemaphore.GetHandle }
{$ENDIF}

function TNativeSemaphore.InitialValue: cardinal;
begin
  Result := FInitial;
end; { TNativeSemaphore.InitialValue }

function TNativeSemaphore.IsSignalled: boolean;
var
  state: TSignalState;
begin
  state := SignalState;
  Result := state = esSignalled;
  if state = esUnknown then
    raise TParallelException.Create(EIsSignalledNotSupported);
end; { TNativeSemaphore.IsSignalled }

function TNativeSemaphore.SignalState: TSignalState;
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
end; { TNativeSemaphore.SignalState }

procedure TNativeSemaphore.Reset;
begin
  raise TParallelException.Create(EIsSignalledNotSupported);
end; { TNativeSemaphore.Reset }

procedure TNativeSemaphore.Signal;
begin
  FLock.Enter;
  FBase.Signal;
  Inc(FShadow);
  FLock.Leave;
end; { TNativeSemaphore.Signal }

function TNativeSemaphore.Value: cardinal;
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
    raise TParallelException.Create(EIsSignalledNotSupported);
end; { TNativeSemaphore.Value }

function TNativeSemaphore.WaitFor(Timeout: cardinal): TWaitResult;
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
end; { TNativeSemaphore.WaitFor }

{ TRecycleCrit }

constructor TRecycleCrit.Create(const APool: TSynchroFactory);
begin
  inherited Create(APool);
  FCrit := TFixedCriticalSection.Create;
  FLockCount.Initialize(0);
end; { TRecycleCrit.Create }

destructor TRecycleCrit.Destroy;
begin
  FCrit.Free;
  FLockCount.Finalize;
  inherited;
end; { TRecycleCrit.Destroy }

function TRecycleCrit.AsCriticalSection: TCriticalSection;
begin
  Result := FCrit;
end; { TRecycleCrit.AsCriticalSection }

function TRecycleCrit.AsSpinLock: PSBDSpinLock;
begin
  Result := nil;
end; { TRecycleCrit.AsSpinLock }

procedure TRecycleCrit.Enter;
begin
  FCrit.Enter;
  FLockCount.Increment;
end; { TRecycleCrit.Enter }

function TRecycleCrit.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scKernelMode);
end; { TRecycleCrit.GetCapabilities }

procedure TRecycleCrit.Leave;
begin
  FLockCount.Decrement;
  FCrit.Leave;
end; { TRecycleCrit.Leave }

function TRecycleCrit.LockCount: integer;
begin
  Result := FLockCount.Read;
end; { TRecycleCrit.LockCount }

function TRecycleCrit.Pool: TSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCrits
  else
    Result := nil;
end; { TRecycleCrit.Pool }

function TRecycleCrit.SignalState: TSignalState;
begin
  Result := esUnknown;
end; { TRecycleCrit.SignalState }

{ TSpinIntf }

constructor TSpinIntf.Create;
begin
  FLock.Initialize;
end; { TSpinIntf.Create }

destructor TSpinIntf.Destroy;
begin
  FLock.Finalize;
  inherited;
end; { TSpinIntf.Destroy }

function TSpinIntf.AsCriticalSection: TCriticalSection;
begin
  Result := nil;
end; { TSpinIntf.AsCriticalSection }

function TSpinIntf.AsObject: TObject;
begin
  Result := Self;
end; { TSpinIntf.AsObject }

function TSpinIntf.AsSpinLock: PSBDSpinLock;
begin
  Result := @FLock;
end; { TSpinIntf.AsSpinLock }

function TSpinIntf.GetCapabilities: TSynchroCapabilities;
begin
  Result := [];
end; { TSpinIntf.GetCapabilities }

procedure TSpinIntf.Enter;
begin
  FLock.Enter;
end; { TSpinIntf.Enter }

procedure TSpinIntf.Leave;
begin
  FLock.Leave;
end; { TSpinIntf.Leave }

function TSpinIntf.LockCount: integer;
begin
  Result := FLock.FEntryCount.Read;
end; { TSpinIntf.LockCount }

{ TRecycleCountDown }

constructor TRecycleCountDown.Create(const APool: TSynchroFactory; AInitialValue: cardinal);
begin
  inherited Create(APool);
  FBase := TCountDown.Create(AInitialValue);
end; { TRecycleCountDown.Create }

destructor TRecycleCountDown.Destroy;
begin
  FBase.Free;
  inherited;
end; { TRecycleCountDown.Destroy }

function TRecycleCountDown.Allocate: cardinal;
begin
  Result := FBase.Allocate;
end; { TRecycleCountDown.Allocate }

procedure TRecycleCountDown.CounterSignal;
begin
  FBase.CounterSignal;
end; { TRecycleCountDown.CounterSignal }

function TRecycleCountDown.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TRecycleCountDown.GetCapabilities }

function TRecycleCountDown.InitialValue: cardinal;
begin
  Result := FBase.FullCount;
end; { TRecycleCountDown.InitialValue }

function TRecycleCountDown.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TRecycleCountDown.IsSignalled }

function TRecycleCountDown.Pool: TSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCountDowns
  else
    Result := nil;
end; { TRecycleCountDown.Pool }

procedure TRecycleCountDown.Reconfigure(AInitial: cardinal);
begin
  TCountDownFriend(FBase).Reconfigure(AInitial);
end; { TRecycleCountDown.Reconfigure }

procedure TRecycleCountDown.Signal;
begin
  FBase.Signal;
end; { TRecycleCountDown.Signal }

function TRecycleCountDown.SignalHit: boolean;
begin
  Result := FBase.SignalHit;
end; { TRecycleCountDown.SignalHit }

function TRecycleCountDown.Value: cardinal;
begin
  Result := FBase.Value;
end; { TRecycleCountDown.Value }

function TRecycleCountDown.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TRecycleCountDown.WaitFor }

{ TRecycleCountUp }

constructor TRecycleCountUp.Create(
  const APool: TSynchroFactory; AInitial, AMaxValue: cardinal);
begin
  inherited Create(APool);
  FBase := TCountUp.Create(AInitial, AMaxValue);
end; { TRecycleCountUp.Create }

destructor TRecycleCountUp.Destroy;
begin
  FBase.Free;
  inherited;
end; { TRecycleCountUp.Destroy }

function TRecycleCountUp.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TRecycleCountUp.GetCapabilities }

function TRecycleCountUp.InitialValue: cardinal;
begin
  Result := FBase.InitialValue;
end; { TRecycleCountUp.InitialValue }

function TRecycleCountUp.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TRecycleCountUp.IsSignalled }

function TRecycleCountUp.MaxValue: cardinal;
begin
  Result := FBase.MaxValue;
end; { TRecycleCountUp.MaxValue }

function TRecycleCountUp.Pool: TSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FCountUps
  else
    Result := nil;
end; { TRecycleCountUp.Pool }

procedure TRecycleCountUp.Reconfigure(AInitial, AMaxValue: cardinal);
begin
  TCountUpFriend(FBase).Reconfigure(AInitial, AMaxValue);
end; { TRecycleCountUp.Reconfigure }

procedure TRecycleCountUp.Signal;
begin
  FBase.Signal;
end; { TRecycleCountUp.Signal }

function TRecycleCountUp.SignalHit: boolean;
begin
  Result := FBase.SignalHit;
end; { TRecycleCountUp.SignalHit }

function TRecycleCountUp.Value: cardinal;
begin
  Result := FBase.Value;
end; { TRecycleCountUp.Value }

function TRecycleCountUp.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TRecycleCountUp.WaitFor }

{ TRecycleFunctional }

constructor TRecycleFunctional.Create(
  const APool: TSynchroFactory; ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  inherited Create(APool);
  FBase := TFunctionalEvent.Create(ASignalTest, APLock);
end; { TRecycleFunctional.Create }

destructor TRecycleFunctional.Destroy;
begin
  FBase.Free;
  inherited
end; { TRecycleFunctional.Destroy }

function TRecycleFunctional.GetCapabilities: TSynchroCapabilities;
begin
  Result := inherited GetCapabilities;
  Include(Result, scSupportsSignalled);
end; { TRecycleFunctional.GetCapabilities }

function TRecycleFunctional.IsSignalled: boolean;
begin
  Result := FBase.IsSignalled;
end; { TRecycleFunctional.IsSignalled }

function TRecycleFunctional.Pool: TSynchroFactory.TObjectQueue;
begin
  if assigned(FPool) then
    Result := FPool.FFunctionals
  else
    Result := nil;
end; { TRecycleFunctional.Pool }

procedure TRecycleFunctional.Reconfigure(
  ASignalTest: TEventFunction; APLock: PSBDSpinLock);
begin
  TFunctionalEventFriend(FBase).Reconfigure(ASignalTest, APLock);
end; { TRecycleFunctional.Reconfigure }

procedure TRecycleFunctional.ReleaseConfiguration;
begin
  TFunctionalEventFriend(FBase).Reconfigure(nil, nil);
end; { TRecycleFunctional.ReleaseConfiguration }

procedure TRecycleFunctional.Signal;
begin
  FBase.Pulse;
end; { TRecycleFunctional.Signal }

function TRecycleFunctional.WaitFor(timeout: cardinal): TWaitResult;
begin
  Result := FBase.WaitFor(timeout);
end; { TRecycleFunctional.WaitFor }

{ TIntfMREW.TFacade }

function TIntfMREW.TFacade.AsCriticalSection: TCriticalSection;
begin
  Result := nil;
end; { TIntfMREW.TFacade.AsCriticalSection }

function TIntfMREW.TFacade.AsObject: TObject;
begin
  Result := Self;
end; { TIntfMREW.TFacade.AsObject }

function TIntfMREW.TFacade.AsSpinLock: PSBDSpinLock;
begin
  Result := nil;
end; { TIntfMREW.TFacade.AsSpinLock }

function TIntfMREW.TFacade.GetCapabilities: TSynchroCapabilities;
begin
  Result := [];
end; { TIntfMREW.TFacade.GetCapabilities }

function TIntfMREW.TFacade.LockCount: integer;
begin
  Result := FController.FMREW.ReaderCount;
end; { TIntfMREW.TFacade.LockCount }

function TIntfMREW.TFacade._AddRef: Integer;
begin
  Result := FController._AddRef;
end; { TIntfMREW.TFacade._AddRef }

function TIntfMREW.TFacade._Release: Integer;
begin
  Result := FController._Release
end; { TIntfMREW.TFacade._Release }

{ TIntfMREW.TReaderLock }

procedure TIntfMREW.TReaderLock.Enter;
begin
  FController.FMREW.EnterRead(INFINITE);
end; { TIntfMREW.TReaderLock.Enter }

procedure TIntfMREW.TReaderLock.Leave;
begin
  FController.FMREW.ExitRead;
end; { TIntfMREW.TReaderLock.Leave }

function TIntfMREW.TReaderLock.LockCount: integer;
begin
  Result := inherited LockCount;
  if result < 0 then
    Result := 1;
end; { TIntfMREW.TReaderLock.LockCount }

{ TIntfMREW.TWriterLock }

procedure TIntfMREW.TWriterLock.Enter;
begin
  FController.FMREW.EnterWrite(INFINITE);
end; { TWriterLock.Enter }

procedure TIntfMREW.TWriterLock.Leave;
begin
  FController.FMREW.ExitWrite;
end; { TWriterLock.Leave }

function TIntfMREW.TWriterLock.LockCount: integer;
begin
  Result := - inherited LockCount;
  if result < 0 then
    Result := 1;
end; { TWriterLock.LockCount }

{ TIntfMREW }

constructor TIntfMREW.Create;
begin
  FMREW := TOtlMREW.Create;
  FReadFacade := TIntfMREW.TReaderLock.Create;
  FReadFacade.FController := Self;
  FWriteFacade := TIntfMREW.TWriterLock.Create;
  FWriteFacade.FController := Self;
end; { TIntfMREW.Create }

destructor TIntfMREW.Destroy;
begin
  FReadFacade.FController := nil;
  FreeAndNil(FReadFacade);
  FreeAndNil(FWriteFacade);
  FreeAndNil(FMREW);
  inherited;
end; { TIntfMREW.Destroy }

function TIntfMREW.AsReadLock: ILock;
begin
  Result := FReadFacade as ILock;
end; { TIntfMREW.AsReadLock }

function TIntfMREW.AsWriteLock: ILock;
begin
  Result := FWriteFacade as ILock;
end; { TIntfMREW.AsWriteLock }

function TIntfMREW.EnterRead(timeout: cardinal): TWaitResult;
begin
  Result := FMREW.EnterRead(timeout);
end; { TIntfMREW.EnterRead }

function TIntfMREW.EnterWrite(timeout: cardinal): TWaitResult;
begin
  Result := FMREW.EnterWrite(timeout);
end; { TIntfMREW.EnterWrite }

procedure TIntfMREW.ExitRead;
begin
  FMREW.ExitRead;
end; { TIntfMREW.ExitRead }

procedure TIntfMREW.ExitWrite;
begin
  FMREW.ExitWrite;
end; { TIntfMREW.ExitWrite }

function TIntfMREW.ReaderCount: integer;
begin
  Result := FMREW.ReaderCount;
end; { TIntfMREW.ReaderCount }

end.
