///<summary>Synchronisation primitives. Part of the OmniThreadLibrary project.</summary>
///<remarks>Move* family of functions require Pentium 4 processor (or newer).</remarks>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010, Primoz Gabrijelcic
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
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2009-03-30
///   Last modification : 2010-07-01
///   Version           : 1.05
///</para><para>
///   History:
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

{$I OTLOptions.inc}

interface

uses
  SyncObjs,
  DSiWin32,
  GpStuff;

type
  IOmniCriticalSection = interface ['{AA92906B-B92E-4C54-922C-7B87C23DABA9}']
    procedure Acquire;
    procedure Release;
    function  GetSyncObj: TSynchroObject;
  end; { IOmniCriticalSection }

  ///<summary>Simple critical section wrapper. Critical section is automatically
  ///    initialised on first use.</summary>
  TOmniCS = record
  strict private
    ocsSync: IOmniCriticalSection;
    function  GetSyncObj: TSynchroObject;
  public
    procedure Initialize;
    procedure Acquire; inline;
    procedure Release; inline;
    property SyncObj: TSynchroObject read GetSyncObj;
  end; { TOmniCS }

  ///<summary>Very lightweight multiple-readers-exclusive-writer lock.</summary>
  TOmniMREW = record
  strict private
    omrewReference: integer;      //Reference.Bit0 is 'writing in progress' flag
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

function CreateOmniCriticalSection: IOmniCriticalSection;
function CreateOmniCancellationToken: IOmniCancellationToken;
function CreateResourceCount(initialCount: integer): IOmniResourceCount;

// Intel Atomic functions support
function CAS8(const oldValue, newValue: byte; var destination): boolean;
function CAS16(const oldValue, newValue: word; var destination): boolean;
function CAS32(const oldValue, newValue: cardinal; var destination): boolean; overload;
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean; overload;
function CAS64(const oldData: pointer; oldReference: cardinal; newData: pointer;
  newReference: cardinal; var destination): boolean;
procedure Move64(var Source, Destination); overload;
procedure Move64(newData: pointer; newReference: cardinal; var Destination); overload; 
procedure Move128(var Source, Destination);

function GetThreadId: cardinal;
function GetCPUTimeStamp: int64;

var
  GOmniCancellationToken: IOmniCancellationToken;

implementation

uses
  Windows,
  SysUtils;

type
  TOmniCriticalSection = class(TInterfacedObject, IOmniCriticalSection)
  strict private
    ocsCritSect: TSynchroObject;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Acquire; inline;
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
  lock cmpxchg byte ptr [destination], newValue
  setz  al
end; { CAS8 }

function CAS16(const oldValue, newValue: word; var destination): boolean;
asm
  lock cmpxchg word ptr [destination], newValue
  setz  al
end; { CAS16 }

function CAS32(const oldValue, newValue: cardinal; var destination): boolean;
//ATOMIC FUNCTION
//begin
//  result := oldValue = PCardinal(destination)^;
//  if result then
//    PCardinal(destination)^ := newValue;
//end;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean;
//ATOMIC FUNCTION
//begin
//  result := oldValue = PPointer(destination)^;
//  if result then
//    PPointer(destination)^ := newValue;
//end;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

function CAS64(const oldData: pointer; oldReference: cardinal; newData: pointer;
  newReference: cardinal; var destination): boolean;
//ATOMIC FUNCTION
//begin
//  result := (destination.PData = oldData) and (destination.Reference = oldReference);
//  if result then
//  begin
//    destination.PData := newData;
//    destination.Reference := newReference;
//  end;
//end;
asm
  push  edi
  push  ebx
  mov   ebx, newData
  mov   ecx, newReference
  mov   edi, destination
  lock cmpxchg8b qword ptr [edi]
  setz  al
  pop   ebx
  pop   edi
end; { CAS64 }

procedure Move64(var Source, Destination);
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

procedure Move128(var Source, Destination);
//Move 16 bytes atomicly from Source to 16-byte aligned to Destination!
asm
  movdqa  xmm0, dqword [Source]
  movdqa  dqword [Destination], xmm0
end;

function GetThreadId: cardinal;
//result := GetCurrentThreadId;
asm
  mov   eax, fs:[$18]      //eax := thread information block
  mov   eax, [eax + $24]   //eax := thread id
end; { GetThreadId }

function GetCPUTimeStamp: int64;
asm
  rdtsc
end; { GetCPUTimeStamp }

{ TOmniCS }

procedure TOmniCS.Acquire;
begin
  Initialize;
  ocsSync.Acquire;
end; { TOmniCS.Acquire }

function TOmniCS.GetSyncObj: TSynchroObject;
begin
  Initialize;
  Result := ocsSync.GetSyncObj;
end; { TOmniCS.GetSyncObj }

procedure TOmniCS.Initialize;
var
  syncIntf: IOmniCriticalSection;
begin
  Assert(cardinal(@ocsSync) mod 4 = 0, 'TOmniCS.Initialize: ocsSync is not 4-aligned!');
  if not assigned(ocsSync) then begin
    syncIntf := CreateOmniCriticalSection;
    if InterlockedCompareExchange(PInteger(@ocsSync)^, integer(syncIntf), 0) = 0 then
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
  ocsCritSect := TCriticalSection.Create;
end; { TOmniCriticalSection.Create }

destructor TOmniCriticalSection.Destroy;
begin
  FreeAndNil(ocsCritSect);
end; { TOmniCriticalSection.Destroy }

procedure TOmniCriticalSection.Acquire;
begin
  ocsCritSect.Acquire;
end; { TOmniCriticalSection.Acquire }

function TOmniCriticalSection.GetSyncObj: TSynchroObject;
begin
  Result := ocsCritSect;
end; { TOmniCriticalSection.GetSyncObj }

procedure TOmniCriticalSection.Release;
begin
  ocsCritSect.Release;
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
  currentReference: integer;
begin
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 2, currentReference);
end; { TOmniMREW.EnterReadLock }

procedure TOmniMREW.EnterWriteLock;
var
  currentReference: integer;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 1, currentReference);
  //Now wait on all readers
  repeat
  until omrewReference = 1;
end; { TOmniMREW.EnterWriteLock }

procedure TOmniMREW.ExitReadLock;
begin
  //Decrease omrewReference
  InterlockedExchangeAdd(omrewReference, -2);
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

initialization
  GOmniCancellationToken := CreateOmniCancellationToken;
end.
