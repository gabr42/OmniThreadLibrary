///<summary>Synchronisation primitives. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2009, Primoz Gabrijelcic
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
///   Last modification : 2009-03-30
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2008-08-26
///       - TOmniCS and IOmniCriticalSection imported from the OtlCommon unit.
///       - [GJ] Added very simple (and very fast) multi-reader-exclusive-writer TOmniMREW.
///       - First official release.
///</para></remarks>

unit OtlSync;

interface

uses
  SyncObjs;

type
  IOmniCriticalSection = interface ['{AA92906B-B92E-4C54-922C-7B87C23DABA9}']
    procedure Acquire;
    procedure Release;
    function  GetSyncObj: TSynchroObject;
  end; { IOmniCriticalSection }

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

  TOmniMREW = record
  strict private
    omrewReference : integer;      //Reference.Bit0 is 'writing in progress' flag
  public
    procedure EnterReadLock;
    procedure EnterWriteLock;
    procedure ExitReadLock;
    procedure ExitWriteLock;
  end; { TOmniMREW }

  function CreateOmniCriticalSection: IOmniCriticalSection;

// Intel Atomic functions support
function CAS32(const oldValue, newValue: cardinal; var destination): boolean; overload;
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean; overload;
function CAS64(const oldData: pointer; oldReference: cardinal; newData: pointer;
  newReference: cardinal; var destination): boolean;
function GetThreadId: cardinal;
function GetCPUTimeStamp: int64;

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

{ exports }

function CreateOmniCriticalSection: IOmniCriticalSection;
begin
  Result := TOmniCriticalSection.Create;
end; { CreateOmniCriticalSection }

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
end; { AtomicCmpXchg8b }

function GetThreadId: cardinal;
//result := GetCurrentThreadId;
asm
  mov   eax, fs:[$18]                                           //eax := thread information block
  mov   eax, [eax + $24]                                        //eax := thread id
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

end.
