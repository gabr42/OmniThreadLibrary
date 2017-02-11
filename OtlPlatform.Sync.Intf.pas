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

unit OtlPlatform.Sync.Intf;

// IMPORTANT!
//  READ THE COMMENTS IN UNIT OtlPlatform.SynchroPrimitives.

{$I OtlOptions.inc}

interface

uses
  System.SyncObjs,
  System.Generics.Collections,
  System.SysUtils,
  OtlPlatform.Atomic;

type
  TEventFunction = reference to procedure(doAcquire: boolean;
    var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);

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
      property Handle: THandle   read GetHandle;
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

implementation

end.
