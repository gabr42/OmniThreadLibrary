///<summary>Documentation for platform independant synchronization primitives.
///    Also contains some commonly used type.
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
///       - Imported from mobile/Otl.Parallel.SynchroPrimitives.pas.

unit OtlSync.Platform;

{
  Synchronisation Primitives are classes, interfaces and solutions for
    basic low-level thread synchronisation mechanisms, such as Event, Semaphore,
    CountDown, etc.

  There are 3 levels of synchronisation primitives:

    1. The basic level. These are non-reference counted classes and records.
       They are very simple and straight forward. The user has responsibility
       for life-cycle management. The basic level is ideal when the synchro
       is used exclusively within a class, and object pooling is not
       an important consideration.

       Implemented in OtlPlatform.Sync.Basic.

    2. The interface level. These are reference counted interface solutions.
       They have very simple interface pointer types. When the last reference
       is released, they return to a pool, and become available for re-use,
       thus minimising creation events, which can be expensive for kernel-mode
       synchros. The interface level is ideal for shared access. Solutions to
       the interface level build on the basic level.

       Implemented in OtlPlatform.Sync.Interfaced.

    3. The modular level. These are interfaced synchros which can be easily
       combined together in a composite design pattern. For example you may
       have an event and a semaphore, and you require to wait until either one
       is signaled. When the synchros are at the modular level, this is
       a trivial exercise. Solutions to the modular level build on the
       interface level.

       Implemented in OtlPlatform.Sync.Modular.

  Synchronisation, under the hood, uses one of two modes:

    1. Kernel-mode locking. This is a heavy-weight mechanism that uses a call
       to the operating system kernel, and quiet likely results in a process
       switch. Process switching is expensive.

    2. Bus-mode locking. This is a light-weight mechanism that locks the bus
       while needed. If the bus lock is being used by other processor,
       this processor will spin until it has the lock. Long waits (>10ms)
       are expensive in terms of wasted CPU utilisation.

  This library provides 9 low-level synchonisation mechanisms. The first 8
  mechanisms are provided at each of the 3 levels. The Composite synchro
  is only available at the modular level.

    1. Manual event. An event models a boolean value. An event is either "Set"
       or not set. The event can be set by calling method SetEvent(), and
       cleared by calling ResetEvent(). A thread can block until the event is set.
       With a manual event, unblocking on a set event does not change the event state.

    2. Auto-reset event. An Auto-reset event is the same as a manual event,
       except that when a thread unblocks on a succesful WaitFor(), the event
       is automatically reset (cleared). The reset and the unblock occurs atomically.

    3. Semaphore. A semaphore models a cardinal number. Signaling the semaphore
       bumps the number up by +1. Behaviour is undefined when the number
       would exceed MaxCardinal. WaitFor() blocks until the number is positive,
       and then decrements the number. The Unblocking and the decrementing
       occurs atomically.

    4. CountDown. A CountDown models a cardinal number. Signaling the CountDown
       decrements the counter. Signaling when zero raises an exception.
       A thread can WaitFor() the count to go to zero. CountDowns can also
       be reset back to thier initial value.

    5. CountUp. The same as a CountDown, but it counts up. Signaling increments
       the counter. Signaling when at the threshold raises an exception.

    6. Critical section. Critical sections provide Enter() and Leave()
       methods and record the entered thread, and entry count.
       Enter() blocks until no other thread has entered. Enter() and Leave()
       calls must be balanced. Critical sections use kernel locking.

    7. SpinLock. SpinLocks are like critical sections but use bus locking
       instead. Unlike the Embarcadero spin lock, the spin locks in this
       library are re-enterent. SpinLock at the basic level is a record.

    8. Functional event. Functional events are like manual or auto-reset
       events, except that instead of an explicitly stored state, the
       signaled state is computed by a user supplied function.
       In other words, the call can wait on any condition which can be
       computed quickly and without blocking.

    9. Composite synchro. Composite synchros combine an array of one or
       more dependent synchros. Composite synchros cant be signaled but
       a calling thread may wait on some condition which is a function
       of the dependent synchros. In the case of the Win32/Win64 platforms,
       waiting on ALL or ANY of the synchros to be signaled, can leverage
       underlying OS support for efficient operation.
}

interface

uses
  System.SysUtils;

type
  TEventFunction = reference to procedure(doAcquire: boolean;
    var wasSuccessfullyAcquired: boolean; var isInSignalledState: boolean);

type
  TSynchroException = class(Exception)
  public
    FErrCode: integer;
    constructor Create(ErrCode: integer; const Args: array of const); overload;
    constructor Create(ErrCode: integer); overload;
  end; { TSynchroException }

  TSynchroExceptionMessage = record
    ErrCode: integer;
    MsgFmt: string;
  end; { TSynchroExceptionMessage }

const
  ESynchroIDFirst = 1;
  EIsSignalledNotSupported     = 1;
  ESignalCountUpDownRange      = 2;
  ECannotDestroySynchroFactory = 3;
  ECompositeNeedsOneFactor     = 4;
  EOnlyModularCombinable       = 5;
  ECompositeSynchroMixedBag    = 6;
  ESynchroIDLast = 6;

  SynchroExceptionMessages: array [ESynchroIDFirst..ESynchroIDLast] of TSynchroExceptionMessage = (
    (ErrCode: EIsSignalledNotSupported;     MsgFmt: 'IsSignalled() not supported on this class.'),
    (ErrCode: ESignalCountUpDownRange;      MsgFmt: 'Signal() out of range for TCountUp/Down'),
    (ErrCode: ECannotDestroySynchroFactory; MsgFmt: 'Cannot destroy TSynchroFactory while there are still allocated synchros.'),
    (ErrCode: ECompositeNeedsOneFactor;     MsgFmt: 'TCompositeSynchro needs at least one factor.'),
    (ErrCode: EOnlyModularCombinable;       MsgFmt: 'TCompositeSynchro - all factors must be modular sychros.'),
    (ErrCode: ECompositeSynchroMixedBag;    MsgFmt: 'TConditionEvent created with invalid mixed bag of direct and indirect members.')
  );

implementation

{ TSynchroException }

constructor TSynchroException.Create(
  ErrCode: integer; const Args: array of const);
var
  i     : integer;
  msgFmt: string;
begin
  msgFmt := '';
  for i := Low(SynchroExceptionMessages) to High(SynchroExceptionMessages) do
    if SynchroExceptionMessages[i].ErrCode = ErrCode then begin
      msgFmt := SynchroExceptionMessages[i].msgFmt;
      break;
    end;
  if msgFmt = '' then
    msgFmt := Format('Unknown error (%d)', [ErrCode])
  else if msgFmt.Contains('%') and (Length(Args) > 0) then
    msgFmt := Format(msgFmt, Args);
  FErrCode := ErrCode;
  inherited Create(msgFmt);
end; { TSynchroException.Create }

constructor TSynchroException.Create(ErrCode: integer);
begin
  Create(ErrCode, []);
end; { TSynchroException.Create }

end.
