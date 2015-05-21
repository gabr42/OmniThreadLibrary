///<summary>Microlocking containers. Part of the OmniThreadLibrary project.</summary>
///<remarks>TOmni[Base]Queue requires Pentium 4 processor (or newer) unless OTL_OLDCPU is defined.</remarks>
///<author>Primoz Gabrijelcic, GJ</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2012 Primoz Gabrijelcic
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
///   Author            : GJ, Primoz Gabrijelcic
///   Creation date     : 2008-07-13
///   Last modification : 2012-12-05
///   Version           : 3.01a
///</para><para>
///   History:
///     3.01a: 2012-12-05
///       - Prevent memory leak if (Queue|Stack).Initialize is called more than once
///         (thx to [h.hasenack]).
///     3.01: 2012-11-09
///       - TOmniBaseBounded(Queue|Stack) internally aligns allocated memory to required
///         CAS granularity (8 bytes on 32-bit platforms, 16 bytes on 64-bit platforms).
///       - TOmniBaseBoundedQueue's ring buffers are internally aligned to 2*SizeOf(pointer).
///     3.0: 2011-12-19
///       - [GJ] Implemented 64-bit TOmni[Base]Bounded(Queue|Stack).
///       - Fixed TOmni[Base]Queue to work in 64-bit world.
///     2.05: 2011-08-26
///       - Implemented TOmni[Base]Queue.IsEmpty. Keep in mind that the returned value may
///         not be valid for any ammount of time if other threads are reading from/
///         writing to the queue.
///     2.04: 2010-07-01
///       - Includes OTLOptions.inc.
///     2.03a: 2010-05-06
///       - Fixed memory leak in TOmni[Base]Queue when queueing String, WideString,
///         Variant and Extended values.
///     2.03: 2010-02-18
///       - Reversed head and tail because they were used illogically.
///     2.02a: 2010-02-09
///       - Dynamically allocate head/tail structures so that they are allways 8-allocated.
///       - Optimized algorithm using atomic move instead of atomic compare-and-swap in
///         some places (thanks to GJ).
///     2.02: 2010-02-08
///       - New ABA- and MREW-free dynamic queue algorithm.
///       - Dynamic queue parameters are settable in the constructor.
///     2.01: 2010-02-04
///       - Uses CAS8 instead of CAS32.
///     2.0: 2009-12-25
///       - Implemented dynamically allocated, O(1) enqueue and dequeue, threadsafe,
///         microlocking queue. Class TOmniBaseQueue contains base implementation
///         while TOmniQueue adds notification support.
///       - Big class rename: TOmniBaseStack -> TOmniBaseBoundedStack,
///         TOmniStack -> TOmniBoundedStack, TOmniBaseQueue -> TOmniBaseBoundedQueue,
///         TOmniQueue -> TOmniBoundedQueue.
///     1.02: 2009-12-22
///       - TOmniContainerSubject moved into OtlContainerObserver because it will also be
///         used in OtlCollections.
///     1.01b: 2009-11-11
///       - [GJ] better fix for the initialization crash.
///     1.01a: 2009-11-10
///       - Bug fixed: Initialization code could crash with range check error.
///     1.01: 2008-10-26
///       - [GJ] Redesigned stack with better lock contention.
///       - [GJ] Totally redesigned queue, which is no longer based on stack and allows
///         multiple readers.
///</para></remarks>

unit OtlContainers;

{$I OtlOptions.inc}

interface

uses
  Classes,
  OtlCommon,
  OtlContainerObserver;

type
  IOmniQueue = interface ['{AE6454A2-CDB4-43EE-9F1B-5A7307593EE9}']
    function  GetContainerSubject: TOmniContainerSubject;
    function  IsEmpty: boolean;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;

    property  ContainerSubject: TOmniContainerSubject read GetContainerSubject;
  end; { IOmniQueue }

/// <param name="UseBusLocking">Set to true to use a spinlock. Otherwise synchronisation is achieved by a critical section.</param>
/// <param name="ThresholdForFull">The count of OmniValues to which if the queue reaches or exceeds, it is considered full.
///   Use a a value of -1 to indicate there is no threshold (and hence events like coiNotifyOnAlmostFull will never fire).</param>
function CreateOmniQueue(UseBusLocking: boolean; ThresholdForFull: integer = -1): IOmniQueue;

var
  CPartlyEmptyLoadFactor: double = 0.8; // When an element count drops below 80%, the container is considered 'partly empty'.
  CAlmostFullLoadFactor : double = 0.9; // When an element count raises above 90%, the container is considered 'almost full'.

implementation

uses
  SysUtils,
  SyncObjs,
  OtlSync,
  Generics.Collections;


type
  TInterestSet = set of TOmniContainerObserverInterest;
  TOmniQueue = class(TInterfacedObject, IOmniQueue)
  strict private
    ocContainerSubject: TOmniContainerSubject;
    FInnerQueue: TQueue<TOmniValue>;
    FFullThreshold: integer;
    FPartlyEmptyThreshold, FAlmostFullThreshold: integer;
    FNotifiableEvents: TInterestSet;
  private
    function  GetContainerSubject: TOmniContainerSubject;
    function  IsEmpty: boolean;
    function  Dequeue: TOmniValue;
    procedure Enqueue(const value: TOmniValue);
    function  TryDequeue(var value: TOmniValue): boolean;
    procedure CollectionNotifyEvent(Sender: TObject; const Item: TOmniValue; Action: TCollectionNotification);
    procedure PropagateNotifications(Events: TInterestSet);
    procedure DoWithCritSec( Proc: TProc);
  protected
    procedure EnterCriticalSection;  virtual; abstract;
    procedure LeaveCriticalSection;  virtual; abstract;
  public
    constructor Create(AThresholdForFull: integer);
    destructor  Destroy; override;
  end; { TOmniQueue }


  TOmniQueueCS = class(TOmniQueue)
  strict private
    FCritSect: TFixedCriticalSection;
  protected
    procedure EnterCriticalSection;  override;
    procedure LeaveCriticalSection;  override;
  public
    constructor Create(AThresholdForFull: integer);
    destructor  Destroy; override;
  end;

  TOmniQueueSpin = class(TOmniQueue)
  strict private
    FLock: TSpinLock;
  protected
    procedure EnterCriticalSection;  override;
    procedure LeaveCriticalSection;  override;
  public
    constructor Create(AThresholdForFull: integer);
  end;

function CreateOmniQueue(UseBusLocking: boolean; ThresholdForFull: integer = -1): IOmniQueue;
begin
  if UseBusLocking then
      result := TOmniQueueSpin.Create( ThresholdForFull)
    else
      result := TOmniQueueCS.Create( ThresholdForFull)
end;

constructor TOmniQueue.Create(AThresholdForFull: integer);
begin
  ocContainerSubject := TOmniContainerSubject.Create;;
  FInnerQueue        := TQueue<TOmniValue>.Create;
  FInnerQueue.OnNotify := CollectionNotifyEvent;
  FFullThreshold     := AThresholdForFull;
  if FFullThreshold > 0 then
      begin
      FPartlyEmptyThreshold := Round(FFullThreshold * CPartlyEmptyLoadFactor);
      FAlmostFullThreshold  := Round(FFullThreshold * CAlmostFullLoadFactor);
      if FPartlyEmptyThreshold = FAlmostFullThreshold then
        Inc( FAlmostFullThreshold)
      end
    else
      begin
      FPartlyEmptyThreshold := -1;
      FAlmostFullThreshold  := -1
      end;
  FNotifiableEvents := []
end;

destructor TOmniQueue.Destroy;
begin
  FreeAndNil( ocContainerSubject);
  FInnerQueue.Free;
  inherited
end;

procedure TOmniQueue.CollectionNotifyEvent(Sender: TObject;
  const Item: TOmniValue; Action: TCollectionNotification);
var
  BeforeCount, AfterCount: integer;
begin
  // This method occurs within the critical section.
  AfterCount := FInnerQueue.Count;
  case Action of
    cnAdded    : begin
                 Include( FNotifiableEvents, coiNotifyOnAllInserts);
                 if AfterCount = FAlmostFullThreshold then
                   Include( FNotifiableEvents, coiNotifyOnAlmostFull);
                 end;
    cnRemoved,
    cnExtracted: begin
                 Include( FNotifiableEvents, coiNotifyOnAllRemoves);
                 if AfterCount = FAlmostFullThreshold then
                   Include( FNotifiableEvents, coiNotifyOnPartlyEmpty);
                 end;
  end;
end;

procedure TOmniQueue.DoWithCritSec(Proc: TProc);
var
  PickUp: TInterestSet;
begin
  EnterCriticalSection;
  try
    FNotifiableEvents := [];
    Proc;
    PickUp := FNotifiableEvents;
    FNotifiableEvents := []
  finally
    EnterCriticalSection;
  end;
  PropagateNotifications( PickUp)
end;

function TOmniQueue.Dequeue: TOmniValue;
var
  EnclosedResult: TOmniValue;
begin
  DoWithCritSec( procedure()
    begin
      if FInnerQueue.Count > 0 then
        EnclosedResult := FInnerQueue.Dequeue
      else
        raise Exception.Create('TOmniBaseQueue.Dequeue: Message queue is empty')
    end);
  result := EnclosedResult
end;

procedure TOmniQueue.Enqueue(const value: TOmniValue);
begin
  DoWithCritSec( procedure()
    begin
      FInnerQueue.Enqueue(Value)
    end)
end;

function TOmniQueue.GetContainerSubject: TOmniContainerSubject;
begin
  Result := ocContainerSubject
end;

function TOmniQueue.IsEmpty: boolean;
begin
  EnterCriticalSection;
  Result := FInnerQueue.Count = 0;
  LeaveCriticalSection
end;

procedure TOmniQueue.PropagateNotifications(Events: TInterestSet);
var
  Ev: TOmniContainerObserverInterest;
begin
  if assigned( ocContainerSubject) and (Events <> []) then
    for Ev := Low(TOmniContainerObserverInterest) to Low(TOmniContainerObserverInterest) do
      if Ev in Events then
        ocContainerSubject.Notify( Ev)
end;

function TOmniQueue.TryDequeue(var value: TOmniValue): boolean;
var
  EnclosedValue : TOmniValue;
  EnclosedResult: boolean;
begin
  DoWithCritSec( procedure()
    begin
      EnclosedResult := FInnerQueue.Count > 0;
      if EnclosedResult then
        EnclosedValue := FInnerQueue.Dequeue
    end);
  value  := EnclosedValue;
  result := EnclosedResult
end;


constructor TOmniQueueSpin.Create(AThresholdForFull: integer);
begin
  inherited Create(AThresholdForFull);
  FLock.Create(True)
end;

procedure TOmniQueueSpin.EnterCriticalSection;
begin
  FLock.Enter
end;

procedure TOmniQueueSpin.LeaveCriticalSection;
begin
  FLock.Exit(True)
end;


constructor TOmniQueueCS.Create(AThresholdForFull: integer);
begin
  inherited Create(AThresholdForFull);
  FCritSect := TFixedCriticalSection.Create
end;

destructor TOmniQueueCS.Destroy;
begin
  FCritSect.Free;
  inherited
end;

procedure TOmniQueueCS.EnterCriticalSection;
begin
  FCritSect.Enter
end;

procedure TOmniQueueCS.LeaveCriticalSection;
begin
  FCritSect.Leave
end;

end.

