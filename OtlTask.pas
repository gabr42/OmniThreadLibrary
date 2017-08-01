///<summary>Task interface. Part of the OmniThreadLibrary project.</summary>
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
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2008-06-12
///   Last modification : 2017-08-01
///   Version           : 1.16
///</para><para>
///   History:
///     1.16: 2017-08-01
///       - Defined IOmniTask.InvokeOnSelf method.
///     1.15: 2017-07-26
///       - Defined IOmniTask.SetTimer overloads accepting TProc and TProc<integer> timer method.
///     1.14: 2016-07-01
///       - Defined IOmniTask.SetProcessorGroup and .SetNUMANode.
///     1.13: 2011-07-14
///       - IOmniTaskExecutionModifier removed again.
///     1.12: 2011-07-04
///       - IOmniTaskExecutor.Execute accepts optional IOmniTaskExecutionModifier parameter.
///     1.11: 2011-03-16
///       - Defined IOmniTask.Invoke method.
///     1.10: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.09: 2010-03-16
///       - Added support for multiple simultaneous timers. SetTimer takes additional
///         'timerID' parameter. The old SetTimer assumes timerID = 0.
///     1.08: 2010-02-03
///       - Defined IOmniTask.CancellationToken property.
///     1.07: 2010-01-13
///       - Defined IOmniTask.Implementor property.
///     1.06: 2009-12-12
///       - Defined IOmniTask.RegisterWaitObject/UnregisterWaitObject.
///       - Implemented TOmniWaitObjectList.
///     1.05: 2009-02-06
///       - Implemented per-thread data storage.
///     1.04: 2009-01-26
///       - Implemented IOmniTask.Enforced behaviour modifier.
///     1.03: 2008-11-01
///       - *** Breaking interface change ***
///         - IOmniTask.Terminated renamed to IOmniTask.Stopped.
///         - New IOmniTask.Terminated that check whether the task
///           *has been requested to terminate*.
///     1.02: 2008-10-05
///       - Added two overloaded SetTimer methods using string/pointer invocation.
///     1.01: 2008-09-18
///       - Exposed SetTimer interface.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

unit OtlTask;

{$I OtlOptions.inc}

interface

uses
  SysUtils,
  Classes,
  SyncObjs,
  GpLists,
  {$IFNDEF MSWINDOWS}
  Generics.Collections,
  {$ENDIF ~MSWINDOWS}
  OtlCommon,
  OtlSync,
  OtlComm;

type
  IOmniTask = interface;

  TOmniWaitObjectMethod = procedure of object;

  TOmniWaitObjectList = class
  strict private
    owolResponseHandlers: TGpTMethodList;
    owolWaitObjects     : {$IFDEF MSWINDOWS}TGpInt64List{$ELSE}TList<IOmniEvent>{$ENDIF};
  strict protected
    function  GetResponseHandlers(idxHandler: integer): TOmniWaitObjectMethod;
    function  GetWaitObjects(idxWaitObject: integer): TOmniTransitionEvent;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(waitObject: TOmniTransitionEvent; responseHandler: TOmniWaitObjectMethod);
    function  Count: integer;
    procedure Remove(waitObject: TOmniTransitionEvent);
    property ResponseHandlers[idxHandler: integer]: TOmniWaitObjectMethod read
      GetResponseHandlers;
    property WaitObjects[idxWaitObject: integer]: TOmniTransitionEvent read GetWaitObjects;
  end; { TOmniWaitObjectList }

  {$IFNDEF MSWINDOWS}
  IOmniEventAndProc = interface(IOmniEvent) ['{2CA14FE0-4616-41CC-BDED-EEDE88BC6492}']
    function BaseEvent: IOmniEvent;
    function Proc: TOmniWaitObjectMethod;
  end; { IOmniEventAndProc }

  TOmniSynchroArray = TArray<IOmniSynchro>;
  TOmniEventProcList = class(TList<IOmniEventAndProc>)
  public
    function  AsSyncroArray: TOmniSynchroArray;
    procedure RemoveBaseEvent(const Base: IOmniEvent);
  end;
  {$ENDIF ~MSWINDOWS}

  {$IFDEF OTL_Anonymous}
  TOmniTaskInvokeFunction = reference to procedure;
//  TOmniTaskInvokeFunctionEx = reference to procedure(const task: IOmniTaskControl);
  {$ENDIF OTL_Anonymous}

  IOmniTask = interface ['{958AE8A3-0287-4911-B475-F275747400E4}']
    function  GetCancellationToken: IOmniCancellationToken;
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetCounter: IOmniCounter;
    function  GetImplementor: TObject;
    function  GetLock: TSynchroObject;
    function  GetName: string;
    function  GetParam: TOmniValueContainer;
    function  GetTerminateEvent: TOmniTransitionEvent;
    function  GetThreadData: IInterface;
    function  GetUniqueID: int64;
  //
    procedure ClearTimer(timerID: integer = 0);
    procedure Enforced(forceExecution: boolean = true);
    {$IFDEF OTL_Anonymous}
    procedure Invoke(remoteFunc: TOmniTaskInvokeFunction); //overload;
    procedure InvokeOnSelf(remoteFunc: TOmniTaskInvokeFunction);
//    procedure Invoke(remoteFunc: TOmniTaskInvokeFunctionEx); overload;
    {$ENDIF OTL_Anonymous}
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure RegisterWaitObject(waitObject: TOmniTransitionEvent; responseHandler: TOmniWaitObjectMethod); overload;
    procedure SetException(exceptionObject: pointer);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetProcessorGroup(procGroupNumber: integer);
    procedure SetNUMANode(numaNodeNumber: integer);
    procedure SetTimer(interval_ms: cardinal); overload; deprecated {$IFDEF Unicode}'use three-parameter version'{$ENDIF Unicode};
    procedure SetTimer(interval_ms: cardinal; const timerMessage: TOmniMessageID); overload; deprecated {$IFDEF Unicode}'use three-parameter version'{$ENDIF Unicode};
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TOmniMessageID); overload;
    {$IFDEF OTL_Anonymous}
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc); overload;
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc<integer>); overload;
    {$ENDIF OTL_Anonymous}
    procedure StopTimer;
    procedure Terminate;
    function  Terminated: boolean;
    function  Stopped: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure UnregisterWaitObject(waitObject: TOmniTransitionEvent);
    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Implementor: TObject read GetImplementor;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param: TOmniValueContainer read GetParam;
    property TerminateEvent: TOmniTransitionEvent read GetTerminateEvent; //use Terminate to terminate a task, don't just set TerminateEvent
    property ThreadData: IInterface read GetThreadData;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTask }

  IOmniTaskExecutor = interface ['{123F2A63-3769-4C5B-89DA-1FEB6C3421ED}']
    procedure Execute;
    procedure SetThreadData(const value: IInterface);
  end; { IOmniTaskExecutor }

{$IFDEF OTL_Anonymous}
  TOmniTaskDelegate = reference to procedure(const task: IOmniTask);
{$ENDIF OTL_Anonymous}

{$IFNDEF MSWINDOWS}
  function DecorateEvent(const Base: IOmniEvent; AProc: TOmniWaitObjectMethod): IOmniEventAndProc;
{$ENDIF ~MSWINDOWS}

implementation

{ exports }

{$IFNDEF MSWINDOWS}
function DecorateEvent(const Base: IOmniEvent; AProc: TOmniWaitObjectMethod): IOmniEventAndProc;
begin
  // TODO
end;
{$ENDIF ~MSWINDOWS}

{ TOmniWaitObjectList }

constructor TOmniWaitObjectList.Create;
begin
  inherited Create;
  owolWaitObjects := {$IFDEF MSWINDOWS}TGpInt64List.Create{$ELSE}TList<IOmniEvent>.Create{$ENDIF};
  owolResponseHandlers := TGpTMethodList.Create;
end; { TOmniWaitObjectList.Create }

destructor TOmniWaitObjectList.Destroy;
begin
  FreeAndNil(owolResponseHandlers);
  FreeAndNil(owolWaitObjects);
  inherited Destroy;
end; { TOmniWaitObjectList.Destroy }

procedure TOmniWaitObjectList.Add(waitObject: TOmniTransitionEvent;
  responseHandler: TOmniWaitObjectMethod);
begin
  Remove(waitObject);
  owolWaitObjects.Add(waitObject);
  owolResponseHandlers.Add(TMethod(responseHandler));
end; { TOmniWaitObjectList.Add }

function TOmniWaitObjectList.Count: integer;
begin
  Result := owolWaitObjects.Count;
end; { TOmniWaitObjectList.Count }

function TOmniWaitObjectList.GetResponseHandlers(idxHandler: integer):
  TOmniWaitObjectMethod;
begin
  Result := TOmniWaitObjectMethod(owolResponseHandlers[idxHandler]);
end; { TOmniWaitObjectList.GetResponseHandlers }

function TOmniWaitObjectList.GetWaitObjects(idxWaitObject: integer): TOmniTransitionEvent;
begin
  Result := owolWaitObjects[idxWaitObject];
end; { TOmniWaitObjectList.GetWaitObjects }

procedure TOmniWaitObjectList.Remove(waitObject: TOmniTransitionEvent);
var
  idxWaitObject: integer;
begin
  idxWaitObject := owolWaitObjects.IndexOf(waitObject);
  if idxWaitObject >= 0 then begin
    owolWaitObjects.Delete(idxWaitObject);
    owolResponseHandlers.Delete(idxWaitObject);
  end;
end; { TOmniWaitObjectList.Remove }

{$IFNDEF MSWINDOWS}
function TOmniEventProcList.AsSyncroArray: TOmniSynchroArray;
begin
  //TODO
end;

procedure TOmniEventProcList.RemoveBaseEvent(const Base: IOmniEvent);
begin
  //TODO
end;
{$ENDIF ~MSWINDOWS}

initialization
  Assert(SizeOf(THandle) <= SizeOf(int64));
end.
