///<summary>Task control encapsulation. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2018, Primoz Gabrijelcic
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
///   Contributors      : GJ, Lee_Nover, Sean B. Durkin
///   Creation date     : 2008-06-12
///   Last modification : 2018-03-16
///   Version           : 1.40a
///</para><para>
///   History:
///     1.40a: 2018-03-16
///       - TOmniMessageExec.OnTerminated checks whether the event handler is assigned
///         before executing it.
///     1.40: 2017-08-01
///       - Implemented IOmniTask.InvokeOnSelf method.
///     1.39: 2017-07-26
///       - Implemented support for timer events implemented as TProc and TProc<integer> methods.
///     1.38b: 2017-04-06
///       - Compiles with Delphi 10.2 Tokyo.
///     1.38a: 2017-03-28
///       - TOmniTaskExecutor now uses own 64-bit time function. DSiTimeGetTime64 cannot
///         be used for this purpose as its results cannot be compared across threads.
///     1.38: 2016-07-01
///       - Defined IOmniTaskControl.ProcessorGroup and .NUMANode.
///       - Added support for executing a task in a specific processor group or NUMA node.
///     1.37: 2015-10-04
///       - Imported mobile support by [Sean].
///     1.36b: 2015-09-07
///       - Added a debug log (when compiling a Debug build) when TWaitFor.MsgWaitAny
///         returns WAIT_FAILED.
///     1.36a: 2015-08-28
///       - CheckTimers is no longer called from DispatchOmniMessage if that one was
///         called from CheckTimers.
///     1.36: 2015-08-27
///       - TOmniWorker message hooks introduced.
///     1.35: 2014-11-16
///       - IOmniTaskControl can wait on any number of comm handles and wait objects.
///         That enables support for >60 tasks in the OtlThreadPool.
///     1.34: 2014-11-03
///       - TOmniTaskGroup can now own more than 60 tasks.
///     1.33a: 2014-09-23
///       - Fixed TOmniTaskControl.SetParameters.
///     1.33: 2014-09-07
///       - Implemented Run overloads that internally call Invoke to start
///         thread worker.
///     1.32c: 2014-01-08
///       - Thread priority is set correctly (to 'normal') if it is not explicitly specified.
///     1.32b: 2013-06-03
///       - Fixed task destruction deadlock introduced in 1.32a.
///     1.32a: 2013-05-26
///       - Fixed a problem in task destruction sequence.
///     1.32: 2013-01-30
///       - Added IOmniTaskControl.Stop - signals thread to stop and immediately returns.
///       - Fixed TOmniTaskGroup.TerminateAll.
///     1.31k: 2012-10-03
///       - Fixed message processing.
///     1.31j: 2012-10-01
///       - Refactored TOmniTaskExecutor.DispatchEvent a bit more.
///     1.31i: 2012-09-27
///       - Task controller implements method FilterMessage which allows the event
///         monitor to filter out internal messages.
///       - Removed TOmniTaskControlEventMonitor (no longer needed).
///     1.31h: 2012-09-24
///       - Fixed bug in TOmniTaskGroup.TerminateAll - maxWait_ms parameter was ignored.
///     1.31g: 2012-06-18
///       - Fixed race condition in task teardown. Big thanks to [meishier] for putting
///         together a reproducible test case.
///     1.31f: 2012-04-21
///       - Fixed race condition in InternalStop.
///     1.31e: 2012-02-07
///       - Bug fixed: Internal event monitor messages must be processed in Terminate,
///         otherwise OnTerminated is not called if the task is terminated from the task
///         controller. Big thanks to [Qmodem] for finding the bug.
///     1.31d: 2012-02-02
///       - Bug fixed: It was not possible to change timer delay once it was created.
///         Big thanks to [Unspoken] for finding the bug.
///     1.31c: 2011-12-14
///       - Fixed race condition between TOmniTask.Execute and TOmniTask.Terminate.
///       - Under some circumstances ProcessMessage failed to rebuild handle
///         array before waiting which could cause 'invalid handle' error.
///     1.31b: 2011-11-08
///       - Fixed invalid "A call to an OS function failed" error in DispatchEvent.
///     1.31a: 2011-11-06
///       - Fixed wrong order in teardown sequence in TOmniTask.Execute. Great thanks to
///         [Anton Alisov] for providing a reproducible test case.
///     1.31: 2011-11-05
///       - Adapted to OtlCommon 1.24.
///     1.30: 2011-11-05
///       - Task parameters are exposed through IOmniTaskControl.Param property.
///     1.29: 2011-08-27
///       - Implemented another OnTerminated overload acception parameterless anonymous
///         function.
///     1.28: 2011-07-17
///       - Implemented IOmniTaskControl.DetachException.
///     1.27: 2011-07-14
///       - IOmniTaskControl implements FatalException property.
///       - Support for non-silent exceptions removed.
///     1.26a: 2011-07-14
///       - Fixed race condition in TOmniTask.Execute. Big thanks to [Anton Alisov] for
///         providing reproducible test case.
///     1.26: 2011-07-04
///       - Changed exception handling.
///     1.25a: 2011-05-27
///       - Passes timer ID to timer proc if it accepts const TOmniValue parameter.
///     1.25: 2011-04-08
///       - IOmniTaskControl termination empties task message queue and calls appropriate
///         OnMessage handlers.
///     1.24: 2011-03-16
///       - Implemented IOmniTaskControl.Invoke(procedure) and
///         .Invoke(procedure const task: IOmniTask).
///       - Implemented IOmniTask.Invoke(procedure).
///     1.23b: 2011-02-28
///       - Bug fixed: Make sure timers are called even if there's a constant stream
///         of messages in registered message queues.
///     1.23a: 2011-01-07
///       - Bug fixed: Enumerating over TOmniTaskControlList (for example when using
///         IOmniTaskGroup.SendToAll) leaked one object.
///     1.23: 2010-12-02
///       - Added IOmniTaskControl.CancelWith(token) which can be used to enforce
///         non-default cancellation token.
///     1.22d: 2010-10-16
///       - Delayed Terminate did not set result.
///     1.22c: 2010-10-13
///       - Allow Terminate to be called from the OnTerminated handler.
///     1.22b: 2010-09-21
///       - Better workaround for the 'invalid handle' error.
///     1.22a: 2010-09-20
///       - Changed the place where internal monitor is destroyed to prevent 'invalid
///         handle' error.
///     1.22: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.21c: 2010-06-12
///       - TOmniTaskExecutor must always call Cleanup in case task was not executed.
///         (Issue #19, http://code.google.com/p/omnithreadlibrary/issues/detail?id=19).
///     1.21b: 2010-05-30
///       - Fixed TOmniTaskControl.WaitFor for pooled tasks.
///     1.21a: 2010-04-06
///       - [LN] Bug fixed: TOmniTaskControl.WaitFor would hang if thread was 
///         terminated externally.
///     1.21: 2010-03-16
///       - Added support for multiple simultaneous timers. SetTimer takes additional
///         'timerID' parameter. The old SetTimer assumes timerID = 0.
///     1.20d: 2010-02-22
///        - D2009 compilation hack moved to OtlCommon.
///     1.20c: 2010-02-22
///       - A better fix for the D2009 compilation issues, thanks to Serg.
///     1.20b: 2010-02-21
///       - TOmniTaskControl.otcOnTerminatedExec was not created when OnTerminated
///         function was called with a "reference to function" parameter.
///       - Fixed to compile with D2009.
///     1.20a: 2010-02-10
///       - Internal message forwarders must be destroyed during task termination.
///     1.20: 2010-02-09
///       - Added IOmniTaskControl.OnMessage(msgID, handler).
///     1.19: 2010-02-03
///       - IOmniTaskControl and IOmniTask implement CancellationToken property.
///     1.18: 2010-02-02
///       - TerminateWhen accepts cancellation token.
///     1.17: 2010-01-31
///       - Added WithLock overload.
///     1.16: 2010-01-14
///       - Implemented IOmniTaskControl.UserData[]. The application can store any values
///         in this array. It can be accessed via the integer or string index.
///     1.15: 2010-01-13
///       - Implemented IOmniTask.GetImplementor.
///     1.14a: 2009-12-18
///       - Worked around a change in Delphi 2010 update 4.
///     1.14: 2009-12-12
///       - Implemented support for IOmniTask.RegisterWaitObject/UnregisterWaitObject.
///     1.13a: 2009-12-12
///       - Raise loud exception for pooled tasks.
///     1.13: 2009-11-19
///       - Implemented IOmniTaskControl.Unobserved behaviour modifier.
///     1.12: 2009-11-15
///       - Event monitor notifications implemented with container observer.
///     1.11a: 2009-11-13
///       - Cleanup in TOmniTask.Execute reordered to fix Issue 13.
///     1.11: 2009-11-13
///       - Implemented automatic event monitor with methods IOmniTaskControl.OnMessage
///         and OnTerminated. Both support 'procedure of object' and
///         'reference to procedure' parameters.
///       - D2010 compatibility changes.
///     1.10: 2009-05-15
///       - Implemented IOmniTaskControl.SilentExceptions.
///     1.09: 2009-02-08
///       - Implemented per-thread task data storage.
///     1.08: 2009-01-26
///       - Implemented IOmniTaskControl.Enforced behaviour modifier.
///       - Added TOmniWorker.ProcessMessages - a support for worker to recursively
///         process messages inside message handlers.
///     1.07: 2009-01-19
///       - Implemented IOmniTaskControlList, a list of IOmniTaskControl interfaces.
///       - TOmniTaskGroup reimplemented using IOmniTaskControlList.
///     1.06: 2008-12-15
///       - TOmniWorker's internal message loop can now be overridden at various places
///         and even fully replaced with a custom code.
///     1.05a: 2008-11-17
///       - [Jamie] Fixed bug in TOmniTaskExecutor.Asy_SetTimerInt.
///     1.05: 2008-11-01
///       - IOmniTaskControl.Terminate kills the task thread if it doesn't terminate in
///         the specified amount of time.
///     1.04a: 2008-10-06
///       - IOmniTaskControl.Invoke modified to return IOmniTaskControl.
///     1.04: 2008-10-05
///       - Implemented IOmniTaskControl.Invoke (six overloads), used for string- and
///         pointer-based method dispatch (see demo 18 for more details and demo 19
///         for benchmarks).
///       - Implemented two SetTimer overloads using new invocation methods.
///       - Implemented IOmniTaskControl.SetQueue, which can be used to increase (or
///         reduce) the size of the IOmniTaskControl<->IOmniTask communication queue.
///         This function must be called before .SetMonitor, .RemoveMonitor, .Run or
///         .Schedule.
///     1.03b: 2008-09-26
///       - More stringent Win32 API result checking.
///     1.03a: 2008-09-25
///       - Bug fixed: TOmniTaskControl.Schedule always scheduled task to the global
///         thread pool.
///     1.03: 2008-09-20
///       - Implemented IOmniTaskGroup.SendToAll. This should be looked at as a temporary
///         solution. IOmniTaskGroup should expose communication interface (just like
///         IOmniTask and IOmniTaskControl) but in this case it should be one-to-many
///         queue connecting IOmniTaskGroup's Comm to all tasks inside the group.
///     1.02: 2008-09-19
///       - Added enumerator to the IOmniTaskGroup interface.
///       - Implemented IOmniTaskGroup.RegisterAllCommWith and .UnregisterAllCommFrom.
///       - Bug fixed in TOmniTaskExecutor.Asy_DispatchMessages - program crashed if
///         communications unregistered inside task's own timer method.
///       - Setting timer interval resets timer countdown.
///     1.01: 2008-09-18
///       - Implemented SetTimer on the IOmniTask side.
///       - Bug fixed: IOmniTaskGroup.RunAll was not returning a result.
///     1.0a: 2008-08-29
///       - Bug fixed: .MsgWait was not functional.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

///Literature
///  - Lock my Object... Please!, Allen Bauer,
///    http://blogs.codegear.com/abauer/2008/02/19/38856
///  - Threading in C#, Joseph Albahari, http://www.albahari.com/threading/
///  - Coordination Data Structures Overview, Emad Omara,
///    http://blogs.msdn.com/pfxteam/archive/2008/06/18/8620615.aspx
///  - Erlang, http://en.wikipedia.org/wiki/Erlang_(programming_language)
///  - A single-word reader/writer spin lock,
///    http://www.bluebytesoftware.com/blog/2009/01/30/ASinglewordReaderwriterSpinLock.aspx
///  - CancellationToken, 
///    http://blogs.msdn.com/pfxteam/archive/2009/06/22/9791840.aspx

// TODO 1 -oPrimoz Gabrijelcic : The whole Unobserved mess should go away - task should be implicitly owned ALWAYS
  
// TODO 3 -oPrimoz Gabrijelcic : Add general way to map unique ID into a task controller/task interface.
// TODO 3 -oPrimoz Gabrijelcic : ChainTo options 'only on success', 'only on fault' (http://blogs.msdn.com/pfxteam/archive/2010/02/09/9960735.aspx)

// TODO 3 -oPrimoz Gabrijelcic : Implement message bus (subscribe/publish)
// http://209.85.129.132/search?q=cache:B2PbIgFSyLcJ:www.dojotoolkit.org/book/dojo-book-0-9/part-3-programmatic-dijit-and-dojo/event-system/publish-and-subscribe-events+dojo+subscribe+publish&hl=sl&client=opera&strip=1
// http://msdn.microsoft.com/en-us/library/ms978583.aspx

// TODO 3 -oPrimoz Gabrijelcic : Could RegisterWaitForSingleObject be used to wait on more than 64 objects at the same time? http://msdn.microsoft.com/en-us/library/ms685061(VS.85).aspx

// TODO 1 -oPrimoz Gabrijelcic : Add cache to TOmniTaskExecutor.DispatchOmniMessage

// TODO 1 -oPrimoz Gabrijelcic : OnTerminated without the 'task' parameter - useful for anonymous functions

unit OtlTaskControl;

{$I OtlOptions.inc}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  OtlCommon,
  {$IFDEF MSWINDOWS}
  Windows,
  Messages,
  DetailedRTTI,
  DSiWin32,
  GpStuff,
  {$ELSE}
  Generics.Collections,
  {$ENDIF ~MSWINDOWS}
  GpLists,
  GpStringHash,
  SysUtils,
  Classes,
  SyncObjs,
  TypInfo,
  OtlSync,
  OtlComm,
  OtlTask,
  OtlContainers,
  OtlThreadPool,
  OtlContainerObserver;

type
  IOmniTaskControl = interface;
  TOmniSharedTaskInfo = class;
  
  IOmniTaskControlMonitor = interface ['{20CB3AB7-04D8-454B-AEFE-CFCFF8F27301}']
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl;
    {$IFDEF MSWINDOWS}
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl;
    {$ENDIF MSWINDOWS}
  end; { IOmniTaskControlMonitor }

  {$TYPEINFO ON} {$METHODINFO ON}
  IOmniWorker = interface ['{CA63E8C2-9B0E-4BFA-A527-31B2FCD8F413}']
    function  GetImplementor: TObject;
    function  GetTask: IOmniTask;
    procedure SetExecutor(executor: TObject);
    procedure SetTask(const value: IOmniTask);
  //message loop hooks; temporary solution, will be replaced with a better one
    procedure AfterWait(waitFor: TWaitFor; awaited: TWaitFor.TWaitForResult);
    procedure BeforeWait(var timeout_ms: cardinal);
    procedure MessageLoopPayload;
    procedure ProcessThreadMessages;
  //
    procedure Cleanup;
    procedure DispatchMessage(var msg: TOmniMessage);
    procedure Timer;
    function  Initialize: boolean;
    property Task: IOmniTask read GetTask write SetTask;
    property Implementor: TObject read GetImplementor;
  end; { IOmniWorker }

  TOmniWorker = class(TInterfacedObject, IOmniWorker)
  strict private
    owExecutor: TObject; {TOmniTaskExecutor}
    owTask    : IOmniTask;
  strict protected
    procedure ProcessMessages;
  protected //message loop hooks
    procedure AfterWait(waitFor: TWaitFor; awaited: TWaitFor.TWaitForResult); virtual;
    procedure BeforeWait(var timeout_ms: cardinal); virtual;
    procedure DispatchMessage(var msg: TOmniMessage); virtual;
    procedure MessageLoopPayload; virtual;
    procedure ProcessThreadMessages; virtual;
  protected
    procedure Cleanup; virtual;
    function  GetImplementor: TObject;
    function  GetTask: IOmniTask;
    function  Initialize: boolean; virtual;
    procedure SetExecutor(executor: TObject);
    procedure SetTask(const value: IOmniTask);
  public
    procedure Timer; virtual;
    property Task: IOmniTask read GetTask write SetTask;
    property Implementor: TObject read GetImplementor;
  end; { TOmniWorker }
  {$TYPEINFO OFF} {$METHODINFO OFF}

  TOmniTaskProcedure = procedure(const task: IOmniTask);
  TOmniTaskMethod = procedure(const task: IOmniTask) of object;

  TOTLThreadPriority = (tpIdle, tpLowest, tpBelowNormal, tpNormal, tpAboveNormal, tpHighest);

  IOmniTaskGroup = interface;

  TOmniTaskMessageEvent = procedure(const task: IOmniTaskControl; const msg: TOmniMessage) of object;
  TOmniTaskTerminatedEvent = procedure(const task: IOmniTaskControl) of object;
{$IFDEF OTL_Anonymous}
  TOmniOnMessageFunction = reference to procedure(const task: IOmniTaskControl; const msg: TOmniMessage);
  TOmniOnTerminatedFunction = reference to procedure(const task: IOmniTaskControl);
  TOmniOnTerminatedFunctionSimple = reference to procedure;
{$ENDIF OTL_Anonymous}
{$IFDEF OTL_Anonymous}
  TOmniTaskControlInvokeFunction = reference to procedure;
  TOmniTaskControlInvokeFunctionEx = reference to procedure(const task: IOmniTask);
{$ENDIF OTL_Anonymous}

  TOmniMessageExec = class
  strict protected
    omeOnMessage        : TOmniExecutable;
    omeOnMessageDispatch: TObject;
    omeOnTerminated     : TOmniExecutable;
  public
    constructor Create(exec: TOmniTaskMessageEvent); overload;
    constructor Create(exec: TOmniTaskTerminatedEvent); overload;
    constructor Create(dispatch: TObject); overload;
    constructor Clone(exec: TOmniMessageExec);
    procedure SetOnMessage(exec: TOmniTaskMessageEvent); overload;
    procedure SetOnTerminated(exec: TOmniTaskTerminatedEvent); overload;
    procedure OnMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OnTerminated(const task: IOmniTaskControl);
  public
    {$IFDEF OTL_Anonymous}
    constructor Create(exec: TOmniOnMessageFunction); overload;
    constructor Create(exec: TOmniOnTerminatedFunction); overload;
    procedure Apply(msgID: word; task: IOmniTaskControl);
    procedure SetOnMessage(exec: TOmniOnMessageFunction); overload;
    procedure SetOnTerminated(exec: TOmniOnTerminatedFunction); overload;
    {$ENDIF OTL_Anonymous}
  end; { TOmniMessageExec }

  IOmniTaskControl = interface ['{881E94CB-8C36-4CE7-9B31-C24FD8A07555}']
    function  GetCancellationToken: IOmniCancellationToken;
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetExitCode: integer;
    function  GetExitMessage: string;
    function  GetLock: TSynchroObject;
    function  GetName: string;
    function  GetUniqueID: int64;
    function  GetUserDataVal(const idxData: TOmniValue): TOmniValue;
    procedure SetUserDataVal(const idxData: TOmniValue; const value: TOmniValue);
  //
    function  Alertable: IOmniTaskControl;
    function  CancelWith(const token: IOmniCancellationToken): IOmniTaskControl;
    function  ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean = false): IOmniTaskControl;
    function  ClearTimer(timerID: integer): IOmniTaskControl;
    function  DetachException: Exception;
    function  Enforced(forceExecution: boolean = true): IOmniTaskControl;
    function  GetFatalException: Exception;
    function  GetParam: TOmniValueContainer;
    function  Invoke(const msgMethod: pointer): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload;
    function  Invoke(const msgName: string): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function  Invoke(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl; overload;
    function  Invoke(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait({$IFDEF MSWINDOWS}wakeMask: DWORD = QS_ALLEVENTS{$ELSE}wakeAll: boolean = true{$ENDIF}): IOmniTaskControl;
    function  NUMANode(numaNodeNumber: integer): IOmniTaskControl;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskControl; overload;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniMessageExec): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function  OnMessage(eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskControl; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskControl; overload;
    function  ProcessorGroup(procGroupNumber: integer): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer): IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload;
    function  Run(const msgName: string): IOmniTaskControl; overload;
    function  Run(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Run(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function  Run(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl; overload;
    function  Run(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  Schedule(const threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    {$IFDEF MSWINDOWS}
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    {$ENDIF MSWINDOWS}
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
    function  SetQueueSize(numMessages: integer): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal): IOmniTaskControl; overload; deprecated {$IFDEF Unicode}'use three-parameter version'{$ENDIF Unicode};
    function  SetTimer(interval_ms: cardinal; const timerMessage: TOmniMessageID): IOmniTaskControl; overload; deprecated {$IFDEF Unicode}'use three-parameter version'{$ENDIF Unicode};
    function  SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TOmniMessageID): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function  SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc): IOmniTaskControl; overload;
    function  SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc<integer>): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  SetUserData(const idxData: TOmniValue; const value: TOmniValue): IOmniTaskControl;
    procedure Stop;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: TOmniTransitionEvent): IOmniTaskControl; overload;
    function  TerminateWhen(token: IOmniCancellationToken): IOmniTaskControl; overload;
    function  Unobserved: IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskControl;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl; overload;
    function  WithLock(const lock: IOmniCriticalSection): IOmniTaskControl; overload;
  //
    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property FatalException: Exception read GetFatalException;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param: TOmniValueContainer read GetParam;
    property UniqueID: int64 read GetUniqueID;
    property UserData[const idxData: TOmniValue]: TOmniValue read GetUserDataVal write SetUserDataVal;
  end; { IOmniTaskControl }

  // Implementation details, needed by the OtlEventMonitor. Not for public consumption.
  IOmniTaskControlSharedInfo = interface(IOmniTaskControl) ['{5C3262CC-C941-406B-81CC-0E3B608E9077}']
    function  GetSharedInfo: TOmniSharedTaskInfo;
  //
    property SharedInfo: TOmniSharedTaskInfo read GetSharedInfo;
  end; { IOmniTaskControlSharedInfo }

  IOmniTaskControlListEnumerator = interface
    function GetCurrent: IOmniTaskControl;
    function MoveNext: boolean;
    property Current: IOmniTaskControl read GetCurrent;
  end; { IOmniTaskControlListEnumerator }

  IOmniTaskControlList = interface
    function  Get(idxItem: integer): IOmniTaskControl;
    function  GetCapacity: integer;
    function  GetCount: integer;
    procedure Put(idxItem: integer; const value: IOmniTaskControl);
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    //
    function  Add(const item: IOmniTaskControl): integer;
    procedure Clear;
    procedure Delete(idxItem: integer);
    procedure Exchange(idxItem1, idxItem2: integer);
    function  First: IOmniTaskControl;
    function  GetEnumerator: IOmniTaskControlListEnumerator;
    function  IndexOf(const item: IOmniTaskControl): integer;
    function  IndexOfID(uniqueID: int64): integer;
    procedure Insert(idxItem: integer; const item: IOmniTaskControl);
    function  Last: IOmniTaskControl;
    function  Remove(const item: IOmniTaskControl): integer;
    function  RemoveByID(uniqueID: int64): integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idxItem: integer]: IOmniTaskControl read Get write Put; default;
  end; { IOmniTaskControlList }

//v1.1 extensions:
//  maybe: Comm: IOmniCommunicationEndpoint, which is actually one-to-many-to-one
//    function  Sequential: IOmniTaskGroup;
//    function  Parallel(useThreadPool: IOmniThreadPool): IOmniTaskGroup;
//  maybe: if one of group processes dies, TerminateAll should automatically happen?
  IOmniTaskGroup = interface ['{B36C08B4-0F71-422C-8613-63C4D04676B7}']
    function  GetTasks: IOmniTaskControlList;
  //
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  GetEnumerator: IOmniTaskControlListEnumerator;
    function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    procedure SendToAll(const msg: TOmniMessage);
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
    property  Tasks: IOmniTaskControlList read GetTasks;
  end; { IOmniTaskGroup }

  TOmniSharedTaskInfo = class
  strict private
    ostiCancellationToken : IOmniCancellationToken; // must be the first field for alignment
  strict private
    ostiChainIgnoreErrors : boolean;
    ostiChainTo           : IOmniTaskControl;
    ostiCommChannel       : IOmniTwoWayChannel;
    ostiCounter           : IOmniCounter;
    ostiLock              : TSynchroObject;
    {$IFDEF MSWINDOWS}
    ostiMonitor           : TOmniContainerWindowsMessageObserver;
    {$ENDIF MSWINDOWS}
    ostiMonitorLock       : TOmniCS;
    ostiNUMANode          : integer;
    ostiProcessorGroup    : integer;
    ostiStopped           : boolean;
    ostiTaskName          : string;
    ostiTerminatedEvent   : TOmniTransitionEvent;
    ostiTerminateEvent    : TOmniTransitionEvent;
    ostiTerminating       : boolean;
    ostiUniqueID          : int64;
  strict protected
    function  GetCancellationToken: IOmniCancellationToken;
  protected
    procedure SetCancellationToken(const token: IOmniCancellationToken);
  public
    constructor Create;
    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property ChainIgnoreErrors: boolean read ostiChainIgnoreErrors write ostiChainIgnoreErrors;
    property ChainTo: IOmniTaskControl read ostiChainTo write ostiChainTo;
    property CommChannel: IOmniTwoWayChannel read ostiCommChannel write ostiCommChannel;
    property Counter: IOmniCounter read ostiCounter write ostiCounter;
    property Lock: TSynchroObject read ostiLock write ostiLock;
    {$IFDEF MSWINDOWS}
    property Monitor: TOmniContainerWindowsMessageObserver read ostiMonitor write ostiMonitor;
    {$ENDIF MSWINDOWS}
    property MonitorLock: TOmniCS read ostiMonitorLock;
    property NUMANode: integer read ostiNUMANode write ostiNUMANode;
    property ProcessorGroup: integer read ostiProcessorGroup write ostiProcessorGroup;
    property Stopped: boolean read ostiStopped write ostiStopped;
    property TaskName: string read ostiTaskName write ostiTaskName;
    property TerminatedEvent: TOmniTransitionEvent read ostiTerminatedEvent write ostiTerminatedEvent;
    property TerminateEvent: TOmniTransitionEvent read ostiTerminateEvent write ostiTerminateEvent;
    property Terminating: boolean read ostiTerminating write ostiTerminating;
    property UniqueID: int64 read ostiUniqueID write ostiUniqueID;
  end; { TOmniSharedTaskInfo }

  function CreateTask(worker: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(const worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
//  function CreateTask(worker: IOmniTaskGroup; const taskName: string = ''): IOmniTaskControl; overload;

{$IFDEF OTL_Anonymous}
  function CreateTask(worker: TOmniTaskDelegate; const taskName: string = ''): IOmniTaskControl; overload;
{$ENDIF OTL_Anonymous}

  function CreateTaskGroup: IOmniTaskGroup;

  function CreateTaskControlList: IOmniTaskControlList;

type
  TOmniInternalMessageType = (imtStringMsg, imtAddressMsg, imtFuncMsg, imtAnonMsg);

  TOmniInternalMessage = class
  strict private
    imInternalMessageType: TOmniInternalMessageType;
  public
    class function InternalType(const msg: TOmniMessage): TOmniInternalMessageType;
    constructor Create(internalMessageType: TOmniInternalMessageType);
    property InternalMessageType: TOmniInternalMessageType read imInternalMessageType;
  end; { TOmniInternalMessage }

  TOmniInternalStringMsg = class(TOmniInternalMessage)
  strict private
    ismMsgData: TOmniValue;
    ismMsgName: string;
  public
    class function  CreateMessage(const msgName: string; msgData: TOmniValue): TOmniMessage; inline;
    class procedure UnpackMessage(const msg: TOmniMessage; var msgName: string;
      var msgData: TOmniValue); inline;
    constructor Create(const msgName: string; const msgData: TOmniValue);
    property MsgData: TOmniValue read ismMsgData;
    property MsgName: string read ismMsgName;
  end; { TOmniInternalStringMsg }

  TOmniInternalAddressMsg = class(TOmniInternalMessage)
  strict private
    ismMsgData  : TOmniValue;
    ismMsgMethod: pointer;
  public
    class function  CreateMessage(const msgMethod: pointer; msgData: TOmniValue):
      TOmniMessage; inline;
    class procedure UnpackMessage(const msg: TOmniMessage; var msgMethod: pointer; var
      msgData: TOmniValue); inline;
    constructor Create(const msgMethod: pointer; const msgData: TOmniValue);
    property MsgData: TOmniValue read ismMsgData;
    property MsgMethod: pointer read ismMsgMethod;
  end; { TOmniInternalAddressMsg }

  {$IFDEF OTL_Anonymous}
  TOmniInternalAnonMsg = class(TOmniInternalMessage)
  strict private
    ismMsgData: TOmniValue;
    ismMsgProc: TProc<integer>;
  public
    class function CreateMessage(const msgAnon: TProc<integer>; msgData: TOmniValue): TOmniMessage; inline;
    class procedure UnpackMessage(const msg: TOmniMessage; var msgAnon: TProc<integer>;
      var msgData: TOmniValue); inline;
    constructor Create(const msgAnon: TProc<integer>; const msgData: TOmniValue);
    property MsgData: TOmniValue read ismMsgData;
    property MsgProc: TProc<integer> read ismMsgProc;
  end; { TOmniInternalAnonMsg }

  TOmniInternalFuncMsg = class(TOmniInternalMessage)
  strict private
    ifmFunc    : TOmniTaskControlInvokeFunction;
    ifmFuncEx  : TOmniTaskControlInvokeFunctionEx;
    ifmTaskFunc: TOmniTaskInvokeFunction;
  public
    class function CreateMessage(func: TOmniTaskControlInvokeFunction): TOmniMessage;
      overload; inline;
    class function  CreateMessage(func: TOmniTaskControlInvokeFunctionEx): TOmniMessage; overload; inline;
    class function CreateMessage(func: TOmniTaskInvokeFunction): TOmniMessage; overload;
      inline;
    class procedure UnpackMessage(const msg: TOmniMessage;
      var func: TOmniTaskControlInvokeFunction; var funcEx: TOmniTaskControlInvokeFunctionEx;
      var taskFunc: TOmniTaskInvokeFunction); overload;
    class function  UnpackMessage(const msg: TOmniMessage; var func: TOmniTaskInvokeFunction):
      boolean; overload;
    constructor Create(func: TOmniTaskControlInvokeFunction); overload;
    constructor Create(func: TOmniTaskControlInvokeFunctionEx); overload;
    constructor Create(func: TOmniTaskInvokeFunction); overload;
    property Func: TOmniTaskControlInvokeFunction read ifmFunc;
    property FuncEx: TOmniTaskControlInvokeFunctionEx read ifmFuncEx;
    property TaskFunc: TOmniTaskInvokeFunction read ifmTaskFunc;
  end; { TOmniInternalFuncMsg }
  {$ENDIF OTL_Anonymous}

  TOmniInvokeType = (itUnknown, itSelf, itSelfAndOmniValue, itSelfAndObject);
  TOmniInvokeSignature_Self           = procedure(Self: TObject);
  TOmniInvokeSignature_Self_OmniValue = procedure(Self: TObject; var value: TOmniValue);
  TOmniInvokeSignature_Self_Object    = procedure(Self: TObject; var obj: TObject);

  TOmniInvokeInfo = class
  strict private
    oiiAddress  : pointer;
    oiiSignature: TOmniInvokeType;
  public
    constructor Create(methodAddr: pointer; methodSignature: TOmniInvokeType);
    property Address: pointer read oiiAddress;
    property Signature: TOmniInvokeType read oiiSignature;
  end; { TOmniInvokeInfo }

  TOmniTaskControlOption = (tcoAlertableWait, tcoMessageWait, tcoForceExecution);
  TOmniTaskControlOptions = set of TOmniTaskControlOption;
  TOmniExecutorType = (etNone, etMethod, etProcedure, etWorker, etFunction);

  TOmniTaskTimerInfo = class
  strict private
    ottiInterval_ms: cardinal;
    ottiMessageID  : TOmniMessageID;
    ottiTimerID    : integer;
  public
    constructor Create(timerID: integer; interval_ms: cardinal; messageID: TOmniMessageID);
    property Interval_ms: cardinal read ottiInterval_ms write ottiInterval_ms;
    property MessageID: TOmniMessageID read ottiMessageID write ottiMessageID;
    property TimerID: integer read ottiTimerID write ottiTimerID;
  end; { TOmniTaskTimerInfo }

  TOmniTaskControl = class;

  TOmniTaskExecutor = class
  strict private type
    TOmniMessageInfo = record
      IdxFirstMessage   : integer;
      IdxFirstTerminate : integer;
      IdxFirstWaitObject: integer;
      IdxLastMessage    : integer;
      IdxLastTerminate  : integer;
      IdxLastWaitObject : integer;
      IdxRebuildHandles : integer;
      NewMessageEvent   : TOmniTransitionEvent;
      NumWaitHandles    : integer;
      WaitHandles       : {$IFDEF MSWINDOWS}array of THandle{$ELSE}TOmniSynchroArray{$ENDIF};
      Waiter            : TWaitFor;
      {$IFDEF MSWINDOWS}
      WaitFlags         : DWORD;
      WaitWakeMask      : DWORD;
      {$ELSE}
      WaitWakeAll       : boolean;
      {$ENDIF ~MSWINDOWS}
      function  AsString: string;
      {$IFNDEF MSWINDOWS}
      function  GetSynchroIndex(WaitResult: TWaitResult; const Handle: IOmniSynchro): integer;
      {$ENDIF ~MSWINDOWS}
    end;
  strict private // those must be 4-aligned, keep them on the top
    oteInternalLock      : TOmniCS;
    oteOptionsLock       : TOmniCS;
    oteTimerLock         : TOmniCS;
  strict private
    oteCommList          : TInterfaceList;
    oteCommNewMsgList    : {$IFDEF MSWINDOWS}TGpInt64List{$ELSE}TList<IOmniEvent>{$ENDIF};
    oteCommRebuildHandles: TOmniTransitionEvent;
    oteException         : Exception;
    oteExecutorType      : TOmniExecutorType;
    oteExitCode          : TOmniAlignedInt32;
    oteExitMessage       : string;
    {$IFDEF OTL_Anonymous}
    oteFunc              : TOmniTaskDelegate;
    {$ENDIF OTL_Anonymous}
    oteMethod            : TOmniTaskMethod;
    oteMethodHash        : TGpStringObjectHash;
    oteMsgInfo           : TOmniMessageInfo;
    oteOptions           : TOmniTaskControlOptions;
    oteOwner_ref         : TOmniTaskControl;
    otePriority          : TOTLThreadPriority;
    oteProc              : TOmniTaskProcedure;
    oteTerminateHandles  : {$IFDEF MSWINDOWS}TGpInt64List{$ELSE}TOmniSynchroArray{$ENDIF};
    oteLastTimeGetTime64 : int64;
    oteTerminating       : boolean;
    oteTimeGetTime64Base : int64;
    oteTimers            : TGpInt64ObjectList;
    {$IFDEF MSWINDOWS}
    oteWakeMask          : DWORD;
    {$ELSE}
    oteWakeAll           : boolean;
    {$ENDIF ~MSWINDOWS}
    oteWaitHandlesGen    : int64;
    oteWaitObjectList    : TOmniWaitObjectList;
    oteWorkerInitialized : TOmniTransitionEvent;
    oteWorkerInitOK      : boolean;
    oteWorkerIntf        : IOmniWorker;
  strict protected
    procedure CallOmniTimer;
    procedure CheckTimers;
    procedure Cleanup;
    procedure DispatchCommMessage(newMsgHandle: TOmniTransitionEvent;
      const task: IOmniTask; var msgInfo: TOmniMessageInfo);
    procedure DispatchMessages(const task: IOmniTask);
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string;
    function  GetImplementor: TObject;
    procedure GetMethodAddrAndSignature(const methodName: string;
      var methodAddress: pointer; var methodSignature: TOmniInvokeType);
    procedure GetMethodNameFromInternalMessage(const msg: TOmniMessage; var msgName: string;
      var msgData: TOmniValue {$IFDEF OTL_Anonymous}; var func:
      TOmniTaskControlInvokeFunction; var funcEx: TOmniTaskControlInvokeFunctionEx; var proc:
      TProc<integer>; var taskFunc: TOmniTaskInvokeFunction {$ENDIF OTL_Anonymous});
    function  GetOptions: TOmniTaskControlOptions;
    function  HaveElapsedTimer: boolean;
    procedure Initialize;
    procedure InsertTimer(wakeUpTime_ms: int64; timerInfo: TOmniTaskTimerInfo);
    function  LocateTimer(timerID: integer): integer;
    {$IFDEF MSWINDOWS}
    procedure ProcessThreadMessages;
    {$ENDIF MSWINDOWS}
    procedure RaiseInvalidSignature(const methodName: string);
    procedure RemoveTerminationEvents(const srcMsgInfo: TOmniMessageInfo; var dstMsgInfo:
      TOmniMessageInfo);
    procedure ReportInvalidHandle(msgInfo: TOmniMessageInfo);
    procedure SetOptions(const value: TOmniTaskControlOptions);
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TOmniMessageID);
    function  TestForInternalRebuild(const task: IOmniTask;
      var msgInfo: TOmniMessageInfo): boolean;
    function  TimeGetTime64: int64;
  protected
    function  DispatchEvent(awaited: TWaitFor.TWaitForResult; const task: IOmniTask;
      var msgInfo: TOmniMessageInfo{$IFNDEF MSWINDOWS}; SignalEvent: IOmnIEvent{$ENDIF}): boolean; virtual;
    procedure DispatchOmniMessage(msg: TOmniMessage; doCheckTimers: boolean); virtual;
    procedure MainMessageLoop(const task: IOmniTask; var msgInfo: TOmniMessageInfo); virtual;
    procedure MessageLoopPayload; virtual;
    procedure ProcessMessages(task: IOmniTask); virtual;
    procedure RebuildWaitHandles(const task: IOmniTask; var msgInfo: TOmniMessageInfo); virtual;
    function  TimeUntilNextTimer_ms: cardinal; virtual;
    function  WaitForEvent(const msgInfo: TOmniMessageInfo; timeout_ms: cardinal
      {$IFNDEF MSWINDOWS}; var SignalEvent: IOmniEvent{$ENDIF}): TWaitFor.TWaitForResult; virtual;
  public
    constructor Create(owner_ref: TOmniTaskControl; const workerIntf: IOmniWorker); overload;
    constructor Create(owner_ref: TOmniTaskControl; method: TOmniTaskMethod); overload;
    constructor Create(owner_ref: TOmniTaskControl; proc: TOmniTaskProcedure); overload;
    {$IFDEF OTL_Anonymous}
    constructor Create(owner_ref: TOmniTaskControl; func: TOmniTaskDelegate); overload;
    {$ENDIF OTL_Anonymous}
    destructor  Destroy; override;
    procedure Asy_Execute(const task: IOmniTask);
    procedure Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure Asy_RegisterWaitObject(waitObject: TOmniTransitionEvent; responseHandler: TOmniWaitObjectMethod);
    procedure Asy_SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetProcessorGroup(procGroupNumber: integer);
    procedure SetNUMANode(numaNodeNumber: integer);
    procedure Asy_SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage:
      TOmniMessageID); overload;
    procedure Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure Asy_UnregisterWaitObject(waitObject: TOmniTransitionEvent);
    procedure EmptyMessageQueues(const task: IOmniTask);
    procedure TerminateWhen(handle: TOmniTransitionEvent); overload;
    {$IFDEF OTL_NUMASupport}
    class function VerifyNUMANode(numaNodeNumber: integer): IOmniNUMANode;
    class procedure VerifyProcessorGroup(procGroupNumber: integer);
    {$ENDIF OTL_NUMASupport}
    function  WaitForInit: boolean;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Implementor: TObject read GetImplementor;
    property Options: TOmniTaskControlOptions read GetOptions write SetOptions;
    property Priority: TOTLThreadPriority read otePriority write otePriority;
    property TaskException: Exception read oteException write oteException;
    property Terminating: boolean read oteTerminating write oteTerminating;
    {$IFDEF MSWINDOWS}
    property WakeMask: DWORD read oteWakeMask write oteWakeMask;
    {$ELSE}
    property WakeAll: boolean read oteWakeAll write oteWakeAll;
    {$ENDIF ~MSWINDOWS}
    property WorkerInitialized: TOmniTransitionEvent read oteWorkerInitialized;
    property WorkerInitOK: boolean read oteWorkerInitOK;
    property WorkerIntf: IOmniWorker read oteWorkerIntf;
  end; { TOmniTaskExecutor }

  TOmniTask = class(TInterfacedObject, IOmniTask, IOmniTaskExecutor)
  strict private
    otCleanupLock             : TOmniMREW;
    otExecuting               : boolean;
    otExecutor_ref            : TOmniTaskExecutor;
    otParameters_ref          : TOmniValueContainer;
    otSharedInfo_ref          : TOmniSharedTaskInfo;
    otTerminateWillCallExecute: boolean;
    otThreadData              : IInterface;
    otThreadID                : TThreadID;
  protected
    function  GetCancellationToken: IOmniCancellationToken; inline;
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetCounter: IOmniCounter;
    function  GetImplementor: TObject;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetParam: TOmniValueContainer; inline;
    function  GetTerminateEvent: TOmniTransitionEvent; inline;
    function  GetThreadData: IInterface; inline;
    function  GetUniqueID: int64; inline;
    procedure InternalExecute(calledFromTerminate: boolean);
    procedure SetThreadData(const value: IInterface); inline;
    procedure Terminate;
  public
    constructor Create(executor: TOmniTaskExecutor; parameters: TOmniValueContainer;
      sharedInfo: TOmniSharedTaskInfo);
    procedure ClearTimer(timerID: integer = 0);
    procedure Enforced(forceExecution: boolean = true);
    procedure Execute;
    {$IFDEF OTL_Anonymous}
    procedure Invoke(remoteFunc: TOmniTaskInvokeFunction);
    procedure InvokeOnSelf(remoteFunc: TOmniTaskInvokeFunction);
    {$ENDIF OTL_Anonymous}
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure RegisterWaitObject(waitObject: TOmniTransitionEvent; responseHandler: TOmniWaitObjectMethod); overload;
    procedure SetException(exceptionObject: pointer);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetProcessorGroup(procGroupNumber: integer);
    procedure SetNUMANode(numaNodeNumber: integer);
    procedure SetTimer(interval_ms: cardinal); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMessage: TOmniMessageID); overload;
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TOmniMessageID); overload;
    {$IFDEF OTL_Anonymous}
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc); overload;
    procedure SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc<integer>); overload;
    {$ENDIF OTL_Anonymous}
    function  Stopped: boolean;
    procedure StopTimer;
    function  Terminated: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure UnregisterWaitObject(waitObject: TOmniTransitionEvent);
    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Implementor: TObject read GetImplementor;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param: TOmniValueContainer read GetParam;
    property SharedInfo: TOmniSharedTaskInfo read otSharedInfo_ref;
    property TerminateEvent: TOmniTransitionEvent read GetTerminateEvent;
    property ThreadData: IInterface read GetThreadData;
    property UniqueID: int64 read GetUniqueID;
  end; { TOmniTask }

  TOmniThread = class(TThread) // Factor this class into OtlThread unit?
  strict private
    otTask: IOmniTask;
    {$IFNDEF MSWINDOWS}
    otThreadTerminationEvent: IOmniEvent;
    {$ENDIF}
  protected
    procedure Execute; override;
    {$IFNDEF MSWINDOWS}
    procedure DoTerminate; override;
    {$ENDIF}
  public
    constructor Create(task: IOmniTask);
    property Task: IOmniTask read otTask;
    {$IFNDEF MSWINDOWS}
    property ThreadTerminationEvent: IOmniEvent read otThreadTerminationEvent;
    {$ENDIF}
  end; { TOmniThread }

  TOmniTaskControlInternalDebugFlag = (dfLogDispatch);
  TOmniTaskControlInternalDebugFlags = set of TOmniTaskControlInternalDebugFlag;

  IOmniTaskControlInternals = interface ['{CE7B53E0-902E-413F-AB6E-B97E7F4B0AD5}']
    function  GetDebugFlags: TOmniTaskControlInternalDebugFlags;
    function  GetTerminatedEvent: TOmniTransitionEvent;
    procedure SetDebugFlags(const value: TOmniTaskControlInternalDebugFlags);
  //
    function  FilterMessage(const msg: TOmniMessage): boolean;
    procedure ForwardTaskMessage(const msg: TOmniMessage);
    procedure ForwardTaskTerminated;
    property DebugFlags: TOmniTaskControlInternalDebugFlags read GetDebugFlags
      write SetDebugFlags;
    property TerminatedEvent: TOmniTransitionEvent read GetTerminatedEvent;
  end; { IOmniTaskControlInternals }

  TOmniTaskControl = class(TInterfacedObject, IOmniTaskControl, IOmniTaskControlSharedInfo, IOmniTaskControlInternals)
  {$IFDEF OTL_Anonymous}
  strict private
    otcOnTerminatedSimple  : TOmniOnTerminatedFunctionSimple;
  {$ENDIF OTL_Anonymous}
  strict private
    otcDebugFlags          : TOmniTaskControlInternalDebugFlags;
    otcDelayedTerminate    : boolean;
    otcDestroyLock         : boolean;
    otcEventMonitor        : TObject{TOmniEventMonitor};
    otcEventMonitorInternal: boolean;
    otcExecutor            : TOmniTaskExecutor;
    otcInEventHandler      : boolean;
    otcOnMessageExec       : TOmniMessageExec;
    otcOnMessageList       : TGpIntegerObjectList;
    otcOnTerminatedExec    : TOmniMessageExec;
    otcOwningPool          : IOmniThreadPool;
    otcParameters          : TOmniValueContainer;
    otcQueueLength         : integer;
    otcSharedInfo          : TOmniSharedTaskInfo;
    otcTerminateTokens     : TInterfaceList;
    otcThread              : TOmniThread;
    otcUserData            : TOmniValueContainer;
    {$IFNDEF MSWINDOWS}
    FMultiWaitLock         : IOmniCriticalSection;
    {$ENDIF ~MSWINDOWS}
  strict protected
    procedure CreateInternalMonitor;
    function  CreateTask: IOmniTask;
    procedure DestroyMonitor;
    procedure EnsureCommChannel; inline;
    procedure Initialize(const taskName: string);
  protected
    function  FilterMessage(const msg: TOmniMessage): boolean;
    procedure ForwardTaskMessage(const msg: TOmniMessage);
    procedure ForwardTaskTerminated;
    function  GetCancellationToken: IOmniCancellationToken;
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetDebugFlags: TOmniTaskControlInternalDebugFlags;
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string; inline;
    function  GetFatalException: Exception;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetOptions: TOmniTaskControlOptions;
    function  GetParam: TOmniValueContainer;
    function  GetSharedInfo: TOmniSharedTaskInfo;
    function  GetTerminatedEvent: TOmniTransitionEvent;
    function  GetTerminateEvent: TOmniTransitionEvent;
    function  GetUniqueID: int64; inline;
    function  GetUserDataVal(const idxData: TOmniValue): TOmniValue;
    procedure SetDebugFlags(const value: TOmniTaskControlInternalDebugFlags);
    procedure SetOptions(const value: TOmniTaskControlOptions);
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
    procedure SetUserDataVal(const idxData: TOmniValue; const value: TOmniValue);
  public
    {$IFDEF OTL_Anonymous}
    constructor Create(worker: TOmniTaskDelegate; const taskName: string); overload;
    function  OnMessage(eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskControl; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskControl; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
  public //not really for public consumption
    property DebugFlags: TOmniTaskControlInternalDebugFlags read GetDebugFlags write
      SetDebugFlags;
  public
    constructor Create(const worker: IOmniWorker; const taskName: string); overload;
    constructor Create(worker: TOmniTaskMethod; const taskName: string); overload;
    constructor Create(worker: TOmniTaskProcedure; const taskName: string); overload;
    destructor  Destroy; override;
    function  Alertable: IOmniTaskControl;
    function  CancelWith(const token: IOmniCancellationToken): IOmniTaskControl;
    function  ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean = false): IOmniTaskControl;
    function  ClearTimer(timerID: integer = 0): IOmniTaskControl;
    function  DetachException: Exception;
    function  Enforced(forceExecution: boolean = true): IOmniTaskControl;
    function  Invoke(const msgMethod: pointer): IOmniTaskControl; overload; inline;
    function  Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload; inline;
    function  Invoke(const msgName: string): IOmniTaskControl; overload; inline;
    function  Invoke(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload; inline;
    {$IFDEF OTL_Anonymous}
    function  Invoke(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl; overload;
    function  Invoke(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait({$IFDEF MSWINDOWS}wakeMask: DWORD = QS_ALLEVENTS{$ELSE}wakeAll: boolean = true{$ENDIF}): IOmniTaskControl;
    function  NUMANode(numaNodeNumber: integer): IOmniTaskControl;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskControl; overload;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskControl; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniMessageExec): IOmniTaskControl; overload;
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskControl; overload;
    function  ProcessorGroup(procGroupNumber: integer): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer): IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Run(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload;
    function  Run(const msgName: string): IOmniTaskControl; overload;
    function  Run(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Run(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function  Run(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl; overload;
    function  Run(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  Schedule(const threadPool: IOmniThreadPool = nil {default pool}):
      IOmniTaskControl;
    {$IFDEF MSWINDOWS}
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    {$ENDIF MSWINDOWS}
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
    function  SetQueueSize(numMessages: integer): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal): IOmniTaskControl; overload;
    function  SetTimer(interval_ms: cardinal; const timerMessage: TOmniMessageID):
      IOmniTaskControl; overload;
    function  SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage:
      TOmniMessageID): IOmniTaskControl; overload;
    {$IFDEF OTL_Anonymous}
    function SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc): IOmniTaskControl; overload;
    function SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc<integer>): IOmniTaskControl; overload;
    {$ENDIF OTL_Anonymous}
    function  SetUserData(const idxData: TOmniValue; const value: TOmniValue): IOmniTaskControl;
    procedure Stop;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: TOmniTransitionEvent): IOmniTaskControl; overload;
    function  TerminateWhen(token: IOmniCancellationToken): IOmniTaskControl; overload;
    function  Unobserved: IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskControl;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl; overload;
    function  WithLock(const lock: IOmniCriticalSection): IOmniTaskControl; overload; inline;

    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property FatalException: Exception read GetFatalException;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Options: TOmniTaskControlOptions read GetOptions write SetOptions;
    property Param: TOmniValueContainer read GetParam;
    property SharedInfo: TOmniSharedTaskInfo read otcSharedInfo;
    property UniqueID: int64 read GetUniqueID;
    property UserData[const idxData: TOmniValue]: TOmniValue read GetUserDataVal write SetUserDataVal;
  end; { TOmniTaskControl }

  TOmniTaskControlList = class;

  TOmniTaskControlListEnumerator = class(TInterfacedObject, IOmniTaskControlListEnumerator)
  strict private
    otcleTaskEnum: TInterfaceListEnumerator;
  protected
    function GetCurrent: IOmniTaskControl;
    function MoveNext: boolean;
  public
    constructor Create(taskList: TInterfaceList);
    destructor Destroy; override;
  end; { TOmniTaskControlListEnumerator }

  TOmniTaskControlList = class(TInterfacedObject, IOmniTaskControlList)
  strict private
    otclList: TInterfaceList;
  protected
    function  Get(idxItem: integer): IOmniTaskControl;
    function  GetCapacity: integer;
    function  GetCount: integer;
    procedure Put(idxItem: integer; const value: IOmniTaskControl);
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(const item: IOmniTaskControl): integer;
    procedure Clear;
    procedure Delete(idxItem: integer);
    procedure Exchange(idxItem1, idxItem2: integer);
    function  First: IOmniTaskControl;
    function  GetEnumerator: IOmniTaskControlListEnumerator;
    function  IndexOf(const item: IOmniTaskControl): integer;
    function  IndexOfID(uniqueID: int64): integer;
    procedure Insert(idxItem: integer; const item: IOmniTaskControl);
    function  Last: IOmniTaskControl;
    function  Remove(const item: IOmniTaskControl): integer;
    function  RemoveByID(uniqueID: int64): integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idxItem: integer]: IOmniTaskControl read Get write Put; default;
  end; { TOmniTaskControlList }

  TOmniTaskGroup = class(TInterfacedObject, IOmniTaskGroup)
  strict private
    otgRegisteredWith: IOmniTask;
    otgTaskList      : IOmniTaskControlList;
    {$IFNDEF MSWINDOWS}
    FMultiWaitLock   : IOmniCriticalSection;
    {$ENDIF ~MSWINDOWS}
  strict protected
    procedure AutoUnregisterComms;
    procedure InternalUnregisterAllCommFrom(const task: IOmniTask);
  protected
    function  GetTasks: IOmniTaskControlList; inline;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  GetEnumerator: IOmniTaskControlListEnumerator;
    function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    procedure SendToAll(const msg: TOmniMessage);
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
    property Tasks: IOmniTaskControlList read GetTasks;
  end; { TOmniTaskGroup }

implementation

uses
  {$IFDEF OTL_Generics}
  Generics.Collections,
  {$ENDIF OTL_Generics}
  ObjAuto,
  OtlHooks,
  {$IFDEF MSWINDOWS}
  MMSystem,
  {$ELSE}
  Rtti,
  Diagnostics,
  {$ENDIF ~MSWINDOWS}
  OtlCommon.Utils,
  OtlEventMonitor;

type
  TOmniTaskControlEventMonitor = class(TOmniEventMonitor)
  strict protected
    procedure ForwardTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure ForwardTaskTerminated(const task: IOmniTaskControl);
  public
    constructor Create(AOwner: TComponent); override; 
  end; { TOmniTaskControlEventMonitor }

  TOmniTaskControlEventMonitorPool = class
  strict private
    monitorPool: TOmniEventMonitorPool;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Allocate: TOmniTaskControlEventMonitor;
    procedure Release(monitor: TOmniTaskControlEventMonitor);
  end; { TOmniTaskControlEventMonitorPool }

var
  GTaskControlEventMonitorPool: TOmniTaskControlEventMonitorPool;

{ exports }

{$IFDEF OTL_Anonymous}
function CreateTask(worker: TOmniTaskDelegate; const taskName: string = ''): IOmniTaskControl;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }
{$ENDIF OTL_Anonymous}

function CreateTask(worker: TOmniTaskProcedure; const taskName: string): IOmniTaskControl;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTask(worker: TOmniTaskMethod; const taskName: string):
  IOmniTaskControl;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTask(const worker: IOmniWorker; const taskName: string): IOmniTaskControl;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTaskGroup: IOmniTaskGroup;
begin
  Result := TOmniTaskGroup.Create;
end; { CreateTaskGroup }

function CreateTaskControlList: IOmniTaskControlList;
begin
  Result := TOmniTaskControlList.Create;
end; { CreateTaskControlList }

{ TOmniInternalMessage }

constructor TOmniInternalMessage.Create(internalMessageType: TOmniInternalMessageType);
begin
  imInternalMessageType := internalMessageType;
end; { TOmniInternalMessage.Create }

class function TOmniInternalMessage.InternalType(
  const msg: TOmniMessage): TOmniInternalMessageType;
begin
  Assert(msg.MsgID = COtlReservedMsgID);
  Result := TOmniInternalMessage(msg.MsgData.AsObject).InternalMessageType;
end; { TOmniInternalMessage.InternalType }

{ TOmniInternalStringMsg }

constructor TOmniInternalStringMsg.Create(const msgName: string;
  const msgData: TOmniValue);
begin
  inherited Create(imtStringMsg);
  ismMsgName := msgName;
  ismMsgData := msgData;
end; { TOmniInternalStringMsg.Create }

class function TOmniInternalStringMsg.CreateMessage(const msgName: string; msgData:
  TOmniValue): TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalStringMsg.Create(msgName, msgData));
end; { TOmniInternalStringMsg.CreateMessage }

class procedure TOmniInternalStringMsg.UnpackMessage(const msg: TOmniMessage; var
  msgName: string; var msgData: TOmniValue);
var
  stringMsg: TOmniInternalStringMsg;
begin
  stringMsg := TOmniInternalStringMsg(msg.MsgData.AsObject);
  msgName := stringMsg.MsgName;
  msgData := stringMsg.MsgData;
  FreeAndNil(stringMsg);
end; { TOmniInternalStringMsg.UnpackMessage }

{ TOmniInternalAddressMsg }

constructor TOmniInternalAddressMsg.Create(const msgMethod: pointer; const msgData:
  TOmniValue);
begin
  inherited Create(imtAddressMsg);
  ismMsgMethod := msgMethod;
  ismMsgData := msgData;
end; { TOmniInternalAddressMsg.Create }

class function TOmniInternalAddressMsg.CreateMessage(const msgMethod: pointer; msgData:
  TOmniValue): TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalAddressMsg.Create(msgMethod, msgData));
end; { TOmniInternalAddressMsg.CreateMessage }

class procedure TOmniInternalAddressMsg.UnpackMessage(const msg: TOmniMessage; var
  msgMethod: pointer; var msgData: TOmniValue);
var
  addr: TOmniInternalAddressMsg;
begin
  addr := TOmniInternalAddressMsg(msg.MsgData.AsObject);
  msgMethod := addr.MsgMethod;
  msgData := addr.MsgData;
  FreeAndNil(addr);
end; { TOmniInternalAddressMsg.UnpackMessage }

{$IFDEF OTL_Anonymous}

{ TOmniInternalAnonMsg }

constructor TOmniInternalAnonMsg.Create(const msgAnon: TProc<integer>; const msgData:
  TOmniValue);
begin
  inherited Create(imtAnonMsg);
  ismMsgProc := msgAnon;
  ismMsgData := msgData;
end; { TOmniInternalAnonMsg.Create }

class function TOmniInternalAnonMsg.CreateMessage(const msgAnon: TProc<integer>; msgData:
  TOmniValue): TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalAnonMsg.Create(msgAnon, msgData));
end; { TOmniInternalAnonMsg.CreateMessage }

class procedure TOmniInternalAnonMsg.UnpackMessage(const msg: TOmniMessage; var msgAnon:
  TProc<integer>; var msgData: TOmniValue);
var
  addr: TOmniInternalAnonMsg;
begin
  addr := TOmniInternalAnonMsg(msg.MsgData.AsObject);
  msgAnon := addr.MsgProc;
  msgData := addr.MsgData;
  FreeAndNil(addr);
end; { TOmniInternalAnonMsg.UnpackMessage }

{ TOmniInternalFuncMsg }

constructor TOmniInternalFuncMsg.Create(func: TOmniTaskControlInvokeFunction);
begin
  inherited Create(imtFuncMsg);
  ifmFunc := func;
end; { TOmniInternalFuncMsg.Create }

constructor TOmniInternalFuncMsg.Create(func: TOmniTaskControlInvokeFunctionEx);
begin
  inherited Create(imtFuncMsg);
  ifmFuncEx := func;
end; { TOmniInternalFuncMsg.Create }

constructor TOmniInternalFuncMsg.Create(func: TOmniTaskInvokeFunction);
begin
  inherited Create(imtFuncMsg);
  ifmTaskFunc := func;
end; { TOmniInternalFuncMsg.Create }

class function TOmniInternalFuncMsg.CreateMessage(func: TOmniTaskControlInvokeFunction):
  TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalFuncMsg.Create(func));
end; { TOmniInternalFuncMsg.CreateMessage }

class function TOmniInternalFuncMsg.CreateMessage(func: TOmniTaskControlInvokeFunctionEx):
  TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalFuncMsg.Create(func));
end; { TOmniInternalFuncMsg.CreateMessage }

class function TOmniInternalFuncMsg.CreateMessage(func: TOmniTaskInvokeFunction):
  TOmniMessage;
begin
  Result := TOmniMessage.Create(COtlReservedMsgID,
    TOmniInternalFuncMsg.Create(func));
end; { TOmniInternalFuncMsg.CreateMessage }

class procedure TOmniInternalFuncMsg.UnpackMessage(const msg: TOmniMessage;
  var func: TOmniTaskControlInvokeFunction; var funcEx: TOmniTaskControlInvokeFunctionEx;
  var taskFunc: TOmniTaskInvokeFunction);
var
  funcMsg: TOmniInternalFuncMsg;
begin
  funcMsg := TOmniInternalFuncMsg(msg.MsgData.AsObject);
  func := funcMsg.Func;
  funcEx := funcMsg.FuncEx;
  taskFunc := funcMsg.TaskFunc;
  FreeAndNil(funcMsg);
end; { TOmniInternalFuncMsg.UnpackMessage }

class function TOmniInternalFuncMsg.UnpackMessage(const msg: TOmniMessage; var func:
  TOmniTaskInvokeFunction): boolean;
var
  funcMsg: TOmniInternalFuncMsg;
begin
  Result := assigned(msg.MsgData.AsObject) and (msg.MsgData.AsObject is TOmniInternalFuncMsg);
  if Result then begin
    funcMsg := TOmniInternalFuncMsg(msg.MsgData.AsObject);
    func := funcMsg.TaskFunc;
    FreeAndNil(funcMsg);
    msg.MsgData.AsObject := nil;
  end;
end; { TOmniInternalFuncMsg.UnpackMessage }

{$ENDIF OTL_Anonymous}

{ TOmniTask }

constructor TOmniTask.Create(executor: TOmniTaskExecutor;
  parameters: TOmniValueContainer; sharedInfo: TOmniSharedTaskInfo);
begin
  inherited Create;
  otExecutor_ref := executor;
  otParameters_ref := parameters;
  otSharedInfo_ref := sharedInfo;
end; { TOmniTask.Create }

procedure TOmniTask.ClearTimer(timerID: integer);
begin
  SetTimer(timerID, 0, 0);
end; { TOmniTask.ClearTimer }

procedure TOmniTask.Enforced(forceExecution: boolean = true);
begin
  if forceExecution then
    otExecutor_ref.Options := otExecutor_ref.Options + [tcoForceExecution]
  else
    otExecutor_ref.Options := otExecutor_ref.Options - [tcoForceExecution];
end; { TOmniTask.Enforced }

procedure TOmniTask.Execute;
begin
  InternalExecute(false);
end; { TOmniTask.Execute }

function TOmniTask.GetCancellationToken: IOmniCancellationToken;
begin
  Result := otSharedInfo_ref.CancellationToken;
end; { TOmniTask.GetCancellationToken }

function TOmniTask.GetComm: IOmniCommunicationEndpoint;
begin
  Result := otSharedInfo_ref.CommChannel.Endpoint2;
end; { TOmniTask.GetComm }

function TOmniTask.GetCounter: IOmniCounter;
begin
  Result := otSharedInfo_ref.Counter;
end; { TOmniTask.GetCounter }

function TOmniTask.GetImplementor: TObject;
begin
  Result := otExecutor_ref.Implementor;
end; { TOmniTask.GetImplementor }

function TOmniTask.GetLock: TSynchroObject;
begin
  Result := otSharedInfo_ref.Lock;
end; { GetLock: TSynchroObject }

function TOmniTask.GetName: string;
begin
  if assigned(otSharedInfo_ref) then
    Result := otSharedInfo_ref.TaskName
  else
    Result := '';
end; { TOmniTask.GetName }

function TOmniTask.GetParam: TOmniValueContainer;
begin
  Result := otParameters_ref;
end; { TOmniTask.GetParam }

function TOmniTask.GetTerminateEvent: TOmniTransitionEvent;
begin
  Result := otSharedInfo_ref.TerminateEvent;
end; { TOmniTask.GetTerminateEvent }

function TOmniTask.GetThreadData: IInterface;
begin
  Result := otThreadData;
end; { TOmniTask.GetThreadData }

function TOmniTask.GetUniqueID: int64;
begin
  Result := otSharedInfo_ref.UniqueID;
end; { TOmniTask.GetUniqueID }

procedure TOmniTask.InternalExecute(calledFromTerminate: boolean);
var
  chainTo       : IOmniTaskControl;
  eventTerminate: TOmniTransitionEvent;
  sync          : TSynchroObject;
  taskException : Exception;
begin
  otCleanupLock.EnterWriteLock;
  try
    if otTerminateWillCallExecute and (not calledFromTerminate) then
      Exit;
    otExecuting := true;
  finally otCleanupLock.ExitWriteLock; end;
  otThreadID := GetThreadID;
  chainTo := nil;
  eventTerminate := {$IFDEF MSWINDOWS}0{$ELSE}nil{$ENDIF};
  try
    try
      try
        SetThreadName(otSharedInfo_ref.TaskName);
        if (tcoForceExecution in otExecutor_ref.Options) or (not Terminated) then
        try
          if otSharedInfo_ref.ProcessorGroup >= 0 then
            otExecutor_ref.SetProcessorGroup(otSharedInfo_ref.ProcessorGroup);
          if otSharedInfo_ref.NUMANode >= 0 then
            otExecutor_ref.SetNUMANode(otSharedInfo_ref.NUMANode);
          otExecutor_ref.Asy_Execute(Self);
        except
          on E: Exception do begin
            taskException := Exception(AcquireExceptionObject);
            FilterException(taskException);
            if assigned(taskException) then
              SetException(taskException);
          end;
        end;
      finally
        otCleanupLock.EnterWriteLock;
        if assigned(otSharedInfo_ref.ChainTo) and
           (otSharedInfo_ref.ChainIgnoreErrors or (otExecutor_ref.ExitCode = EXIT_OK))
        then
          chainTo := otSharedInfo_ref.ChainTo;
        otSharedInfo_ref.ChainTo := nil;
        eventTerminate := otSharedInfo_ref.TerminatedEvent;
        otSharedInfo_ref.Stopped := true;
        // with internal monitoring this will not be processed if the task controller owner is also shutting down
        sync := nil; // to remove the warning in the 'finally' clause below
        otSharedInfo_ref.MonitorLock.Acquire;
        try
          sync := otSharedInfo_ref.MonitorLock.SyncObj;
          {$IFDEF MSWINDOWS}
          if assigned(otSharedInfo_ref.Monitor) then
            otSharedInfo_ref.Monitor.Send(COmniTaskMsg_Terminated,
              integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi));
          {$ENDIF MSWINDOWS}
          otSharedInfo_ref := nil;
        finally sync.Release; end;
      end;
    finally
      //Task controller could die any time now. Make sure we're not using shared
      //structures anymore.
      otExecutor_ref   := nil;
      otParameters_ref := nil;
      otSharedInfo_ref := nil;
      SetEvent(eventTerminate);
    end;
    if assigned(chainTo) then
      chainTo.Run; // TODO 1 -oPrimoz Gabrijelcic : Should InternalExecute the chained task in the same thread (should work when run in a pool)
  finally otCleanupLock.ExitWriteLock; end;
end; { TOmniTask.InternalExecute }

{$IFDEF OTL_Anonymous}
procedure TOmniTask.Invoke(remoteFunc: TOmniTaskInvokeFunction);
begin
  otSharedInfo_ref.CommChannel.Endpoint2.Send(TOmniInternalFuncMsg.CreateMessage(remoteFunc));
end; { TOmniTask.Invoke }

procedure TOmniTask.InvokeOnSelf(remoteFunc: TOmniTaskInvokeFunction);
begin
  otSharedInfo_ref.CommChannel.Endpoint1.Send(TOmniInternalFuncMsg.CreateMessage(remoteFunc));
end; { TOmniTask.InvokeOnSelf }
{$ENDIF OTL_Anonymous}

procedure TOmniTask.RegisterComm(const comm: IOmniCommunicationEndpoint);
begin
  otExecutor_ref.Asy_RegisterComm(comm);
end; { TOmniTask.RegisterComm }

procedure TOmniTask.RegisterWaitObject(waitObject: TOmniTransitionEvent; responseHandler: TOmniWaitObjectMethod);
begin
  otExecutor_ref.Asy_RegisterWaitObject(waitObject, responseHandler);
end; { TOmniTask.RegisterWaitObject }

procedure TOmniTask.SetException(exceptionObject: pointer);
begin
  Exception(otExecutor_ref.TaskException).Free;
  otExecutor_ref.TaskException := exceptionObject;
end; { TOmniTask.SetException }

procedure TOmniTask.SetExitStatus(exitCode: integer; const exitMessage: string);
begin
  otExecutor_ref.Asy_SetExitStatus(exitCode, exitMessage);
end; { TOmniTask.SetExitStatus }

procedure TOmniTask.SetNUMANode(numaNodeNumber: integer);
begin
  otExecutor_ref.SetNUMANode(numaNodeNumber);
end; { TOmniTask.SetNUMANode }

procedure TOmniTask.SetProcessorGroup(procGroupNumber: integer);
begin
  otExecutor_ref.SetProcessorGroup(procGroupNumber);
end; { TOmniTask.SetProcessorGroup }

procedure TOmniTask.SetThreadData(const value: IInterface);
begin
  otThreadData := value;
end; { TOmniTask.SetThreadData }

procedure TOmniTask.SetTimer(interval_ms: cardinal);
begin
  SetTimer(interval_ms, -1);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(interval_ms: cardinal; const timerMessage: TOmniMessageID);
begin
  SetTimer(0, interval_ms, timerMessage);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TOmniMessageID);
begin
  otExecutor_ref.Asy_SetTimer(timerID, interval_ms, timerMessage);
end; { TOmniTask.SetTimer }

{$IFDEF OTL_Anonymous}
procedure TOmniTask.SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc);
begin
  otExecutor_ref.Asy_SetTimer(timerID, interval_ms, TOmniMessageID.Create(
    procedure (timerID: integer)
    begin
      timerMessage()
    end));
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(timerID: integer; interval_ms: cardinal; const timerMessage: TProc<integer>);
begin
  otExecutor_ref.Asy_SetTimer(timerID, interval_ms, TOmniMessageID.Create(timerMessage));
end; { TOmniTask.SetTimer }
{$ENDIF OTL_Anonymous}

function TOmniTask.Stopped: boolean;
begin
  Result := otSharedInfo_ref.Stopped;
end; { TOmniTask.Stopped }

procedure TOmniTask.StopTimer;
begin
  SetTimer(0);
end; { TOmniTask.StopTimer }

procedure TOmniTask.Terminate;
begin
  otCleanupLock.EnterWriteLock;
  try
    if otExecuting and (not assigned(otSharedInfo_ref)) then
      Exit; // thread has just completed execution
    otSharedInfo_ref.Terminating := true;
    SetEvent(otSharedInfo_ref.TerminateEvent);
    if not otExecuting then
      otTerminateWillCallExecute := true;
  finally otCleanupLock.ExitWriteLock; end;
  // TODO 1 -oPrimoz Gabrijelcic : This is very suspicious :(
  if otTerminateWillCallExecute then //call Execute to run at least cleanup code
    InternalExecute(true);
end; { TOmniTask.Terminate }

function TOmniTask.Terminated: boolean;
begin
  Result := otSharedInfo_ref.Terminating;
end; { TOmniTask.Terminated }

procedure TOmniTask.UnregisterComm(const comm: IOmniCommunicationEndpoint);
begin
  otExecutor_ref.Asy_UnregisterComm(comm);
end; { TOmniTask.UnregisterComm }

procedure TOmniTask.UnregisterWaitObject(waitObject: TOmniTransitionEvent);
begin
  otExecutor_ref.Asy_UnregisterWaitObject(waitObject);
end; { TOmniTask.UnregisterWaitObject }

{ TOmniWorker }

procedure TOmniWorker.AfterWait(waitFor: TWaitFor; awaited: TWaitFor.TWaitForResult);
begin
  // do nothing
end; { TOmniWorker.AfterWait }

procedure TOmniWorker.BeforeWait(var timeout_ms: cardinal);
begin
  // do nothing
end; { TOmniWorker.BeforeWait }

procedure TOmniWorker.Cleanup;
begin
  //do-nothing
end; { TOmniWorker.Cleanup }

procedure TOmniWorker.DispatchMessage(var msg: TOmniMessage);
begin
  Dispatch(msg);
end; { TOmniWorker.DispatchMessage }

function TOmniWorker.GetImplementor: TObject;
begin
  Result := Self;
end; { TOmniWorker.GetImplementor }

function TOmniWorker.GetTask: IOmniTask;
begin
  Result := owTask;
end; { TOmniWorker.GetTask }

function TOmniWorker.Initialize: boolean;
begin
  //do-nothing
  Result := true;
end; { TOmniWorker.Initialize }

procedure TOmniWorker.MessageLoopPayload;
begin
  //do nothing
end; { TOmniWorker.MessageLoopPayload }

procedure TOmniWorker.ProcessMessages;
begin
  (owExecutor as TOmniTaskExecutor).ProcessMessages(Task);
end; { TOmniWorker.ProcessMessages }

procedure TOmniWorker.ProcessThreadMessages;
begin
  // do nothing
end; { TOmniWorker.ProcessThreadMessages }

procedure TOmniWorker.SetExecutor(executor: TObject);
begin
  owExecutor := executor;
end; { TOmniWorker.SetExecutor }

procedure TOmniWorker.SetTask(const value: IOmniTask);
begin
  owTask := value;
end; { TOmniWorker.SetTask }

procedure TOmniWorker.Timer;
begin
  //do-nothing
end; { TOmniWorker.Timer }

{ TOmniInvokeInfo }

constructor TOmniInvokeInfo.Create(methodAddr: pointer; methodSignature: TOmniInvokeType);
begin
  inherited Create;
  oiiAddress := methodAddr;
  oiiSignature := methodSignature;
end; { TOmniInvokeInfo.Create }

{ TOmniTaskTimerInfo }

constructor TOmniTaskTimerInfo.Create(timerID: integer; interval_ms: cardinal;
  messageID: TOmniMessageID);
begin
  inherited Create;
  ottiTimerID := timerID;
  ottiInterval_ms := interval_ms;
  ottiMessageID := messageID;
end; { TOmniTaskTimerInfo.Create }

{ TOmniTaskExecutor.TOmniMessageInfo }

function TOmniTaskExecutor.TOmniMessageInfo.AsString: string;
var
  iHandle: integer;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  for iHandle := 0 to NumWaitHandles - 1 do
    Result := Result + IntToStr(WaitHandles[iHandle]) + ' ';
  {$ENDIF MSWINDOWS}
end; { TOmniTaskExecutor.TOmniMessageInfo.AsString }

{$IFNDEF MSWINDOWS}
function TOmniTaskExecutor.TOmniMessageInfo.GetSynchroIndex(
  WaitResult: TWaitResult; const Handle: IOmniSynchro): integer;
var
  Member   : IOmniSynchro;
  Idx      : integer;
  Comparand: IOmniSynchro;

  function Canonical(const Overridable: IOmniSynchro): IOmniSynchro;
  begin
    if not Supports(Overridable, IOmniSynchro, result) then
     result := Overridable
  end;

begin
  Result := -1;
  if WaitResult <> wrSignaled then
    Exit;
  Idx := -1;
  Comparand := Canonical(Handle);
  for Member in WaitHandles do begin
    Inc(Idx);
    if (Member <> Comparand) and (Canonical(Member) <> Comparand) then
      continue; //for
    Exit(Idx);
  end
end; { TOmniTaskExecutor.TOmniMessageInfo.GetSynchroIndex }
{$ENDIF ~MSWINDOWS}

{ TOmniTaskExecutor }

constructor TOmniTaskExecutor.Create(owner_ref: TOmniTaskControl; const workerIntf: IOmniWorker);
begin
  oteOwner_ref := owner_ref;
  oteExecutorType := etWorker;
  oteWorkerIntf := workerIntf;
  workerIntf.SetExecutor(Self);
  Initialize;
end; { TOmniTaskExecutor.Create }

constructor TOmniTaskExecutor.Create(owner_ref: TOmniTaskControl; method: TOmniTaskMethod);
begin
  oteOwner_ref := owner_ref;
  oteExecutorType := etMethod;
  oteMethod := method;
  Initialize;
end; { TOmniTaskExecutor.Create }

constructor TOmniTaskExecutor.Create(owner_ref: TOmniTaskControl; proc: TOmniTaskProcedure);
begin
  oteOwner_ref := owner_ref;
  oteExecutorType := etProcedure;
  oteProc := proc;
  Initialize;
end; { TOmniTaskExecutor.Create }

{$IFDEF OTL_Anonymous}
constructor TOmniTaskExecutor.Create(owner_ref: TOmniTaskControl; func: TOmniTaskDelegate);
begin
  oteOwner_ref := owner_ref;
  oteExecutorType := etFunction;
  oteFunc := func;
  Initialize;
end; { TOmniTaskExecutor.Create }
{$ENDIF OTL_Anonymous}

destructor TOmniTaskExecutor.Destroy;
begin
  oteInternalLock.Acquire;
  try
    Cleanup;
    oteMsgInfo.Waiter.Free;
    oteMsgInfo.Waiter := nil;
    FreeAndNil(oteCommList);
    FreeAndNil(oteCommNewMsgList);
    FreeAndNil(oteWaitObjectList);
  finally oteInternalLock.Release; end;
  {$IFDEF MSWINDOWS}
  FreeAndNil(oteTerminateHandles);
  {$ENDIF ~MSWINDOWS}
  FreeAndNil(oteMethodHash);
  FreeAndNil(oteException);
  {$IFDEF MSWINDOWS}
  DSiCloseHandleAndNull(oteCommRebuildHandles);
  DSiCloseHandleAndNull(oteWorkerInitialized);
  {$ELSE}
  oteCommRebuildHandles := nil;
  oteWorkerInitialized := nil;
  {$ENDIF ~MSWINDOWS}
  inherited;
end; { TOmniTaskExecutor.Destroy }

procedure TOmniTaskExecutor.Asy_Execute(const task: IOmniTask);
const
  {$IFDEF MSWINDOWS}
  CThreadPriorityNum: array [TOTLThreadPriority] of integer = (
    THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST);
  {$ENDIF MSWINDOWS}

  {$IFDEF POSIX}
  CThreadPriorityNum: array [TOTLThreadPriority] of integer = (1, 10, 25, 50, 75, 99);
  {$ENDIF}
{$IFDEF POSIX}
var
  This : TThread;
  Value: integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  SetThreadPriority(GetCurrentThread, CThreadPriorityNum[Priority]);
  {$ELSE}
  This  := TThread.CurrentThread;
  Value := CThreadPriorityNum[Priority];
  if This.Policy = 0 {SCHED_OTHER} then begin
    if Value = 0 then
      This.Priority := 0
  end
  else begin
    if Value <= 1 then
      Value := 1;
    if Value >= 99 then
      Value := 99;
    try
      This.Priority := Value
    except
      // We don't have privilege. We need to be root to do this.
    end
  end;
  {$ENDIF}
  try
    case oteExecutorType of
      etMethod:
        oteMethod(task);
      etProcedure:
        oteProc(task);
      etFunction:
        {$IFDEF OTL_Anonymous}
        oteFunc(task);
        {$ELSE}
        raise Exception.Create('TOmniTaskExecutor.Asy_Execute: ' +
          'Anonymous function execution is not supported on Delphi 2007');
        {$ENDIF OTL_Anonymous}
      etWorker:
        DispatchMessages(task);
      else
        raise Exception.Create('TOmniTaskExecutor.Asy_Execute: Executor is not set');
    end;
  finally Cleanup; end;
end; { TOmniTaskExecutor.Asy_Execute }

procedure TOmniTaskExecutor.Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_RegisterComm: ' +
      'Additional communication support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    if not assigned(oteCommList) then begin
      oteCommList := TInterfaceList.Create;
      oteCommNewMsgList := {$IFDEF MSWINDOWS}TGpInt64List.Create{$ELSE}TList<IOmniEvent>.Create{$ENDIF};
    end;
    oteCommList.Add(comm);
    oteCommNewMsgList.Add((comm as IOmniCommunicationEndpoint).NewMessageEvent);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_RegisterComm }

procedure TOmniTaskExecutor.Asy_RegisterWaitObject(waitObject: TOmniTransitionEvent;
  responseHandler: TOmniWaitObjectMethod);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_RegisterWaitObject: ' +
      'WaitObject support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    if not assigned(oteWaitObjectList) then
      oteWaitObjectList := TOmniWaitObjectList.Create;
    oteWaitObjectList.Add(waitObject, responseHandler);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_RegisterWaitObject }

procedure TOmniTaskExecutor.Asy_SetExitStatus(exitCode: integer;
  const exitMessage: string);
begin
  oteExitCode.Value := exitCode;
  oteInternalLock.Acquire;
  try
    oteExitMessage := exitMessage;
    UniqueString(oteExitMessage);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_SetExitStatus }

procedure TOmniTaskExecutor.Asy_SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TOmniMessageID);
begin
  oteTimerLock.Acquire;
  try
    SetTimer(timerID, interval_ms, timerMessage);
  finally oteTimerLock.Release; end;
  SetEvent(oteCommRebuildHandles);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
var
  idxComm: integer;
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_UnregisterComm: ' +
      'Additional communication support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    idxComm := oteCommList.IndexOf(comm);
    oteCommList.Delete(idxComm);
    oteCommNewMsgList.Delete(idxComm);
    if oteCommList.Count = 0 then begin
      FreeAndNil(oteCommList);
      FreeAndNil(oteCommNewMsgList);
    end;
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_UnregisterComm }

procedure TOmniTaskExecutor.Asy_UnregisterWaitObject(waitObject: TOmniTransitionEvent);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_UnregisterWaitObject: ' +
      'WaitObject support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    oteWaitObjectList.Remove(waitObject);
    if oteWaitObjectList.Count = 0 then
      FreeAndNil(oteWaitObjectList);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_UnregisterWaitObject }

procedure TOmniTaskExecutor.CallOmniTimer;
var
  msg      : TOmniMessage;
  timerInfo: TOmniTaskTimerInfo;
begin
  oteTimerLock.Acquire;
  try
    timerInfo := TOmniTaskTimerInfo(oteTimers.Objects[0]);
    SetTimer(timerInfo.TimerID, timerInfo.Interval_ms, timerInfo.MessageID); // rearm
  finally oteTimerLock.Release; end;
  if (timerInfo.MessageID.MessageType <> mitInteger) or (integer(timerInfo.MessageID) >= 0) then begin
    case timerInfo.MessageID.MessageType of
      mitInteger:
        begin
          msg.MsgID := timerInfo.MessageID;
          msg.MsgData := timerInfo.TimerID;
        end;
      mitString:
        msg := TOmniInternalStringMsg.CreateMessage(timerInfo.MessageID, timerInfo.TimerID);
      mitPointer:
        msg := TOmniInternalAddressMsg.CreateMessage(timerInfo.MessageID, timerInfo.TimerID);
      {$IFDEF OTL_Anonymous}
      mitAnon:
        msg := TOmniInternalAnonMsg.CreateMessage(timerInfo.MessageID.Proc, timerInfo.TimerID);
      {$ENDIF OTL_Anonymous}
      else
        raise Exception.Create('TOmniTaskExecutor.CallOmniTimer: Invalid message type');
    end;
    DispatchOmniMessage(msg, false);
  end
  else if assigned(WorkerIntf) then
    WorkerIntf.Timer;
end; { TOmniTaskExecutor.CallOmniTimer }

procedure TOmniTaskExecutor.CheckTimers;
begin
  if (not Terminating) and HaveElapsedTimer then
    CallOmniTimer;
end; { TOmniTaskExecutor.CheckTimers }

procedure TOmniTaskExecutor.Cleanup;
begin
  oteWorkerIntf := nil;
  FreeAndNil(oteTimers);
end; { TOmniTaskExecutor.Cleanup }

procedure TOmniTaskExecutor.DispatchCommMessage(newMsgHandle: TOmniTransitionEvent;
  const task: IOmniTask; var msgInfo: TOmniMessageInfo);
var
  gotMsg: boolean;
  i     : integer;
  msg   : TOmniMessage;
begin
  repeat
    if newMsgHandle = msgInfo.NewMessageEvent then
      gotMsg := task.Comm.Receive(msg)
    else begin
      oteInternalLock.Acquire;
      try
        gotMsg := false;
        for i := 0 to oteCommNewMsgList.Count - 1 do
          if oteCommNewMsgList[i] = newMsgHandle then begin
            gotMsg := (oteCommList[i] as IOmniCommunicationEndpoint).Receive(msg);
            break; //for i
          end;
      finally oteInternalLock.Release; end;
    end;
    if gotMsg and assigned(WorkerIntf) then
      DispatchOmniMessage(msg, true);
  until (not gotMsg) or TestForInternalRebuild(task, msgInfo);
end; { TOmniTaskExecutor.DispatchCommMessage }

function TOmniTaskExecutor.DispatchEvent(awaited: TWaitFor.TWaitForResult;
  const task: IOmniTask; var msgInfo: TOmniMessageInfo
  {$IFNDEF MSWINDOWS}; SignalEvent: IOmnIEvent{$ENDIF}): boolean;
var
  info           : TWaitFor.THandleInfo;
  rebuildHandles : boolean;
  responseHandler: TOmniWaitObjectMethod;
begin
  // Keep logic in sync with ReportInvalidHandle!

  rebuildHandles := false;
  if awaited = waFailed then
    // do-nothing; it is possible that the handle was closed and unregistered but handle array was not rebuilt yet and WaitForEvent returned this error
  else if awaited = waIOCompletion then
    // do-nothing
  else if awaited = waTimeout then
    CheckTimers
  else if awaited = waMessage then begin
    // thread messages are always processed below
    if assigned(WorkerIntf) then
      WorkerIntf.ProcessThreadMessages;
  end
  else if awaited <> waAwaited then
    raise Exception.Create('TOmniTaskExecutor.DispatchEvent: Unexpected TWaitResult')
  else begin
    {$IFNDEF MSWINDOWS}
    info.Index := msgInfo.GetSynchroIndex(TWaitResult(awaited), SignalEvent); //TODO: This TWaitResult cast is probably incorrect
    {$ENDIF ~MSWINDOWS}
    // First test if any of Terminate events was signalled
    if (msgInfo.IdxFirstTerminate <> -1) then
      {$IFDEF MSWINDOWS}
      for info in msgInfo.Waiter.Signalled do
      {$ENDIF MSWINDOWS}
        if ((info.Index >= msgInfo.IdxFirstTerminate) and (info.Index <= msgInfo.IdxLastTerminate)) then begin
          Result := false; //break out of the message loop
          Exit;
        end;

    // Only then test other events
    {$IFDEF MSWINDOWS}
    for info in msgInfo.Waiter.Signalled do
    {$ENDIF MSWINDOWS}
    begin
      if (info.Index >= msgInfo.IdxFirstMessage) and (info.Index <= msgInfo.IdxLastMessage) then begin
        if (info.Index = msgInfo.IdxFirstMessage) or assigned(oteCommList) then
          DispatchCommMessage({$IFDEF MSWINDOWS}msgInfo.WaitHandles[info.Index]{$ELSE}SignalEvent{$ENDIF}, task, msgInfo);
      end
      else if (info.Index >= msgInfo.IdxFirstWaitObject) and (info.Index <= msgInfo.IdxLastWaitObject) then begin
        if assigned(oteWaitObjectList) then begin
          oteInternalLock.Acquire;
          try
            responseHandler := oteWaitObjectList.ResponseHandlers[info.Index - msgInfo.IdxFirstWaitObject];
          finally oteInternalLock.Release; end;
          responseHandler();
        end;
//        TestForInternalRebuild(task, msgInfo); // doesn't seem safe anymore
      end // comm handles
      else if info.Index = msgInfo.IdxRebuildHandles then
        rebuildHandles := true;
    end;
  end;

  {$IFDEF MSWINDOWS}
  ProcessThreadMessages;
  {$ENDIF MSWINDOWS}
  if rebuildHandles then begin
    RebuildWaitHandles(task, msgInfo);
    EmptyMessageQueues(task);
  end
  else
    TestForInternalRebuild(task, msgInfo);
  Result := true;
end; { TOmniTaskExecutor.DispatchEvent }

procedure TOmniTaskExecutor.DispatchMessages(const task: IOmniTask);
begin
  try
    oteWorkerInitOK := false;
    try
      if assigned(WorkerIntf) then begin
        WorkerIntf.SetExecutor(Self);
        WorkerIntf.Task := task;
        if not WorkerIntf.Initialize then
          Exit;
      end;
      oteWorkerInitOK := true;
    finally SetEvent(WorkerInitialized); end;

    if tcoMessageWait in Options then
      {$IFDEF MSWINDOWS}
      oteMsgInfo.WaitWakeMask := WakeMask
      {$ELSE}
      oteMsgInfo.WaitWakeAll := WakeAll
      {$ENDIF}
    else
      {$IFDEF MSWINDOWS}
      oteMsgInfo.WaitWakeMask := 0;
      {$ELSE}
      oteMsgInfo.WaitWakeAll := false;
      {$ENDIF}
    {$IFDEF MSWINDOWS}
    if tcoAlertableWait in Options then
      oteMsgInfo.WaitFlags := MWMO_ALERTABLE
    else
      oteMsgInfo.WaitFlags := 0;
    {$ENDIF}
    RebuildWaitHandles(task, oteMsgInfo);
    MainMessageLoop(task, oteMsgInfo);
  finally
    if assigned(WorkerIntf) then begin
      WorkerIntf.Cleanup;
      WorkerIntf.Task := nil;
    end;
  end;
end; { TOmniTaskExecutor.DispatchMessages }

procedure TOmniTaskExecutor.DispatchOmniMessage(msg: TOmniMessage;
  doCheckTimers: boolean);
{$IFDEF OTL_Anonymous}
var
  func    : TOmniTaskControlInvokeFunction;
  funcEx  : TOmniTaskControlInvokeFunctionEx;
  proc    : TProc<integer>;
  taskFunc: TOmniTaskInvokeFunction;
{$ENDIF OTL_Anonymous}
var
  methodAddr     : pointer;
  methodInfoObj  : TObject;
  methodInfo     : TOmniInvokeInfo absolute methodInfoObj;
  methodName     : string;
  methodSignature: TOmniInvokeType;
  msgData        : TOmniValue;
  obj            : TObject;
begin
  if doCheckTimers then
    CheckTimers;
  if msg.MsgID = COtlReservedMsgID then begin
    Assert(assigned(WorkerIntf));
    GetMethodNameFromInternalMessage(msg, methodName, msgData {$IFDEF OTL_Anonymous},
      func, funcEx, proc, taskFunc{$ENDIF OTL_Anonymous});
    {$IFDEF OTL_Anonymous}
    if assigned(func) then
      func
    else if assigned(funcEx) then
      funcEx(WorkerIntf.Task)
    else if assigned(proc) then
      proc(msgData)
    else if assigned(taskFunc) then
      taskFunc()
    else
    {$ENDIF OTL_Anonymous}
    begin
      if methodName = '' then
        raise Exception.Create('TOmniTaskExecutor.DispatchOmniMessage: Method name not set');
      if not assigned(oteMethodHash) then
        oteMethodHash := TGpStringObjectHash.Create(17, true); //usually there won't be many methods
      if not oteMethodHash.Find(methodName, methodInfoObj) then begin
        GetMethodAddrAndSignature(methodName, methodAddr, methodSignature);
        methodInfo := TOmniInvokeInfo.Create(methodAddr, methodSignature);
        oteMethodHash.Add(methodName, methodInfo);
      end;
      case methodInfo.Signature of
        itSelf:
          TOmniInvokeSignature_Self(methodInfo.Address)(WorkerIntf.Implementor);
        itSelfAndOmniValue:
          TOmniInvokeSignature_Self_OmniValue(methodInfo.Address)(WorkerIntf.Implementor, msgData);
        itSelfAndObject:
          begin
            obj := msgData.AsObject;
            TOmniInvokeSignature_Self_Object(methodInfo.Address)(WorkerIntf.Implementor, obj);
          end
        else
          RaiseInvalidSignature(methodName);
      end; //case methodSignature
    end
  end
  else
    WorkerIntf.DispatchMessage(msg);
end; { TOmniTaskExecutor.DispatchOmniMessage }

procedure TOmniTaskExecutor.EmptyMessageQueues(const task: IOmniTask);
var
  iComm: IOmniCommunicationEndpoint;
  iIntf: IInterface;
  msg  : TOmniMessage;
begin
  while task.Comm.Receive(msg) do
    if assigned(WorkerIntf) then
      DispatchOmniMessage(msg, false);
  if assigned(oteCommList) then begin
    oteInternalLock.Acquire;
    try
      for iIntf in oteCommList do begin
        iComm := iIntf as IOmniCommunicationEndpoint;
        while iComm.Receive(msg) do begin
          if assigned(WorkerIntf) then begin
            DispatchOmniMessage(msg, false);
            if not assigned(oteCommList) then
              break; //while
          end;
        end; //while
        if not assigned(oteCommList) then
          break; //for
      end; //for iIntf
    finally oteInternalLock.Release; end;
  end;
end; { TOmniTaskExecutor.EmptyMessageQueues }

function TOmniTaskExecutor.GetExitCode: integer;
begin
  Result := oteExitCode;
end; { TOmniTaskExecutor.GetExitCode }

function TOmniTaskExecutor.GetExitMessage: string;
begin
  oteInternalLock.Acquire;
  try
    Result := oteExitMessage;
    UniqueString(Result);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.GetExitMessage }

function TOmniTaskExecutor.GetImplementor: TObject;
begin
  case oteExecutorType of
    etWorker:
      Result := oteWorkerIntf.Implementor;
    else
      Result := nil;
  end;
end; { TOmniTaskExecutor.GetImplementor }

procedure TOmniTaskExecutor.GetMethodAddrAndSignature(const methodName: string; var
  methodAddress: pointer; var methodSignature: TOmniInvokeType);
const
  CShortLen = SizeOf(ShortString) - 1;
var
  headerEnd       : cardinal;
  methodInfoHeader: PMethodInfoHeader;
  paramNum        : integer;
  params          : PParamInfo;
  paramType       : PTypeInfo;
  returnInfo      : PReturnInfo;

  function VerifyObjectFlags(flags, requiredFlags: {$IF CompilerVersion >= 23}System.TypInfo.{$IFEND}TParamFlags): boolean;
  begin
    Result := ((flags * requiredFlags) = requiredFlags);
    if not Result then
      Exit;
    flags := flags - requiredFlags;
    {$IF CompilerVersion < 21}
    Result := (flags = []);
    {$ELSEIF CompilerVersion = 21}
    // Delphi 2010 original and Update 1: []
    // Delphi 2010 with Update 2 and 4: [pfAddress]
    Result := (flags = []) or (flags = [pfAddress]);
    {$ELSE} // best guess
    Result := (flags = [pfAddress]);
    {$IFEND}
  end; { VerifyObjectFlags }

  function VerifyConstFlags(flags: {$IF CompilerVersion >= 23}System.TypInfo.{$IFEND}TParamFlags): boolean;
  begin
    {$IF CompilerVersion < 21}
    Result := (flags = [pfVar]);
    {$ELSEIF CompilerVersion = 21}
    // Delphi 2010 original and Update 1: [pfVar]
    // Delphi 2010 Update 2 and 4: [pfConst, pfReference]
    Result := (flags = [pfVar]) or (flags = [pfConst, pfReference]);
    {$ELSE} // best guess
    Result := (flags = [pfConst, pfReference]);
    {$IFEND}
  end; { VerifyConstFlags }

begin { TOmniTaskExecutor.GetMethodAddrAndSignature }
  {$IFNDEF MSWINDOWS} //TODO: repimplement using ERTTI for D2010+
  methodAddress := nil;
  {$ELSE}
  // with great thanks to Hallvar Vassbotn [http://hallvards.blogspot.com/2006/04/published-methods_27.html]
  // and David Glassborow [http://davidglassborow.blogspot.com/2006/05/class-rtti.html]
  methodInfoHeader := ObjAuto.GetMethodInfo(WorkerIntf.Implementor,
    {$IFNDEF OTL_LongGetMethodInfo}ShortString{$ENDIF}(methodName));
  methodAddress := WorkerIntf.Implementor.MethodAddress(methodName);
  // find the method info
  if not (assigned(methodInfoHeader) and assigned(methodAddress)) then
    raise Exception.CreateFmt('TOmniTaskExecutor.DispatchMessage: ' +
                              'Cannot find message method %s.%s',
                              [WorkerIntf.Implementor.ClassName, methodName]);
  // check the RTTI sanity
  if methodInfoHeader.Len <= (SizeOf(TMethodInfoHeader) - CShortLen + Length(methodInfoHeader.Name)) then
    raise Exception.CreateFmt('TOmniTaskExecutor.DispatchMessage: ' +
                              'Class %d was compiled without RTTI',
                              [WorkerIntf.Implementor.ClassName]);
  // we can only process procedures
  if assigned(methodInfoHeader.ReturnInfo.ReturnType) then
    raise Exception.CreateFmt('TOmniTaskExecutor.DispatchMessage: ' +
                              'Method %s.%s must not return result',
                              [WorkerIntf.Implementor.ClassName, methodName]);
  // only limited subset of method signatures is allowed:
  // (Self), (Self, const TOmniValue), (Self, var TObject)
  headerEnd := cardinal(methodInfoHeader) + methodInfoHeader^.Len;
  returnInfo := PReturnInfo(cardinal(methodInfoHeader) + SizeOf(methodInfoHeader^)
            - CShortLen + Length(methodInfoHeader^.Name));
  params := PParamInfo(cardinal(returnInfo) + SizeOf(TReturnInfo));
  paramNum := 0;
  methodSignature := itUnknown;
  // Loop over the parameters
  while (cardinal(params) < headerEnd) do begin
    {$IFDEF OTL_KnowsParamCount}
    if paramNum >= returnInfo.ParamCount then
      break; //while
    {$ENDIF OTL_KnowsParamCount}
    Inc(paramNum);
    paramType := params.ParamType^;
    if paramNum = 1 then
      if (not VerifyObjectFlags(params^.Flags, [])) or (paramType^.Kind <> tkClass) then
        RaiseInvalidSignature(methodName)
      else
        methodSignature := itSelf
    else if paramNum = 2 then
      if VerifyConstFlags(params^.Flags) and (paramType^.Kind = tkRecord) and
         (SameText(string(paramType^.Name), 'TOmniValue'))
      then
        methodSignature := itSelfAndOmniValue
      else if VerifyObjectFlags(params^.Flags, [pfVar]) and (paramType^.Kind = tkClass) then
        methodSignature := itSelfAndObject
      else
        RaiseInvalidSignature(methodName)
    else
      RaiseInvalidSignature(methodName);
    params := params.NextParam;
  end;
  {$ENDIF MSWINDOWS}
end; { TOmniTaskExecutor.GetMethodAddrAndSignature }

procedure TOmniTaskExecutor.GetMethodNameFromInternalMessage(const msg: TOmniMessage;
  var msgName: string; var msgData: TOmniValue {$IFDEF OTL_Anonymous};
  var func: TOmniTaskControlInvokeFunction; var funcEx: TOmniTaskControlInvokeFunctionEx;
  var proc: TProc<integer>; var taskFunc: TOmniTaskInvokeFunction {$ENDIF OTL_Anonymous});
var
  internalType: TOmniInternalMessageType;
  method      : pointer;
begin
  {$IFDEF OTL_Anonymous}
  func := nil;
  funcEx := nil;
  proc := nil;
  {$ENDIF OTL_Anonymous}
  internalType := TOmniInternalMessage.InternalType(msg);
  case internalType of
    imtStringMsg:
      TOmniInternalStringMsg.UnpackMessage(msg, msgName, msgData);
    imtAddressMsg:
      begin
        TOmniInternalAddressMsg.UnpackMessage(msg, method, msgData);
        msgName := WorkerIntf.Implementor.MethodName(method);
        if msgName = '' then
          raise Exception.CreateFmt('TOmniTaskExecutor.GetMethodNameFromInternalMessage: ' +
                  'Cannot find method name for method %p', [method]);
      end;
    {$IFDEF OTL_Anonymous}
    imtAnonMsg:
      TOmniInternalAnonMsg.UnpackMessage(msg, proc, msgData);
    imtFuncMsg:
      TOmniInternalFuncMsg.UnpackMessage(msg, func, funcEx, taskFunc);
    {$ENDIF OTL_Anonymous}
    else
      raise Exception.CreateFmt('TOmniTaskExecutor.GetMethodNameFromInternalMessage: ' +
              'Internal message type %s is not supported',
              [GetEnumName(TypeInfo(TOmniInternalMessageType), Ord(internalType))]);
  end; //case internalType
end; { TOmniTaskExecutor.GetMethodNameFromInternalMessage }

function TOmniTaskExecutor.GetOptions: TOmniTaskControlOptions;
begin
  oteOptionsLock.Acquire;
  try
    Result := oteOptions;
  finally oteOptionsLock.Release; end;
end; { TOmniTaskExecutor.GetOptions }

function TOmniTaskExecutor.HaveElapsedTimer: boolean;
begin
  oteTimerLock.Acquire;
  try
    Result := (oteTimers.Count > 0) and (oteTimers[0] <= TimeGetTime64);
  finally oteTimerLock.Release; end;
end; { TOmniTaskExecutor.HaveElapsedTimer }

procedure TOmniTaskExecutor.Initialize;
begin
  oteMsgInfo.Waiter := TWaitFor.Create({$IFNDEF MSWINDOWS}[]{$ENDIF}); //TODO: Not implemented for non-Windows platforms.
  oteTimers := TGpInt64ObjectList.Create;
  {$IFDEF MSWINDOWS}
  oteWorkerInitialized := CreateEvent(nil, true, false, nil);
  Win32Check(oteWorkerInitialized <> 0);
  oteCommRebuildHandles := CreateEvent(nil, false, false, nil);
  Win32Check(oteCommRebuildHandles <> 0);
  {$ELSE}
  oteWorkerInitialized := CreateOmniEvent(true, false);
  oteCommRebuildHandles := CreateOmniEvent(false, false);
  {$ENDIF ~MSWINDOWS}
  otePriority := tpNormal;
end; { TOmniTaskExecutor.Initialize }

procedure TOmniTaskExecutor.InsertTimer(wakeUpTime_ms: int64; timerInfo: TOmniTaskTimerInfo);
var
  idxTimer: integer;
begin
  // expects the caller to take care of the synchronicity
  idxTimer := 0;
  while (idxTimer < oteTimers.Count) and (wakeUpTime_ms > oteTimers[idxTimer]) do
    Inc(idxTimer);
  oteTimers.InsertObject(idxTimer, wakeUpTime_ms, timerInfo);
end; { TOmniTaskExecutor.InsertTimer }

function TOmniTaskExecutor.LocateTimer(timerID: integer): integer;
begin
  // expects the caller to take care of the synchronicity
  for Result := 0 to oteTimers.Count - 1 do
    if TOmniTaskTimerInfo(oteTimers.Objects[Result]).TimerID = timerID then
      Exit;
  Result := -1;
end; { TOmniTaskExecutor.LocateTimer }

procedure TOmniTaskExecutor.MainMessageLoop(const task: IOmniTask; var msgInfo:
  TOmniMessageInfo);
{$IFNDEF MSWINDOWS}
var
  SignalEvent: IOmnIEvent;
{$ENDIF ~MSWINDOWS}
begin
  EmptyMessageQueues(task);
  while DispatchEvent(
          WaitForEvent(msgInfo, TimeUntilNextTimer_ms {$IFNDEF MSWINDOWS}, SignalEvent{$ENDIF}),
          task, msgInfo {$IFNDEF MSWINDOWS}, SignalEvent{$ENDIF}) do
    MessageLoopPayload;
end; { TOmniTaskExecutor.MainMessageLoop }

procedure TOmniTaskExecutor.MessageLoopPayload;
begin
  //placeholder that can be overridden
  if assigned(WorkerIntf) then
    WorkerIntf.MessageLoopPayload;
end; { TOmniTaskExecutor.MessageLoopPayload }

procedure TOmniTaskExecutor.ProcessMessages(task: IOmniTask);
var
  awaited       : TWaitFor.TWaitForResult;
  msgInfo       : TOmniMessageInfo;
  waitHandlesGen: int64;
  {$IFNDEF MSWINDOWS}
  SignalEvent   : IOmniEvent;
  {$ENDIF ~MSWINDOWS}
begin
  msgInfo.Waiter := TWaitFor.Create({$IFNDEF MSWINDOWS}[]{$ENDIF}); //TODO: Not implemented for non-Windows platforms.
  try
    RemoveTerminationEvents(oteMsgInfo, msgInfo);
    waitHandlesGen := oteWaitHandlesGen;
    repeat
      awaited := WaitForEvent(msgInfo, 1 {$IFNDEF MSWINDOWS}, SignalEvent{$ENDIF});
      if awaited = waTimeout then
        Exit;
      if not DispatchEvent(awaited, task, msgInfo {$IFNDEF MSWINDOWS}, SignalEvent{$ENDIF}) then
        Exit;
      MessageLoopPayload;
      if waitHandlesGen <> oteWaitHandlesGen then begin
        RebuildWaitHandles(task, oteMsgInfo);
        RemoveTerminationEvents(oteMsgInfo, msgInfo);
        EmptyMessageQueues(task);
      end;
    until false;
  finally msgInfo.Waiter.Free; end;
end; { TOmniTaskExecutor.ProcessMessages }

{$IFDEF MSWINDOWS}
procedure TOmniTaskExecutor.ProcessThreadMessages;
var
  msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) and (Msg.Message <> WM_QUIT) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end; { TOmniTaskExecutor.ProcessThreadMessages }
{$ENDIF MSWINDOWS}

procedure TOmniTaskExecutor.RaiseInvalidSignature(const methodName: string);
begin
  raise Exception.CreateFmt('TOmniTaskExecutor: ' +
                            'Method %s.%s has invalid signature. Only following ' +
                            'signatures are supported: (Self), ' +
                            '(Self, const TOmniValue), (Self, var TObject)',
                            [WorkerIntf.Implementor.ClassName, methodName]);
end; { TOmniTaskExecutor.RaiseInvalidSignature }

procedure TOmniTaskExecutor.RebuildWaitHandles(const task: IOmniTask; var msgInfo:
  TOmniMessageInfo);
var
  aHandle    : {$IFDEF MSWINDOWS}THandle{$ELSE}IOmniSynchro{$ENDIF};
  handles    : {$IFDEF MSWINDOWS}TGpInt64List{$ELSE}TList<IOmniSynchro>{$ENDIF};
  iHandle    : integer;
  iIntf      : integer;
  intf       : IInterface;
  iWaitObject: integer;
begin
  Inc(oteWaitHandlesGen);
  oteInternalLock.Acquire;
  try
    handles := {$IFDEF MSWINDOWS}TGpInt64List.Create{$ELSE}TList<IOmniSynchro>.Create{$ENDIF};
    try
      // termination events
      msgInfo.IdxFirstTerminate := 0;
      handles.Add(task.TerminateEvent);
      msgInfo.IdxLastTerminate := msgInfo.IdxFirstTerminate;
      if assigned(oteTerminateHandles) then
        for aHandle in oteTerminateHandles do begin
          Inc(msgInfo.IdxLastTerminate);
          handles.Add(aHandle);
        end;

      // rebuild handles
      msgInfo.IdxRebuildHandles := msgInfo.IdxLastTerminate + 1;
      handles.Add(oteCommRebuildHandles);

      // message queues
      msgInfo.IdxFirstMessage := msgInfo.IdxRebuildHandles + 1;
      msgInfo.NewMessageEvent := task.Comm.NewMessageEvent;
      handles.Add(msgInfo.NewMessageEvent);
      msgInfo.IdxLastMessage := msgInfo.IdxFirstMessage;
      if assigned(oteCommList) then begin
        for iIntf := 0 to oteCommList.Count - 1 do begin
          intf := oteCommList[iIntf];
          Inc(msgInfo.IdxLastMessage);
          handles.Add((intf as IOmniCommunicationEndpoint).NewMessageEvent);
        end;
      end;

      // wait objects
      msgInfo.IdxFirstWaitObject := msgInfo.IdxLastMessage + 1;
      msgInfo.IdxLastWaitObject := msgInfo.IdxFirstWaitObject - 1;
      if assigned(oteWaitObjectList) then begin
        for iWaitObject := 0 to oteWaitObjectList.Count - 1 do begin
          Inc(msgInfo.IdxLastWaitObject);
          handles.Add(oteWaitObjectList.WaitObjects[iWaitObject]);
        end;
      end;
      msgInfo.NumWaitHandles := msgInfo.IdxLastWaitObject + 1;

      SetLength(msgInfo.WaitHandles, handles.Count);
      for iHandle := 0 to handles.Count - 1 do
        msgInfo.WaitHandles[iHandle] := handles[iHandle];

      {$IFDEF MSWINDOWS} //TODO: Add support for non-Windows platforms
      msgInfo.Waiter.SetHandles(msgInfo.WaitHandles);
      {$ENDIF MSWINDOWS}
    finally FreeAndNil(handles); end;
  finally oteInternalLock.Release; end;
end; { RebuildWaitHandles }

procedure TOmniTaskExecutor.RemoveTerminationEvents(const srcMsgInfo: TOmniMessageInfo;
  var dstMsgInfo: TOmniMessageInfo);
var
  offset: integer;
begin
  offset := srcMsgInfo.IdxLastTerminate + 1;
  dstMsgInfo.IdxFirstTerminate := -1;
  dstMsgInfo.IdxLastTerminate := -1;
  dstMsgInfo.IdxFirstMessage := srcMsgInfo.IdxFirstMessage - offset;
  dstMsgInfo.IdxLastMessage := srcMsgInfo.IdxLastMessage - offset;
  dstMsgInfo.IdxRebuildHandles := srcMsgInfo.IdxRebuildHandles - offset;
  dstMsgInfo.NumWaitHandles := srcMsgInfo.NumWaitHandles - offset;
  {$IFDEF MSWINDOWS}
  dstMsgInfo.WaitFlags := srcMsgInfo.WaitFlags;
  dstMsgInfo.WaitWakeMask := srcMsgInfo.WaitWakeMask;
  {$ELSE}
  dstMsgInfo.WaitWakeAll := srcMsgInfo.WaitWakeAll;
  {$ENDIF ~MSWINDOWS}
  SetLength(dstMsgInfo.WaitHandles, dstMsgInfo.NumWaitHandles);
  Move(srcMsgInfo.WaitHandles[offset], dstMsgInfo.WaitHandles[0],
    Length(dstMsgInfo.WaitHandles) * SizeOf(THandle));
  {$IFDEF MSWINDOWS} //TODO: Add support for non-Windows platforms
  dstMsgInfo.Waiter.SetHandles(dstMsgInfo.WaitHandles);
  {$ENDIF MSWINDOWS}
end; { TOmniTaskExecutor.RemoveTerminationEvents }

procedure TOmniTaskExecutor.ReportInvalidHandle(msgInfo: TOmniMessageInfo);
var
  failedList: string;
  iHandle   : integer;
begin
  failedList := SysErrorMessage(GetLastError);
  for iHandle := 0 to msgInfo.NumWaitHandles - 1 do begin
    {$IFDEF MSWINDOWS}
    if WaitForSingleObject(msgInfo.WaitHandles[iHandle], 0) = WAIT_FAILED then begin
    {$ELSE}
    if msgInfo.WaitHandles[iHandle].WaitFor(0) in [wrAbandoned, wrError] then begin
    {$ENDIF ~MSWINDOWS}
      failedList := failedList + #13#10;
      failedList := failedList + Format('Invalid handle: %d; ', [msgInfo.WaitHandles[iHandle]]);
      if ((msgInfo.IdxFirstTerminate <> -1) and
          ((iHandle >= msgInfo.IdxFirstTerminate) and (iHandle <= msgInfo.IdxLastTerminate)))
      then
        failedList := failedList + 'termination'
      else if (iHandle >= msgInfo.IdxFirstMessage) and (iHandle <= msgInfo.IdxLastMessage) then
        failedList := failedList + 'message'
      else if (iHandle >= msgInfo.IdxFirstWaitObject) and (iHandle <= msgInfo.IdxLastWaitObject) then
        failedList := failedList + 'wait object'
      else if iHandle = msgInfo.IdxRebuildHandles then
        failedList := failedList + 'rebuild handles'
    end;
  end;
  raise Exception.CreateFmt('[%d] TOmniTaskExecutor: Invalid handle!'#13#10'%s',
    [{$IFDEF MSWINDOWS}GetCurrentThreadID{$ELSE}GetThreadID{$ENDIF}, failedList]);
end; { TOmniTaskExecutor.ReportInvalidHandle}

procedure TOmniTaskExecutor.SetOptions(const value: TOmniTaskControlOptions);
begin
  if (([tcoAlertableWait, tcoMessageWait] * Options) <> []) and
     (oteExecutorType <> etWorker)
  then
    raise Exception.Create('TOmniTaskExecutor.SetOptions: ' +
      'Trying to set IOmniWorker specific option(s)');
  oteOptionsLock.Acquire;
  try
    oteOptions := value;
  finally oteOptionsLock.Release; end;
end; { TOmniTaskExecutor.SetOptions }

procedure TOmniTaskExecutor.SetNUMANode(numaNodeNumber: integer);
{$IFDEF OTL_NUMASupport}
var
  node         : IOmniNUMANode;
{$IFDEF MSWindows}
  groupAffinity: TGroupAffinity;
{$ENDIF MSWindows}
{$ENDIF OTL_NUMASupport}
begin
  {$IFDEF OTL_NUMASupport}
  node := VerifyNUMANode(numaNodeNumber);
  {$IFDEF MSWindows}
  FillChar(groupAffinity, SizeOf(groupAffinity), 0);
  groupAffinity.Group := node.GroupNumber;
  groupAffinity.Mask := node.Affinity.AsMask;
  DSiSetThreadGroupAffinity(GetCurrentThread, groupAffinity, nil);
  {$ENDIF MSWindows}
  {$ENDIF OTL_NUMASupport}
end; { TOmniTaskExecutor.SetNUMANode }

procedure TOmniTaskExecutor.SetProcessorGroup(procGroupNumber: integer);
{$IFDEF OTL_NUMASupport}
{$IFDEF MSWindows}
var
  groupAffinity: TGroupAffinity;
{$ENDIF MSWindows}
{$ENDIF OTL_NUMASupport}
begin
  {$IFDEF OTL_NUMASupport}
  VerifyProcessorGroup(procGroupNumber);
  {$IFDEF MSWindows}
  FillChar(groupAffinity, SizeOf(groupAffinity), 0);
  groupAffinity.Group := procGroupNumber;
  groupAffinity.Mask := Environment.ProcessorGroups[procGroupNumber].Affinity.AsMask;
  DSiSetThreadGroupAffinity(GetCurrentThread, groupAffinity, nil);
  {$ENDIF MSWindows}
  {$ENDIF OTL_NUMASupport}
end; { TOmniTaskExecutor.SetProcessorGroup }

procedure TOmniTaskExecutor.SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TOmniMessageID);
var
  idxTimer : integer;
  timerInfo: TOmniTaskTimerInfo;
begin
  // expects the caller to take care of the synchronicity
  idxTimer := LocateTimer(timerID);
  if interval_ms = 0 then begin // delete the timer
    if idxTimer >= 0 then
      oteTimers.Delete(idxTimer)
    else
      Exit; // no change, don't rebuild handles
  end
  else begin
    if idxTimer < 0 then // new timer
      timerInfo := TOmniTaskTimerInfo.Create(timerID, interval_ms, timerMessage)
    else begin // rearm
      timerInfo := TOmniTaskTimerInfo(oteTimers.ExtractObject(idxTimer));
      timerInfo.Interval_ms := interval_ms;
      timerInfo.MessageID := timerMessage;
    end;
    InsertTimer(TimeGetTime64 + interval_ms, timerInfo);
  end;
end; { TOmniTaskExecutor.SetTimer }

procedure TOmniTaskExecutor.TerminateWhen(handle: TOmniTransitionEvent);
begin
  Assert(SizeOf(THandle) <= SizeOf(int64));
  {$IFDEF MSWINDOWS}
  if not assigned(oteTerminateHandles) then
    oteTerminateHandles := TGpInt64List.Create;
  oteTerminateHandles.Add(handle);
  {$ELSE}
  SetLength(oteTerminateHandles, Length(oteTerminateHandles) + 1);
  oteTerminateHandles[Length(oteTerminateHandles) - 1] := handle;
  {$ENDIF MSWINDOWS}
end; { TOmniTaskExecutor.TerminateWhen }

function TOmniTaskExecutor.TestForInternalRebuild(const task: IOmniTask; var msgInfo:
  TOmniMessageInfo): boolean;
begin
  Result := false;
  {$IFDEF MSWINDOWS}
  if WaitForSingleObject(oteCommRebuildHandles, 0) = WAIT_OBJECT_0 then begin
  {$ELSE}
  if oteCommRebuildHandles.WaitFor(0) = wrSignaled then begin
  {$ENDIF ~MSWINDOWS}
    //could get set inside timer or message handler
    RebuildWaitHandles(task, msgInfo);
    EmptyMessageQueues(task);
    Result := true;
  end;
end; { TOmniTaskExecutor.TestForInternalRebuild }

function TOmniTaskExecutor.TimeGetTime64: int64;
begin
  {$IFNDEF MSWINDOWS}
  Result := TStopwatch.GetTimeStamp;
  {$ELSE}

  {$IFDEF DEBUG}
  Assert(oteTimerLock.LockCount > 0);
  {$ENDIF}

  Result := timeGetTime;
  if Result < oteLastTimeGetTime64 then
    oteTimeGetTime64Base := oteTimeGetTime64Base + $100000000;
  oteLastTimeGetTime64 := Result;
  Result := Result + oteTimeGetTime64Base;
  {$ENDIF}
end; { TOmniTaskExecutor.TimeGetTime64 }

function TOmniTaskExecutor.TimeUntilNextTimer_ms: cardinal;
var
  timeout_ms: int64;
begin
  oteTimerLock.Acquire;
  try
    if oteTimers.Count = 0 then
      Result := INFINITE
    else begin
      timeout_ms := oteTimers[0] - TimeGetTime64;
      if timeout_ms < 0 then
        timeout_ms := 0;
      Result := timeout_ms;
    end;
  finally oteTimerLock.Release; end;
end; { TOmniTaskExecutor.TimeUntilNextTimer_ms }

{$IFDEF OTL_NUMASupport}
class function TOmniTaskExecutor.VerifyNUMANode(numaNodeNumber: integer): IOmniNUMANode;
begin
  Result := Environment.NUMANodes.FindNode(numaNodeNumber);
  if not assigned(Result) then
    raise Exception.CreateFmt('NUMA node %d not found', [numaNodeNumber]);
end; { TOmniTaskExecutor.VerifyNUMANode }

class procedure TOmniTaskExecutor.VerifyProcessorGroup(procGroupNumber: integer);
var
  procGroups: IOmniProcessorGroups;
begin
  procGroups := Environment.ProcessorGroups;
  if (procGroupNumber < 0) or (procGroupNumber >= procGroups.Count) then
    raise Exception.CreateFmt(
            'Processor group number (%d) is out of range [0..%d]',
            [procGroupNumber, procGroups.Count - 1]);
end; { TOmniTaskExecutor.VerifyProcessorGroup }
{$ENDIF OTL_NUMASupport}

function TOmniTaskExecutor.WaitForInit: boolean;
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.WaitForInit: ' +
      'Wait for init is only available when working with an IOmniWorker');
  {$IFDEF MSWINDOWS}
  WaitForSingleObject(WorkerInitialized, INFINITE);
  {$ELSE}
  WorkerInitialized.WaitFor(INFINITE);
  {$ENDIF ~MSWINDOWS}
  Result := WorkerInitOK;
end; { TOmniTaskExecutor.WaitForInit }

function TOmniTaskExecutor.WaitForEvent(const msgInfo: TOmniMessageInfo; timeout_ms: cardinal
  {$IFNDEF MSWINDOWS}; var SignalEvent: IOmniEvent{$ENDIF}): TWaitFor.TWaitForResult;
begin
  if assigned(WorkerIntf) then
    WorkerIntf.BeforeWait(timeout_ms);
  {$IFDEF MSWINDOWS} //TODO: Implement for non-Windows platforms
  Result := msgInfo.Waiter.MsgWaitAny(timeout_ms, msgInfo.WaitWakeMask, msgInfo.WaitFlags);
  {$IFDEF Debug}
  if Result = waFailed then
    OutputDebugString(PChar(Format('*** TOmniTaskExecutor.WaitForEvent failed with error [%d] %s',
      [GetLastError, SysErrorMessage(GetLastError)])));
  {$ENDIF Debug}
  if assigned(WorkerIntf) then
    WorkerIntf.AfterWait(msgInfo.Waiter, Result);
  {$ENDIF MSWINDOWS}
end; { TOmniTaskExecutor.WaitForEvent }

{ TOmniTaskControl }

constructor TOmniTaskControl.Create(const worker: IOmniWorker; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(Self, worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskMethod; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(Self, worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskProcedure; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(Self, worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

{$IFDEF OTL_Anonymous}
constructor TOmniTaskControl.Create(worker: TOmniTaskDelegate; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(Self, worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }
{$ENDIF OTL_Anonymous}

destructor TOmniTaskControl.Destroy;
begin
  { TODO : Do we need wait-and-kill mechanism here to prevent shutdown locks? }
  // TODO 1 -oPrimoz Gabrijelcic : ! if we are being scheduled, the thread pool must be notified that we are dying !
  _AddRef; // Ugly ugly hack to prevent destructor being called twice when internal event monitor is in use
  DestroyMonitor;
  if assigned(otcThread) then begin
    Terminate;
    FreeAndNil(otcThread);
  end;
  if assigned(otcSharedInfo) then // TOmniTask.InternalExecute could still own the shared info
    otcSharedInfo.MonitorLock.Acquire;
  try
    if otcDestroyLock then begin
      otcSharedInfo.Lock.Free;
      otcSharedInfo.Lock := nil;
    end;
    FreeAndNil(otcExecutor);
    otcSharedInfo.CommChannel := nil;

    {$IFDEF MSWINDOWS}
    if otcSharedInfo.TerminateEvent <> 0 then begin
      CloseHandle(otcSharedInfo.TerminateEvent);
      otcSharedInfo.TerminateEvent := 0;
    end;
    if otcSharedInfo.TerminatedEvent <> 0 then begin
      CloseHandle(otcSharedInfo.TerminatedEvent);
      otcSharedInfo.TerminatedEvent := 0;
    end;
    {$ELSE}
    otcSharedInfo.TerminateEvent := nil;
    otcSharedInfo.TerminatedEvent := nil;
    {$ENDIF ~MSWINDOWS}
    FreeAndNil(otcParameters);
  finally
    if assigned(otcSharedInfo) then begin
      otcSharedInfo.MonitorLock.Release;
      FreeAndNil(otcSharedInfo);
    end;
  end;
  FreeAndNil(otcUserData);
  FreeAndNil(otcTerminateTokens);
  FreeAndNil(otcOnMessageList);
  FreeAndNil(otcOnMessageExec);
  FreeAndNil(otcOnTerminatedExec);
  inherited Destroy;
end; { TOmniTaskControl.Destroy }

procedure TOmniTaskControl.EnsureCommChannel; //inline
begin
  if not assigned(otcSharedInfo.CommChannel) then
    otcSharedInfo.CommChannel :=
      CreateTwoWayChannel(otcQueueLength, otcSharedInfo.TerminatedEvent);
end; { TOmniTaskControl.EnsureCommChannel }

function TOmniTaskControl.Alertable: IOmniTaskControl;
begin
  Options := Options + [tcoAlertableWait];
  Result := Self;
end; { TOmniTaskControl.Alertable }

function TOmniTaskControl.CancelWith(
  const token: IOmniCancellationToken): IOmniTaskControl;
begin
  if otcParameters.IsLocked then
    raise Exception.Create('TOmniTaskControl.CancelWith: Cancellation token can only be set before the task is started');
  otcSharedInfo.SetCancellationToken(token);
  Result := Self;
end; { TOmniTaskControl.CancelWith }

function TOmniTaskControl.ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean):
  IOmniTaskControl;
begin
  otcSharedInfo.ChainTo := task;
  otcSharedInfo.ChainIgnoreErrors := ignoreErrors;
  Result := Self;
end; { TOmniTaskControl.ChainTo }

function TOmniTaskControl.ClearTimer(timerID: integer = 0): IOmniTaskControl;
begin
  SetTimer(timerID, 0, 0);
  Result := Self;
end; { TOmniTaskControl.ClearTimer }

procedure TOmniTaskControl.CreateInternalMonitor;
begin
  if not assigned(otcEventMonitor) then begin
    otcEventMonitorInternal := true;
    otcEventMonitor := GTaskControlEventMonitorPool.Allocate;
    {$IFDEF MSWINDOWS}
    TOmniEventMonitor(otcEventMonitor).Monitor(Self);
    {$ENDIF MSWINDOWS}
  end;
end; { TOmniTaskControl.CreateInternalMonitor }

function TOmniTaskControl.CreateTask: IOmniTask;
begin
  EnsureCommChannel;
  Result := TOmniTask.Create(otcExecutor, otcParameters, otcSharedInfo);
end; { TOmniTaskControl.CreateTask }

procedure TOmniTaskControl.DestroyMonitor;
begin
  if assigned(otcEventMonitor) then begin
    RemoveMonitor;
    if assigned(GTaskControlEventMonitorPool) then
      GTaskControlEventMonitorPool.Release(TOmniTaskControlEventMonitor(otcEventMonitor));
    otcEventMonitor := nil;
    otcEventMonitorInternal := false;
  end;
end; { TOmniTaskControl.DestroyMonitor }

function TOmniTaskControl.DetachException: Exception;
begin
  if not assigned(otcExecutor) then
    Result := nil
  else begin
    Result := otcExecutor.TaskException;
    otcExecutor.TaskException := nil;
  end;
end; { TOmniTaskControl.DetachException }

function TOmniTaskControl.Enforced(forceExecution: boolean = true): IOmniTaskControl;
begin
  if forceExecution then
    Options := Options + [tcoForceExecution]
  else
    Options := Options - [tcoForceExecution];
  Result := Self;
end; { TOmniTaskControl.Enforced }

function TOmniTaskControl.FilterMessage(const msg: TOmniMessage): boolean;
{$IFDEF OTL_Anonymous}
var
  func: TOmniTaskInvokeFunction;
{$ENDIF OTL_Anonymous}
begin
  {$IFDEF OTL_Anonymous}
  Result := (msg.MsgID = COtlReservedMsgID);
  if Result and TOmniInternalFuncMsg.UnpackMessage(msg, func) then
    func;
  {$ELSE}
  Result := false;
  {$ENDIF OTL_Anonymous}
end; { TOmniTaskControl.FilterMessage }

procedure TOmniTaskControl.ForwardTaskMessage(const msg: TOmniMessage);
{$IFDEF OTL_Anonymous}
var
  func: TOmniTaskInvokeFunction;
{$ENDIF OTL_Anonymous}
var
  exec: TOmniMessageExec;
  kv  : TGpKeyValue;
  msg1: TOmniMessage;
begin
  {$IFDEF OTL_Anonymous}
  if (msg.MsgID = COtlReservedMsgID) and TOmniInternalFuncMsg.UnpackMessage(msg, func) then
    func
  else
  {$ENDIF OTL_Anonymous}
  begin
    for kv in otcOnMessageList.WalkKV do
      if kv.Key = COtlReservedMsgID then begin
        msg1 := msg;
        TOmniMessageExec(kv.Value).OnMessage(Self, msg1);
      end;
    exec := TOmniMessageExec(otcOnMessageList.FetchObject(msg.MsgID));
    otcInEventHandler := true;
    try
      if assigned(exec) then
        exec.OnMessage(Self, msg)
      else if assigned(otcOnMessageExec) then
        otcOnMessageExec.OnMessage(Self, msg);
    finally otcInEventHandler := false; end;
  end;
end; { TOmniTaskControl.ForwardTaskMessage }

procedure TOmniTaskControl.ForwardTaskTerminated;
begin
  if assigned(otcOnTerminatedExec) then begin
    otcInEventHandler := true;
    try
      otcOnTerminatedExec.OnTerminated(Self);
    finally otcInEventHandler := false; end;
  end;
end; { TOmniTaskControl.ForwardTaskTerminated }

function TOmniTaskControl.GetCancellationToken: IOmniCancellationToken;
begin
  Result := otcSharedInfo.CancellationToken;
end; { TOmniTaskControl.GetCancellationToken }

function TOmniTaskControl.GetComm: IOmniCommunicationEndpoint;
begin
  EnsureCommChannel;
  Result := otcSharedInfo.CommChannel.Endpoint1;
end; { TOmniTaskControl.GetComm }

function TOmniTaskControl.GetDebugFlags: TOmniTaskControlInternalDebugFlags;
begin
  Result := otcDebugFlags;
end; { TOmniTaskControl.GetDebugFlags }

function TOmniTaskControl.GetExitCode: integer;
begin
  Result := otcExecutor.ExitCode;
end; { TOmniTaskControl.GetExitCode }

function TOmniTaskControl.GetExitMessage: string;
begin
  Result := otcExecutor.ExitMessage;
end; { TOmniTaskControl.GetExitMessage }

function TOmniTaskControl.GetFatalException: Exception;
begin
  Result := nil;
  if assigned(otcExecutor) then
    Result := otcExecutor.TaskException;
end; { TOmniTaskControl.GetFatalException }

function TOmniTaskControl.GetLock: TSynchroObject;
begin
  Result := otcSharedInfo.Lock;
end; { TOmniTaskControl.GetLock }

function TOmniTaskControl.GetName: string;
begin
  Result := otcSharedInfo.TaskName;
end; { TOmniTaskControl.GetName }

function TOmniTaskControl.GetOptions: TOmniTaskControlOptions;
begin
  Result := otcExecutor.Options;
end; { TOmniTaskControl.GetOptions }

function TOmniTaskControl.GetParam: TOmniValueContainer;
begin
  Result := otcParameters;
end; { TOmniTaskControl.GetParam }

function TOmniTaskControl.GetSharedInfo: TOmniSharedTaskInfo;
begin
  result := otcSharedInfo;
end; { GetSharedInfo: TOmniSharedTaskInfo }

function TOmniTaskControl.GetTerminatedEvent: TOmniTransitionEvent;
begin
  Result := otcSharedInfo.TerminatedEvent;
end; { TOmniTaskControl.GetTerminatedEvent }

function TOmniTaskControl.GetTerminateEvent: TOmniTransitionEvent;
begin
  Result := otcSharedInfo.TerminateEvent;
end; { TOmniTaskControl.GetTerminateEvent }

function TOmniTaskControl.GetUniqueID: int64;
begin
  Result := otcSharedInfo.UniqueID;
end; { TOmniTaskControl.GetUniqueID }

function TOmniTaskControl.GetUserDataVal(const idxData: TOmniValue): TOmniValue;
begin
  Result := otcUserData[idxData];
end; { TOmniTaskControl.GetUserDataVal }

procedure TOmniTaskControl.Initialize(const taskName: string);
begin
  otcExecutor.Options := [tcoForceExecution];
  otcQueueLength := CDefaultQueueSize;
  otcSharedInfo := TOmniSharedTaskInfo.Create;
  otcSharedInfo.TaskName := taskName;
  otcSharedInfo.UniqueID := OtlUID.Increment;
  otcParameters := TOmniValueContainer.Create;
  {$IFDEF MSWINDOWS}
  otcSharedInfo.TerminateEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminateEvent <> 0);
  otcSharedInfo.TerminatedEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminatedEvent <> 0);
  {$ELSE}
  otcSharedInfo.TerminateEvent := CreateOmniEvent(true, false);
  otcSharedInfo.TerminatedEvent := CreateOmniEvent(true, false);
  {$ENDIF ~MSWINDOWS}
  otcUserData := TOmniValueContainer.Create;
  otcOnMessageList := TGpIntegerObjectList.Create(true);
end; { TOmniTaskControl.Initialize }

function TOmniTaskControl.Invoke(const msgMethod: pointer): IOmniTaskControl;
begin
  Invoke(msgMethod, TOmniValue.Null);
  Result := Self;
end; { TOmniTaskControl.Invoke }

function TOmniTaskControl.Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl;
begin
  Invoke(msgMethod, TOmniValue.Create(msgData));
  Result := Self;
end; { TOmniTaskControl.Invoke }

function TOmniTaskControl.Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl;
begin
  Comm.Send(TOmniInternalAddressMsg.CreateMessage(msgMethod, msgData));
  Result := Self;
end; { TOmniTaskControl.Invoke }

function TOmniTaskControl.Invoke(const msgName: string): IOmniTaskControl;
begin
  Invoke(msgName, TOmniValue.Null);
  Result := Self;
end; { TOmniCommunicationEndpoint.Invoke }

function TOmniTaskControl.Invoke(const msgName: string; msgData: array of const): IOmniTaskControl;
begin
  Invoke(msgName, TOmniValue.Create(msgData));
  Result := Self;
end; { TOmniCommunicationEndpoint.Invoke }

function TOmniTaskControl.Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl;
begin
  Comm.Send(TOmniInternalStringMsg.CreateMessage(msgName, msgData));
  Result := Self;
end; { TOmniCommunicationEndpoint.Invoke }

{$IFDEF OTL_Anonymous}
function TOmniTaskControl.Invoke(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl;
begin
  Comm.Send(TOmniInternalFuncMsg.CreateMessage(remoteFunc));
  Result := Self;
end; { TOmniTaskControl.Invoke }

function TOmniTaskControl.Invoke(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl;
begin
  Comm.Send(TOmniInternalFuncMsg.CreateMessage(remoteFunc));
  Result := Self;
end; { TOmniTaskControl.Invoke }
{$ENDIF OTL_Anonymous}

function TOmniTaskControl.Join(const group: IOmniTaskGroup): IOmniTaskControl;
begin
  group.Add(Self);
  Result := Self;
end; { TOmniTaskControl.Join }

function TOmniTaskControl.Leave(const group: IOmniTaskGroup): IOmniTaskControl;
begin
  group.Remove(Self);
  Result := Self;
end; { TOmniTaskControl.Leave }

function TOmniTaskControl.MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
begin
  {$IFDEF MSWINDOWS}
  monitor.Monitor(Self);
  {$ENDIF MSWINDOWS}
  Result := Self;
end; { TOmniTaskControl.MonitorWith }

function TOmniTaskControl.MsgWait({$IFDEF MSWINDOWS}wakeMask: DWORD = QS_ALLEVENTS{$ELSE}wakeAll: boolean = true{$ENDIF}): IOmniTaskControl;
begin
  Options := Options + [tcoMessageWait];
  {$IFDEF MSWINDOWS}
  otcExecutor.WakeMask := wakeMask;
  {$ELSE}
  otcExecutor.WakeAll := wakeAll;
  {$ENDIF ~MSWINDOWS}
  Result := Self;
end; { TOmniTaskControl.MsgWait }

function TOmniTaskControl.NUMANode(numaNodeNumber: integer): IOmniTaskControl;
begin
  {$IFDEF OTL_NUMASupport}
  TOmniTaskExecutor.VerifyNUMANode(numaNodeNumber);
  {$ENDIF OTL_NUMASupport}
  otcSharedInfo.NUMANode := numaNodeNumber;
  Result := Self;
end; { TOmniTaskControl.NUMANode }

function TOmniTaskControl.OnMessage(eventDispatcher: TObject): IOmniTaskControl;
begin
  otcOnMessageList.AddObject(COtlReservedMsgID, TOmniMessageExec.Create(eventDispatcher));
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }

function TOmniTaskControl.OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskControl;
begin
  if not assigned(otcOnMessageExec) then
    otcOnMessageExec := TOmniMessageExec.Create;
  otcOnMessageExec.SetOnMessage(eventHandler);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }

function TOmniTaskControl.OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent):
  IOmniTaskControl;
begin
  otcOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }

function TOmniTaskControl.OnMessage(msgID: word;
  eventHandler: TOmniMessageExec): IOmniTaskControl;
begin
  otcOnMessageList.AddObject(msgID, eventHandler);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }

{$IFDEF OTL_Anonymous}
function TOmniTaskControl.OnMessage(eventHandler: TOmniOnMessageFunction):
    IOmniTaskControl;
begin
  if not assigned(otcOnMessageExec) then
    otcOnMessageExec := TOmniMessageExec.Create;
  otcOnMessageExec.SetOnMessage(eventHandler);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }

function TOmniTaskControl.OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction):
  IOmniTaskControl;
begin
  otcOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnMessage }
{$ENDIF OTL_Anonymous}

function TOmniTaskControl.OnTerminated(eventHandler: TOmniTaskTerminatedEvent):
  IOmniTaskControl;
begin
  if not assigned(otcOnTerminatedExec) then
    otcOnTerminatedExec := TOmniMessageExec.Create;
  otcOnTerminatedExec.SetOnTerminated(eventHandler);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnTerminated }

function TOmniTaskControl.ProcessorGroup(procGroupNumber: integer): IOmniTaskControl;
begin
  {$IFDEF OTL_NUMASupport}
  TOmniTaskExecutor.VerifyProcessorGroup(procGroupNumber);
  {$ENDIF OTL_NUMASupport}
  otcSharedInfo.ProcessorGroup := procGroupNumber;
  Result := Self;
end; { TOmniTaskControl.ProcessorGroup }

{$IFDEF OTL_Anonymous}
function TOmniTaskControl.OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskControl;
begin
  if not assigned(otcOnTerminatedExec) then
    otcOnTerminatedExec := TOmniMessageExec.Create;
  otcOnTerminatedExec.SetOnTerminated(eventHandler);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnTerminated }

function TOmniTaskControl.OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple):
  IOmniTaskControl;
begin
  if not assigned(otcOnTerminatedExec) then
    otcOnTerminatedExec := TOmniMessageExec.Create;
  otcOnTerminatedSimple := eventHandler;
  otcOnTerminatedExec.SetOnTerminated(
    procedure (const task: IOmniTaskControl)
    begin
      otcOnTerminatedSimple();
    end);
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.OnTerminated }
{$ENDIF OTL_Anonymous}

function TOmniTaskControl.Run: IOmniTaskControl;
begin
  otcParameters.Lock;
  otcThread := TOmniThread.Create(CreateTask);
  {$IFDEF OTL_DeprecatedResume}
  otcThread.Start;
  {$ELSE}
  otcThread.Resume;
  {$ENDIF OTL_DeprecatedResume}
  Result := Self;
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgMethod: pointer): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgMethod);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgMethod: pointer; msgData: array of const): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgMethod, msgData);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgMethod, msgData);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgName: string): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgName);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgName: string; msgData: array of const): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgName, msgData);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(const msgName: string; msgData: TOmniValue): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(msgName, msgData);
end; { TOmniTaskControl.Run }

{$IFDEF OTL_Anonymous}
function TOmniTaskControl.Run(remoteFunc: TOmniTaskControlInvokeFunction): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(remoteFunc);
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Run(remoteFunc: TOmniTaskControlInvokeFunctionEx): IOmniTaskControl;
begin
  Result := Run;
  Result.Invoke(remoteFunc);
end; { TOmniTaskControl.Run }
{$ENDIF OTL_Anonymous}

function TOmniTaskControl.RemoveMonitor: IOmniTaskControl;
begin
  {$IFDEF MSWINDOWS}
  if assigned(otcSharedInfo.Monitor) then begin
    EnsureCommChannel;
    otcSharedInfo.CommChannel.Endpoint2.Writer.ContainerSubject.Detach(
      otcSharedInfo.Monitor, coiNotifyOnAllInserts);
    otcSharedInfo.MonitorLock.Acquire;
    try
      otcSharedInfo.Monitor.Free;
      otcSharedInfo.Monitor := nil;
    finally otcSharedInfo.MonitorLock.Release; end;
  end;
  {$ENDIF MSWINDOWS}
  Result := Self;
  if otcDelayedTerminate then begin
    otcDelayedTerminate := false;
    Terminate;
  end;
end; { TOmniTaskControl.RemoveMonitor }

function TOmniTaskControl.Schedule(const threadPool: IOmniThreadPool): IOmniTaskControl;
begin
  otcParameters.Lock;
  if assigned(threadPool) then
    otcOwningPool := threadPool
  else
    otcOwningPool := GlobalOmniThreadPool;
  (otcOwningPool as IOmniThreadPoolScheduler).Schedule(CreateTask);
  Result := Self;
end; { TOmniTaskControl.Schedule }

procedure TOmniTaskControl.SetDebugFlags(const value: TOmniTaskControlInternalDebugFlags);
begin
  otcDebugFlags := value;
end; { TOmniTaskControl.SetDebugFlags }

{$IFDEF MSWINDOWS}
function TOmniTaskControl.SetMonitor(hWindow: THandle): IOmniTaskControl;
begin
  if not assigned(otcSharedInfo.Monitor) then begin
    if otcParameters.IsLocked then
      raise Exception.Create('TOmniTaskControl.SetMonitor: Monitor can only be assigned while task is not running');
    EnsureCommChannel;
    otcSharedInfo.Monitor := CreateContainerWindowsMessageObserver(
      hWindow, COmniTaskMsg_NewMessage, integer(Int64Rec(UniqueID).Lo),
      integer(Int64Rec(UniqueID).Hi));
    otcSharedInfo.CommChannel.Endpoint2.Writer.ContainerSubject.Attach(
      otcSharedInfo.Monitor, coiNotifyOnAllInserts);
  end
  else if otcSharedInfo.Monitor.Handle <> hWindow then
    raise Exception.Create('TOmniTaskControl.SetMonitor: Task can be only monitored with a single monitor');
  otcSharedInfo.Monitor.Activate;
  Result := Self;
end; { TOmniTaskControl.SetMonitor }
{$ENDIF MSWINDOWS}

procedure TOmniTaskControl.SetOptions(const value: TOmniTaskControlOptions);
begin
  otcExecutor.Options := value;
end; { TOmniTaskControl.SetOptions }

function TOmniTaskControl.SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl;
begin
  otcParameters.Add(paramValue, paramName);
  Result := Self;
end; { TOmniTaskControl.SetParameter }

function TOmniTaskControl.SetParameter(const paramValue: TOmniValue): IOmniTaskControl;
begin
  Result := SetParameter('', paramValue);
end; { TOmniTaskControl.SetParameter }

function TOmniTaskControl.SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
begin
  otcParameters.AssignNamed(parameters);
  Result := Self;
end; { TOmniTaskControl.SetParameters }

function TOmniTaskControl.SetPriority(threadPriority: TOTLThreadPriority):
  IOmniTaskControl;
begin
  otcExecutor.Priority := threadPriority;
  Result := Self;
end; { TOmniTaskControl.SetPriority }

function TOmniTaskControl.SetQueueSize(numMessages: integer): IOmniTaskControl;
begin
  if assigned(otcSharedInfo.CommChannel) then
    raise Exception.Create('TOmniTaskControl.SetQueueSize: Cannot set queue size. ' +
                           'Queue already exists');
  otcQueueLength := numMessages;
  Result := Self;
end; { TOmniTaskControl.SetQueueSize }

function TOmniTaskControl.SetTimer(interval_ms: cardinal): IOmniTaskControl;
begin
  Result := SetTimer(interval_ms, -1);
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetTimer(interval_ms: cardinal; const timerMessage:
  TOmniMessageID): IOmniTaskControl;
begin
  Result := SetTimer(0, interval_ms, timerMessage);
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TOmniMessageID): IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(timerID, interval_ms, timerMessage);
  Result := Self;
end; { TOmniTaskControl.SetTimer }

{$IFDEF OTL_Anonymous}
function TOmniTaskControl.SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TProc): IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(timerID, interval_ms, TOmniMessageID.Create(
    procedure (timerID: integer)
    begin
      timerMessage()
    end));
  Result := Self;
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetTimer(timerID: integer; interval_ms: cardinal;
  const timerMessage: TProc<integer>): IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(timerID, interval_ms, TOmniMessageID.Create(timerMessage));
  Result := Self;
end; { TOmniTaskControl.SetTimer }
{$ENDIF OTL_Anonymous}

function TOmniTaskControl.SetUserData(const idxData: TOmniValue; const value:
  TOmniValue): IOmniTaskControl;
begin
  SetUserDataVal(idxData, value);
  Result := Self;
end; { TOmniTaskControl.SetUserData }

procedure TOmniTaskControl.SetUserDataVal(const idxData: TOmniValue; const value:
  TOmniValue);
begin
  if idxData.IsInteger then
    otcUserData.Insert(idxData, value)
  else if idxData.IsString then
    otcUserData.Add(value, idxData)
  else
    raise Exception.Create('UserData can only be indexed by integer or string.');
end; { TOmniTaskControl.SetUserDataInt }

procedure TOmniTaskControl.Stop;
begin
  if assigned(otcSharedInfo) then begin
    otcSharedInfo.Terminating := true;
    SetEvent(otcSharedInfo.TerminateEvent);
  end;
end; { TOmniTaskControl.Stop }

function TOmniTaskControl.Terminate(maxWait_ms: cardinal): boolean;
var
  msg: TOmniMessage;
begin
  //TODO : reset executor and exit immediately if task was not started at all or raise exception?
  if otcInEventHandler then begin
    otcDelayedTerminate := true;
    Result := true;
    Exit;
  end;
  otcExecutor.Terminating := true;
  Stop;
  Result := WaitFor(maxWait_ms);
  while Comm.Receive(msg) do
    ForwardTaskMessage(msg);
  if otcEventMonitorInternal and assigned(otcEventMonitor) then begin
    //! must process monitor messages first
    {$IFDEF MSWINDOWS}
    TOmniEventMonitor(otcEventMonitor).ProcessMessages;
    {$ENDIF MSWINDOWS}
    DestroyMonitor;
  end;
  if not Result then begin
    if assigned(otcThread) then begin
      {$IFDEF MSWINDOWS}
      TerminateThread(otcThread.Handle, cardinal(-1));
      {$ELSE}
      otcThread.Terminate;
      {$ENDIF MSWINDOWS}
      otcThread := nil;
    end
    else if assigned(otcOwningPool) then begin
      otcOwningPool.Cancel(UniqueID);
      otcOwningPool := nil;
    end;
  end;
  if assigned(otcOnMessageList) then
    otcOnMessageList.Clear;
  FreeAndNil(otcOnMessageExec);
  FreeAndNil(otcOnTerminatedExec);
end; { TOmniTaskControl.Terminate }

function TOmniTaskControl.TerminateWhen(event: TOmniTransitionEvent): IOmniTaskControl;
begin
  otcExecutor.TerminateWhen(event);
  Result := Self;
end; { TOmniTaskControl.TerminateWhen }

function TOmniTaskControl.TerminateWhen(token: IOmniCancellationToken): IOmniTaskControl;
begin
  if not assigned(otcTerminateTokens) then
    otcTerminateTokens := TInterfaceList.Create;
  otcTerminateTokens.Add(token);
  {$IFDEF MSWINDOWS}
  otcExecutor.TerminateWhen(token.Handle);
  {$ELSE}
  otcExecutor.TerminateWhen(token.Event);
  {$ENDIF ~MSWINDOWS}
  Result := Self;
end; { TOmniTaskControl.TerminateWhen }

function TOmniTaskControl.Unobserved: IOmniTaskControl;
begin
  { TODO 1 -oPrimoz Gabrijelcic : reimplement without the internal monitor }
  CreateInternalMonitor;
  Result := Self;
end; { TOmniTaskControl.Unobserved }

function TOmniTaskControl.WaitFor(maxWait_ms: cardinal): boolean;
{$IFNDEF MSWINDOWS}
var
  DualWaiter: TSynchroWaitFor;
  Signaller : IOmniSynchro;
{$ENDIF ~MSWINDOWS}
begin
  if assigned(otcThread) then begin
    {$IFDEF MSWINDOWS}
    Result := DSiWaitForTwoObjects(otcSharedInfo.TerminatedEvent, otcThread.Handle, false, maxWait_ms) in [WAIT_OBJECT_0, WAIT_OBJECT_1]
    {$ELSE}
    DualWaiter := TSynchroWaitFor.Create([otcSharedInfo.TerminatedEvent, otcThread.ThreadTerminationEvent], FMultiWaitLock);
    try
      Result     := DualWaiter.WaitAny(maxWait_ms, Signaller) = wrSignaled;
    finally DualWaiter.Free; end;
    {$ENDIF ~MSWINDOWS}
  end
  else begin
    {$IFDEF MSWINDOWS}
    Result := WaitForSingleObject(otcSharedInfo.TerminatedEvent, maxWait_ms) = WAIT_OBJECT_0;
    {$ELSE}
    Result := otcSharedInfo.TerminatedEvent.WaitFor(maxWait_ms) = wrSignaled;
    {$ENDIF ~MSWINDOWS}
  end;
end; { TOmniTaskControl.WaitFor }

function TOmniTaskControl.WaitForInit: boolean;
begin
  Result := otcExecutor.WaitForInit;
end; { TOmniTaskControl.WaitForInit }

function TOmniTaskControl.WithCounter(const counter: IOmniCounter): IOmniTaskControl;
begin
  otcSharedInfo.Counter := counter;
  Result := Self;
end; { TOmniTaskControl.WithCounter }

function TOmniTaskControl.WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl;
begin
  otcSharedInfo.Lock := lock;
  otcDestroyLock := autoDestroyLock;
  Result := Self;
end; { TOmniTaskControl.WithLock }

function TOmniTaskControl.WithLock(const lock: IOmniCriticalSection): IOmniTaskControl;
begin
  Result := WithLock(lock.GetSyncObj, false);
end; { TOmniTaskControl.WithLock }

{ TOmniThread }

constructor TOmniThread.Create(task: IOmniTask);
begin
  inherited Create(true);
  otTask := task;
end; { TOmniThread.Create }

procedure TOmniThread.Execute;
var
  taskName: string;
begin
  taskName := task.Name;
  SendThreadNotifications(tntCreate, taskName);
  try
    (otTask as IOmniTaskExecutor).Execute;
    // task reference may not be valid anymore
  finally SendThreadNotifications(tntDestroy, taskName); end;
end; { TOmniThread.Execute }


{$IFNDEF MSWINDOWS}
procedure TOmniThread.DoTerminate;
begin
  inherited;
  otThreadTerminationEvent.SetEvent;
end; { TOmniThread.DoTerminate }
{$ENDIF}

{ TOmniTaskControlListEnumerator }

constructor TOmniTaskControlListEnumerator.Create(taskList: TInterfaceList);
begin
  otcleTaskEnum := taskList.GetEnumerator;
end; { TOmniTaskControlListEnumerator.Create }

destructor TOmniTaskControlListEnumerator.Destroy;
begin
  FreeAndNil(otcleTaskEnum);
  inherited;
end; { TOmniTaskControlListEnumerator.Destroy }

function TOmniTaskControlListEnumerator.GetCurrent: IOmniTaskControl;
begin
  Result := otcleTaskEnum.GetCurrent as IOmniTaskControl;
end; { TOmniTaskControlListEnumerator.GetCurrent }

function TOmniTaskControlListEnumerator.MoveNext: boolean;
begin
  Result := otcleTaskEnum.MoveNext;
end; { TOmniTaskControlListEnumerator.MoveNext }

{ TOmniTaskControlList }

constructor TOmniTaskControlList.Create;
begin
  inherited Create;
  otclList := TInterfaceList.Create;
end; { TOmniTaskControlList.Create }

destructor TOmniTaskControlList.Destroy;
begin
  FreeAndNil(otclList);
  inherited Destroy;
end; { TOmniTaskControlList.Destroy }

function TOmniTaskControlList.Add(const item: IOmniTaskControl): integer;
begin
  Result := otclList.Add(item);
end; { TOmniTaskControlList.Add }

procedure TOmniTaskControlList.Clear;
begin
  otclList.Clear;
end; { TOmniTaskControlList.Clear }

procedure TOmniTaskControlList.Delete(idxItem: integer);
begin
  otclList.Delete(idxItem);
end; { TOmniTaskControlList.Delete }

procedure TOmniTaskControlList.Exchange(idxItem1, idxItem2: integer);
begin
  otclList.Exchange(idxItem1, idxItem2);
end; { TOmniTaskControlList.Exchange }

function TOmniTaskControlList.First: IOmniTaskControl;
begin
  Result := otclList.First as IOmniTaskControl;
end; { TOmniTaskControlList.First }

function TOmniTaskControlList.Get(idxItem: integer): IOmniTaskControl;
begin
  Result := otclList[idxItem] as IOmniTaskControl;
end; { TOmniTaskControlList.Get }

function TOmniTaskControlList.GetCapacity: integer;
begin
  Result := otclList.Capacity;
end; { TOmniTaskControlList.GetCapacity }

function TOmniTaskControlList.GetCount: integer;
begin
  Result := otclList.Count;
end; { TOmniTaskControlList.GetCount }

function TOmniTaskControlList.GetEnumerator: IOmniTaskControlListEnumerator;
begin
  Result := TOmniTaskControlListEnumerator.Create(otclList);
end; { TOmniTaskControlList.GetEnumerator }

function TOmniTaskControlList.IndexOf(const item: IOmniTaskControl): integer;
begin
  Result := otclList.IndexOf(item);
end; { TOmniTaskControlList.IndexOf }

function TOmniTaskControlList.IndexOfID(uniqueID: int64): integer;
begin
  otclList.Lock;
  try
    for Result := 0 to Count - 1 do
      if Items[Result].UniqueID = uniqueID then
        Exit;
    Result := -1;
  finally otclList.Unlock; end;
end; { TOmniTaskControlList.IndexOfID }

procedure TOmniTaskControlList.Insert(idxItem: integer; const item: IOmniTaskControl);
begin
  otclList.Insert(idxItem, item);
end; { TOmniTaskControlList.Insert }

function TOmniTaskControlList.Last: IOmniTaskControl;
begin
  Result := otclList.Last as IOmniTaskControl;
end; { TOmniTaskControlList.Last }

procedure TOmniTaskControlList.Put(idxItem: integer; const value: IOmniTaskControl);
begin
  otclList[idxItem] := value;
end; { TOmniTaskControlList.Put }

function TOmniTaskControlList.Remove(const item: IOmniTaskControl): integer;
begin
  Result := otclList.Remove(item);
end; { TOmniTaskControlList.Remove }

function TOmniTaskControlList.RemoveByID(uniqueID: int64): integer;
begin
  otclList.Lock;
  try
    Result := IndexOfID(uniqueID);
    if Result >= 0 then
      otclList.Remove(Items[Result]);
  finally otclList.Unlock; end;
end; { TOmniTaskControlList.RemoveByID }

procedure TOmniTaskControlList.SetCapacity(const value: integer);
begin
  otclList.Capacity := value;
end; { TOmniTaskControlList.SetCapacity }

procedure TOmniTaskControlList.SetCount(const value: integer);
begin
  otclList.Count := value;
end; { TOmniTaskControlList.SetCount }

{ TOmniTaskGroup }

constructor TOmniTaskGroup.Create;
begin
  inherited Create;
  otgTaskList := TOmniTaskControlList.Create;
end; { TOmniTaskGroup.Create }

destructor TOmniTaskGroup.Destroy;
begin
  AutoUnregisterComms;
  inherited Destroy;
end; { TOmniTaskGroup.Destroy }

function TOmniTaskGroup.Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
begin
  otgTaskList.Add(taskControl);
  Result := Self;
end; { TOmniTaskGroup.Add }

procedure TOmniTaskGroup.AutoUnregisterComms;
begin
  if assigned(otgRegisteredWith) then
    InternalUnregisterAllCommFrom(otgRegisteredWith);
end; { TOmniTaskGroup.AutoUnregisterComms }

function TOmniTaskGroup.GetEnumerator: IOmniTaskControlListEnumerator;
begin
  Result := otgTaskList.GetEnumerator;
end; { TOmniTaskGroup.GetEnumerator }

function TOmniTaskGroup.GetTasks: IOmniTaskControlList;
begin
  Result := otgTaskList;
end; { TOmniTaskGroup.GetTasks }

procedure TOmniTaskGroup.InternalUnregisterAllCommFrom(const task: IOmniTask);
var
  groupTask: IOmniTaskControl;
begin
  for groupTask in Self do
    task.UnregisterComm(groupTask.Comm);
  otgRegisteredWith := nil;
end; { TOmniTaskGroup.InternalUnregisterAllCommFrom }

function TOmniTaskGroup.RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
var
  groupTask: IOmniTaskControl;
begin
  AutoUnregisterComms;
  for groupTask in Self do
    task.RegisterComm(groupTask.Comm);
  otgRegisteredWith := task;
  Result := Self;
end; { TOmniTaskGroup.RegisterAllCommWith }

function TOmniTaskGroup.Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
begin
  otgTaskList.Remove(taskControl);
  Result := Self;
end; { TOmniTaskGroup.Remove }

function TOmniTaskGroup.RunAll: IOmniTaskGroup;
var
  iIntf: integer;
begin
  for iIntf := 0 to otgTaskList.Count - 1 do
    (otgTaskList[iIntf] as IOmniTaskControl).Run;
  Result := Self;
end; { TOmniTaskGroup.RunAll }

procedure TOmniTaskGroup.SendToAll(const msg: TOmniMessage);
var
  groupTask: IOmniTaskControl;
begin
  for groupTask in Self do
    groupTask.Comm.Send(msg);
end; { TOmniTaskGroup.SendToAll }

function TOmniTaskGroup.TerminateAll(maxWait_ms: cardinal): boolean;
var
  iIntf: integer;
begin
  for iIntf := 0 to otgTaskList.Count - 1 do
    (otgTaskList[iIntf] as IOmniTaskControl).Stop;
  Result := WaitForAll(maxWait_ms);
end; { TOmniTaskGroup.TerminateAll }

function TOmniTaskGroup.UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
begin
  InternalUnregisterAllCommFrom(task);
  Result := Self;
end; { TOmniTaskGroup.UnregisterAllCommFrom }

function TOmniTaskGroup.WaitForAll(maxWait_ms: cardinal): boolean;
var
  {$IFDEF MSWINDOWS}
  iIntf      : integer;
  waitHandles: array of TOmniTransitionEvent;
  {$ELSE}
  MultiWaiter: TSynchroWaitFor;
  Signaller  : IOmniSynchro;
  Syncs      : array of IOmniSynchro;
  Idx        : integer;
  {$ENDIF ~MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  SetLength(waitHandles, otgTaskList.Count);
  for iIntf := 0 to otgTaskList.Count - 1 do
    waitHandles[iIntf] := (otgTaskList[iIntf] as IOmniTaskControlInternals).TerminatedEvent;
  Result := WaitForAllObjects(waitHandles, maxWait_ms);
  {$ELSE}
  SetLength(Syncs, otgTaskList.Count);
  for Idx := 0 to otgTaskList.Count - 1 do
    Syncs[Idx] := (otgTaskList[Idx] as IOmniTaskControlInternals).TerminatedEvent;
  MultiWaiter := TSynchroWaitFor.Create(Syncs, FMultiWaitLock);
  try
    Result := MultiWaiter.WaitAny(maxWait_ms, Signaller) = wrSignaled;
  finally MultiWaiter.Free; end;
  {$ENDIF ~MSWINDOWS}
end; { TOmniTaskGroup.WaitForAll }

{ TOmniTaskControlEventMonitor }

constructor TOmniTaskControlEventMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTaskMessage := ForwardTaskMessage;
  OnTaskTerminated := ForwardTaskTerminated;
end; { TOmniTaskControlEventMonitor.Create }

procedure TOmniTaskControlEventMonitor.ForwardTaskMessage(
  const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  (task as IOmniTaskControlInternals).ForwardTaskMessage(msg);
end; { TOmniTaskControlEventMonitor.ForwardTaskMessage }

procedure TOmniTaskControlEventMonitor.ForwardTaskTerminated(
  const task: IOmniTaskControl);
begin
  (task as IOmniTaskControlInternals).ForwardTaskTerminated;
end; { TOmniTaskControlEventMonitor.ForwardTaskTerminated }

{ TOmniTaskControlEventMonitorPool }

constructor TOmniTaskControlEventMonitorPool.Create;
begin
  inherited Create;
  monitorPool := TOmniEventMonitorPool.Create;
  monitorPool.MonitorClass := TOmniTaskControlEventMonitor;
end; { TOmniTaskControlEventMonitorPool.Create }

destructor TOmniTaskControlEventMonitorPool.Destroy;
begin
  FreeAndNil(monitorPool);
  inherited;
end; { TOmniTaskControlEventMonitorPool.Destroy }

function TOmniTaskControlEventMonitorPool.Allocate: TOmniTaskControlEventMonitor;
begin
  Result := monitorPool.Allocate as TOmniTaskControlEventMonitor;
end; { TOmniTaskControlEventMonitorPool.Allocate }

procedure TOmniTaskControlEventMonitorPool.Release(monitor: TOmniTaskControlEventMonitor);
begin
  monitorPool.Release(monitor);
end; { TOmniTaskControlEventMonitorPool.Release }

{ TOmniSharedTaskInfo }

constructor TOmniSharedTaskInfo.Create;
begin
  inherited;
  ostiNUMANode := -1; //any
  ostiProcessorGroup := -1; //any
end; { TOmniSharedTaskInfo.Create }

function TOmniSharedTaskInfo.GetCancellationToken: IOmniCancellationToken;
var
  token: IOmniCancellationToken;
begin
  Assert(NativeInt(@ostiCancellationToken) mod SizeOf(pointer) = 0,
    'TOmniSharedTaskInfo.GetCancellationToken: ostiCancellationToken is not 4-aligned!');
  Assert(NativeInt(@token) mod SizeOf(pointer) = 0,
    'TOmniSharedTaskInfo.GetCancellationToken: ostiCancellationToken is not 4-aligned!');
  Assert(SizeOf(IOmniCancellationToken) = SizeOf(pointer));

  if not assigned(ostiCancellationToken) then begin
    token := CreateOmniCancellationToken;
    if TInterlockedEx.CAS(nil, pointer(token), ostiCancellationToken) then
      pointer(token) := nil;
  end;
  Result := ostiCancellationToken;
end; { TOmniSharedTaskInfo.GetCancellationToken }

procedure TOmniSharedTaskInfo.SetCancellationToken(const token: IOmniCancellationToken);
begin
  // SetCancellationToken can only be called before the task is is created
  ostiCancellationToken := token;
end; { TOmniSharedTaskInfo.SetCancellationToken }

{ TOmniMessageExec }

constructor TOmniMessageExec.Create(exec: TOmniTaskMessageEvent);
begin
  inherited Create;
  SetOnMessage(exec);
end; { TOmniMessageExec.Create }

constructor TOmniMessageExec.Create(exec: TOmniTaskTerminatedEvent);
begin
  inherited Create;
  SetOnTerminated(exec);
end; { TOmniMessageExec.Create }

constructor TOmniMessageExec.Create(dispatch: TObject);
begin
  inherited Create;
  omeOnMessageDispatch := dispatch;
end; { TOmniMessageExec.Create }

constructor TOmniMessageExec.Clone(exec: TOmniMessageExec);
begin
  inherited Create;
  omeOnMessage := exec.omeOnMessage;
  omeOnMessageDispatch := exec.omeOnMessageDispatch;
  omeOnTerminated := exec.omeOnTerminated;
end; { TOmniMessageExec.Clone }

procedure TOmniMessageExec.OnMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
var
  msg1: TOmniMessage;
begin
  if assigned(omeOnMessageDispatch) then begin
    msg1 := msg;
    omeOnMessageDispatch.Dispatch(msg1);
  end
  else case omeOnMessage.Kind of
    {$IFDEF OTL_Anonymous}
    oekDelegate: TOmniOnMessageFunction(TProc(omeOnMessage))(task, msg);
    {$ENDIF OTL_Anonymous}
    oekMethod: TOmniTaskMessageEvent(TMethod(omeOnMessage))(task, msg);
    else raise Exception.CreateFmt('TOmniMessageExec.OnMessage: Unexpected kind %s',
      [GetEnumName(TypeInfo(TOmniExecutableKind), Ord(omeOnMessage.Kind))]);
  end;
end; { TOmniMessageExec.OnMessage }

procedure TOmniMessageExec.OnTerminated(const task: IOmniTaskControl);
begin
  case omeOnTerminated.Kind of
    {$IFDEF OTL_Anonymous}
    oekDelegate:
      if assigned(TProc(omeOnTerminated)) then
        TOmniOnTerminatedFunction(TProc(omeOnTerminated))(task);
    {$ENDIF OTL_Anonymous}
    oekMethod:
      if TMethod(omeOnTerminated).Code <> nil then
        TOmniTaskTerminatedEvent(TMethod(omeOnTerminated))(task);
    else raise Exception.CreateFmt('TOmniMessageExec.OnTerminated: Unexpected kind %s',
      [GetEnumName(TypeInfo(TOmniExecutableKind), Ord(omeOnTerminated.Kind))]);
  end;
end; { TOmniMessageExec.OnTerminate }

procedure TOmniMessageExec.SetOnMessage(exec: TOmniTaskMessageEvent);
begin
  omeOnMessage := TMethod(exec);
end; { TOmniMessageExec.SetOnMessage }

procedure TOmniMessageExec.SetOnTerminated(exec: TOmniTaskTerminatedEvent);
begin
  omeOnTerminated := TMethod(exec);
end; { TOmniMessageExec.SetOnTerminated }

{$IFDEF OTL_Anonymous}
constructor TOmniMessageExec.Create(exec: TOmniOnMessageFunction);
begin
  inherited Create;
  SetOnMessage(exec);
end; { TOmniMessageExec.Create }

constructor TOmniMessageExec.Create(exec: TOmniOnTerminatedFunction);
begin
  inherited Create;
  SetOnTerminated(exec);
end; { TOmniMessageExec.Create }

procedure TOmniMessageExec.Apply(msgID: word; task: IOmniTaskControl);
begin
  case omeOnMessage.Kind of
    oekDelegate:
      task.OnMessage(msgID, TOmniOnMessageFunction(TProc(omeOnMessage)));
    oekMethod:
      task.OnMessage(msgID, TOmniTaskMessageEvent(TMethod(omeOnMessage)));
    else raise Exception.CreateFmt('TOmniMessageExec.Apply: Unexpected kind %s',
      [GetEnumName(TypeInfo(TOmniExecutableKind), Ord(omeOnMessage.Kind))]);
  end;
end; { TOmniMessageExec.Apply }

procedure TOmniMessageExec.SetOnMessage(exec: TOmniOnMessageFunction);
begin
  omeOnMessage.SetDelegate(exec);
end; { TOmniMessageExec.SetOnMessage }

procedure TOmniMessageExec.SetOnTerminated(exec: TOmniOnTerminatedFunction);
begin
  omeOnTerminated.SetDelegate(exec);
end; { TOmniMessageExec.SetOnTerminated }
{$ENDIF OTL_Anonymous}

initialization
  GTaskControlEventMonitorPool := TOmniTaskControlEventMonitorPool.Create;
finalization
  FreeAndNil(GTaskControlEventMonitorPool);
end.

