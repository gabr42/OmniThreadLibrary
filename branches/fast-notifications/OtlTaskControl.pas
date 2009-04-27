///<summary>Task control encapsulation. Part of the OmniThreadLibrary project.</summary>
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
///   Creation date     : 2008-06-12
///   Last modification : 2009-02-08
///   Version           : 1.09
///</para><para>
///   History:
///     1.09: 2009-02-08
///       - Implemented per-thread task data storage.
///     1.08: 2009-01-26
///       - Implemented IOmniTaskControl.Enforced decorator.
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
///  - Lock my Object… Please!, Allen Bauer,
///    http://blogs.codegear.com/abauer/2008/02/19/38856
///  - Threading in C#, Joseph Albahari, http://www.albahari.com/threading/
///  - Coordination Data Structures Overview, Emad Omara,
///    http://blogs.msdn.com/pfxteam/archive/2008/06/18/8620615.aspx
///  - Erlang, http://en.wikipedia.org/wiki/Erlang_(programming_language)
///  - A single-word reader/writer spin lock,
///    http://www.bluebytesoftware.com/blog/2009/01/30/ASinglewordReaderwriterSpinLock.aspx

// TODO 3 -oPrimoz Gabrijelcic : Implement message bus (subscribe/publish)
// http://209.85.129.132/search?q=cache:B2PbIgFSyLcJ:www.dojotoolkit.org/book/dojo-book-0-9/part-3-programmatic-dijit-and-dojo/event-system/publish-and-subscribe-events+dojo+subscribe+publish&hl=sl&client=opera&strip=1
// http://msdn.microsoft.com/en-us/library/ms978583.aspx

{$IF CompilerVersion >= 20}
  {$DEFINE OTL_Anonymous}
{$IFEND}
{$WARN SYMBOL_PLATFORM OFF}

unit OtlTaskControl;

interface

uses
  Windows, SysUtils, Variants, Classes, SyncObjs, Messages, TypInfo, ObjAuto,
  GpStuff,
  OtlCommon,
  OtlSync,
  OtlComm,
  OtlTask,
  OtlContainers,
  OtlThreadPool,
  OtlContainerObserver,
  DetailedRTTI,
  DSiWin32,
  GpLists,
  GpStringHash;

type
  IOmniTaskControl = interface;
  TOmniSharedTaskInfo = class;
  
  IOmniTaskControlMonitor = interface ['{20CB3AB7-04D8-454B-AEFE-CFCFF8F27301}']
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl;
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl;
  end; { IOmniTaskControlMonitor }

  {$TYPEINFO ON} {$METHODINFO ON}
  IOmniWorker = interface ['{CA63E8C2-9B0E-4BFA-A527-31B2FCD8F413}']
    function  GetImplementor: TObject;
    function  GetTask: IOmniTask;
    procedure SetExecutor(executor: TObject);
    procedure SetTask(const value: IOmniTask);
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
  protected
    procedure Cleanup; virtual;
    procedure DispatchMessage(var msg: TOmniMessage); virtual;
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

  IOmniTaskControl = interface ['{881E94CB-8C36-4CE7-9B31-C24FD8A07555}']
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetExitCode: integer;
    function  GetExitMessage: string;
    function  GetLock: TSynchroObject;
    function  GetName: string;
    function  GetUniqueID: int64;
  //
    function  Alertable: IOmniTaskControl;
    function  ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean = false): IOmniTaskControl;
    function  Enforced(forceExecution: boolean = true): IOmniTaskControl;
    function  GetSharedInfo: TOmniSharedTaskInfo;
    function  Invoke(const msgMethod: pointer): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload;
    function  Invoke(const msgName: string): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload;
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(const threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessageID: integer = -1): IOmniTaskControl; overload;
    function  SetTimer(interval_ms: cardinal; const timerMessageName: string): IOmniTaskControl; overload;
    function  SetTimer(interval_ms: cardinal; const timerMethod: pointer): IOmniTaskControl; overload;
    function  SetQueueSize(numMessages: integer): IOmniTaskControl;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskControl;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl;
  //
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property SharedInfo: TOmniSharedTaskInfo read GetSharedInfo;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTaskControl }

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
    procedure Insert(idxItem: integer; const item: IOmniTaskControl);
    function  Last: IOmniTaskControl;
    function  Remove(const item: IOmniTaskControl): integer;
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
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  GetEnumerator: IOmniTaskControlListEnumerator;
    function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    procedure SendToAll(const msg: TOmniMessage); 
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  end; { IOmniTaskGroup }

  TOmniSharedTaskInfo = class
  strict private
    ostiChainIgnoreErrors : boolean;
    ostiChainTo           : IOmniTaskControl;
    ostiCommChannel       : IOmniTwoWayChannel;
    ostiCounter           : IOmniCounter;
    ostiLock              : TSynchroObject;
    ostiMonitorWindow     : THandle;
    ostiStopped           : boolean;
    ostiTaskName          : string;
    ostiTerminatedEvent   : THandle;
    ostiTerminateEvent    : THandle;
    ostiTerminating       : boolean;
    ostiUniqueID          : int64;
  public
    property ChainIgnoreErrors: boolean read ostiChainIgnoreErrors write ostiChainIgnoreErrors;
    property ChainTo: IOmniTaskControl read ostiChainTo write ostiChainTo;
    property CommChannel: IOmniTwoWayChannel read ostiCommChannel write ostiCommChannel;
    property Counter: IOmniCounter read ostiCounter write ostiCounter;
    property Lock: TSynchroObject read ostiLock write ostiLock;
    property MonitorWindow: THandle read ostiMonitorWindow write ostiMonitorWindow; // TODO 1 -oPrimoz Gabrijelcic : Why do we need it?
    property Stopped: boolean read ostiStopped write ostiStopped;
    property TaskName: string read ostiTaskName write ostiTaskName;
    property TerminatedEvent: THandle read ostiTerminatedEvent write ostiTerminatedEvent;
    property TerminateEvent: THandle read ostiTerminateEvent write ostiTerminateEvent;
    property Terminating: boolean read ostiTerminating write ostiTerminating;
    property UniqueID: int64 read ostiUniqueID write ostiUniqueID;
  end; { TOmniSharedTaskInfo }

  function CreateTask(worker: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(const worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
//  function CreateTask(worker: IOmniTaskGroup; const taskName: string = ''): IOmniTaskControl; overload;

{$IFDEF OTL_Anonymous}
type
  TOmniTaskFunction = reference to procedure (task: IOmniTask);
  function CreateTask(worker: TOmniTaskFunction; const taskName: string = ''): IOmniTaskControl; overload;
{$ENDIF OTL_Anonymous}

  function CreateTaskGroup: IOmniTaskGroup;

  function CreateTaskControlList: IOmniTaskControlList;

type
  TOmniInternalMessageType = (imtStringMsg, imtAddressMsg);

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
    class function CreateMessage(const msgMethod: pointer; msgData: TOmniValue):
      TOmniMessage; inline;
    class procedure UnpackMessage(const msg: TOmniMessage; var msgMethod: pointer; var
      msgData: TOmniValue); inline;
    constructor Create(const msgMethod: pointer; const msgData: TOmniValue);
    property MsgData: TOmniValue read ismMsgData;
    property MsgMethod: pointer read ismMsgMethod;
  end; { TOmniInternalAddressMsg }

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

  TOmniTaskExecutor = class
  strict private type
    TOmniMessageInfo = record
      IdxFirstMessage  : cardinal;
      IdxFirstTerminate: cardinal;
      IdxLastMessage   : cardinal;
      IdxLastTerminate : cardinal;
      IdxRebuildHandles: cardinal;
      NumWaitHandles   : cardinal;
      WaitFlags        : DWORD;
      WaitHandles      : array [0..63] of THandle;
      WaitWakeMask     : DWORD;
    end;
  strict private // those must be 4-aligned, keep them on the top
    oteInternalLock      : TOmniCS;
    oteOptionsLock       : TOmniCS;
  strict private
    oteCommList          : TInterfaceList;
    oteCommRebuildHandles: THandle;
    oteExecutorType      : TOmniExecutorType;
    oteExitCode          : TGp4AlignedInt;
    oteExitMessage       : string;
    {$IFDEF OTL_Anonymous}
    oteFunc              : TOmniTaskFunction;
    {$ENDIF OTL_Anonymous}
    oteLastTimer_ms      : int64;
    oteMethod            : TOmniTaskMethod;
    oteMethodHash        : TGpStringObjectHash;
    oteMsgInfo           : TOmniMessageInfo;
    oteOptions           : TOmniTaskControlOptions;
    otePriority          : TOTLThreadPriority;
    oteProc              : TOmniTaskProcedure;
    oteTerminateHandles  : TGpIntegerList;
    oteTimerInterval_ms  : TGp4AlignedInt;
    oteTimerMessageID    : TGp4AlignedInt;
    oteTimerMessageMethod: TGp4AlignedInt;
    oteTimerMessageName  : string;
    oteWakeMask          : DWORD;
    oteWaitHandlesGen    : int64;
    oteWorkerInitialized : THandle;
    oteWorkerInitOK      : boolean;
    oteWorkerIntf        : IOmniWorker;
  strict protected
    procedure CallOmniTimer;
    procedure Cleanup;
    procedure DispatchMessages(const task: IOmniTask);
    procedure DispatchOmniMessage(msg: TOmniMessage);
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string;
    procedure GetMethodAddrAndSignature(const methodName: string;
      var methodAddress: pointer; var methodSignature: TOmniInvokeType);
    procedure GetMethodNameFromInternalMessage(const msg: TOmniMessage;
      var msgName: string; var msgData: TOmniValue);
    function  GetOptions: TOmniTaskControlOptions;
    function  GetTimerInterval_ms: cardinal; inline;
    function  GetTimerMessageID: integer; inline;
    function  GetTimerMessageMethod: pointer;
    function  GetTimerMessageName: string;
    procedure Initialize;
    procedure ProcessThreadMessages;
    procedure RaiseInvalidSignature(const methodName: string);
    procedure RemoveTerminationEvents(const srcMsgInfo: TOmniMessageInfo; var dstMsgInfo:
      TOmniMessageInfo);
    procedure SetOptions(const value: TOmniTaskControlOptions);
    procedure SetTimerInt(interval_ms: cardinal; timerMsgID: integer; const timerMsgName:
      string; const timerMsgMethod: pointer);
    procedure SetTimerInterval_ms(const value: cardinal);
    procedure SetTimerMessageID(const value: integer);
    procedure SetTimerMessageMethod(const value: pointer);
    procedure SetTimerMessageName(const value: string);
  protected
    function  DispatchEvent(awaited: cardinal; const task: IOmniTask; var msgInfo:
      TOmniMessageInfo): boolean; virtual;
    procedure MainMessageLoop(const task: IOmniTask; var msgInfo: TOmniMessageInfo); virtual;
    procedure MessageLoopPayload; virtual;
    procedure ProcessMessages(task: IOmniTask); virtual;
    procedure RebuildWaitHandles(const task: IOmniTask; var msgInfo: TOmniMessageInfo); virtual;
    function  TimeUntilNextTimer_ms: cardinal; virtual;
    function  WaitForEvent(msgInfo: TOmniMessageInfo; timeout_ms: cardinal): cardinal; virtual;
  public
    constructor Create(const workerIntf: IOmniWorker); overload;
    constructor Create(method: TOmniTaskMethod); overload;
    constructor Create(proc: TOmniTaskProcedure); overload;
    {$IFDEF OTL_Anonymous}
    constructor Create(func: TOmniTaskFunction); overload;
    {$ENDIF OTL_Anonymous}
    destructor  Destroy; override;
    procedure Asy_Execute(const task: IOmniTask);
    procedure Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure Asy_SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Asy_SetTimer(interval_ms: cardinal; timerMsgID: integer); overload;
    procedure Asy_SetTimer(interval_ms: cardinal; const timerMethod: pointer); overload;
    procedure Asy_SetTimer(interval_ms: cardinal; const timerMsgName: string); overload;
    procedure Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure EmptyMessageQueues(const task: IOmniTask);
    procedure TerminateWhen(handle: THandle);
    function WaitForInit: boolean;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Options: TOmniTaskControlOptions read GetOptions write SetOptions;
    property Priority: TOTLThreadPriority read otePriority write otePriority;
    property TimerInterval_ms: cardinal read GetTimerInterval_ms write SetTimerInterval_ms;
    property TimerMessageID: integer read GetTimerMessageID write SetTimerMessageID;
    property TimerMessageMethod: pointer read GetTimerMessageMethod write
      SetTimerMessageMethod;
    property TimerMessageName: string read GetTimerMessageName write SetTimerMessageName;
    property WakeMask: DWORD read oteWakeMask write oteWakeMask;
    property WorkerInitialized: THandle read oteWorkerInitialized;
    property WorkerInitOK: boolean read oteWorkerInitOK;
    property WorkerIntf: IOmniWorker read oteWorkerIntf;
  end; { TOmniTaskExecutor }

  TOmniTask = class(TInterfacedObject, IOmniTask, IOmniTaskExecutor)
  strict private
    otExecuting     : boolean;
    otExecutor_ref  : TOmniTaskExecutor;
    otParameters_ref: TOmniValueContainer;
    otSharedInfo    : TOmniSharedTaskInfo;
    otThreadData    : IInterface;
  protected
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetCounter: IOmniCounter;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetParam(idxParam: integer): TOmniValue; inline;
    function  GetParamByName(const paramName: string): TOmniValue; inline;
    function  GetTerminateEvent: THandle; inline;
    function  GetThreadData: IInterface; inline;
    function  GetUniqueID: int64; inline;
    procedure SetThreadData(const value: IInterface); inline;
    procedure Terminate;
  public
    constructor Create(executor: TOmniTaskExecutor; parameters: TOmniValueContainer;
      sharedInfo: TOmniSharedTaskInfo);
    procedure Enforced(forceExecution: boolean = true);
    procedure Execute;
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetTimer(interval_ms: cardinal; timerMessageID: integer = -1); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMethod: pointer); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMessageName: string); overload;
    function  Stopped: boolean;
    procedure StopTimer;
    function  Terminated: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property SharedInfo: TOmniSharedTaskInfo read otSharedInfo;
    property TerminateEvent: THandle read GetTerminateEvent;
    property ThreadData: IInterface read GetThreadData;
    property UniqueID: int64 read GetUniqueID;
  end; { TOmniTask }

  TOmniThread = class(TThread) // Factor this class into OtlThread unit?
  strict private
    otTask: IOmniTask;
  protected
    procedure Execute; override;
  public
    constructor Create(task: IOmniTask);
    property Task: IOmniTask read otTask;
  end; { TOmniThread }

  // TODO 1 -oPrimoz Gabrijelcic : Used in TOmniTaskGroup, maybe it could be removed?
  IOmniTaskControlInternals = interface ['{CE7B53E0-902E-413F-AB6E-B97E7F4B0AD5}']
    function  GetTerminatedEvent: THandle;
  //
    property TerminatedEvent: THandle read GetTerminatedEvent;
  end; { IOmniTaskControlInternals }

  TOmniTaskControl = class(TInterfacedObject, IOmniTaskControl, IOmniTaskControlInternals)
  strict private
    otcDestroyLock    : boolean;
    otcExecutor       : TOmniTaskExecutor;
    otcMonitorObserver: TOmniContainerObserver;
    otcOwningPool     : IOmniThreadPool;
    otcParameters     : TOmniValueContainer;
    otcQueueLength    : integer;
    otcSharedInfo     : TOmniSharedTaskInfo;
    otcThread         : TOmniThread;
  strict protected
    function  CreateTask: IOmniTask;
    procedure EnsureCommChannel; inline;
    procedure Initialize(const taskName: string);
  protected
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string; inline;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetOptions: TOmniTaskControlOptions;
    function  GetSharedInfo: TOmniSharedTaskInfo;
    function  GetTerminatedEvent: THandle;
    function  GetTerminateEvent: THandle;
    function  GetUniqueID: int64; inline;
    procedure SetOptions(const value: TOmniTaskControlOptions);
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
  public
    constructor Create(const worker: IOmniWorker; const taskName: string); overload;
    constructor Create(worker: TOmniTaskMethod; const taskName: string); overload;
    constructor Create(worker: TOmniTaskProcedure; const taskName: string); overload;
    {$IFDEF OTL_Anonymous}
    constructor Create(worker: TOmniTaskFunction; const taskName: string); overload;
    {$ENDIF OTL_Anonymous}
    destructor  Destroy; override;
    function  Alertable: IOmniTaskControl;
    function  ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean = false): IOmniTaskControl;
    function  Enforced(forceExecution: boolean = true): IOmniTaskControl;
//    function  GetSelf: pointer;
    function  Invoke(const msgMethod: pointer): IOmniTaskControl; overload; inline;
    function  Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgMethod: pointer; msgData: TOmniValue): IOmniTaskControl; overload; inline;
    function  Invoke(const msgName: string): IOmniTaskControl; overload; inline;
    function  Invoke(const msgName: string; msgData: array of const): IOmniTaskControl; overload;
    function  Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl; overload; inline;
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(const threadPool: IOmniThreadPool = nil {default pool}):
      IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
    function  SetQueueSize(numMessages: integer): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessageID: integer = -1): IOmniTaskControl; overload;
    function  SetTimer(interval_ms: cardinal; const timerMethod: pointer): IOmniTaskControl; overload;
    function  SetTimer(interval_ms: cardinal; const timerMessageName: string): IOmniTaskControl; overload;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskControl;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Options: TOmniTaskControlOptions read GetOptions write SetOptions;
    property SharedInfo: TOmniSharedTaskInfo read otcSharedInfo;
    property UniqueID: int64 read GetUniqueID;
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
    procedure Insert(idxItem: integer; const item: IOmniTaskControl);
    function  Last: IOmniTaskControl;
    function  Remove(const item: IOmniTaskControl): integer;
  end; { TOmniTaskControlList }

  TOmniTaskGroup = class(TInterfacedObject, IOmniTaskGroup)
  strict private
    otgRegisteredWith: IOmniTask;
    otgTaskList      : IOmniTaskControlList;
  strict protected
    procedure AutoUnregisterComms;
    procedure InternalUnregisterAllCommFrom(const task: IOmniTask);
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
  end; { TOmniTaskGroup }

implementation

uses
  OtlEventMonitor;

{ exports }

{$IFDEF OTL_Anonymous}
function CreateTask(worker: TOmniTaskFunction; const taskName: string = ''): IOmniTaskControl;
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
  FreeAndNil(stringMsg)
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
  FreeAndNil(addr)
end; { TOmniInternalAddressMsg.UnpackMessage }

{ TOmniTask }

constructor TOmniTask.Create(executor: TOmniTaskExecutor; parameters:
  TOmniValueContainer; sharedInfo: TOmniSharedTaskInfo);
begin
  inherited Create;
  otExecutor_ref := executor;
  otParameters_ref := parameters;
  otSharedInfo := sharedInfo;
end; { TOmniTask.Create }

procedure TOmniTask.Enforced(forceExecution: boolean);
begin
  if forceExecution then
    otExecutor_ref.Options := otExecutor_ref.Options + [tcoForceExecution]
  else
    otExecutor_ref.Options := otExecutor_ref.Options - [tcoForceExecution];
end; { TOmniTask.Enforced }

procedure TOmniTask.Execute;
begin
  otExecuting := true;
  try
    try
      {$IFNDEF OTL_DontSetThreadName}
      SetThreadName(otSharedInfo.TaskName);
      {$ENDIF OTL_DontSetThreadName}
      if (tcoForceExecution in otExecutor_ref.Options) or (not Terminated) then
      try
        otExecutor_ref.Asy_Execute(Self);
      except
        on E: Exception do
          SetExitStatus(EXIT_EXCEPTION, E.ClassName + ': ' + E.Message);
      end;
    finally
      if otSharedInfo.MonitorWindow <> 0 then
        Win32Check(PostMessage(otSharedInfo.MonitorWindow, COmniTaskMsg_Terminated,
          integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi)));
    end;
  finally
    otSharedInfo.Stopped := true;
    SetEvent(otSharedInfo.TerminatedEvent);
  end;
  if assigned(otSharedInfo.ChainTo) and
     (otSharedInfo.ChainIgnoreErrors or (otExecutor_ref.ExitCode = EXIT_OK))
  then
    otSharedInfo.ChainTo.Run; // TODO 1 -oPrimoz Gabrijelcic : Should execute the chained task in the same thread (should work when run in a pool  otSharedInfo.ChainTo := nil;
end; { TOmniTask.Execute }

function TOmniTask.GetComm: IOmniCommunicationEndpoint;
begin
  Result := otSharedInfo.CommChannel.Endpoint2;
end; { TOmniTask.GetComm }

function TOmniTask.GetCounter: IOmniCounter;
begin
  Result := otSharedInfo.Counter;
end; { TOmniTask.GetCounter }

function TOmniTask.GetLock: TSynchroObject;
begin
  Result := otSharedInfo.Lock;
end; { GetLock: TSynchroObject }

function TOmniTask.GetName: string;
begin
  Result := otSharedInfo.TaskName;
end; { TOmniTask.GetName }

function TOmniTask.GetParam(idxParam: integer): TOmniValue;
begin
  Result := otParameters_ref.ParamByIdx(idxParam);
end; { TOmniTask.GetParam }

function TOmniTask.GetParamByName(const paramName: string): TOmniValue;
begin
  Result := otParameters_ref.ParamByName(paramName);
end; { TOmniTask.GetParamByName }

function TOmniTask.GetTerminateEvent: THandle;
begin
  Result := otSharedInfo.TerminateEvent;
end; { TOmniTask.GetTerminateEvent }

function TOmniTask.GetThreadData: IInterface;
begin
  Result := otThreadData;
end; { TOmniTask.GetThreadData }

function TOmniTask.GetUniqueID: int64;
begin
  Result := otSharedInfo.UniqueID;
end; { TOmniTask.GetUniqueID }

procedure TOmniTask.RegisterComm(const comm: IOmniCommunicationEndpoint);
begin
  otExecutor_ref.Asy_RegisterComm(comm);
end; { TOmniTask.RegisterComm }

procedure TOmniTask.SetExitStatus(exitCode: integer; const exitMessage: string);
begin
  otExecutor_ref.Asy_SetExitStatus(exitCode, exitMessage);
end; { TOmniTask.SetExitStatus }

procedure TOmniTask.SetTimer(interval_ms: cardinal; const timerMethod: pointer);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMethod);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(interval_ms: cardinal; timerMessageID: integer);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMessageID);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetThreadData(const value: IInterface);
begin
  otThreadData := value;
end; { TOmniTask.SetThreadData }

procedure TOmniTask.SetTimer(interval_ms: cardinal; const timerMessageName: string);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMessageName);
end; { TOmniTask.SetTimer }

function TOmniTask.Stopped: boolean;
begin
  Result := otSharedInfo.Stopped;
end; { TOmniTask.Stopped }

procedure TOmniTask.StopTimer;
begin
  SetTimer(0);
end; { TOmniTask.StopTimer }

procedure TOmniTask.Terminate;
begin
  otSharedInfo.Terminating := true;
  SetEvent(otSharedInfo.TerminateEvent);
  if not otExecuting then //call Execute to run at least cleanup code
    Execute;
end; { TOmniTask.Terminate }

function TOmniTask.Terminated: boolean;
begin
  Result := otSharedInfo.Terminating;
end; { TOmniTask.Terminated }

procedure TOmniTask.UnregisterComm(const comm: IOmniCommunicationEndpoint);
begin
  otExecutor_ref.Asy_UnregisterComm(comm);
end; { TOmniTask.UnregisterComm }

{ TOmniWorker }

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

procedure TOmniWorker.ProcessMessages;
begin
  (owExecutor as TOmniTaskExecutor).ProcessMessages(Task);
end; { TOmniWorker.ProcessMessages }

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

{ TOmniTaskExecutor }

constructor TOmniTaskExecutor.Create(const workerIntf: IOmniWorker);
begin
  oteExecutorType := etWorker;
  oteWorkerIntf := workerIntf;
  workerIntf.SetExecutor(Self);
  Initialize;
end; { TOmniTaskExecutor.Create }

constructor TOmniTaskExecutor.Create(method: TOmniTaskMethod);
begin
  oteExecutorType := etMethod;
  oteMethod := method;
  Initialize;
end; { TOmniTaskExecutor.Create }

constructor TOmniTaskExecutor.Create(proc: TOmniTaskProcedure);
begin
  oteExecutorType := etProcedure;
  oteProc := proc;
  Initialize;
end; { TOmniTaskExecutor.Create }

{$IFDEF OTL_Anonymous}
constructor TOmniTaskExecutor.Create(func: TOmniTaskFunction);
begin
  oteExecutorType := etFunction;
  oteFunc := func;
  Initialize;
end; { TOmniTaskExecutor.Create }
{$ENDIF OTL_Anonymous}

destructor TOmniTaskExecutor.Destroy;
begin
  oteInternalLock.Acquire;
  try
    FreeAndNil(oteCommList);
  finally oteInternalLock.Release; end;
  FreeAndNil(oteTerminateHandles);
  FreeAndNil(oteMethodHash);
  DSiCloseHandleAndNull(oteCommRebuildHandles);
  DSiCloseHandleAndNull(oteWorkerInitialized);
  inherited;
end; { TOmniTaskExecutor.Destroy }

procedure TOmniTaskExecutor.Asy_Execute(const task: IOmniTask);
const
  CThreadPriorityNum: array [TOTLThreadPriority] of integer = (
    THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST);
begin
  SetThreadPriority(GetCurrentThread, CThreadPriorityNum[Priority]);
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
    if not assigned(oteCommList) then
      oteCommList := TInterfaceList.Create;
    oteCommList.Add(comm);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_RegisterComm }

procedure TOmniTaskExecutor.Asy_SetExitStatus(exitCode: integer;
  const exitMessage: string);
begin
  oteExitCode.Value := cardinal(exitCode);
  oteInternalLock.Acquire;
  try
    oteExitMessage := exitMessage;
    UniqueString(oteExitMessage);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_SetExitStatus }

procedure TOmniTaskExecutor.Asy_SetTimer(interval_ms: cardinal; timerMsgID: integer);
begin
  SetTimerInt(interval_ms, timerMsgID, '', nil);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_SetTimer(interval_ms: cardinal;
  const timerMethod: pointer);
begin
  SetTimerInt(interval_ms, -1, '', timerMethod);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_SetTimer(interval_ms: cardinal; const timerMsgName: string);
begin
  SetTimerInt(interval_ms, -1, timerMsgName, nil);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_UnregisterComm: ' +
      'Additional communication support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    oteCommList.Remove(comm);
    if oteCommList.Count = 0 then
      FreeAndNil(oteCommList);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_UnregisterComm }

procedure TOmniTaskExecutor.CallOmniTimer;
var
  msg           : TOmniMessage;
  timerMsgID    : integer;
  timerMsgMethod: pointer;
  timerMsgName  : string;
begin
  oteInternalLock.Acquire;
  try
    timerMsgName := oteTimerMessageName;
    UniqueString(timerMsgName);
    timerMsgID := integer(oteTimerMessageID.Value);
    timerMsgMethod := pointer(oteTimerMessageMethod.Value);
  finally oteInternalLock.Release; end;
  if (timerMsgID >= 0) or (timerMsgName <> '') or (timerMsgMethod <> nil) then begin
    if timerMsgID >= 0 then begin
      msg.MsgID := timerMsgID;
      msg.MsgData := TOmniValue.Null;
    end
    else if timerMsgName <> '' then
      msg := TOmniInternalStringMsg.CreateMessage(timerMsgName, TOmniValue.Null)
    else
      msg := TOmniInternalAddressMsg.CreateMessage(timerMsgMethod, TOmniValue.Null);
    DispatchOmniMessage(msg);
  end
  else if assigned(WorkerIntf) then
    WorkerIntf.Timer;
end; { TOmniTaskExecutor.CallOmniTimer }

procedure TOmniTaskExecutor.Cleanup;
begin
  oteWorkerIntf := nil;
end; { TOmniTaskExecutor.Cleanup }

function TOmniTaskExecutor.DispatchEvent(awaited: cardinal; const task: IOmniTask; var
  msgInfo: TOmniMessageInfo): boolean;
var
  gotMsg: boolean;
  msg   : TOmniMessage;
begin
  Result := false;
  if ((msgInfo.IdxFirstTerminate <> cardinal(-1)) and
      ((awaited >= msgInfo.IdxFirstTerminate) and (awaited <= msgInfo.IdxLastTerminate)))
     or
     (awaited = WAIT_ABANDONED)
  then
    Exit
  else if (awaited >= msgInfo.IdxFirstMessage) and (awaited <= msgInfo.IdxLastMessage) then begin
    repeat
      if awaited = msgInfo.IdxFirstMessage then
        gotMsg := task.Comm.Receive(msg)
      else begin
        oteInternalLock.Acquire;
        try
          gotMsg := (oteCommList[awaited - msgInfo.IdxFirstMessage - 1] as IOmniCommunicationEndpoint).Receive(msg);
        finally oteInternalLock.Release; end;
      end;
      if gotMsg and assigned(WorkerIntf) then
        DispatchOmniMessage(msg);
    until not gotMsg;
  end // comm handles
  else if awaited = msgInfo.IdxRebuildHandles then begin
    RebuildWaitHandles(task, msgInfo);
    EmptyMessageQueues(task);
  end
  else if awaited = (WAIT_OBJECT_0 + msgInfo.NumWaitHandles) then //message
    ProcessThreadMessages
  else if awaited = WAIT_IO_COMPLETION then
    // do-nothing
  else if awaited = WAIT_TIMEOUT then begin
    if (TimerInterval_ms > 0) and ((DSiTimeGetTime64 - oteLastTimer_ms) >= TimerInterval_ms) then
    begin
      CallOmniTimer;
      oteLastTimer_ms := DSiTimeGetTime64;
    end;
  end //WAIT_TIMEOUT
  else //errors
    RaiseLastOSError;
  if WaitForSingleObject(oteCommRebuildHandles, 0) = WAIT_OBJECT_0 then begin
    //could get set inside timer or message handler
    RebuildWaitHandles(task, msgInfo);
    EmptyMessageQueues(task);
  end;
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
      oteMsgInfo.WaitWakeMask := WakeMask
    else
      oteMsgInfo.WaitWakeMask := 0;
    if tcoAlertableWait in Options then
      oteMsgInfo.WaitFlags := MWMO_ALERTABLE
    else
      oteMsgInfo.WaitFlags := 0;
    RebuildWaitHandles(task, oteMsgInfo);
    oteLastTimer_ms := DSiTimeGetTime64;
    MainMessageLoop(task, oteMsgInfo);
  finally
    if assigned(WorkerIntf) then begin
      WorkerIntf.Cleanup;
      WorkerIntf.Task := nil;
    end;
  end;
end; { TOmniTaskExecutor.DispatchMessages }

procedure TOmniTaskExecutor.DispatchOmniMessage(msg: TOmniMessage);
var
  methodAddr     : pointer;
  methodInfoObj  : TObject;
  methodInfo     : TOmniInvokeInfo absolute methodInfoObj;
  methodName     : string;
  methodSignature: TOmniInvokeType;
  msgData        : TOmniValue;
  obj            : TObject;
begin
  if msg.MsgID = COtlReservedMsgID then begin
    Assert(assigned(WorkerIntf));
    GetMethodNameFromInternalMessage(msg, methodName, msgData);
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
  else
    WorkerIntf.DispatchMessage(msg);
end; { TOmniTaskExecutor.DispatchMessage }

procedure TOmniTaskExecutor.EmptyMessageQueues(const task: IOmniTask);
var
  iComm: IOmniCommunicationEndpoint;
  iIntf: IInterface;
  msg  : TOmniMessage;
begin
  while task.Comm.Receive(msg) do
    if assigned(WorkerIntf) then
      DispatchOmniMessage(msg);
  if assigned(oteCommList) then begin
    oteInternalLock.Acquire;
    try
      for iIntf in oteCommList do begin
        iComm := iIntf as IOmniCommunicationEndpoint;
        while iComm.Receive(msg) do
          if assigned(WorkerIntf) then
            DispatchOmniMessage(msg);
      end;
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
begin
  // with great thanks to Hallvar Vassbotn [http://hallvards.blogspot.com/2006/04/published-methods_27.html]
  // and David Glassborow [http://davidglassborow.blogspot.com/2006/05/class-rtti.html]
  methodInfoHeader := ObjAuto.GetMethodInfo(WorkerIntf.Implementor, ShortString(methodName));
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
  params := PParamInfo(cardinal(methodInfoHeader) + SizeOf(methodInfoHeader^)
            - CShortLen + SizeOf(TReturnInfo) + Length(methodInfoHeader^.Name));
  paramNum := 0;
  methodSignature := itUnknown;
  // Loop over the parameters
  while cardinal(params) < headerEnd do begin
    Inc(paramNum);
    paramType := params.ParamType^;
    if paramNum = 1 then
      if (params^.Flags <> []) or (paramType^.Kind <> tkClass) then
        RaiseInvalidSignature(methodName)
      else
        methodSignature := itSelf
    else if paramNum = 2 then
      //code says 'const' but GetMethodInfo says 'pfVar' :(
      if (params^.Flags * [pfConst, pfVar] <> []) and (paramType^.Kind = tkRecord) and
         (SameText(string(paramType^.Name), 'TOmniValue'))
      then
        methodSignature := itSelfAndOmniValue
      else if (params^.Flags = [pfVar]) and (paramType^.Kind = tkClass) then
        methodSignature := itSelfAndObject
      else
        RaiseInvalidSignature(methodName)
    else
      RaiseInvalidSignature(methodName);
    params := params.NextParam;
  end;
end; { TOmniTaskExecutor.GetMethodAddrAndSignature }

procedure TOmniTaskExecutor.GetMethodNameFromInternalMessage(const msg: TOmniMessage; var
  msgName: string; var msgData: TOmniValue);
var
  internalType: TOmniInternalMessageType;
  method      : pointer;
begin
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
      end
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

function TOmniTaskExecutor.GetTimerInterval_ms: cardinal;
begin
  Result := oteTimerInterval_ms.Value;
end; { TOmniTaskExecutor.GetTimerInterval_ms }

function TOmniTaskExecutor.GetTimerMessageID: integer;
begin
  Result := integer(oteTimerMessageID.Value);
end; { TOmniTaskExecutor.GetTimerMessageID }

function TOmniTaskExecutor.GetTimerMessageMethod: pointer;
begin
  Result := pointer(oteTimerMessageMethod.Value);
end; { TOmniTaskExecutor.GetTimerMessageMethod }

function TOmniTaskExecutor.GetTimerMessageName: string;
begin
  oteInternalLock.Acquire;
  try
    Result := oteTimerMessageName;
    UniqueString(Result);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.GetTimerMessageName }

procedure TOmniTaskExecutor.Initialize;
begin
  oteWorkerInitialized := CreateEvent(nil, true, false, nil);
  Win32Check(oteWorkerInitialized <> 0);
  oteCommRebuildHandles := CreateEvent(nil, false, false, nil);
  Win32Check(oteCommRebuildHandles <> 0);
  oteTimerMessageID.Value := cardinal(-1);
end; { TOmniTaskExecutor.Initialize }

procedure TOmniTaskExecutor.MainMessageLoop(const task: IOmniTask; var msgInfo:
  TOmniMessageInfo);
begin
  EmptyMessageQueues(task);
  while DispatchEvent(WaitForEvent(msgInfo, TimeUntilNextTimer_ms), task, msgInfo) do
    MessageLoopPayload;
end; { TOmniTaskExecutor.MainMessageLoop }

procedure TOmniTaskExecutor.ProcessMessages(task: IOmniTask);
var
  awaited       : cardinal;
  msgInfo       : TOmniMessageInfo;
  waitHandlesGen: int64;
begin
  RemoveTerminationEvents(oteMsgInfo, msgInfo);
  waitHandlesGen := oteWaitHandlesGen;
  repeat
    awaited := WaitForEvent(msgInfo, 0);
    if awaited = WAIT_TIMEOUT then
      Exit;
    if not DispatchEvent(awaited, task, msgInfo) then
      Exit;
    MessageLoopPayload;
    if waitHandlesGen <> oteWaitHandlesGen then begin
      //DispatchEvent just rebuilt our internal copy
      RebuildWaitHandles(task, oteMsgInfo);
      EmptyMessageQueues(task);
    end;
  until false;
end; { TOmniTaskExecutor.ProcessMessages }

procedure TOmniTaskExecutor.MessageLoopPayload;
begin
  //placeholder that can be overridden
end; { TOmniTaskExecutor.MessageLoopPayload }

procedure TOmniTaskExecutor.ProcessThreadMessages;
var
  msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) and (Msg.Message <> WM_QUIT) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end; { TOmniTaskControl.ProcessThreadMessages }

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
  iHandle: integer;
  iIntf  : integer;
  intf   : IInterface;
begin
  Inc(oteWaitHandlesGen);
  oteInternalLock.Acquire;
  try
    msgInfo.IdxFirstTerminate := 0;
    msgInfo.WaitHandles[0] := task.TerminateEvent;
    msgInfo.IdxLastTerminate := msgInfo.IdxFirstTerminate;
    if assigned(oteTerminateHandles) then
      for iHandle in oteTerminateHandles do begin
        Inc(msgInfo.IdxLastTerminate);
        if msgInfo.IdxLastTerminate > High(msgInfo.WaitHandles) then
          raise Exception.CreateFmt('TOmniTaskExecutor: ' +
            'Cannot wait on more than %d handles', [High(msgInfo.WaitHandles)]);
        msgInfo.WaitHandles[msgInfo.IdxLastTerminate] := THandle(iHandle);
      end;
    msgInfo.IdxRebuildHandles := msgInfo.IdxLastTerminate + 1;
    msgInfo.WaitHandles[msgInfo.IdxRebuildHandles] := oteCommRebuildHandles;
    msgInfo.IdxFirstMessage := msgInfo.IdxRebuildHandles + 1;
    msgInfo.WaitHandles[msgInfo.IdxFirstMessage] := task.Comm.NewMessageEvent;
    msgInfo.IdxLastMessage := msgInfo.IdxFirstMessage;
    if assigned(oteCommList) then
      for iIntf := 0 to oteCommList.Count - 1 do begin
        intf := oteCommList[iIntf];
        Inc(msgInfo.IdxLastMessage);
        if msgInfo.IdxLastMessage > High(msgInfo.WaitHandles) then
          raise Exception.CreateFmt('TOmniTaskExecutor: ' +
            'Cannot wait on more than %d handles', [High(msgInfo.WaitHandles)]);
        msgInfo.WaitHandles[msgInfo.IdxLastMessage] := (intf as IOmniCommunicationEndpoint).NewMessageEvent;
      end;
    msgInfo.NumWaitHandles := msgInfo.IdxLastMessage + 1;
  finally oteInternalLock.Release; end;
end; { RebuildWaitHandles }

procedure TOmniTaskExecutor.RemoveTerminationEvents(const srcMsgInfo: TOmniMessageInfo;
  var dstMsgInfo: TOmniMessageInfo);
var
  offset: cardinal;
begin
  offset := srcMsgInfo.IdxLastTerminate + 1;
  dstMsgInfo.IdxFirstTerminate := cardinal(-1);
  dstMsgInfo.IdxLastTerminate := cardinal(-1);
  dstMsgInfo.IdxFirstMessage := srcMsgInfo.IdxFirstMessage - offset;
  dstMsgInfo.IdxLastMessage := srcMsgInfo.IdxLastMessage - offset;
  dstMsgInfo.IdxRebuildHandles := srcMsgInfo.IdxRebuildHandles - offset;
  dstMsgInfo.NumWaitHandles := srcMsgInfo.NumWaitHandles - offset;
  dstMsgInfo.WaitFlags := srcMsgInfo.WaitFlags;
  dstMsgInfo.WaitWakeMask := srcMsgInfo.WaitWakeMask;
  Move(srcMsgInfo.WaitHandles[offset], dstMsgInfo.WaitHandles[0],
    (Length(dstMsgInfo.WaitHandles) - integer(offset)) * SizeOf(THandle));
end; { TOmniTaskExecutor.RemoveTerminationEvents }

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

procedure TOmniTaskExecutor.SetTimerInterval_ms(const value: cardinal);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerInterval_ms: ' +
      'Timer support is only available when working with an IOmniWorker');
  oteTimerInterval_ms.Value := value;
end; { TOmniTaskExecutor.SetTimerInterval_ms }

procedure TOmniTaskExecutor.SetTimerInt(interval_ms: cardinal; timerMsgID: integer; const
  timerMsgName: string; const timerMsgMethod: pointer);
begin
  oteInternalLock.Acquire;
  try
    oteTimerMessageID.Value := cardinal(timerMsgID);
    oteTimerInterval_ms.Value := cardinal(interval_ms);
    oteTimerMessageMethod.Value := cardinal(timerMsgMethod);
    oteTimerMessageName := timerMsgName;
    UniqueString(oteTimerMessageName);
    oteLastTimer_ms := DSiTimeGetTime64;
  finally oteInternalLock.Release; end;
  SetEvent(oteCommRebuildHandles);
end; { TOmniTaskExecutor.SetTimerInt }

procedure TOmniTaskExecutor.SetTimerMessageID(const value: integer);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerMessageID: ' +
      'Timer support is only available when working with an IOmniWorker');
  oteTimerMessageID.Value := cardinal(value);
end; { TOmniTaskExecutor.SetTimerMessageID }

procedure TOmniTaskExecutor.SetTimerMessageMethod(const value: pointer);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerMessageID: ' +
      'Timer support is only available when working with an IOmniWorker');
  oteTimerMessageMethod.Value := cardinal(value);
end; { TOmniTaskExecutor.SetTimerMessageMethod }

procedure TOmniTaskExecutor.SetTimerMessageName(const value: string);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerMessageID: ' +
      'Timer support is only available when working with an IOmniWorker');
    oteTimerMessageName := value;
    UniqueString(oteTimerMessageName);
end; { TOmniTaskExecutor.SetTimerMessageName }

procedure TOmniTaskExecutor.TerminateWhen(handle: THandle);
begin
  Assert(SizeOf(THandle) = SizeOf(integer));
  if not assigned(oteTerminateHandles) then
    oteTerminateHandles := TGpIntegerList.Create;
  oteTerminateHandles.Add(handle);
end; { TOmniTaskExecutor.TerminateWhen }

function TOmniTaskExecutor.TimeUntilNextTimer_ms: cardinal;
var
  timeout_ms: int64;
begin
  if TimerInterval_ms <= 0 then
    Result := INFINITE
  else begin
    timeout_ms := TimerInterval_ms - (DSiTimeGetTime64 - oteLastTimer_ms);
    if timeout_ms < 0 then
      timeout_ms := 0;
    Result := timeout_ms;
  end;
end; { TOmniTaskExecutor.TimeUntilNextTimer_ms }

function TOmniTaskExecutor.WaitForInit: boolean;
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.WaitForInit: ' +
      'Wait for init is only available when working with an IOmniWorker');
  WaitForSingleObject(WorkerInitialized, INFINITE);
  Result := WorkerInitOK;
end; { TOmniTaskExecutor.WaitForInit }

function TOmniTaskExecutor.WaitForEvent(msgInfo: TOmniMessageInfo; timeout_ms: cardinal):
  cardinal;
begin
  Result := MsgWaitForMultipleObjectsEx(msgInfo.NumWaitHandles, msgInfo.WaitHandles,
    timeout_ms, msgInfo.WaitWakeMask, msgInfo.WaitFlags);
end; { TOmniTaskExecutor.WaitForEvent } 

{ TOmniTaskControl }

constructor TOmniTaskControl.Create(const worker: IOmniWorker; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskMethod; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskProcedure; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }

{$IFDEF OTL_Anonymous}
constructor TOmniTaskControl.Create(worker: TOmniTaskFunction; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  Initialize(taskName);
end; { TOmniTaskControl.Create }
{$ENDIF OTL_Anonymous}

destructor TOmniTaskControl.Destroy;
begin
  { TODO : Do we need wait-and-kill mechanism here to prevent shutdown locks? }
  if assigned(otcThread) then begin
    Terminate;
    FreeAndNil(otcThread);
  end;
  if otcDestroyLock then begin
    otcSharedInfo.Lock.Free;
    otcSharedInfo.Lock := nil;
  end;
  FreeAndNil(otcExecutor);
  otcSharedInfo.CommChannel := nil;
  if otcSharedInfo.TerminateEvent <> 0 then begin
    CloseHandle(otcSharedInfo.TerminateEvent);
    otcSharedInfo.TerminateEvent := 0;
  end;
  if otcSharedInfo.TerminatedEvent <> 0 then begin
    CloseHandle(otcSharedInfo.TerminatedEvent);
    otcSharedInfo.TerminatedEvent := 0;
  end;
  FreeAndNil(otcParameters);
  FreeAndNil(otcSharedInfo);
  inherited Destroy;
end; { TOmniTaskControl.Destroy }

function TOmniTaskControl.Alertable: IOmniTaskControl;
begin
  Options := Options + [tcoAlertableWait];
  Result := Self;
end; { TOmniTaskControl.Alertable }

function TOmniTaskControl.ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean):
  IOmniTaskControl;
begin
  otcSharedInfo.ChainTo := task;
  otcSharedInfo.ChainIgnoreErrors := ignoreErrors;
  Result := Self;
end; { TOmniTaskControl.ChainTo }

function TOmniTaskControl.CreateTask: IOmniTask;
begin
  EnsureCommChannel;
  Result := TOmniTask.Create(otcExecutor, otcParameters, otcSharedInfo);
end; { TOmniTaskControl.CreateTask }

function TOmniTaskControl.Enforced(forceExecution: boolean = true): IOmniTaskControl;
begin
  if forceExecution then
    Options := Options + [tcoForceExecution]
  else
    Options := Options - [tcoForceExecution];
  Result := Self;
end; { TOmniTaskControl.Enforced }

procedure TOmniTaskControl.EnsureCommChannel;
begin
  if not assigned(otcSharedInfo.CommChannel) then
    otcSharedInfo.CommChannel :=
      CreateTwoWayChannel(otcQueueLength, otcSharedInfo.TerminatedEvent);
end; { TOmniTaskControl.EnsureCommChannel }

function TOmniTaskControl.GetComm: IOmniCommunicationEndpoint;
begin
  EnsureCommChannel;
  Result := otcSharedInfo.CommChannel.Endpoint1;
end; { TOmniTaskControl.GetComm }

function TOmniTaskControl.GetExitCode: integer;
begin
  Result := otcExecutor.ExitCode;
end; { TOmniTaskControl.GetExitCode }

function TOmniTaskControl.GetExitMessage: string;
begin
  Result := otcExecutor.ExitMessage;
end; { TOmniTaskControl.GetExitMessage }

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

{function TOmniTaskControl.GetSelf: pointer;
begin
  result := self;
end;}

function TOmniTaskControl.GetSharedInfo: TOmniSharedTaskInfo;
begin
  result := otcSharedInfo;
end; { GetSharedInfo: TOmniSharedTaskInfo }

{ TOmniTaskControl.GetSelf }

function TOmniTaskControl.GetTerminatedEvent: THandle;
begin
  Result := otcSharedInfo.TerminatedEvent;
end; { TOmniTaskControl.GetTerminatedEvent }

function TOmniTaskControl.GetTerminateEvent: THandle;
begin
  Result := otcSharedInfo.TerminateEvent;
end; { TOmniTaskControl.GetTerminateEvent }

function TOmniTaskControl.GetUniqueID: int64;
begin
  Result := otcSharedInfo.UniqueID;
end; { TOmniTaskControl.GetUniqueID }

procedure TOmniTaskControl.Initialize;
begin
  otcExecutor.Options := [tcoForceExecution];
  otcQueueLength := CDefaultQueueSize;
  otcSharedInfo := TOmniSharedTaskInfo.Create;
  otcSharedInfo.TaskName := taskName;
  otcSharedInfo.UniqueID := OtlUID.Increment;
  otcParameters := TOmniValueContainer.Create;
  otcSharedInfo.TerminateEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminateEvent <> 0);
  otcSharedInfo.TerminatedEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminatedEvent <> 0);
end; { TOmniTaskControl.Initialize }

function TOmniTaskControl.Invoke(const msgMethod: pointer): IOmniTaskControl;
begin
  Invoke(msgMethod, TOmniValue.Null);
  Result := Self;
end; { TOmniTaskControl.Invoke }

function TOmniTaskControl.Invoke(const msgMethod: pointer; msgData: array of const): IOmniTaskControl;
begin
  Invoke(msgMethod, OpenArrayToVarArray(msgData));
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
  Invoke(msgName, OpenArrayToVarArray(msgData));
  Result := Self;
end; { TOmniCommunicationEndpoint.Invoke }

function TOmniTaskControl.Invoke(const msgName: string; msgData: TOmniValue): IOmniTaskControl;
begin
  Comm.Send(TOmniInternalStringMsg.CreateMessage(msgName, msgData));
  Result := Self;
end; { TOmniCommunicationEndpoint.Invoke }

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
  monitor.Monitor(Self);
  Result := Self;
end; { TOmniTaskControl.MonitorWith }

function TOmniTaskControl.MsgWait(wakeMask: DWORD): IOmniTaskControl;
begin
  Options := Options + [tcoMessageWait];
  otcExecutor.WakeMask := wakeMask;
  Result := Self;
end; { TOmniTaskControl.MsgWait }

function TOmniTaskControl.RemoveMonitor: IOmniTaskControl;
begin
  otcSharedInfo.MonitorWindow := 0;
  EnsureCommChannel;
  otcSharedInfo.CommChannel.Endpoint2.Writer.ContainerSubject.Detach(otcMonitorObserver);
  otcMonitorObserver := nil;
  Result := Self;
end; { TOmniTaskControl.RemoveMonitor }

function TOmniTaskControl.Run: IOmniTaskControl;
begin
  otcParameters.Lock;
  otcThread := TOmniThread.Create(CreateTask);
  otcThread.Resume;
  Result := Self;
end; { TOmniTaskControl.Run }

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

function TOmniTaskControl.SetMonitor(hWindow: THandle): IOmniTaskControl;
begin
  if not assigned(otcMonitorObserver) then begin
    if otcParameters.IsLocked then
      raise Exception.Create('TOmniTaskControl.SetMonitor: Monitor can only be assigned while task is not running');
    otcSharedInfo.MonitorWindow := hWindow;
    EnsureCommChannel;
    otcMonitorObserver := CreateContainerWindowsMessageObserver(
      hWindow, COmniTaskMsg_NewMessage, integer(Int64Rec(UniqueID).Lo),
      integer(Int64Rec(UniqueID).Hi));
  end;
  otcSharedInfo.CommChannel.Endpoint2.Writer.ContainerSubject.Attach(
    otcMonitorObserver, coiNotifyOnFirstInsert);
  Result := Self;
end; { TOmniTaskControl.SetMonitor }

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
  otcParameters.Assign(parameters);
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

function TOmniTaskControl.SetTimer(interval_ms: cardinal; timerMessageID: integer):
  IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(interval_ms, timerMessageID);
  Result := Self;
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetTimer(interval_ms: cardinal; const timerMethod: pointer):
  IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(interval_ms, timerMethod);
  Result := Self;
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetTimer(interval_ms: cardinal; const timerMessageName: string):
  IOmniTaskControl;
begin
  otcExecutor.Asy_SetTimer(interval_ms, timerMessageName);
  Result := Self;
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.Terminate(maxWait_ms: cardinal): boolean;
begin
  //TODO : reset executor and exit immediately if task was not started at all or raise exception?
  otcSharedInfo.Terminating := true;
  SetEvent(otcSharedInfo.TerminateEvent);
  Result := WaitFor(maxWait_ms);
  if not Result then begin
    if assigned(otcThread) then begin
      TerminateThread(otcThread.Handle, cardinal(-1));
      otcThread := nil;
    end
    else if assigned(otcOwningPool) then begin
      otcOwningPool.Cancel(UniqueID);
      otcOwningPool := nil;
    end;
  end;
end; { TOmniTaskControl.Terminate }

function TOmniTaskControl.TerminateWhen(event: THandle): IOmniTaskControl;
begin
  otcExecutor.TerminateWhen(event);
  Result := Self;
end; { TOmniTaskControl.TerminateWhen }

function TOmniTaskControl.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := (WaitForSingleObject(otcSharedInfo.TerminatedEvent, maxWait_ms) = WAIT_OBJECT_0);
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
end; { TOmniTaskControl.WithLock }

{ TOmniThread }

constructor TOmniThread.Create(task: IOmniTask);
begin
  inherited Create(true);
  otTask := task;
end; { TOmniThread.Create }

procedure TOmniThread.Execute;
begin
  (otTask as IOmniTaskExecutor).Execute;
end; { TOmniThread.Execute }

{ TOmniTaskControlListEnumerator }

constructor TOmniTaskControlListEnumerator.Create(taskList: TInterfaceList);
begin
  otcleTaskEnum := taskList.GetEnumerator;
end; { TOmniTaskControlListEnumerator.Create }

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
    (otgTaskList[iIntf] as IOmniTaskControl).Terminate;
  Result := WaitForAll(maxWait_ms);
end; { TOmniTaskGroup.TerminateAll }

function TOmniTaskGroup.UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
begin
  InternalUnregisterAllCommFrom(task);
  Result := Self;
end; { TOmniTaskGroup.UnregisterAllCommFrom }

function TOmniTaskGroup.WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
var
  iIntf      : integer;
  waitHandles: array [0..63] of THandle;
begin
  for iIntf := 0 to otgTaskList.Count - 1 do
    waitHandles[iIntf] := (otgTaskList[iIntf] as IOmniTaskControlInternals).TerminatedEvent;
  Result := WaitForMultipleObjects(otgTaskList.Count, @waitHandles, true, maxWait_ms) = WAIT_OBJECT_0;
end; { TOmniTaskGroup.WaitForAll }

end.
