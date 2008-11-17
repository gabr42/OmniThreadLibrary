///<summary>Task control encapsulation. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2008, Primoz Gabrijelcic
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
///   Last modification : 2008-11-17
///   Version           : 1.05a
///</para><para>
///   History:
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

{$IF CompilerVersion >= 20}
  {$DEFINE OTL_Anonymous}
{$IFEND}
{$WARN SYMBOL_PLATFORM OFF}

unit OtlTaskControl;

interface

uses
  Windows,
  SysUtils,
  Variants,
  Classes,
  SyncObjs,
  GpStuff,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlThreadPool;

type
  IOmniTaskControl = interface;
  
  IOmniTaskControlMonitor = interface ['{20CB3AB7-04D8-454B-AEFE-CFCFF8F27301}']
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl;
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl;
  end; { IOmniTaskControlMonitor }

  {$TYPEINFO ON} {$METHODINFO ON}
  IOmniWorker = interface ['{CA63E8C2-9B0E-4BFA-A527-31B2FCD8F413}']
    function  GetImplementor: TObject;
    function  GetTask: IOmniTask;
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
    owTask: IOmniTask;
  protected
    procedure Cleanup; virtual;
    procedure DispatchMessage(var msg: TOmniMessage); virtual;
    function  GetImplementor: TObject;
    function  GetTask: IOmniTask;
    function  Initialize: boolean; virtual;
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
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTaskControl }

  IOmniTaskGroupEnumerator = interface
    function GetCurrent: IOmniTaskControl;
    function MoveNext: boolean;
    property Current: IOmniTaskControl read GetCurrent;
  end; { IOmniTaskGroupEnumerator }

//v1.1 extensions:
//  maybe: Comm: IOmniCommunicationEndpoint, which is actually one-to-many-to-one
//    function  Sequential: IOmniTaskGroup;
//    function  Parallel(useThreadPool: IOmniThreadPool): IOmniTaskGroup;
//  maybe: if one of group processes dies, TerminateAll should automatically happen?
  IOmniTaskGroup = interface ['{B36C08B4-0F71-422C-8613-63C4D04676B7}']
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  GetEnumerator: IOmniTaskGroupEnumerator;
    function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    procedure SendToAll(const msg: TOmniMessage); 
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  end; { IOmniTaskGroup }

{$IFDEF OTL_Anonymous}
  TOmniTaskFunction = reference to procedure (task: IOmniTask);
  function CreateTask(worker: TOmniTaskFunction; const taskName: string = ''): IOmniTaskControl; overload;
{$ENDIF OTL_Anonymous}

  function CreateTask(worker: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(const worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
//  function CreateTask(worker: IOmniTaskGroup; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTaskGroup: IOmniTaskGroup;

implementation

uses
  Messages,
  TypInfo,
  ObjAuto,
  DetailedRTTI,
  DSiWin32,
  GpLists,
  GpStringHash,
  SpinLock,
  OtlEventMonitor;

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
    oiiAddress: pointer;
    oiiSignature: TOmniInvokeType;
  public
    constructor Create(methodAddr: pointer; methodSignature: TOmniInvokeType);
    property Address: pointer read oiiAddress;
    property Signature: TOmniInvokeType read oiiSignature;
  end; { TOmniInvokeInfo }

  TOmniTaskControlOption = (tcoAlertableWait, tcoMessageWait, tcoFreeOnTerminate);
  TOmniTaskControlOptions = set of TOmniTaskControlOption;
  TOmniExecutorType = (etNone, etMethod, etProcedure, etWorker, etFunction);

  TOmniTaskExecutor = class
  strict private
    oteCommList          : TInterfaceList;
    oteCommRebuildHandles: THandle;
    oteExecutorType      : TOmniExecutorType;
    oteExitCode          : TGp4AlignedInt;
    oteExitMessage       : string;
    {$IFDEF OTL_Anonymous}
    oteFunc              : TOmniTaskFunction;
    {$ENDIF OTL_Anonymous}
    oteInternalLock      : TSynchroObject;
    oteLastTimer_ms      : int64;
    oteMethod            : TOmniTaskMethod;
    oteMethodHash        : TGpStringObjectHash;
    oteOptions           : TOmniTaskControlOptions;
    otePriority          : TOTLThreadPriority;
    oteProc              : TOmniTaskProcedure;
    oteTerminateHandles  : TGpIntegerList;
    oteTimerInterval_ms  : TGp4AlignedInt;
    oteTimerMessageID    : TGp4AlignedInt;
    oteTimerMessageMethod: TGp4AlignedInt;
    oteTimerMessageName  : string;
    oteWakeMask          : DWORD;
    oteWorkerInitialized : THandle;
    oteWorkerInitOK      : boolean;
    oteWorkerIntf        : IOmniWorker;
  strict protected
    procedure Asy_SetTimerInt(interval_ms: cardinal; timerMsgID: integer;
      const timerMsgName: string; const timerMsgMethod: pointer);
    procedure CallOmniTimer;
    procedure Cleanup;
    procedure DispatchOmniMessage(msg: TOmniMessage);
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string;
    procedure GetMethodAddrAndSignature(const methodName: string;
      var methodAddress: pointer; var methodSignature: TOmniInvokeType);
    procedure GetMethodNameFromInternalMessage(const msg: TOmniMessage;
      var msgName: string; var msgData: TOmniValue);
    function  GetTimerInterval_ms: cardinal; inline;
    function  GetTimerMessageID: integer; inline;
    function  GetTimerMessageMethod: pointer;
    function  GetTimerMessageName: string;
    procedure Initialize;
    procedure ProcessThreadMessages;
    procedure RaiseInvalidSignature(const methodName: string);
    procedure SetOptions(const value: TOmniTaskControlOptions);
    procedure SetTimerInterval_ms(const value: cardinal);
    procedure SetTimerMessageID(const value: integer);
    procedure SetTimerMessageMethod(const value: pointer);
    procedure SetTimerMessageName(const value: string);
  public
    constructor Create(const workerIntf: IOmniWorker); overload;
    constructor Create(method: TOmniTaskMethod); overload;
    constructor Create(proc: TOmniTaskProcedure); overload;
    {$IFDEF OTL_Anonymous}
    constructor Create(func: TOmniTaskFunction); overload;
    {$ENDIF OTL_Anonymous}
    destructor  Destroy; override;
    procedure Asy_DispatchMessages(const task: IOmniTask);
    procedure Asy_Execute(const task: IOmniTask);
    procedure Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure Asy_SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Asy_SetTimer(interval_ms: cardinal; timerMsgID: integer); overload;
    procedure Asy_SetTimer(interval_ms: cardinal; const timerMethod: pointer); overload;
    procedure Asy_SetTimer(interval_ms: cardinal; const timerMsgName: string); overload;
    procedure Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure TerminateWhen(handle: THandle);
    function WaitForInit: boolean;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Options: TOmniTaskControlOptions read oteOptions write SetOptions;
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

  TOmniSharedTaskInfo = class
  strict private
    ostiChainIgnoreErrors: boolean;
    ostiChainTo          : IOmniTaskControl;
    ostiCommChannel      : IOmniTwoWayChannel;
    ostiCounter          : IOmniCounter;
    ostiLock             : TSynchroObject;
    ostiMonitorWindow    : THandle;
    ostiTaskName         : string;
    ostiTerminatedEvent  : THandle;
    ostiTerminateEvent   : THandle;
    ostiUniqueID         : int64;
  public
    property ChainIgnoreErrors: boolean read ostiChainIgnoreErrors write ostiChainIgnoreErrors;
    property ChainTo: IOmniTaskControl read ostiChainTo write ostiChainTo;
    property CommChannel: IOmniTwoWayChannel read ostiCommChannel write ostiCommChannel;
    property Counter: IOmniCounter read ostiCounter write ostiCounter;
    property Lock: TSynchroObject read ostiLock write ostiLock;
    property MonitorWindow: THandle read ostiMonitorWindow write ostiMonitorWindow;
    property TaskName: string read ostiTaskName write ostiTaskName;
    property TerminatedEvent: THandle read ostiTerminatedEvent write ostiTerminatedEvent;
    property TerminateEvent: THandle read ostiTerminateEvent write ostiTerminateEvent;
    property UniqueID: int64 read ostiUniqueID write ostiUniqueID;
  end; { TOmniSharedTaskInfo }

  TOmniTask = class(TInterfacedObject, IOmniTask, IOmniTaskExecutor)
  strict private
    otExecuting     : boolean;
    otExecutor_ref  : TOmniTaskExecutor;
    otParameters_ref: TOmniValueContainer;
    otSharedInfo    : TOmniSharedTaskInfo;
  protected
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetCounter: IOmniCounter;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetParam(idxParam: integer): TOmniValue; inline;
    function  GetParamByName(const paramName: string): TOmniValue; inline;
    function  GetTerminateEvent: THandle; inline;
    function  GetUniqueID: int64; inline;
    procedure Terminate; 
  public
    constructor Create(executor: TOmniTaskExecutor; parameters: TOmniValueContainer;
      sharedInfo: TOmniSharedTaskInfo);
    procedure Execute;
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetTimer(interval_ms: cardinal; timerMessageID: integer = -1); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMethod: pointer); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMessageName: string); overload;
    function Stopped: boolean;
    procedure StopTimer;
    function Terminated: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property TerminateEvent: THandle read GetTerminateEvent;
    property UniqueID: int64 read GetUniqueID;
  end; { TOmniTask }

  TOmniThread = class(TThread) // TODO 3 -oPrimoz Gabrijelcic : Factor this class into OtlThread unit?
  strict private
    otTask: IOmniTask;
  strict protected
  protected
    procedure Execute; override;
  public
    constructor Create(task: IOmniTask);
    property Task: IOmniTask read otTask;
  end; { TOmniThread }

  IOmniTaskControlInternals = interface ['{CE7B53E0-902E-413F-AB6E-B97E7F4B0AD5}']
    function  GetTerminatedEvent: THandle;
    function  GetTerminateEvent: THandle;
  //
    property TerminatedEvent: THandle read GetTerminatedEvent;
    property TerminateEvent: THandle read GetTerminateEvent;
  end; { IOmniTaskControlInternals }

  TOmniTaskControl = class(TInterfacedObject, IOmniTaskControl, IOmniTaskControlInternals)
  strict private
    otcDestroyLock: boolean;
    otcExecutor   : TOmniTaskExecutor;
    otcOwningPool : IOmniThreadPool;
    otcParameters : TOmniValueContainer;
    otcQueueLength: integer;
    otcSharedInfo : TOmniSharedTaskInfo;
    otcThread     : TOmniThread;
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
    property UniqueID: int64 read GetUniqueID;
  end; { TOmniTaskControl }

  TOmniTaskGroup = class;
  
  TOmniTaskGroupEnumerator = class(TInterfacedObject, IOmniTaskGroupEnumerator)
  strict private
    otgeTaskEnum: TInterfaceListEnumerator;
  protected
    function GetCurrent: IOmniTaskControl;
    function MoveNext: boolean;
  public
    constructor Create(taskList: TInterfaceList);
  end; { TOmniTaskGroupEnumerator }

  TOmniTaskGroup = class(TInterfacedObject, IOmniTaskGroup)
  strict private
    otgRegisteredWith: IOmniTask;
    otgTaskList      : TInterfaceList;
  strict protected
    procedure AutoUnregisterComms;
    procedure InternalUnregisterAllCommFrom(const task: IOmniTask);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  GetEnumerator: IOmniTaskGroupEnumerator;
    function  RegisterAllCommWith(const task: IOmniTask): IOmniTaskGroup;
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    procedure SendToAll(const msg: TOmniMessage);
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  UnregisterAllCommFrom(const task: IOmniTask): IOmniTaskGroup;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  end; { TOmniTaskGroup }

{ exports }

{$IFDEF OTL_Anonymous}
function CreateTask(worker: TOmniTaskFunction; const taskName: string = ''): IOmniTaskControl;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }
{$ENDIF OTL_Anonymous}

function CreateTask(worker: TOmniTaskProcedure; const taskName: string):
  IOmniTaskControl;
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

procedure TOmniTask.Execute;
begin
  otExecuting := true;
  try
    try
      try
        {$IFNDEF OTL_DontSetThreadName}
        SetThreadName(otSharedInfo.TaskName);
        {$ENDIF OTL_DontSetThreadName}
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
  finally SetEvent(otSharedInfo.TerminatedEvent); end;
  if assigned(otSharedInfo.ChainTo) and
     (otSharedInfo.ChainIgnoreErrors or (otExecutor_ref.ExitCode = EXIT_OK))
  then                        
    otSharedInfo.ChainTo.Run;
  otSharedInfo.ChainTo := nil;
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
end; { TOmniTask.GetLock }

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

procedure TOmniTask.SetTimer(interval_ms: cardinal; timerMessageID: integer);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMessageID);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(interval_ms: cardinal; const timerMethod: pointer);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMethod);
end; { TOmniTask.SetTimer }

procedure TOmniTask.SetTimer(interval_ms: cardinal; const timerMessageName: string);
begin
  otExecutor_ref.Asy_SetTimer(interval_ms, timerMessageName);
end; { TOmniTask.SetTimer }

function TOmniTask.Stopped: boolean;
begin
  Result := WaitForSingleObject(otSharedInfo.TerminatedEvent, 0) <> WAIT_TIMEOUT;
end; { TOmniTask.Stopped }

procedure TOmniTask.StopTimer;
begin
  SetTimer(0);
end; { TOmniTask.StopTimer }

procedure TOmniTask.Terminate;
begin
  SetEvent(otSharedInfo.TerminateEvent);
  if not otExecuting then //for execution so that proper cleanup can occur
    Execute;
end; { TOmniTask.Terminate }

function TOmniTask.Terminated: boolean;
begin
  Result := WaitForSingleObject(otSharedInfo.TerminateEvent, 0) <> WAIT_TIMEOUT;
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
  FreeAndNil(oteInternalLock);
  FreeAndNil(oteTerminateHandles);
  FreeAndNil(oteMethodHash);
  DSiCloseHandleAndNull(oteCommRebuildHandles);
  DSiCloseHandleAndNull(oteWorkerInitialized);
  inherited;
end; { TOmniTaskExecutor.Destroy }

procedure TOmniTaskExecutor.Asy_DispatchMessages(const task: IOmniTask);

var
  idxFirstMessage  : cardinal;
  idxFirstTerminate: cardinal;
  idxLastMessage   : cardinal;
  idxLastTerminate : cardinal;
  idxRebuildHandles: cardinal;
  numWaitHandles   : cardinal;
  waitHandles      : array [0..63] of THandle;

  procedure RebuildWaitHandles;
  var
    iHandle: integer;
    iIntf  : integer;
    intf   : IInterface;
  begin
    oteInternalLock.Acquire;
    try
      idxFirstTerminate := 0;
      waitHandles[0] := task.TerminateEvent;
      idxLastTerminate := idxFirstTerminate;
      if assigned(oteTerminateHandles) then
        for iHandle in oteTerminateHandles do begin
          Inc(idxLastTerminate);
          if idxLastTerminate > High(waitHandles) then
            raise Exception.CreateFmt('TOmniTaskExecutor.Asy_DispatchMessages: ' +
              'Cannot wait on more than %d handles', [High(waitHandles)]);
          waitHandles[idxLastTerminate] := THandle(iHandle);
        end;
      idxRebuildHandles := idxLastTerminate + 1;
      waitHandles[idxRebuildHandles] := oteCommRebuildHandles;
      idxFirstMessage := idxRebuildHandles + 1;
      waitHandles[idxFirstMessage] := task.Comm.NewMessageEvent;
      idxLastMessage := idxFirstMessage;
      if assigned(oteCommList) then
        for iIntf := 0 to oteCommList.Count - 1 do begin
          intf := oteCommList[iIntf];
          Inc(idxLastMessage);
          if idxLastMessage > High(waitHandles) then
            raise Exception.CreateFmt('TOmniTaskExecutor.Asy_DispatchMessages: ' +
              'Cannot wait on more than %d handles', [High(waitHandles)]);
          waitHandles[idxLastMessage] := (intf as IOmniCommunicationEndpoint).NewMessageEvent;
        end;
      numWaitHandles := idxLastMessage + 1;
    finally oteInternalLock.Release; end;
  end; { RebuildWaitHandles }

var
  awaited     : DWORD;
  flags       : DWORD;
  gotMsg      : boolean;
  msg         : TOmniMessage;
  timeout_ms  : int64;
  waitWakeMask: DWORD;

begin { TOmniTaskExecutor.Asy_DispatchMessages }
  try
    oteWorkerInitOK := false;
    try
      if assigned(WorkerIntf) then begin
        WorkerIntf.Task := task;
        if not WorkerIntf.Initialize then
          Exit;
      end;
      oteWorkerInitOK := true;
    finally SetEvent(WorkerInitialized); end;
    if tcoMessageWait in Options then
      waitWakeMask := WakeMask
    else
      waitWakeMask := 0;
    if tcoAlertableWait in Options then
      flags := MWMO_ALERTABLE
    else
      flags := 0;
    RebuildWaitHandles;
    oteLastTimer_ms := DSiTimeGetTime64;
    repeat
      if WaitForSingleObject(oteCommRebuildHandles, 0) = WAIT_OBJECT_0 then //could get set inside timer or message handler
        RebuildWaitHandles;      
      if TimerInterval_ms <= 0 then
        timeout_ms := INFINITE
      else begin
        timeout_ms := TimerInterval_ms - (DSiTimeGetTime64 - oteLastTimer_ms);
        if timeout_ms < 0 then
          timeout_ms := 0;
      end;
      awaited := MsgWaitForMultipleObjectsEx(numWaitHandles, waitHandles,
        cardinal(timeout_ms), waitWakeMask, flags);
      if ((awaited >= idxFirstTerminate) and (awaited <= idxLastTerminate)) or
         (awaited = WAIT_ABANDONED)
      then
        break //repeat
      else if (awaited >= idxFirstMessage) and (awaited <= idxLastMessage) then begin
        if awaited = idxFirstMessage then
          gotMsg := task.Comm.Receive(msg)
        else begin
          oteInternalLock.Acquire;
          try
            gotMsg := (oteCommList[awaited - idxFirstMessage - 1] as IOmniCommunicationEndpoint).Receive(msg);
          finally oteInternalLock.Release; end;
        end;
        if gotMsg and assigned(WorkerIntf) then 
          DispatchOmniMessage(msg);
      end // comm handles
      else if awaited = idxRebuildHandles then
        RebuildWaitHandles
      else if awaited = (WAIT_OBJECT_0 + numWaitHandles) then //message
        ProcessThreadMessages
      else if awaited = WAIT_IO_COMPLETION then
        // do-nothing
      else if awaited = WAIT_TIMEOUT then begin
        CallOmniTimer;
        oteLastTimer_ms := DSiTimeGetTime64;
      end //WAIT_TIMEOUT
      else //errors
        RaiseLastOSError;
    until false;
  finally
    if assigned(WorkerIntf) then begin
      WorkerIntf.Cleanup;
      WorkerIntf.Task := nil;
    end;
  end;
end; { TOmniTaskExecutor.Asy_DispatchMessages }

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
        Asy_DispatchMessages(task);
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
  Asy_SetTimerInt(interval_ms, timerMsgID, '', nil);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_SetTimer(interval_ms: cardinal;
  const timerMethod: pointer);
begin
  Asy_SetTimerInt(interval_ms, -1, '', timerMethod);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_SetTimer(interval_ms: cardinal; const timerMsgName: string);
begin
  Asy_SetTimerInt(interval_ms, -1, timerMsgName, nil);
end; { TOmniTaskExecutor.Asy_SetTimer }

procedure TOmniTaskExecutor.Asy_SetTimerInt(interval_ms: cardinal; timerMsgID: integer;
  const timerMsgName: string; const timerMsgMethod: pointer);
begin
  oteInternalLock.Acquire;
  try
    oteTimerMessageID.Value := timerMsgID;
    oteTimerInterval_ms.Value := cardinal(interval_ms);
    oteTimerMessageMethod.Value := cardinal(timerMsgMethod);
    oteTimerMessageName := timerMsgName;
    UniqueString(oteTimerMessageName);
    oteLastTimer_ms := DSiTimeGetTime64;
  finally oteInternalLock.Release; end;
  SetEvent(oteCommRebuildHandles);
end; { TOmniTaskExecutor.Asy_SetTimerInt }

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
  oteInternalLock := TTicketSpinLock.Create;
  oteCommRebuildHandles := CreateEvent(nil, false, false, nil);
  Win32Check(oteCommRebuildHandles <> 0);
  oteTimerMessageID.Value := cardinal(-1);
end; { TOmniTaskExecutor.Initialize }

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

procedure TOmniTaskExecutor.SetOptions(const value: TOmniTaskControlOptions);
begin
  if (([tcoAlertableWait, tcoMessageWait] * Options) <> []) and
     (oteExecutorType <> etWorker)
  then
    raise Exception.Create('TOmniTaskExecutor.SetOptions: ' +
      'Trying to set IOmniWorker specific option(s)');
  oteOptions := value;
end; { TOmniTaskExecutor.SetOptions }

procedure TOmniTaskExecutor.SetTimerInterval_ms(const value: cardinal);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerInterval_ms: ' +
      'Timer support is only available when working with an IOmniWorker');
  oteTimerInterval_ms.Value := value;
end; { TOmniTaskExecutor.SetTimerInterval_ms }

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

function TOmniTaskExecutor.WaitForInit: boolean;
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.WaitForInit: ' +
      'Wait for init is only available when working with an IOmniWorker');
  WaitForSingleObject(WorkerInitialized, INFINITE);
  Result := WorkerInitOK;
end; { TOmniTaskExecutor.WaitForInit }

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

procedure TOmniTaskControl.EnsureCommChannel;
begin
  if not assigned(otcSharedInfo.CommChannel) then
    otcSharedInfo.CommChannel := CreateTwoWayChannel(otcQueueLength);
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
  otcSharedInfo.CommChannel.Endpoint2.RemoveMonitor;
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
  if otcParameters.IsLocked then
    raise Exception.Create('TOmniTaskControl.SetMonitor: Monitor can only be assigned while task is not running');
  otcSharedInfo.MonitorWindow := hWindow;
  EnsureCommChannel;
  otcSharedInfo.CommChannel.Endpoint2.SetMonitor(hWindow, COmniTaskMsg_NewMessage,
    integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi));
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

{ TOmniTaskGroupEnumerator }

constructor TOmniTaskGroupEnumerator.Create(taskList: TInterfaceList);
begin
  otgeTaskEnum := taskList.GetEnumerator;
end; { TOmniTaskGroupEnumerator.Create }

function TOmniTaskGroupEnumerator.GetCurrent: IOmniTaskControl;
begin
  Result := otgeTaskEnum.GetCurrent as IOmniTaskControl;
end; { TOmniTaskGroupEnumerator.GetCurrent }

function TOmniTaskGroupEnumerator.MoveNext: boolean;
begin
  Result := otgeTaskEnum.MoveNext;
end; { TOmniTaskGroupEnumerator.MoveNext }

{ TOmniTaskGroup }

constructor TOmniTaskGroup.Create;
begin
  inherited Create;
  otgTaskList := TInterfaceList.Create;
end; { TOmniTaskGroup.Create }

destructor TOmniTaskGroup.Destroy;
begin
  AutoUnregisterComms;
  FreeAndNil(otgTaskList);
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

function TOmniTaskGroup.GetEnumerator: IOmniTaskGroupEnumerator;
begin
  Result := TOmniTaskGroupEnumerator.Create(otgTaskList);
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
    (otgTaskList[iIntf] as IOMniTaskControl).Run;
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
    SetEvent((otgTaskList[iIntf] as IOmniTaskControlInternals).TerminateEvent);
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
