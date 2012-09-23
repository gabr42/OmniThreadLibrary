///<summary>Thread pool. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011, Primoz Gabrijelcic
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
///   Last modification : 2012-01-31
///   Version           : 2.09a
/// </para><para>
///   History:
///     2.09a: 2012-01-31
///       - More accurate CountQueued.
///     2.09: 2011-11-08
///       - Adapted to OtlCommon 1.24.
///     2.08: 2011-11-06
///       - Sets thread name to 'Idle thread worker' when a thread is idle.
///     2.07: 2011-07-14
///       - Exceptions are no longer reported through the OnPoolWorkItemCompleted event.
///     2.06: 2011-07-04
///       - Fixed task exception handling. Exceptions are now reported through the
///         OnPoolWorkItemCompleted event.
///     2.05b: 2010-11-25
///       - Bug fixed: Thread pool was immediately closing unused threads if MaxExecuting
///         was set to -1.
///     2.05a: 2010-07-19
///       - Works correctly if MaxExecuting is set to 0. Set MaxExecuting to -1 to allow
///         "infinite" number of execution threads.
///       - When MaxExecuting is changed, the code checks immediately if tasks from the
///         idle queue can now be activated.
///     2.05: 2010-07-01
///       - Includes OTLOptions.inc.
///     2.04a: 2010-06-06
///       - Modified patch from 2.04 so that it's actually working.
///     2.04: 2010-05-30
///       - ThreadDataFactory can now accept either a function or a method.
///     2.03c: 2010-01-09
///       - Fixed CancelAll.
///       - Can be compiled with /dLogThreadPool.
///     2.03b: 2009-12-12
///       - Fixed exception handling for silent exceptions.
///     2.03a: 2009-11-17
///       - Task worker must not depend on monitor to be assigned.
///       - SetMonitor must be synchronous.
///     2.02: 2009-11-13
///       - D2010 compatibility changes.
///     2.01b: 2009-03-03
///       - Bug fixed: TOTPWorkerThread.Create was not waiting on the worker object to
///         initialize.
///     2.01a: 2009-02-09
///       - Removed critical section added in 2.0b - it is not needed as the
///         IOmniTaskControl.Invoke is thread-safe.
///     2.01: 2009-02-08
///       - Added support for per-thread data storage.
///     2.0b: 2009-02-06
///       - Protect communication between TOmniThreadPool and TOTPWorker with a critical
///         section. That should allow multiple threads to Schedule tasks into one
///         thread pool. 
///     2.0a: 2009-02-06
///       - Removed OnWorkerThreadCreated_Asy/OnWorkerThreadDestroyed_Asy
///         notification mechanism which was pretty much useless.
///     2.0: 2009-01-26
///       - Reimplemented using OmniThreadLibrary :)
///     1.0: 2008-08-26
///       - First official release. 
/// </para></remarks>

unit OtlThreadPool;

{$I OtlOptions.inc}

interface

// TODO 1 -oPrimoz Gabrijelcic : Should be monitorable by the OmniTaskEventDispatch
// TODO 3 -oPrimoz Gabrijelcic : Needs an async event reporting unexpected states (kill threads, for example)
// TODO 5 -oPrimoz Gabrijelcic : Loggers should (maybe) send log info to the event monitor 

uses
  Windows,
  SysUtils,
  OtlCommon,
  OtlTask;

const
  CDefaultIdleWorkerThreadTimeout_sec = 10;
  CDefaultWaitOnTerminate_sec = 30;

  CMaxConcurrentWorkers = 60; // enforced by the TOmniWorker limitations 
  // this is not configurable - don't increment it and expect the code to magically work! 

type
  IOmniThreadPool = interface;

  IOmniThreadPoolMonitor = interface
    ['{09EFADE8-3F14-4184-87CA-131100EC57E4}']
    function  Detach(const task: IOmniThreadPool): IOmniThreadPool;
    function  Monitor(const task: IOmniThreadPool): IOmniThreadPool;
  end; { IOmniThreadPoolMonitor }

  TThreadPoolOperation = (tpoCreateThread, tpoDestroyThread, tpoKillThread,
    tpoWorkItemCompleted);

  TOmniThreadPoolMonitorInfo = class
  strict private
    otpmiTaskID             : int64;
    otpmiThreadID           : integer;
    otpmiThreadPoolOperation: TThreadPoolOperation;
    otpmiUniqueID           : int64;
  public
    constructor Create(uniqueID: int64; threadPoolOperation: TThreadPoolOperation;
      threadID: integer); overload;
    constructor Create(uniqueID, taskID: int64); overload;
    property TaskID: int64 read otpmiTaskID;
    property ThreadPoolOperation: TThreadPoolOperation read
      otpmiThreadPoolOperation;
    property ThreadID: integer read otpmiThreadID;
    property UniqueID: int64 read otpmiUniqueID;
  end; { TOmniThreadPoolMonitorInfo }

  TOTPThreadDataFactoryFunction = function: IInterface;
  TOTPThreadDataFactoryMethod = function: IInterface of object;

  /// <summary>Worker thread lifetime reporting handler.</summary>
  TOTPWorkerThreadEvent = procedure(Sender: TObject; threadID: DWORD) of object;

  IOmniThreadPool = interface
    ['{1FA74554-1866-46DD-AC50-F0403E378682}']
    function  GetIdleWorkerThreadTimeout_sec: integer;
    function  GetMaxExecuting: integer;
    function  GetMaxQueued: integer;
    function  GetMaxQueuedTime_sec: integer;
    function  GetMinWorkers: integer;
    function  GetName: string;
    function  GetUniqueID: int64;
    function  GetWaitOnTerminate_sec: integer;
    procedure SetIdleWorkerThreadTimeout_sec(value: integer);
    procedure SetMaxExecuting(value: integer);
    procedure SetMaxQueued(value: integer); overload;
    procedure SetMaxQueuedTime_sec(value: integer);
    procedure SetMinWorkers(value: integer);
    procedure SetName(const value: string);
    procedure SetWaitOnTerminate_sec(value: integer);
    //
    function  Cancel(taskID: int64): boolean;
    procedure CancelAll;
    function  CountExecuting: integer;
    function  CountQueued: integer;
    function  IsIdle: boolean;
    function  MonitorWith(const monitor: IOmniThreadPoolMonitor): IOmniThreadPool;
    function  RemoveMonitor: IOmniThreadPool;
    function  SetMonitor(hWindow: THandle): IOmniThreadPool;
    procedure SetThreadDataFactory(const value: TOTPThreadDataFactoryMethod); overload;
    procedure SetThreadDataFactory(const value: TOTPThreadDataFactoryFunction); overload;
    property IdleWorkerThreadTimeout_sec: integer read GetIdleWorkerThreadTimeout_sec
      write SetIdleWorkerThreadTimeout_sec;
    property MaxExecuting: integer read GetMaxExecuting write SetMaxExecuting;
    property MaxQueued: integer read GetMaxQueued write SetMaxQueued;
    property MaxQueuedTime_sec: integer read GetMaxQueuedTime_sec write
      SetMaxQueuedTime_sec;
    property MinWorkers: integer read GetMinWorkers write SetMinWorkers;
    property Name: string read GetName write SetName;
    property UniqueID: int64 read GetUniqueID;
    property WaitOnTerminate_sec: integer read GetWaitOnTerminate_sec
      write SetWaitOnTerminate_sec;
  end; { IOmniThreadPool }

  IOmniThreadPoolScheduler = interface
    ['{B7F5FFEF-2704-4CE0-ABF1-B20493E73650}']
    procedure Schedule(const task: IOmniTask);
  end; { IOmniThreadPoolScheduler }

function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;

function GlobalOmniThreadPool: IOmniThreadPool;

implementation

uses
  Messages,
  Classes,
  Contnrs,
  Variants,
  TypInfo,
{$IFDEF OTL_HasSystemTypes}
  System.Types,
{$ENDIF}
{$IFNDEF Unicode} // D2009+ provides own TStringBuilder class
  HVStringBuilder,
{$ENDIF}
  DSiWin32,
  GpStuff,
  OtlHooks,
  OtlSync,
  OtlComm,
  OtlContainerObserver,
  OtlTaskControl,
  OtlEventMonitor;

const
  WM_REQUEST_COMPLETED = WM_USER;

  MSG_RUN               = 1;
  MSG_THREAD_CREATED    = 2;
  MSG_THREAD_DESTROYING = 3;
  MSG_COMPLETED         = 4;
  MSG_STOP              = 5;
  MSG_CANCEL_RESULT     = 6;

type
{$IFNDEF Unicode}
  TStringBuilder = HVStringBuilder.StringBuilder;
{$ENDIF}
  TOTPWorkerThread = class;
  TOmniThreadPool = class;

  TOTPWorkItem = class
  strict private
    owiScheduled_ms: int64;
    owiScheduledAt : TDateTime;
    owiStartedAt   : TDateTime;
    owiTask        : IOmniTask;
    owiThread      : TOTPWorkerThread;
    owiUniqueID    : int64;
  public
    constructor Create(const task: IOmniTask);
    function  Description: string;
    procedure TerminateTask(exitCode: integer; const exitMessage: string);
    property ScheduledAt: TDateTime read owiScheduledAt;
    property Scheduled_ms: int64 read owiScheduled_ms;
    property StartedAt: TDateTime read owiStartedAt write owiStartedAt;
    property UniqueID: int64 read owiUniqueID;
    property Task: IOmniTask read owiTask;
    property Thread: TOTPWorkerThread read owiThread write owiThread;
  end; { TOTPWorkItem }

  TOTPThreadDataFactory = record
  private
    tdfExecutable: TOmniExecutable;
  public
    constructor Create(const a: TOTPThreadDataFactoryFunction); overload;
    constructor Create(const a: TOTPThreadDataFactoryMethod); overload;
    function  Execute: IInterface; inline;
    function  IsEmpty: boolean; inline;
  end; { TOTPThreadDataFactory }

  TOTPWorkerThread = class(TThread)
  strict private
    owtCommChannel      : IOmniTwoWayChannel;
    owtNewWorkEvent     : TDSiEventHandle;
    owtRemoveFromPool   : boolean;
    owtStartIdle_ms     : int64;
    owtStartStopping_ms : int64;
    owtStopped          : boolean;
    owtTerminateEvent   : TDSiEventHandle;
    owtThreadData       : IInterface;
    owtThreadDataFactory: TOTPThreadDataFactory;
    owtWorkItemLock     : IOmniCriticalSection;
    owtWorkItem_ref     : TOTPWorkItem;
  strict protected
    function  Comm: IOmniCommunicationEndpoint;
    procedure ExecuteWorkItem(workItem: TOTPWorkItem);
    function  GetOwnerCommEndpoint: IOmniCommunicationEndpoint;
    procedure Log(const msg: string; const params: array of const);
  public
    constructor Create(const ThreadDataFactory: TOTPThreadDataFactory);
    destructor  Destroy; override;
    procedure Asy_Stop;
    function  Asy_TerminateWorkItem(var workItem: TOTPWorkItem): boolean;
    function  Description: string;
    procedure Execute; override;
    function  GetWorkItemInfo(var scheduledAt, startedAt: TDateTime;
      var description: string): boolean;
    function  IsExecuting(taskID: int64): boolean;
    procedure Start;
    function  WorkItemDescription: string;
    property NewWorkEvent: TDSiEventHandle read owtNewWorkEvent;
    property OwnerCommEndpoint: IOmniCommunicationEndpoint read GetOwnerCommEndpoint;
    property RemoveFromPool: boolean read owtRemoveFromPool;
    property StartIdle_ms: int64 read owtStartIdle_ms write owtStartIdle_ms;
    property StartStopping_ms: int64 read owtStartStopping_ms
      write owtStartStopping_ms; // always modified from the owner thread
    property Stopped: boolean read owtStopped
      write owtStopped; // always modified from the owner thread
    property TerminateEvent: TDSiEventHandle read owtTerminateEvent;
    property WorkItem_ref: TOTPWorkItem read owtWorkItem_ref
      write owtWorkItem_ref; // address of the work item this thread is working on
  end; { TOTPWorkerThread }

  TOTPWorker = class(TOmniWorker)
  strict private
    owDestroying       : boolean;
    owIdleWorkers      : TObjectList;
    owMonitorObserver  : TOmniContainerWindowsMessageObserver;
    owName             : string;
    owRunningWorkers   : TObjectList;
    owStoppingWorkers  : TObjectList;
    owThreadDataFactory: TOTPThreadDataFactory;
    owUniqueID         : int64;
    owWorkItemQueue    : TObjectList;
  strict protected
    function  ActiveWorkItemDescriptions: string;
    procedure ForwardThreadCreated(threadID: DWORD);
    procedure ForwardThreadDestroying(threadID: DWORD;
      threadPoolOperation: TThreadPoolOperation; worker: TOTPWorkerThread = nil);
    procedure InternalStop;
    function  LocateThread(threadID: DWORD): TOTPWorkerThread;
    procedure Log(const msg: string; const params: array of const);
    function  NumRunningStoppedThreads: integer;
    procedure ProcessCompletedWorkItem(workItem: TOTPWorkItem);
    procedure RequestCompleted(workItem: TOTPWorkItem; worker: TOTPWorkerThread);
    procedure ScheduleNext(workItem: TOTPWorkItem);
    procedure StopThread(worker: TOTPWorkerThread);
  protected
    procedure Cleanup; override;
    function  Initialize: boolean; override;
  public
    CountQueued                : TGp4AlignedInt;
    CountQueuedLock            : TOmniCS;
    CountRunning               : TGp4AlignedInt;
    IdleWorkerThreadTimeout_sec: TGp4AlignedInt;
    MaxExecuting               : TGp4AlignedInt;
    MaxQueued                  : TGp4AlignedInt;
    MaxQueuedTime_sec          : TGp4AlignedInt;
    MinWorkers                 : TGp4AlignedInt;
    WaitOnTerminate_sec        : TGp4AlignedInt;
    constructor Create(const name: string; uniqueID: int64);
  published
    // invoked from TOmniThreadPool
    procedure Cancel(const params: TOmniValue);
    procedure CancelAll(var doneSignal: TOmniWaitableValue);
    procedure MaintainanceTimer;
    // invoked from TOTPWorkerThreads
    procedure CheckIdleQueue;
    procedure MsgCompleted(var msg: TOmniMessage); message MSG_COMPLETED;
    procedure MsgThreadCreated(var msg: TOmniMessage); message MSG_THREAD_CREATED;
    procedure MsgThreadDestroying(var msg: TOmniMessage); message MSG_THREAD_DESTROYING;
    procedure PruneWorkingQueue;
    procedure RemoveMonitor;
    procedure Schedule(var workItem: TOTPWorkItem);
    procedure SetMonitor(const params: TOmniValue);
    procedure SetName(const name: TOmniValue);
    procedure SetThreadDataFactory(const threadDataFactory: TOmniValue);
  end; { TOTPWorker }

  TOTPThreadDataFactoryData = class
  strict private
    tdfdExecutable: TOTPThreadDataFactory;
  public
    constructor Create(const executable: TOTPThreadDataFactoryMethod); overload;
    constructor Create(const executable: TOTPThreadDataFactoryFunction); overload;
    property Executable: TOTPThreadDataFactory read tdfdExecutable;
  end; { TOTPThreadDataFactoryData }

  TOmniThreadPool = class(TInterfacedObject, IOmniThreadPool, IOmniThreadPoolScheduler)
  strict private
    otpPoolName         : string;
    otpThreadDataFactory: TOTPThreadDataFactory;
    otpUniqueID         : int64;
    otpWorker           : IOmniWorker;
    otpWorkerTask       : IOmniTaskControl;
  strict protected
    procedure Log(const msg: string; const params: array of const);
  protected
    function  GetIdleWorkerThreadTimeout_sec: integer;
    function  GetMaxExecuting: integer;
    function  GetMaxQueued: integer;
    function  GetMaxQueuedTime_sec: integer;
    function  GetMinWorkers: integer;
    function  GetName: string;
    function  GetUniqueID: int64;
    function  GetWaitOnTerminate_sec: integer;
    procedure SetIdleWorkerThreadTimeout_sec(value: integer);
    procedure SetMaxExecuting(value: integer);
    procedure SetMaxQueued(value: integer);
    procedure SetMaxQueuedTime_sec(value: integer);
    procedure SetMinWorkers(value: integer);
    procedure SetName(const value: string);
    procedure SetWaitOnTerminate_sec(value: integer);
    function  WorkerObj: TOTPWorker;
  public
    constructor Create(const name: string);
    destructor  Destroy; override;
    function  Cancel(taskID: int64): boolean;
    procedure CancelAll;
    function  CountExecuting: integer;
    function  CountQueued: integer;
    function  IsIdle: boolean;
    function  MonitorWith(const monitor: IOmniThreadPoolMonitor): IOmniThreadPool;
    function  RemoveMonitor: IOmniThreadPool;
    procedure Schedule(const task: IOmniTask);
    function  SetMonitor(hWindow: THandle): IOmniThreadPool;
    procedure SetThreadDataFactory(const value: TOTPThreadDataFactoryMethod); overload;
    procedure SetThreadDataFactory(const value: TOTPThreadDataFactoryFunction); overload;
    property IdleWorkerThreadTimeout_sec: integer
      read GetIdleWorkerThreadTimeout_sec write SetIdleWorkerThreadTimeout_sec;
    property MaxExecuting: integer read GetMaxExecuting write SetMaxExecuting;
    property MaxQueued: integer read GetMaxQueued write SetMaxQueued;
    property MaxQueuedTime_sec: integer read GetMaxQueuedTime_sec
      write SetMaxQueuedTime_sec;
    property MinWorkers: integer read GetMinWorkers write SetMinWorkers;
    property Name: string read GetName write SetName;
    property UniqueID: int64 read GetUniqueID;
    property WaitOnTerminate_sec: integer read GetWaitOnTerminate_sec write
      SetWaitOnTerminate_sec;
  end; { TOmniThreadPool }

const
  CGlobalOmniThreadPoolName = 'GlobalOmniThreadPool';

var
  GOmniThreadPool: IOmniThreadPool = nil;

{ exports }

function GlobalOmniThreadPool: IOmniThreadPool;
begin
  if not assigned(GOmniThreadPool) then
    GOmniThreadPool := CreateThreadPool(CGlobalOmniThreadPoolName);
  Result := GOmniThreadPool;
end; { GlobalOmniThreadPool }

function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;
begin
  Result := TOmniThreadPool.Create(threadPoolName);
end; { CreateThreadPool }

{ TOmniThreadPoolMonitorInfo }

constructor TOmniThreadPoolMonitorInfo.Create(uniqueID: int64;
  threadPoolOperation: TThreadPoolOperation; threadID: integer);
begin
  otpmiUniqueID := uniqueID;
  otpmiThreadPoolOperation := threadPoolOperation;
  otpmiThreadID := threadID;
end; { TOmniThreadPoolMonitorInfo.Create }

constructor TOmniThreadPoolMonitorInfo.Create(uniqueID, taskID: int64);
begin
  otpmiUniqueID := uniqueID;
  otpmiThreadPoolOperation := tpoWorkItemCompleted;
  otpmiTaskID := taskID;
end; { TOmniThreadPoolMonitorInfo.Create }

{ TOTPThreadDataFactory }

constructor TOTPThreadDataFactory.Create(const a: TOTPThreadDataFactoryFunction);
begin
  tdfExecutable.Proc := TProcedure(a);
end; { TOTPThreadDataFactory.Create }

constructor TOTPThreadDataFactory.Create(const a: TOTPThreadDataFactoryMethod);
begin
  tdfExecutable.Method := TMethod(a);
end; { TOTPThreadDataFactory.Create }

function TOTPThreadDataFactory.Execute: IInterface;
begin
  case tdfExecutable.Kind of
    oekProcedure:
      Result := TOTPThreadDataFactoryFunction(tdfExecutable.Proc)();
    oekMethod:
      Result := TOTPThreadDataFactoryMethod(tdfExecutable.Method)();
    else raise Exception.Create('TOTPThreadDataFactory.Execute: Not supported!');
  end;
end; { TOTPThreadDataFactory.Execute }

function TOTPThreadDataFactory.IsEmpty: boolean;
begin
  Result := tdfExecutable.IsNull;
end; { TOTPThreadDataFactory.IsEmpty }

{ TOTPWorkItem }

constructor TOTPWorkItem.Create(const task: IOmniTask);
begin
  inherited Create;
  owiTask := task;
  owiScheduledAt := Now;
  owiScheduled_ms := DSiTimeGetTime64;
  owiUniqueID := owiTask.UniqueID;
end; { TOTPWorkItem.Create }

function TOTPWorkItem.Description: string;
begin
  if assigned(task) then
    Result := Format('%s:%d', [task.Name, UniqueID])
  else
    Result := Format(':%d', [UniqueID]);
end; { TOTPWorkItem.Description }

procedure TOTPWorkItem.TerminateTask(exitCode: integer; const exitMessage: string);
begin
  if assigned(owiTask) then begin
    owiTask.Enforced(false);
    owiTask.SetExitStatus(exitCode, exitMessage);
    owiTask.Terminate;
    owiTask := nil;
  end;
end; { TOTPWorkItem.TerminateTask }

{ TOTPWorkerThread }

constructor TOTPWorkerThread.Create(const ThreadDataFactory: TOTPThreadDataFactory);
begin
  inherited Create(true);
  {$IFDEF LogThreadPool}Log('Creating thread %s', [Description]);{$ENDIF LogThreadPool}
  owtThreadDataFactory := ThreadDataFactory;
  owtNewWorkEvent := CreateEvent(nil, false, false, nil);
  owtTerminateEvent := CreateEvent(nil, false, false, nil);
  owtWorkItemLock := CreateOmniCriticalSection;
  owtCommChannel := CreateTwoWayChannel(100, owtTerminateEvent);
end; { TOTPWorkerThread.Create }

destructor TOTPWorkerThread.Destroy;
begin
  {$IFDEF LogThreadPool}Log('Destroying thread %s', [Description]);{$ENDIF LogThreadPool}
  owtWorkItemLock := nil;
  DSiCloseHandleAndNull(owtTerminateEvent);
  DSiCloseHandleAndNull(owtNewWorkEvent);
  inherited Destroy;
end; { TOTPWorkerThread.Destroy }

/// <summary>Gently stop the worker thread.
procedure TOTPWorkerThread.Asy_Stop;
var
  task: IOmniTask;
begin
  {$IFDEF LogThreadPool}Log('Stop thread %s', [Description]);{$ENDIF LogThreadPool}
  if assigned(owtWorkItemLock) then begin // Stop may be called during Cancel[All] and owtWorkItemLock may already be destroyed
    owtWorkItemLock.Acquire;
    try
      if assigned(WorkItem_ref) then begin
        task := WorkItem_ref.task;
        if assigned(task) then
          task.Terminate;
      end;
    finally owtWorkItemLock.Release end;
  end;
end; { TOTPWorkerThread.Asy_Stop }

/// <summary>Take the work item ownership from the thread. Called asynchronously from the thread pool.</summary>
/// <returns>True if thread should be killed.</returns>
/// <since>2008-07-26</since>
function TOTPWorkerThread.Asy_TerminateWorkItem(var workItem: TOTPWorkItem): boolean;
begin
  {$IFDEF LogThreadPool}Log('Asy_TerminateWorkItem thread %s', [Description]);{$ENDIF LogThreadPool}
  Result := false;
  owtWorkItemLock.Acquire;
  try
    if assigned(WorkItem_ref) then begin
      {$IFDEF LogThreadPool}Log('Thread %s has work item', [Description]);{$ENDIF LogThreadPool}
      workItem := WorkItem_ref;
      WorkItem_ref := nil;
      if assigned(workItem) and assigned(workItem.task) and
        (not workItem.task.Stopped) then
      begin
        workItem.TerminateTask(EXIT_THREADPOOL_CANCELLED, 'Cancelled');
        Result := true;
      end
      else if assigned(workItem) then
        Result := false;
    end;
  finally owtWorkItemLock.Release; end;
end; { TOTPWorkerThread.Asy_TerminateWorkItem }

function TOTPWorkerThread.Comm: IOmniCommunicationEndpoint;
begin
  Result := owtCommChannel.Endpoint1;
end; { TOTPWorkerThread.Comm }

function TOTPWorkerThread.Description: string;
begin
  if not assigned(Self) then
    Result := '<none>'
  else
    Result := Format('%p:%d', [pointer(Self), ThreadID]);
end; { TOTPWorkerThread.Description }

procedure TOTPWorkerThread.Execute;
var
  msg: TOmniMessage;
begin
  {$IFDEF LogThreadPool}Log('>>>Execute thread %s', [Description]);{$ENDIF LogThreadPool}
  SendThreadNotifications(tntCreate, 'OtlThreadPool worker');
  try
    Comm.Send(MSG_THREAD_CREATED, threadID);
    try
      if owtThreadDataFactory.IsEmpty then
        owtThreadData := nil
      else
        owtThreadData := owtThreadDataFactory.Execute;
      while true do begin
        if Comm.ReceiveWait(msg, INFINITE) then begin
          case msg.MsgID of
            MSG_RUN:
              ExecuteWorkItem(TOTPWorkItem(msg.MsgData.AsObject));
            MSG_STOP:
              break; // while
          else
            raise Exception.CreateFmt(
              'TOTPWorkerThread.Execute: Unexpected message %d', [msg.MsgID]);
          end; // case
        end; // if Comm.ReceiveWait
      end; // while Comm.ReceiveWait()
    finally Comm.Send(MSG_THREAD_DESTROYING, threadID); end;
  finally SendThreadNotifications(tntDestroy, 'OtlThreadPool worker'); end;
  {$IFDEF LogThreadPool}Log('<<<Execute thread %s', [Description]);{$ENDIF LogThreadPool}
end; { TOTPWorkerThread.Execute }

procedure TOTPWorkerThread.ExecuteWorkItem(workItem: TOTPWorkItem);
{$IFDEF LogThreadPool}
var
  creationTime   : TDateTime;
  startKernelTime: int64;
  startUserTime  : int64;
  stopKernelTime : int64;
  stopUserTime   : int64;
{$ENDIF LogThreadPool}
var
  task     : IOmniTask;
begin
  WorkItem_ref := workItem;
  task := WorkItem_ref.task;
  try
    {$IFDEF LogThreadPool}Log('Thread %s starting execution of %s', [Description, WorkItem_ref.Description]);
    DSiGetThreadTimes(creationTime, startUserTime, startKernelTime); {$ENDIF LogThreadPool}
    if assigned(task) then
      with (task as IOmniTaskExecutor) do begin
        SetThreadData(owtThreadData);
        Execute;
      end;
    {$IFDEF LogThreadPool}DSiGetThreadTimes(creationTime, stopUserTime, stopKernelTime);
    Log(
      'Thread %s completed execution of %s; user time = %d ms, kernel time = %d ms',
      [Description, WorkItem_ref.Description, Round
        ((stopUserTime - startUserTime) / 10000), Round
        ((stopKernelTime - startKernelTime) / 10000)]); {$ENDIF LogThreadPool}
  finally task := nil; end;
  if assigned(owtWorkItemLock) then owtWorkItemLock.Acquire;
  try
    workItem := WorkItem_ref;
    WorkItem_ref := nil;
    if assigned(workItem) then begin // not already canceled
      {$IFDEF LogThreadPool}Log(
        'Thread %s sending notification of completed work item %s',
        [Description, workItem.Description]); {$ENDIF LogThreadPool}
      Comm.Send(MSG_COMPLETED, workItem);
    end;
  finally if assigned(owtWorkItemLock) then owtWorkItemLock.Release; end;
  SetThreadName('Idle thread worker');
end; { TOTPWorkerThread.ExecuteWorkItem }

function TOTPWorkerThread.GetOwnerCommEndpoint: IOmniCommunicationEndpoint;
begin
  Result := owtCommChannel.Endpoint2;
end; { TOTPWorkerThread.GetOwnerCommEndpoint }

function TOTPWorkerThread.GetWorkItemInfo(var scheduledAt, startedAt: TDateTime;
  var description: string): boolean;
begin
  owtWorkItemLock.Acquire;
  try
    if not assigned(WorkItem_ref) then
      Result := false
    else begin
      scheduledAt := WorkItem_ref.scheduledAt;
      startedAt := WorkItem_ref.startedAt;
      description := WorkItem_ref.description;
      UniqueString(description);
      Result := true;
    end;
  finally owtWorkItemLock.Release; end;
end; { TOTPWorkerThread.GetWorkItemInfo }

function TOTPWorkerThread.IsExecuting(taskID: int64): boolean;
begin
  owtWorkItemLock.Acquire;
  try
    Result := assigned(WorkItem_ref) and (WorkItem_ref.UniqueID = taskID);
  finally owtWorkItemLock.Release; end;
end; { TOTPWorkerThread.IsExecuting }

procedure TOTPWorkerThread.Log(const msg: string; const params: array of const);
begin
  {$IFDEF LogThreadPool}
  OutputDebugString(PChar(Format(msg, params)));
  {$ENDIF LogThreadPool}
end; { TOTPWorkerThread.Log }

procedure TOTPWorkerThread.Start;
begin
  {$IFDEF OTL_DeprecatedResume}
  inherited Start;
  {$ELSE}
  inherited Resume;
  {$ENDIF OTL_DeprecatedResume}
end; { TOTPWorkerThread.Start }

function TOTPWorkerThread.WorkItemDescription: string;
begin
  owtWorkItemLock.Acquire;
  try
    if assigned(WorkItem_ref) then
      Result := WorkItem_ref.Description
    else
      Result := '';
  finally owtWorkItemLock.Release; end;
end; { TOTPWorkerThread.WorkItemDescription }

{ TOTPWorker }

constructor TOTPWorker.Create(const name: string; uniqueID: int64);
begin
  inherited Create;
  owName := name;
  owUniqueID := uniqueID;
end; { TOTPWorker.Create }

function TOTPWorker.ActiveWorkItemDescriptions: string;
var
  description   : string;
  iWorker       : integer;
  sbDescriptions: TStringBuilder;
  ScheduledAt   : TDateTime;
  StartedAt     : TDateTime;
  worker        : TOTPWorkerThread;
begin
  sbDescriptions := TStringBuilder.Create;
  try
    for iWorker := 0 to owRunningWorkers.Count - 1 do begin
      worker := TOTPWorkerThread(owRunningWorkers[iWorker]);
      if worker.GetWorkItemInfo(ScheduledAt, StartedAt, description)
      then
        sbDescriptions.Append('[').Append(iWorker + 1).Append('] ').Append
          (FormatDateTime('hh:nn:ss', ScheduledAt)).Append(' / ').Append
          (FormatDateTime('hh:nn:ss', StartedAt)).Append(' ').Append
          (description);
    end;
    Result := sbDescriptions.ToString;
  finally FreeAndNil(sbDescriptions); end;
end; { TGpThreadPool.ActiveWorkItemDescriptions }

/// <returns>True: Normal exit, False: Thread was killed.</returns> 
procedure TOTPWorker.Cancel(const params: TOmniValue);
var
  endWait_ms   : int64;
  iWorker      : integer;
  taskID       : int64;
  waitParam    : TOmniValue;
  wasTerminated: boolean;
  worker       : TOTPWorkerThread;
  workItem     : TOTPWorkItem;
begin
  taskID := params[0];
  wasTerminated := true;
  for iWorker := 0 to owRunningWorkers.Count - 1 do begin
    worker := TOTPWorkerThread(owRunningWorkers[iWorker]);
    if worker.IsExecuting(taskID) then begin
      {$IFDEF LogThreadPool}Log('Cancel request %d on thread %p:%d', [taskID, pointer(worker), worker.threadID]); {$ENDIF LogThreadPool}
      owRunningWorkers.Delete(iWorker);
      worker.Asy_Stop;
      endWait_ms := DSiTimeGetTime64 + int64(WaitOnTerminate_sec) * 1000;
      while (DSiTimeGetTime64 < endWait_ms) and (not worker.Stopped) do begin
        ProcessMessages;
        Sleep(10);
      end;
      SuspendThread(worker.Handle);
      if worker.Asy_TerminateWorkItem(workItem) then begin
        ProcessCompletedWorkItem(workItem);
        {$IFDEF LogThreadPool}Log(
          'Terminating unstoppable thread %s, num idle = %d, num running = %d[%d]',
          [worker.Description, owIdleWorkers.Count, owRunningWorkers.Count,
          MaxExecuting.Value]); {$ENDIF LogThreadPool}
        TerminateThread(worker.Handle, cardinal(-1));
        ForwardThreadDestroying(worker.threadID, tpoKillThread, worker);
        FreeAndNil(worker);
        wasTerminated := false;
      end
      else begin
        ResumeThread(worker.Handle);
        owIdleWorkers.Add(worker);
        {$IFDEF LogThreadPool}Log(
          'Thread %s moved to the idle list, num idle = %d, num running = %d[%d]',
          [worker.Description, owIdleWorkers.Count, owRunningWorkers.Count,
          MaxExecuting.Value]); {$ENDIF LogThreadPool}
      end;
      break; // for 
    end;
  end; // for iWorker
  waitParam := params[1];
  (waitParam.AsObject as TOmniWaitableValue).Signal(wasTerminated);
end; { TOTPWorker.Cancel }

procedure TOTPWorker.CancelAll(var doneSignal: TOmniWaitableValue);
begin
  InternalStop;
  doneSignal.Signal;
end; { TOTPWorker.CancelAll }

procedure TOTPWorker.Cleanup;
begin
  owDestroying := true;
  InternalStop;
  FreeAndNil(owStoppingWorkers);
  FreeAndNil(owRunningWorkers);
  FreeAndNil(owIdleWorkers);
  FreeAndNil(owWorkItemQueue);
end; { TOTPWorker.Cleanup }

procedure TOTPWorker.ForwardThreadCreated(threadID: DWORD);
begin
  if assigned(owMonitorObserver) then
    owMonitorObserver.Send(COmniPoolMsg, 0, cardinal
        (TOmniThreadPoolMonitorInfo.Create(owUniqueID, tpoCreateThread, threadID))
      );
end; { TOTPWorker.ForwardThreadCreated }

procedure TOTPWorker.ForwardThreadDestroying(threadID: DWORD;
  threadPoolOperation: TThreadPoolOperation; worker: TOTPWorkerThread);
begin
  if not assigned(worker) then
    worker := LocateThread(threadID);
  if assigned(worker) then begin
    task.UnregisterComm(worker.OwnerCommEndpoint);
    worker.Stopped := true;
  end;
  if assigned(owMonitorObserver) then
    owMonitorObserver.Send(COmniPoolMsg, 0, cardinal
      (TOmniThreadPoolMonitorInfo.Create(owUniqueID, threadPoolOperation,
        threadID)));
end; { TOTPWorker.ForwardThreadDestroying }

function TOTPWorker.Initialize: boolean;
begin
  owIdleWorkers := TObjectList.Create(false);
  owRunningWorkers := TObjectList.Create(false);
  CountRunning.Value := 0;
  owStoppingWorkers := TObjectList.Create(false);
  owWorkItemQueue := TObjectList.Create(false);
  CountQueued.Value := 0;
  IdleWorkerThreadTimeout_sec.Value := CDefaultIdleWorkerThreadTimeout_sec;
  WaitOnTerminate_sec.Value := CDefaultWaitOnTerminate_sec;
  MaxExecuting.Value := Length(DSiGetThreadAffinity);
  task.SetTimer(1, 1000, @TOTPWorker.MaintainanceTimer);
  Result := true;
end; { TOTPWorker.Initialize }

procedure TOTPWorker.InternalStop;
var
  endWait_ms: int64;
  iWorker: integer;
  iWorkItem: integer;
  queuedItems: TObjectList { of TOTPWorkItem } ;
  worker: TOTPWorkerThread;
  workItem: TOTPWorkItem;
begin
  {$IFDEF LogThreadPool}Log('Terminating queued tasks', []);{$ENDIF LogThreadPool}
  queuedItems := TObjectList.Create(false);
  try
    for iWorkItem := 0 to owWorkItemQueue.Count - 1 do
      queuedItems.Add(owWorkItemQueue[iWorkItem]);
    owWorkItemQueue.Clear;
    CountQueued.Value := 0;
    for iWorkItem := 0 to queuedItems.Count - 1 do begin
      workItem := TOTPWorkItem(queuedItems[iWorkItem]);
      workItem.TerminateTask(EXIT_THREADPOOL_CANCELLED, 'Cancelled');
      RequestCompleted(workItem, nil);
    end; // for iWorkItem
  finally FreeAndNil(queuedItems); end;
  {$IFDEF LogThreadPool}Log('Stopping all threads', []); {$ENDIF LogThreadPool}
  for iWorker := 0 to owIdleWorkers.Count - 1 do
    StopThread(TOTPWorkerThread(owIdleWorkers[iWorker]));
  owIdleWorkers.Clear;
  for iWorker := 0 to owRunningWorkers.Count - 1 do
    StopThread(TOTPWorkerThread(owRunningWorkers[iWorker]));
  owRunningWorkers.Clear;
  CountRunning.Value := 0;
  endWait_ms := DSiTimeGetTime64 + int64(WaitOnTerminate_sec) * 1000;
  while (endWait_ms > DSiTimeGetTime64) and (NumRunningStoppedThreads > 0) do begin
    ProcessMessages;
    // TODO 1 -oPrimoz Gabrijelcic : ! what happens here during CancelAll? can the task die? !
    Sleep(10);
  end;
  for iWorker := 0 to owStoppingWorkers.Count - 1 do begin
    worker := TOTPWorkerThread(owStoppingWorkers[iWorker]);
    worker.Asy_TerminateWorkItem(workItem);
    FreeAndNil(worker);
  end;
  owStoppingWorkers.Clear;
end; { TOTPWorker.InternalStop }

function TOTPWorker.LocateThread(threadID: DWORD): TOTPWorkerThread;
var
  oThread: pointer;
begin
  for oThread in owRunningWorkers do begin
    Result := TOTPWorkerThread(oThread);
    if Result.threadID = threadID then
      Exit;
  end;
  for oThread in owIdleWorkers do begin
    Result := TOTPWorkerThread(oThread);
    if Result.threadID = threadID then
      Exit;
  end;
  for oThread in owStoppingWorkers do begin
    Result := TOTPWorkerThread(oThread);
    if Result.threadID = threadID then
      Exit;
  end;
  Result := nil;
end; { TOTPWorker.LocateThread }

procedure TOTPWorker.Log(const msg: string; const params: array of const );
begin
  {$IFDEF LogThreadPool}
  OutputDebugString(PChar(Format(msg, params)));
  {$ENDIF LogThreadPool}
end; { TOTPWorker.Log }

procedure TOTPWorker.MaintainanceTimer;
var
  iWorker: integer;
  worker : TOTPWorkerThread;
begin
  PruneWorkingQueue;
  if IdleWorkerThreadTimeout_sec > 0 then begin
    iWorker := 0;
    while (owIdleWorkers.Count > MinWorkers.Value) and
          (iWorker < owIdleWorkers.Count) do
    begin
      worker := TOTPWorkerThread(owIdleWorkers[iWorker]);
      if (worker.StartStopping_ms = 0) and
        ((worker.StartIdle_ms + int64(IdleWorkerThreadTimeout_sec) * 1000)
          < DSiTimeGetTime64) then
      begin
        {$IFDEF LogThreadPool}Log(
          'Destroying idle thread %s because it was idle for more than %d seconds',
          [worker.Description, IdleWorkerThreadTimeout_sec.Value]);
        {$ENDIF LogThreadPool}
        owIdleWorkers.Delete(iWorker);
        StopThread(worker);
      end
      else
        Inc(iWorker);
    end; // while 
  end;
  iWorker := 0;
  while iWorker < owStoppingWorkers.Count do begin
    worker := TOTPWorkerThread(owStoppingWorkers[iWorker]);
    if worker.Stopped or ((worker.StartStopping_ms + int64(WaitOnTerminate_sec)
          * 1000) < DSiTimeGetTime64) then
    begin
      if not worker.Stopped then begin
        SuspendThread(worker.Handle);
        if worker.Stopped then begin
          ResumeThread(worker.Handle);
          break; // while 
        end;
        TerminateThread(worker.Handle, cardinal(-1));
        ForwardThreadDestroying(worker.threadID, tpoKillThread, worker);
      end
      else begin
        {$IFDEF LogThreadPool}Log('Removing stopped thread %s', [worker.Description]);{$ENDIF LogThreadPool}
      end;
      owStoppingWorkers.Delete(iWorker);
      FreeAndNil(worker);
    end
    else
      Inc(iWorker);
  end;
end; { TOTPWorker.MaintainanceTimer }

procedure TOTPWorker.CheckIdleQueue;
var
  workItem: TOTPWorkItem;
begin
  if (not owDestroying) and (owWorkItemQueue.Count > 0) and
    ((owIdleWorkers.Count > 0) or (owRunningWorkers.Count < MaxExecuting.Value)) then
  begin
    workItem := TOTPWorkItem(owWorkItemQueue[0]);
    owWorkItemQueue.Delete(0);
    CountQueued.Decrement;
    {$IFDEF LogThreadPool}Log('Dequeueing %s ', [workItem.Description]);{$ENDIF LogThreadPool}
    ScheduleNext(workItem);
  end;
end; { TOTPWorker.CheckIdleQueue }

procedure TOTPWorker.MsgCompleted(var msg: TOmniMessage);
begin
  ProcessCompletedWorkItem(TOTPWorkItem(msg.MsgData.AsObject));
end; { TOTPWorker.MsgCompleted }

procedure TOTPWorker.MsgThreadCreated(var msg: TOmniMessage);
begin
  ForwardThreadCreated(msg.MsgData);
end; { TOTPWorker.MsgThreadCreated }

procedure TOTPWorker.MsgThreadDestroying(var msg: TOmniMessage);
begin
  ForwardThreadDestroying(msg.MsgData, tpoDestroyThread);
end; { TOTPWorker.MsgThreadDestroying }

/// <summary>Counts number of threads in the 'stopping' queue that are still doing work.</summary> 
/// <since>2007-07-10</since> 
function TOTPWorker.NumRunningStoppedThreads: integer;
var
  iThread: integer;
  worker : TOTPWorkerThread;
begin
  Result := 0;
  for iThread := 0 to owStoppingWorkers.Count - 1 do begin
    worker := TOTPWorkerThread(owStoppingWorkers[iThread]);
    if not worker.Stopped then
      Inc(Result);
  end; // for iThread 
end; { TOTPWorker.NumRunningStoppedThreads }

procedure TOTPWorker.ProcessCompletedWorkItem(workItem: TOTPWorkItem);
var
  worker: TOTPWorkerThread;
begin
  worker := workItem.Thread;
  {$IFDEF LogThreadPool}Log('Thread %s completed request %s',
    [worker.Description, workItem.Description]); {$ENDIF LogThreadPool}
  if owDestroying then begin
    FreeAndNil(workItem);
    Exit;
  end;
  {$IFDEF LogThreadPool}
  Log('Thread %s completed request %s', [worker.Description, workItem.Description]);
  Log('Destroying %s', [workItem.Description]);
  {$ENDIF LogThreadPool}
  if assigned(owMonitorObserver) then
    owMonitorObserver.Send(COmniPoolMsg, 0, cardinal
      (TOmniThreadPoolMonitorInfo.Create(owUniqueID, workItem.UniqueID)));
  FreeAndNil(workItem);
  // if (not (owDestroying)) and (owStoppingWorkers.IndexOf(worker) >= 0) then begin 
  // owStoppingWorkers.Remove(worker); 
  // owIdleWorkers.Add(worker); 
  // end; 
  if owRunningWorkers.IndexOf(worker) < 0 then
    worker := nil;
  if assigned(worker) then begin // move it back to the idle queue 
    owRunningWorkers.Extract(worker);
    CountRunning.Decrement;
    if (not worker.RemoveFromPool) and
      ((MaxExecuting.Value < 0) or (owRunningWorkers.Count < MaxExecuting.Value)) then
    begin
      worker.StartIdle_ms := DSiTimeGetTime64;
      owIdleWorkers.Add(worker);
      {$IFDEF LogThreadPool}Log(
        'Thread %s moved back to the idle list, num idle = %d, num running = %d[%d]'
          , [worker.Description, owIdleWorkers.Count, owRunningWorkers.Count,
        MaxExecuting.Value]); {$ENDIF LogThreadPool}
    end
    else begin
      {$IFDEF LogThreadPool}Log(
        'Destroying thread %s, num idle = %d, num running = %d[%d]',
        [worker.Description, owIdleWorkers.Count, owRunningWorkers.Count,
        MaxExecuting.Value]); {$ENDIF LogThreadPool}
      StopThread(worker);
    end;
  end;
  CheckIdleQueue;
end; { TOTPWorker.ProcessCompletedWorkItem }

procedure TOTPWorker.PruneWorkingQueue;
var
  errorMsg      : string;
  iWorkItem     : integer;
  maxWaitTime_ms: int64;
  workItem      : TOTPWorkItem;
begin
  if MaxQueued.Value > 0 then begin
    while owWorkItemQueue.Count > MaxQueued.Value do begin
      workItem := TOTPWorkItem(owWorkItemQueue[owWorkItemQueue.Count - 1]);
      {$IFDEF LogThreadPool}Log(
        'Removing request %s from work item queue because queue length > %d',
        [workItem.Description, MaxQueued.Value]); {$ENDIF LogThreadPool}
      owWorkItemQueue.Delete(owWorkItemQueue.Count - 1);
      CountQueued.Decrement;
      errorMsg := Format('Execution queue is too long (%d work items)',
        [owWorkItemQueue.Count]);
      workItem.TerminateTask(EXIT_THREADPOOL_QUEUE_TOO_LONG, errorMsg);
      RequestCompleted(workItem, nil);
    end; // while
  end;
  if MaxQueuedTime_sec.Value > 0 then begin
    iWorkItem := 0;
    while iWorkItem < owWorkItemQueue.Count do begin
      workItem := TOTPWorkItem(owWorkItemQueue[iWorkItem]);
      maxWaitTime_ms := workItem.Scheduled_ms + int64(MaxQueuedTime_sec.Value) * 1000;
      if maxWaitTime_ms > DSiTimeGetTime64 then
        Inc(iWorkItem)
      else begin
        {$IFDEF LogThreadPool}Log(
          'Removing request %s from work item queue because it is older than %d seconds',
          [workItem.Description, MaxQueuedTime_sec.Value]); {$ENDIF LogThreadPool}
        owWorkItemQueue.Delete(iWorkItem);
        CountQueued.Decrement;
        errorMsg := Format('Maximum queued time exceeded.' +
            ' Pool = %0:s, Now = %1:s, Max executing = %2:d,' +
            ' Removed entry queue time = %3:s, Removed entry description = %4:s.'
            + ' Active entries: %5:s', [ { 0 } owName,
          { 1 } FormatDateTime('hh:nn:ss', Now), { 2 } MaxExecuting.Value,
          { 3 } FormatDateTime('hh:nn:ss', workItem.ScheduledAt),
          { 4 } workItem.Description, { 5 } ActiveWorkItemDescriptions]);
        workItem.TerminateTask(EXIT_THREADPOOL_STALE_TASK, errorMsg);
        RequestCompleted(workItem, nil);
      end;
    end; // while
  end;
end; { TOTPWorker.PruneWorkingQueue }

procedure TOTPWorker.RemoveMonitor;
begin
  FreeAndNil(owMonitorObserver);
end; { TOTPWorker.RemoveMonitor }

procedure TOTPWorker.RequestCompleted(workItem: TOTPWorkItem;
  worker: TOTPWorkerThread);
begin
  workItem.Thread := worker;
  ProcessCompletedWorkItem(workItem);
end; { TOTPWorker.RequestCompleted }

procedure TOTPWorker.Schedule(var workItem: TOTPWorkItem);
begin
  CountQueuedLock.Acquire;
  try
    CountQueued.Decrement;
    ScheduleNext(workItem);
  finally CountQueuedLock.Release; end;
  PruneWorkingQueue;
end; { TOTPWorker.Schedule }

procedure TOTPWorker.ScheduleNext(workItem: TOTPWorkItem);
var
  worker: TOTPWorkerThread;
begin
  worker := nil;
  if (MaxExecuting = -1) or (owRunningWorkers.Count < MaxExecuting.Value) then begin
    if owIdleWorkers.Count > 0 then begin
      worker := TOTPWorkerThread(owIdleWorkers[owIdleWorkers.Count - 1]);
      owIdleWorkers.Delete(owIdleWorkers.Count - 1);
      owRunningWorkers.Add(worker);
      CountRunning.Increment;
      {$IFDEF LogThreadPool}Log(
        'Allocated thread from idle pool, num idle = %d, num running = %d[%d]',
        [owIdleWorkers.Count, owRunningWorkers.Count, MaxExecuting.Value]);
      {$ENDIF LogThreadPool}
    end
    else begin
      if (owRunningWorkers.Count + owIdleWorkers.Count + owStoppingWorkers.Count)
         >= CMaxConcurrentWorkers
      then
        raise Exception.CreateFmt(
          'TOTPWorker.ScheduleNext: Cannot start more than %d threads ' +
            'due to the implementation limitations', [CMaxConcurrentWorkers]);
      worker := TOTPWorkerThread.Create(owThreadDataFactory);
      worker.Start;
      task.RegisterComm(worker.OwnerCommEndpoint);
      owRunningWorkers.Add(worker);
      CountRunning.Increment;
      {$IFDEF LogThreadPool}Log(
        'Created new thread %s, num idle = %d, num running = %d[%d]',
        [worker.Description, owIdleWorkers.Count, owRunningWorkers.Count,
        MaxExecuting.Value]); {$ENDIF LogThreadPool}
    end;
  end;
  if assigned(worker) then begin
    {$IFDEF LogThreadPool}Log('Started %s', [workItem.Description]);{$ENDIF LogThreadPool}
    workItem.StartedAt := Now;
    workItem.Thread := worker;
    // worker.WorkItem_ref := workItem; 
    worker.OwnerCommEndpoint.Send(MSG_RUN, workItem);
  end
  else begin
    {$IFDEF LogThreadPool}Log('Queued %s ', [workItem.Description]);{$ENDIF LogThreadPool}
    owWorkItemQueue.Add(workItem);
    CountQueued.Increment;
    if (MaxQueued > 0) and (owWorkItemQueue.Count >= MaxQueued.Value) then
      PruneWorkingQueue;
  end;
end; { TOTPWorker.ScheduleNext }

procedure TOTPWorker.SetMonitor(const params: TOmniValue);
var
  hWindow  : THandle;
  waitParam: TOmniValue;
begin
  hWindow := params[0];
  if not assigned(owMonitorObserver) then
    owMonitorObserver :=
      CreateContainerWindowsMessageObserver(hWindow, COmniPoolMsg, 0, 0)
  else if owMonitorObserver.Handle <> hWindow then
    raise Exception.Create(
      'TOTPWorker.SetMonitor: Task can be only monitored with a single monitor'
      );
  waitParam := params[1];
  (waitParam.AsObject as TOmniWaitableValue).Signal;
end; { TOTPWorker.SetMonitor }

procedure TOTPWorker.SetName(const name: TOmniValue);
begin
  owName := name;
end; { TOTPWorker.SetName }

procedure TOTPWorker.SetThreadDataFactory(const threadDataFactory: TOmniValue);
var
  factoryData: TOTPThreadDataFactoryData;
begin
  factoryData := threadDataFactory.AsObject as TOTPThreadDataFactoryData;
  owThreadDataFactory := factoryData.Executable;
  FreeAndNil(factoryData);
end; { TOTPWorker.SetThreadDataFactory }

/// <summary>Move the thread to the 'stopping' list and tell it to CancelAll.<para> 
/// Thread is guaranted not to be in 'idle' or 'working' list when StopThread is called.</para></summary> 
/// <since>2007-07-10</since>
procedure TOTPWorker.StopThread(worker: TOTPWorkerThread);
begin
  {$IFDEF LogThreadPool}Log('Stopping worker thread %s', [worker.Description]);{$ENDIF LogThreadPool}
  owStoppingWorkers.Add(worker);
  worker.StartStopping_ms := DSiTimeGetTime64;
  worker.Asy_Stop; // have to force asynchronous stop as the worker thread may be stuck in the ExecuteWorkItem
  worker.OwnerCommEndpoint.Send(MSG_STOP);
  {$IFDEF LogThreadPool}Log('num stopped = %d', [owStoppingWorkers.Count]);{$ENDIF LogThreadPool}
end; { TOTPWorker.StopThread }

{ TOTPThreadDataFactoryData }

constructor TOTPThreadDataFactoryData.Create(const executable:
  TOTPThreadDataFactoryFunction);
begin
  inherited Create;
  tdfdExecutable := TOTPThreadDataFactory.Create(executable);
end; { TOTPThreadDataFactoryData.Create }

constructor TOTPThreadDataFactoryData.Create(const executable:
  TOTPThreadDataFactoryMethod);
begin
  inherited Create;
  tdfdExecutable := TOTPThreadDataFactory.Create(executable);
end; { TOTPThreadDataFactoryData.Create }

{ TOmniThreadPool }

constructor TOmniThreadPool.Create(const name: string);
begin
  inherited Create;
  {$IFDEF LogThreadPool}Log('Creating thread pool %p [%s]', [pointer(Self), name]);{$ENDIF LogThreadPool}
  otpPoolName := name;
  otpUniqueID := OtlUID.Increment;
  otpWorker := TOTPWorker.Create(name, otpUniqueID);
  otpWorkerTask := CreateTask
    (otpWorker, Format('OmniThreadPool manager %s', [name])).Run;
  otpWorkerTask.WaitForInit;
end; { TOmniThreadPool.Create }

destructor TOmniThreadPool.Destroy;
begin
  {$IFDEF LogThreadPool}Log('Destroying thread pool %p', [pointer(Self), otpPoolName]);{$ENDIF LogThreadPool}
  otpWorkerTask.Terminate;
  inherited;
end; { TOmniThreadPool.Destroy }

/// <returns>True: Normal exit, False: Thread was killed.</returns> 
function TOmniThreadPool.Cancel(taskID: int64): boolean;
var
  res: TOmniWaitableValue;
begin
  {$IF CompilerVersion >= 22} // starting with XE, Delphi complains that result is not always assigned if this line is removed
  Result := false; // not really used
  {$IFEND}
  res := TOmniWaitableValue.Create;
  try
    otpWorkerTask.Invoke(@TOTPWorker.Cancel, [taskID, res]);
    res.WaitFor(INFINITE);
    Result := res.Value;
  finally FreeAndNil(res); end;
end; { TOmniThreadPool.Cancel }

procedure TOmniThreadPool.CancelAll;
var
  res: TOmniWaitableValue;
begin
  res := TOmniWaitableValue.Create;
  try
    otpWorkerTask.Invoke(@TOTPWorker.CancelAll, res);
    res.WaitFor(INFINITE);
  finally FreeAndNil(res); end;
end; { TOmniThreadPool.CancelAll }

function TOmniThreadPool.CountExecuting: integer;
begin
  Result := WorkerObj.CountRunning.Value;
end; { TOmniThreadPool.CountExecuting }

function TOmniThreadPool.CountQueued: integer;
begin
  WorkerObj.CountQueuedLock.Acquire;
  try
    Result := WorkerObj.CountQueued.Value;
  finally WorkerObj.CountQueuedLock.Release; end;
end; { TOmniThreadPool.CountQueued }

function TOmniThreadPool.GetIdleWorkerThreadTimeout_sec: integer;
begin
  Result := WorkerObj.IdleWorkerThreadTimeout_sec.Value;
end; { TOmniThreadPool.GetIdleWorkerThreadTimeout_sec }

function TOmniThreadPool.GetMaxExecuting: integer;
begin
  Result := WorkerObj.MaxExecuting.Value;
end; { TOmniThreadPool.GetMaxExecuting }

function TOmniThreadPool.GetMaxQueued: integer;
begin
  Result := WorkerObj.MaxQueued.Value;
end; { TOmniThreadPool.GetMaxQueued }

function TOmniThreadPool.GetMaxQueuedTime_sec: integer;
begin
  Result := WorkerObj.MaxQueuedTime_sec.Value;
end; { TOmniThreadPool.GetMaxQueuedTime_sec }

function TOmniThreadPool.GetMinWorkers: integer;
begin
  Result := WorkerObj.MinWorkers.Value;
end; { TOmniThreadPool.GetMinWorkers }

function TOmniThreadPool.GetName: string;
begin
  Result := otpPoolName;
end; { TOmniThreadPool.GetName }

function TOmniThreadPool.GetUniqueID: int64;
begin
  Result := otpUniqueID;
end; { TOmniThreadPool.GetUniqueID }

function TOmniThreadPool.GetWaitOnTerminate_sec: integer;
begin
  Result := WorkerObj.WaitOnTerminate_sec.Value;
end; { TOmniThreadPool.GetWaitOnTerminate_sec }

function TOmniThreadPool.IsIdle: boolean;
begin
  if CountQueued <> 0 then
    Result := false
  else if CountExecuting <> 0 then
    Result := false
  else
    Result := true;
end; { TOmniThreadPool.IsIdle }

procedure TOmniThreadPool.Log(const msg: string; const params: array of const);
begin
  {$IFDEF LogThreadPool}
  OutputDebugString(PChar(Format(msg, params)));
  {$ENDIF LogThreadPool}
end; { TGpThreadPool.Log }

function TOmniThreadPool.MonitorWith(const monitor: IOmniThreadPoolMonitor):
  IOmniThreadPool;
begin
  monitor.Monitor(Self);
  Result := Self;
end; { TOmniThreadPool.MonitorWith }

function TOmniThreadPool.RemoveMonitor: IOmniThreadPool;
begin
  otpWorkerTask.Invoke(@TOTPWorker.RemoveMonitor);
  Result := Self;
end; { TOmniThreadPool.RemoveMonitor }

procedure TOmniThreadPool.Schedule(const task: IOmniTask);
begin
  WorkerObj.CountQueued.Increment;
  otpWorkerTask.Invoke(@TOTPWorker.Schedule, TOTPWorkItem.Create(task));
end; { TOmniThreadPool.Schedule }

procedure TOmniThreadPool.SetIdleWorkerThreadTimeout_sec(value: integer);
begin
  WorkerObj.IdleWorkerThreadTimeout_sec.Value := value;
end; { TOmniThreadPool.SetIdleWorkerThreadTimeout_sec }

procedure TOmniThreadPool.SetMaxExecuting(value: integer);
begin
  if value > CMaxConcurrentWorkers then
    raise Exception.CreateFmt('TOmniThreadPool.SetMaxExecuting: ' +
        'MaxExecuting cannot be larger than %d due to the implementation limitations',
      [CMaxConcurrentWorkers]);
  WorkerObj.MaxExecuting.Value := value;
  otpWorkerTask.Invoke(@TOTPWorker.CheckIdleQueue);
end; { TOmniThreadPool.SetMaxExecuting }

procedure TOmniThreadPool.SetMaxQueued(value: integer);
begin
  WorkerObj.MaxQueued.Value := value;
  otpWorkerTask.Invoke(@TOTPWorker.PruneWorkingQueue);
end; { TOmniThreadPool.SetMaxQueued }

procedure TOmniThreadPool.SetMaxQueuedTime_sec(value: integer);
begin
  WorkerObj.MaxQueuedTime_sec.Value := value;
  otpWorkerTask.Invoke(@TOTPWorker.PruneWorkingQueue);
end; { TOmniThreadPool.SetMaxQueuedTime_sec }

procedure TOmniThreadPool.SetMinWorkers(value: integer);
begin
  WorkerObj.MinWorkers.Value := value;
end; { TOmniThreadPool.SetMinWorkers }

function TOmniThreadPool.SetMonitor(hWindow: THandle): IOmniThreadPool;
var
  res: TOmniWaitableValue;
begin
  res := TOmniWaitableValue.Create;
  try
    otpWorkerTask.Invoke(@TOTPWorker.SetMonitor, [hWindow, res]);
    res.WaitFor(INFINITE);
  finally FreeAndNil(res); end;
  Result := Self;
end; { TOmniThreadPool.SetMonitor }

procedure TOmniThreadPool.SetName(const value: string);
begin
  otpPoolName := value;
  otpWorkerTask.Invoke(@TOTPWorker.SetName, value);
end; { TOmniThreadPool.SetName }

procedure TOmniThreadPool.SetThreadDataFactory(const value: TOTPThreadDataFactoryMethod);
begin
  otpThreadDataFactory := TOTPThreadDataFactory.Create(value);
  otpWorkerTask.Invoke(@TOTPWorker.SetThreadDataFactory,
    TOTPThreadDataFactoryData.Create(value));
end; { TOmniThreadPool.SetThreadDataFactory }

procedure TOmniThreadPool.SetThreadDataFactory(const value:
  TOTPThreadDataFactoryFunction);
begin
  otpThreadDataFactory := TOTPThreadDataFactory.Create(value);
  otpWorkerTask.Invoke(@TOTPWorker.SetThreadDataFactory,
    TOTPThreadDataFactoryData.Create(value));
end; { TOmniThreadPool.SetThreadDataFactory }

procedure TOmniThreadPool.SetWaitOnTerminate_sec(value: integer);
begin
  WorkerObj.WaitOnTerminate_sec.Value := value;
end; { TOmniThreadPool.SetWaitOnTerminate_sec }

function TOmniThreadPool.WorkerObj: TOTPWorker;
begin
  Result := (otpWorker.Implementor as TOTPWorker);
end; { TOmniThreadPool.WorkerObj }

initialization
finalization
  GOmniThreadPool := nil;
end.
