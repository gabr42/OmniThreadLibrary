///<summary>Thread pool implementation. Part of the OmniThreadLibrary project.</summary>
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
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2008-06-12
///   Last modification : 2008-06-30
///   Version           : 0.0
///</para><para>
///   History:
///</para></remarks>

unit OtlThreadPool;

interface

uses
  OtlTask;

type
  IOmniThreadPool = interface ['{1FA74554-1866-46DD-AC50-F0403E378682}']
    function  GetMaxExecuting: integer; 
    function  GetName: string;
    procedure SetMaxExecuting(value: integer);
    procedure SetName(const value: string);
  //
    procedure Schedule(task: IOmniTask);
    property MaxExecuting: integer read GetMaxExecuting write SetMaxExecuting;
    property Name: string read GetName write SetName;
  //thrown in from my private thread pool unit; to be cleaned up
  {
    procedure CancelAll;
    procedure Cancel(workItemID: int64); <-- task?
    //function  GetActiveWorkItemDescriptions: string; <-- debugging interface, does not belong here
    function  IsIdle: boolean;
    property CountExecuting: integer read GetExecutingCount;
    property CountQueued: integer read GetQueuedCount;
    property IdleWorkerThreadTimeout_sec: integer read tpIdleWorkerThreadTimeout_sec write
      tpIdleWorkerThreadTimeout_sec default CDefaultIdleWorkerThreadTimeout_sec;
    property MinWorkers: integer read tpMinWorkers write tpMinWorkers;
    property MaxQueued: integer read tpMaxQueueLength write SetMaxQueueLength;
    property MaxQueuedTime_sec: integer read tpMaxQueuedTime_sec write SetMaxQueuedTime_sec;
    property WaitOnTerminate_sec: integer read tpWaitOnTerminate_sec write
      tpWaitOnTerminate_sec default 30;
    property OnError: TGpTPError read tpOnError write tpOnError;
    //:Thread created event. Will be called from the context of the worker thread.
    property OnWorkerThreadCreated_Asy: TGpTPWorkerThreadEvent read tpOnWorkerThreadCreated
      write tpOnWorkerThreadCreated;
    //:Thread destroying event. Will be called from the context of the worker thread.
    property OnWorkerThreadDestroying_Asy: TGpTPWorkerThreadEvent read
      tpOnWorkerThreadDestroying write tpOnWorkerThreadDestroying;
    property OnWorkItemDone: TGpTPWorkItemDone read tpOnWorkItemDone write tpOnWorkItemDone;
  }
  end;

  function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;

  function GlobalOmniThreadPool: IOmniThreadPool;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  DSiWin32,
  SpinLock,
  OtlCommon;

type
  ///<enum name='tsUnknownError'>An unknown error has occured.</enum>
  ///<enum name='tsCompleted'>Work item has completed execution.</enum>
  ///<enum name='tsException'>Work item's Execute method has raised an exception.</enum>
  ///<enum name='tsCanceled'>Application called CancelAll.</enum>
  ///<enum name='tsServerBusy'>Server is too busy.</enum>
  TOTPTaskStatus = (tsUnknownError, tsCompleted, tsException, tsCanceled, tsServerBusy);

  TOTPWorkItem = class
  strict private
    owiLastError         : string;
    owiScheduled_ms      : int64;
    owiStarted_ms        : int64;
    owiStatus            : TOTPTaskStatus;
    owiTask              : IOmniTask;
    owiTerminateExecution: boolean;
  private
    function GetUniqueID: int64;
  protected
    procedure SetLastError(status: TOTPTaskStatus; const errorMsg: string);
  public
    constructor Create(task: IOmniTask);
    function  Description: string;
    property LastError: string read owiLastError;
    property Scheduled_ms: int64 read owiScheduled_ms;
    property Started_ms: int64 read owiStarted_ms write owiStarted_ms;
    property Status: TOTPTaskStatus read owiStatus;
    property TerminateExecution: boolean read owiTerminateExecution write owiTerminateExecution;
    property UniqueID: int64 read GetUniqueID;
    property Task: IOmniTask read owiTask;
  end; { TOTPWorkItem }

  TOmniThreadPool = class;

  TOTPWorkerThread = class(TThread)
  private
    owtNewWorkEvent     : TDSiEventHandle;
    owtOwner            : TOmniThreadPool;
    owtRemoveFromPool   : boolean;
    owtStartIdle_ms     : int64;
    owtStartStopping_ms : int64;
    owtTerminateEvent   : TDSiEventHandle;
    owtWorkItemLock     : TTicketSpinLock;
    owtWorkItem_ref     : TOTPWorkItem;
  protected
    procedure Log(const msg: string; params: array of const);
  public
    constructor Create(owner: TOmniThreadPool);
    destructor  Destroy; override;
    procedure Asy_Stop;
    function  Asy_TerminateWorkItem: boolean;
    function  Description: string;
    procedure Execute; override;
    function  GetWorkItemInfo(var scheduled_ms, started_ms: int64;
      var description: string): boolean;
    function  IsExecuting(taskID: int64): boolean;
    function  WorkItemDescription: string;
    property NewWorkEvent: TDSiEventHandle read owtNewWorkEvent;
    property Owner: TOmniThreadPool read owtOwner;
    property RemoveFromPool: boolean read owtRemoveFromPool;
    property StartIdle_ms: int64 read owtStartIdle_ms write owtStartIdle_ms;
    property StartStopping_ms: int64 read owtStartStopping_ms write owtStartStopping_ms;
    property TerminateEvent: TDSiEventHandle read owtTerminateEvent;
    property WorkItem_ref: TOTPWorkItem read owtWorkItem_ref write owtWorkItem_ref; //address of the work item this thread is working on
  end; { TOTPWorkerThread }

  TOmniThreadPool = class(TInterfacedObject, IOmniThreadPool)
  strict private
    otpIdleWorkers    : TObjectList {of TOTPWorkerThread};
    otpMaxExecuting   : integer;
    otpName           : string;
    otpRunningWorkers : TObjectList {of TOTPWorkerThread};
    otpStoppingWorkers: TObjectList {of TOTPWorkerThread};
    otpWorkItemQueue  : TObjectList {of TOTPWorkItem};
  strict protected
    procedure PruneWorkingQueue; protected
    procedure ScheduleNext(workItem: TOTPWorkItem);
  protected
    function  GetMaxExecuting: integer;
    function  GetName: string;
    procedure SetMaxExecuting(value: integer);
    procedure SetName(const value: string);
  public
    constructor Create(const name: string);
    destructor  Destroy; override;
    procedure Schedule(task: IOmniTask);
    property Name: string read GetName write SetName;
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

{ TOTPWorkItem }

constructor TOTPWorkItem.Create(task: IOmniTask);
begin
  inherited Create;
  owiTask := task;
end; { TOTPWorkItem.Create }

function TOTPWorkItem.Description: string;
begin
  Result := Format('%s:%d', [Task.Name, UniqueID]);
end; { TOTPWorkItem.Description }

function TOTPWorkItem.GetUniqueID: int64;
begin
  Result := Task.UniqueID;
end; { TOTPWorkItem.GetUniqueID }

procedure TOTPWorkItem.SetLastError(status: TOTPTaskStatus; const errorMsg: string);
begin
  owiStatus := status;
  owiLastError := errorMsg;
  UniqueString(owiLastError);
end; { TOTPWorkItem.SetLastError }

{ TOTPWorkerThread }

constructor TOTPWorkerThread.Create(owner: TOmniThreadPool);
begin
  inherited Create(true);
  owtOwner := owner;
  {$IFDEF LogThreadPool}Log('Creating thread %s', [Description]);{$ENDIF LogThreadPool}
  owtNewWorkEvent := CreateEvent(nil, false, false, nil);
  owtTerminateEvent := CreateEvent(nil, false, false, nil);
  owtWorkItemLock := TTicketSpinLock.Create;
  Resume;
end; { TOTPWorkerThread.Create }

destructor TOTPWorkerThread.Destroy;
begin
  {$IFDEF LogThreadPool}Log('Destroying thread %s', [Description]);{$ENDIF LogThreadPool}
  FreeAndNil(owtWorkItemLock);
  DSiCloseHandleAndNull(owtTerminateEvent);
  DSiCloseHandleAndNull(owtNewWorkEvent);
  inherited Destroy;
end; { TOTPWorkerThread.Destroy }

///<summary>Gently stop the worker thread. Called asynchronously from the thread pool.</summary>
procedure TOTPWorkerThread.Asy_Stop;
begin
  {$IFDEF LogThreadPool}Log('Asy_Stop thread %s', [Description]);{$ENDIF LogThreadPool}
  StartStopping_ms := DSiTimeGetTime64;
  owtWorkItemLock.Acquire;
  try
    if assigned(WorkItem_ref) then
      WorkItem_ref.TerminateExecution := true;
  finally owtWorkItemLock.Release end;
  SetEvent(TerminateEvent);
end; { TOTPWorkerThread.Asy_Stop }

///<summary>Take the work item ownership from the thread. Called asynchronously from the thread pool.</summary>
function TOTPWorkerThread.Asy_TerminateWorkItem: boolean;
var
  workItem: TOTPWorkItem;
begin
  {$IFDEF LogThreadPool}Log('Asy_TerminateWorkItem thread %s', [Description]);{$ENDIF LogThreadPool}
  Result := false;
  owtWorkItemLock.Acquire;
  try
    if assigned(WorkItem_ref) then begin
      {$IFDEF LogThreadPool}Log('Thread %s has work item', [Description]);{$ENDIF LogThreadPool}
      workItem := WorkItem_ref;
      WorkItem_ref := nil;
      workItem.SetLastError(tsCanceled, 'Cancelled');
// TODO 1 -oPrimoz Gabrijelcic : implement: TOTPWorkerThread.Asy_TerminateWorkItem
//      Owner.Asy_RequestCompleted(workItem, Self);
      Result := true;
    end;
  finally owtWorkItemLock.Release end;
end; { TOTPWorkerThread.Asy_TerminateWorkItem }

function TOTPWorkerThread.Description: string;
begin
  if not assigned(Self) then
    Result := '<none>'
  else
    Result := Format('%p:%d', [pointer(Self), GetCurrentThreadID]);
end; { TOTPWorkerThread.Description }

procedure TOTPWorkerThread.Execute;
var
  creationTime   : TDateTime;
  startKernelTime: int64;
  startUserTime  : int64;
  stopKernelTime : int64;
  stopUserTime   : int64;
  workItem       : TOTPWorkItem;
begin
  {$IFDEF LogThreadPool}Log('>>>Execute thread %s', [Description]);{$ENDIF LogThreadPool}
// TODO 1 -oPrimoz Gabrijelcic : implement: TOTPWorkerThread.Execute
//    owtOwner.Asy_ForwardThreadCreated(ThreadID);
    try
      while DSiWaitForTwoObjects(owtNewWorkEvent, TerminateEvent, false, INFINITE) = WAIT_OBJECT_0 do begin
        try
          {$IFDEF LogThreadPool}Log('Thread %s starting execution of %s', [Description, WorkItem_ref.Description]);{$ENDIF LogThreadPool}
          DSiGetThreadTimes(creationTime, startUserTime, startKernelTime);
          {$IFNDEF OTL_DontSetThreadName}
          SetThreadName(WorkItem_ref.Task.Name);
          {$ENDIF OTL_DontSetThreadName}
          (WorkItem_ref.Task as IOmniTaskExecutor).Execute;
          DSiGetThreadTimes(creationTime, stopUserTime, stopKernelTime);
          WorkItem_ref.SetLastError(tsCompleted, '');
          {$IFDEF LogThreadPool}Log('Thread %s completed execution of %s; user time = %d ms, kernel time = %d ms', [Description, WorkItem_ref.Description, Round((stopUserTime - startUserTime)/10000), Round((stopKernelTime - startKernelTime)/10000)]);{$ENDIF LogThreadPool}
        except
          on E: Exception do begin
// TODO 1 -oPrimoz Gabrijelcic : implement: TOTPWorkerThread.Execute
//            LogSilentException(ClassName+'.Execute');
            {$IFDEF LogThreadPool}Log('Thread %s caught exception %s during exection of %s', [Description, E.Message, WorkItem_ref.Description]);{$ENDIF LogThreadPool}
            WorkItem_ref.SetLastError(tsException, E.Message);
            owtRemoveFromPool := true;
          end;
        end;
        owtWorkItemLock.Acquire;
        try
          workItem := WorkItem_ref;
          WorkItem_ref := nil;
          if assigned(workItem) then begin
            {$IFDEF LogThreadPool}Log('Thread %s sending notification of completed work item %s', [Description, workItem.Description]);{$ENDIF LogThreadPool}
// TODO 1 -oPrimoz Gabrijelcic : implement: TOTPWorkerThread.Execute
//            Owner.Asy_RequestCompleted(workItem, Self);
          end;
        finally owtWorkItemLock.Release; end;
      end; //while
// TODO 1Primoz Gabrijelcic : implement: TOTPWorkerThread.Execute      
    finally {owtOwner.Asy_ForwardThreadDestroying(ThreadID);} end;
  {$IFDEF LogThreadPool}Log('<<<Execute thread %s', [Description]);{$ENDIF LogThreadPool}
end; { TOTPWorkerThread.Execute }

function TOTPWorkerThread.GetWorkItemInfo(var scheduled_ms, started_ms: int64; var
  description: string): boolean;
begin
  owtWorkItemLock.Acquire;
  try
    if not assigned(WorkItem_ref) then
      Result := false
    else begin
      scheduled_ms := WorkItem_ref.Scheduled_ms;
      started_ms := WorkItem_ref.Started_ms;
      description := WorkItem_ref.Description; UniqueString(description);
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

procedure TOTPWorkerThread.Log(const msg: string; params: array of const);
begin
  {$IFDEF LogThreadPool}
  Owner.Log(msg, params);
  {$ENDIF LogThreadPool}
end; { TOTPWorkerThread.Log }

function TOTPWorkerThread.WorkItemDescription: string;
begin
  owtWorkItemLock.Acquire;
  try
    if assigned(WorkItem_ref) then begin
      Result := WorkItem_ref.Description;
    end
    else
      Result := '';
  finally owtWorkItemLock.Release; end;
end; { TOTPWorkerThread.WorkItemDescription }

{ TOmniThreadPool }

constructor TOmniThreadPool.Create(const name: string);
begin
  inherited Create;
  otpName := name;
  otpStoppingWorkers := TObjectList.Create;
  otpWorkItemQueue := TObjectList.Create;
  otpIdleWorkers := TObjectList.Create;
  otpRunningWorkers := TObjectList.Create;
  otpMaxExecuting := Length(DSiGetThreadAffinity);
end; { TOmniThreadPool.Create }

destructor TOmniThreadPool.Destroy;
begin
  FreeAndNil(otpRunningWorkers);
  FreeAndNil(otpIdleWorkers);
  FreeAndNil(otpWorkItemQueue);
  FreeAndNil(otpStoppingWorkers);
  inherited Destroy;
end; { TOmniThreadPool.Destroy }

function TOmniThreadPool.GetMaxExecuting: integer;
begin
  Result := otpMaxExecuting;
end; { TOmniThreadPool.GetMaxExecuting }

function TOmniThreadPool.GetName: string;
begin
  Result := otpName;
end; { TOmniThreadPool.GetName }

procedure TOmniThreadPool.PruneWorkingQueue;
begin
  // TODO -cMM: TOmniThreadPool.PruneWorkingQueue default body inserted
end; { TOmniThreadPool.PruneWorkingQueue } 

procedure TOmniThreadPool.Schedule(task: IOmniTask);
begin
  ScheduleNext(TOTPWorkItem.Create(task));
  PruneWorkingQueue;
end; { TOmniThreadPool.Schedule }

procedure TOmniThreadPool.ScheduleNext(workItem: TOTPWorkItem);
var
  worker: TOTPWorkerThread;
begin
  worker := nil;
  if otpIdleWorkers.Count > 0 then begin
    worker := TOTPWorkerThread(otpIdleWorkers[otpIdleWorkers.Count - 1]);
    otpIdleWorkers.Delete(otpIdleWorkers.Count - 1);
    otpRunningWorkers.Add(worker);
    {$IFDEF LogThreadPool}Log('Allocated thread from idle pool, num idle = %d, num running = %d[%d]', [otpIdleWorkers.Count, otpRunningWorkers.Count, MaxExecuting]);{$ENDIF LogThreadPool}
  end
  else if (otpMaxExecuting <= 0) or (otpRunningWorkers.Count < otpMaxExecuting) then begin
    worker := TOTPWorkerThread.Create(Self);
    otpRunningWorkers.Add(worker);
    {$IFDEF LogThreadPool}Log('Created new thread %s, num idle = %d, num running = %d[%d]', [worker.Description, otpIdleWorkers.Count, otpRunningWorkers.Count, MaxExecuting]);{$ENDIF LogThreadPool}
  end;
  if assigned(worker) then begin
    {$IFDEF LogThreadPool}Log('Started %s', [workItem.Description]);{$ENDIF LogThreadPool}
    workItem.Started_ms := DSiTimeGetTime64;
    worker.WorkItem_ref := workItem;
    SetEvent(worker.NewWorkEvent);
  end
  else begin
    {$IFDEF LogThreadPool}Log('Queued %s ', [workItem.Description]);{$ENDIF LogThreadPool}
    otpWorkItemQueue.Add(workItem);
  end;
  {$IFDEF LogThreadPoolQueues}LogQueues('Schedule next');{$ENDIF LogThreadPoolQueues}
end; { TOmniThreadPool.ScheduleNext }

procedure TOmniThreadPool.SetMaxExecuting(value: integer);
begin
  otpMaxExecuting := value;
end; { TOmniThreadPool.SetMaxExecuting }

procedure TOmniThreadPool.SetName(const value: string);
begin
  otpName := value;
end; { TOmniThreadPool.SetName }

end.

