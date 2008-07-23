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
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2008-06-12
///   Last modification : 2008-07-23
///   Version           : 0.3
///</para><para>
///   History:
///     0.3: 2008-07-23
///       - Catch task exceptions and map them into EXIT_EXCEPTION exit codes.
///       - TOmniWorker.Initialize and .Cleanup made protected.
///       - Semantic change: (T|I)OmniWorker.Cleanup is called even if Initialize fails
///         or raises exception.
///     0.2: 2008-07-22
///       - Added Lock property and WithLock method.
///       - Added SetPriority method.
///     0.1: 2008-07-15
///       - Everything but the IOmniTask interface declaration moved from the OtlTask unit.
///</para></remarks>

///Literature
///  - Lock my Object… Please!, Allen Bauer,
///    http://blogs.codegear.com/abauer/2008/02/19/38856
///  - Threading in C#, Joseph Albahari, http://www.albahari.com/threading/
///  - Coordination Data Structures Overview, Emad Omara,
///    http://blogs.msdn.com/pfxteam/archive/2008/06/18/8620615.aspx

unit OtlTaskControl;

interface

// TODO 1 -oPrimoz Gabrijelcic : Catch thread exceptions (and add a mechanism to report them)

uses
  Windows,
  SysUtils,
  Variants,
  Classes,
  SyncObjs,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlThreadPool;

type
  IOmniWorker = interface ['{CA63E8C2-9B0E-4BFA-A527-31B2FCD8F413}']
    function  GetTask: IOmniTask;
    procedure SetTask(const value: IOmniTask);
  //
    procedure Cleanup;
    procedure DispatchMessage(var msg: TOmniMessage);
    procedure Timer;
    function  Initialize: boolean;
    property Task: IOmniTask read GetTask write SetTask;
  end; { IOmniWorker }

  TOmniWorker = class(TInterfacedObject, IOmniWorker)
  strict private
    owTask: IOmniTask;
  protected
    procedure Cleanup; virtual;
    procedure DispatchMessage(var msg: TOmniMessage); virtual;
    function  GetTask: IOmniTask;
    function  Initialize: boolean; virtual;
    procedure SetTask(const value: IOmniTask);
  public
    procedure Timer; virtual;
    property Task: IOmniTask read GetTask write SetTask;
  end; { TOmniWorker }

  TOmniTaskProcedure = procedure(task: IOmniTask);
  TOmniTaskMethod = procedure(task: IOmniTask) of object;

  TOTLThreadPriority = (tpIdle, tpLowest, tpBelowNormal, tpNormal, tpAboveNormal, tpHighest);

  IOmniTaskControl = interface ['{881E94CB-8C36-4CE7-9B31-C24FD8A07555}']
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetExitCode: integer;
    function  GetExitMessage: string;
    function  GetLock: TSynchroObject;
    function  GetName: string;
    function  GetUniqueID: int64;
  //
    function  Alertable: IOmniTaskControl;
    function  FreeOnTerminate: IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(counter: IOmniCounter): IOmniTaskControl;
    function  WithLock(lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskControl;
  //
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTaskControl }

  function CreateTask(worker: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;

implementation

uses
  Messages,
  HVStringBuilder,
  DSiWin32,
  GpStuff,
  SpinLock,
  OtlTaskEvents;

type
  TOmniTaskControlOption = (tcoAlertableWait, tcoMessageWait, tcoFreeOnTerminate);
  TOmniTaskControlOptions = set of TOmniTaskControlOption;

  TOmniTaskExecutor = class 
  strict private
    oteCommList          : TInterfaceList;
    oteCommRebuildHandles: THandle;
    oteExecutorType      : (etNone, etMethod, etProcedure, etWorkerIntf, etWorkerObj);
    oteExitCode          : TGp4AlignedInt;
    oteExitMessage       : string;
    oteInternalLock      : TTicketSpinLock;
    oteMethod            : TOmniTaskMethod;
    oteOptions           : TOmniTaskControlOptions;
    otePriority          : TOTLThreadPriority;
    oteProc              : TOmniTaskProcedure;
    oteTimerInterval_ms  : cardinal;
    oteTimerMessage      : integer;
    oteWakeMask          : DWORD;
    oteWorkerInitialized : THandle;
    oteWorkerInitOK      : boolean;
    oteWorkerIntf        : IOmniWorker;
    oteWorkerObj_ref     : TOmniWorker;
  strict protected
    procedure Initialize;
    procedure ProcessThreadMessages;
    procedure SetOptions(const value: TOmniTaskControlOptions);
    procedure SetTimerInterval_ms(const value: cardinal);
    procedure SetTimerMessage(const value: integer);
  protected
    function GetExitCode: integer; inline;
    function GetExitMessage: string;
  public
    constructor Create(workerIntf: IOmniWorker); overload;
    constructor Create(method: TOmniTaskMethod); overload;
    constructor Create(proc: TOmniTaskProcedure); overload;
    constructor Create(workerObj_ref: TOmniWorker); overload;
    destructor  Destroy; override;
    procedure Asy_DispatchMessages(task: IOmniTask);
    procedure Asy_Execute(task: IOmniTask);
    procedure Asy_RegisterComm(comm: IOmniCommunicationEndpoint);
    procedure Asy_SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Asy_UnregisterComm(comm: IOmniCommunicationEndpoint);
    function WaitForInit: boolean;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Options: TOmniTaskControlOptions read oteOptions write SetOptions;
    property Priority: TOTLThreadPriority read otePriority write otePriority;
    property TimerInterval_ms: cardinal read oteTimerInterval_ms write SetTimerInterval_ms;
    property TimerMessage: integer read oteTimerMessage write SetTimerMessage;
    property WakeMask: DWORD read oteWakeMask write oteWakeMask;
    property WorkerInitialized: THandle read oteWorkerInitialized;
    property WorkerInitOK: boolean read oteWorkerInitOK;
    property WorkerIntf: IOmniWorker read oteWorkerIntf;
    property WorkerObj_ref: TOmniWorker read oteWorkerObj_ref;
  end; { TOmniTaskExecutor }

  TOmniSharedTaskInfo = class
  strict private
    ostiCommChannel    : IOmniTwoWayChannel;
    ostiCounter        : IOmniCounter;
    ostiLock           : TSynchroObject;
    ostiMonitorWindow  : THandle;
    ostiTaskName       : string;
    ostiTerminatedEvent: THandle;
    ostiTerminateEvent : THandle;
    ostiUniqueID       : int64;
  public
    constructor Create(const taskName: string; commChannel: IOmniTwoWayChannel; uniqueID:
      int64; terminateEvent, terminatedEvent, monitorWindow: THandle; counter: IOmniCounter;
      lock: TSynchroObject);
    property CommChannel: IOmniTwoWayChannel read ostiCommChannel;
    property Counter: IOmniCounter read ostiCounter;
    property Lock: TSynchroObject read ostiLock;
    property MonitorWindow: THandle read ostiMonitorWindow;
    property TaskName: string read ostiTaskName;
    property TerminatedEvent: THandle read ostiTerminatedEvent;
    property TerminateEvent: THandle read ostiTerminateEvent;
    property UniqueID: int64 read ostiUniqueID;
  end; { TOmniSharedTaskInfo }

  TOmniTask = class(TInterfacedObject, IOmniTask, IOmniTaskExecutor)
  strict private
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
    procedure Terminate; inline;
  public
    constructor Create(executor: TOmniTaskExecutor; parameters: TOmniValueContainer;
      sharedInfo: TOmniSharedTaskInfo);
    destructor  Destroy; override;
    procedure Execute;
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure RegisterComm(comm: IOmniCommunicationEndpoint);
    procedure UnregisterComm(comm: IOmniCommunicationEndpoint);
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

  TOmniTaskControl = class(TInterfacedObject, IOmniTaskControl)
  strict private
    otcCommChannel    : IOmniTwoWayChannel;
    otcCounter        : IOmniCounter;
    otcDestroyLock    : boolean;
    otcExecutor       : TOmniTaskExecutor;
    otcLock           : TSynchroObject;
    otcMonitorWindow  : THandle;
    otcParameters     : TOmniValueContainer;
    otcTaskName       : string;
    otcTerminatedEvent: TDSiEventHandle;
    otcTerminateEvent : TDSiEventHandle;
    otcThread         : TOmniThread;
    otcUniqueID       : int64;
  strict protected
    function  CreateTask: IOmniTask;
    procedure Initialize;
  protected
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string; inline;
    function  GetLock: TSynchroObject;
    function  GetName: string; inline;
    function  GetOptions: TOmniTaskControlOptions;
    function  GetUniqueID: int64; inline;
    procedure SetOptions(const value: TOmniTaskControlOptions);
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
    function  WithLock(lock: TSynchroObject; autoDestroyLock: boolean = true):
      IOmniTaskControl;
  public
    constructor Create(worker: IOmniWorker; const taskName: string); overload;
    constructor Create(worker: TOmniWorker; const taskName: string); overload;
    constructor Create(worker: TOmniTaskMethod; const taskName: string); overload;
    constructor Create(worker: TOmniTaskProcedure; const taskName: string); overload;
    destructor  Destroy; override;
    function  Alertable: IOmniTaskControl;
    function  FreeOnTerminate: IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    function  WaitForInit: boolean;
    function  WithCounter(counter: IOmniCounter): IOmniTaskControl;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Options: TOmniTaskControlOptions read GetOptions write SetOptions;
    property UniqueID: int64 read GetUniqueID;
  end; { TOmniTaskControl }

var
  taskUID: TGp8AlignedInt;

{ exports }

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

function CreateTask(worker: IOmniWorker; const taskName: string): IOmniTaskControl; overload;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTask(worker: TOmniWorker; const taskName: string): IOmniTaskControl; overload;
begin
  if taskName = '' then
    Result := TOmniTaskControl.Create(worker, worker.ClassName)
  else
    Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

{ TOmniSharedTaskInfo }

constructor TOmniSharedTaskInfo.Create(const taskName: string; commChannel:
  IOmniTwoWayChannel; uniqueID: int64; terminateEvent, terminatedEvent, monitorWindow:
  THandle; counter: IOmniCounter; lock: TSynchroObject);
begin
  inherited Create;
  ostiMonitorWindow := monitorWindow;
  ostiTerminatedEvent := terminatedEvent;
  ostiLock := lock;
  ostiCounter := counter;
  ostiCommChannel := commChannel;
  ostiTaskName := taskName; UniqueString(ostiTaskName);
  ostiTerminateEvent := terminateEvent;
  ostiUniqueID := uniqueID;
end; { TOmniSharedTaskInfo.Create }

{ TOmniTask }

constructor TOmniTask.Create(executor: TOmniTaskExecutor; parameters:
  TOmniValueContainer; sharedInfo: TOmniSharedTaskInfo);
begin
  inherited Create;
  otExecutor_ref := executor;
  otParameters_ref := parameters;
  otSharedInfo := sharedInfo;
end; { TOmniTask.Create }

destructor TOmniTask.Destroy;
begin
  FreeAndNil(otSharedInfo);
  inherited;
end; { TOmniTask.Destroy }

procedure TOmniTask.Execute;
begin
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
        PostMessage(otSharedInfo.MonitorWindow, COmniTaskMsg_Terminated,
          integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi));
    end;
  finally
    SetEvent(otSharedInfo.TerminatedEvent);
  end;
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

procedure TOmniTask.RegisterComm(comm: IOmniCommunicationEndpoint);
begin
  otExecutor_ref.Asy_RegisterComm(comm);
end; { TOmniTask.RegisterComm }

procedure TOmniTask.SetExitStatus(exitCode: integer; const exitMessage: string);
begin
  otExecutor_ref.Asy_SetExitStatus(exitCode, exitMessage);
end; { TOmniTask.SetExitStatus }

procedure TOmniTask.Terminate;
begin
  SetEvent(otSharedInfo.TerminateEvent);
end; { TOmniTask.Terminate }

procedure TOmniTask.UnregisterComm(comm: IOmniCommunicationEndpoint);
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

{ TOmniTaskExecutor }

constructor TOmniTaskExecutor.Create(workerIntf: IOmniWorker);
begin
  oteExecutorType := etWorkerIntf;
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

constructor TOmniTaskExecutor.Create(workerObj_ref: TOmniWorker);
begin
  oteExecutorType := etWorkerObj;
  oteWorkerObj_ref := workerObj_ref;
  Initialize;
end; { TOmniTaskExecutor.Create }

destructor TOmniTaskExecutor.Destroy;
begin
  oteInternalLock.Acquire;
  try
    FreeAndNil(oteCommList);
  finally oteInternalLock.Release; end;
  FreeAndNil(oteInternalLock);
  DSiCloseHandleAndNull(oteCommRebuildHandles);
  DSiCloseHandleAndNull(oteWorkerInitialized);
  inherited;
end; { TOmniTaskExecutor.Destroy }

procedure TOmniTaskExecutor.Asy_DispatchMessages(task: IOmniTask);

var
  numWaitHandles: cardinal;
  waitHandles   : array [0..63] of THandle;

  procedure RebuildWaitHandles;
  var
    intf: IInterface;
  begin
    oteInternalLock.Acquire;
    try
      waitHandles[0] := task.TerminateEvent;
      waitHandles[1] := task.Comm.NewMessageEvent;
      waitHandles[2] := oteCommRebuildHandles;
      numWaitHandles := 3;
      if assigned(oteCommList) then
        for intf in oteCommList do begin
          if numWaitHandles > High(waitHandles) then
            raise Exception.CreateFmt('TOmniTaskExecutor.Asy_DispatchMessages: Cannot wait on more than %d handles', [numWaitHandles]);
          waitHandles[numWaitHandles] := (intf as IOmniCommunicationEndpoint).NewMessageEvent;
          Inc(numWaitHandles);
        end;
    finally oteInternalLock.Release; end;
  end; { RebuildWaitHandles }

var
  awaited     : DWORD;
  flags       : DWORD;
  gotMsg      : boolean;
  lastTimer_ms: int64;
  msg         : TOmniMessage;
  timeout_ms  : int64;
  waitWakeMask: DWORD;

begin { TOmniTaskExecutor.Asy_DispatchMessages }
  try
    try
      try
        if assigned(WorkerIntf) and assigned(WorkerObj_ref) then
          raise Exception.Create('TOmniTaskControl: Internal error, both WorkerIntf and WorkerObj are assigned');
        oteWorkerInitOK := false;
        try
          if assigned(WorkerIntf) then begin
            WorkerIntf.Task := task;
            if not WorkerIntf.Initialize then
              Exit;
          end;
          if assigned(WorkerObj_ref) then begin
            WorkerObj_ref.Task := task;
            if not WorkerObj_ref.Initialize then
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
        lastTimer_ms := DSiTimeGetTime64;
        repeat
          if TimerInterval_ms <= 0 then
            timeout_ms := INFINITE
          else begin
            timeout_ms := TimerInterval_ms - (DSiTimeGetTime64 - lastTimer_ms);
            if timeout_ms < 0 then
              timeout_ms := 0;
          end;
          awaited := MsgWaitForMultipleObjectsEx(numWaitHandles, waitHandles,
            cardinal(timeout_ms), waitWakeMask, flags);
          if (awaited = WAIT_OBJECT_0) or (awaited = WAIT_ABANDONED) then
            break //repeat
          else if (awaited = WAIT_OBJECT_1) or
                  ((awaited >= WAIT_OBJECT_3) and (awaited <= numWaitHandles)) then
          begin
            if awaited = WAIT_OBJECT_1 then
              gotMsg := task.Comm.Receive(msg)
            else begin
              oteInternalLock.Acquire;
              try
                gotMsg := (oteCommList[awaited - WAIT_OBJECT_3] as IOmniCommunicationEndpoint).Receive(msg);
              finally oteInternalLock.Release; end;
            end;
            if gotMsg then begin
              if assigned(WorkerIntf) then
                WorkerIntf.DispatchMessage(msg);
              if assigned(WorkerObj_ref) then
                WorkerObj_ref.DispatchMessage(msg)
            end;
          end // WAIT_OBJECT_1, WAIT_OBJECT3 .. numWaitHandles
          else if awaited = WAIT_OBJECT_2 then
            RebuildWaitHandles
          else if awaited = (numWaitHandles + 1) then //message
            ProcessThreadMessages
          else if awaited = WAIT_IO_COMPLETION then
            // do-nothing
          else if awaited = WAIT_TIMEOUT then begin
            if TimerMessage >= 0 then begin
              msg.MsgID := TimerMessage;
              msg.MsgData := Null;
              if assigned(WorkerIntf) then
                WorkerIntf.DispatchMessage(msg);
              if assigned(WorkerObj_ref) then
                WorkerObj_ref.DispatchMessage(msg);
            end
            else if assigned(WorkerIntf) then
              WorkerIntf.Timer
            else if assigned(WorkerObj_ref) then
              WorkerObj_ref.Timer;
            lastTimer_ms := DSiTimeGetTime64;
          end //WAIT_TIMEOUT
          else //errors
            RaiseLastOSError;
        until false;
      finally
        if assigned(WorkerIntf) then begin
          WorkerIntf.Cleanup;
          WorkerIntf.Task := nil;
        end;
        if assigned(WorkerObj_ref) then begin
          WorkerObj_ref.Cleanup;
          WorkerObj_ref.Task := nil;
        end;
      end;
    finally
      if tcoFreeOnTerminate in Options then
        oteWorkerObj_ref.Free;
    end;
  finally
    oteWorkerIntf := nil;
    oteWorkerObj_ref := nil;
  end;
end; { TOmniTaskExecutor.Asy_DispatchMessages }

procedure TOmniTaskExecutor.Asy_Execute(task: IOmniTask);
const
  CThreadPriorityNum: array [tpIdle..tpHighest] of integer = (
    THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST);
begin
  SetThreadPriority(GetCurrentThread, CThreadPriorityNum[Priority]);
  case oteExecutorType of
    etMethod:
      oteMethod(task);
    etProcedure:
      oteProc(task);
    etWorkerIntf,
    etWorkerObj:
      Asy_DispatchMessages(task);
    else
      raise Exception.Create('TOmniTaskExecutor.Asy_Execute: Executor is not set');
  end;
end; { TOmniTaskExecutor.Asy_Execute }

procedure TOmniTaskExecutor.Asy_RegisterComm(comm: IOmniCommunicationEndpoint);
begin
  if not (oteExecutorType in [etWorkerIntf, etWorkerObj]) then
    raise Exception.Create('TOmniTaskExecutor.Asy_RegisterComm: Additional communication support is only available when working with an IOmniWorker/TOmniWorker');
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

procedure TOmniTaskExecutor.Asy_UnregisterComm(comm: IOmniCommunicationEndpoint);
begin
  if not (oteExecutorType in [etWorkerIntf, etWorkerObj]) then
    raise Exception.Create('TOmniTaskExecutor.Asy_UnregisterComm: Additional communication support is only available when working with an IOmniWorker/TOmniWorker');
  oteInternalLock.Acquire;
  try
    oteCommList.Remove(comm);
    if oteCommList.Count = 0 then
      FreeAndNil(oteCommList);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_UnregisterComm }

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

procedure TOmniTaskExecutor.Initialize;
begin
  oteWorkerInitialized := CreateEvent(nil, true, false, nil);
  oteInternalLock := TTicketSpinLock.Create;
  oteCommRebuildHandles := CreateEvent(nil, false, false, nil);
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

procedure TOmniTaskExecutor.SetOptions(const value: TOmniTaskControlOptions);
begin
  if (([tcoAlertableWait, tcoMessageWait] * Options) <> []) and
     (not (oteExecutorType in [etWorkerIntf, etWorkerObj]))
  then
    raise Exception.Create('TOmniTaskExecutor.SetOptions: Trying to set IOmniWorker/TOmniWorker specific option(s)');
  if (([tcoFreeOnTerminate] * Options) <> []) and (oteExecutorType <> etWorkerObj) then
    raise Exception.Create('TOmniTaskExecutor.SetOptions: Trying to set TOmniWorker specific option(s)');
  oteOptions := value;
end; { TOmniTaskExecutor.SetOptions }

procedure TOmniTaskExecutor.SetTimerInterval_ms(const value: cardinal);
begin
  if not (oteExecutorType in [etWorkerIntf, etWorkerObj]) then
    raise Exception.Create('TOmniTaskExecutor.SetTimerInterval_ms: Timer support is only available when working with an IOmniWorker/TOmniWorker');
  oteTimerInterval_ms := value;
end; { TOmniTaskExecutor.SetTimerInterval_ms }

procedure TOmniTaskExecutor.SetTimerMessage(const value: integer);
begin
  if not (oteExecutorType in [etWorkerIntf, etWorkerObj]) then
    raise Exception.Create('TOmniTaskExecutor.SetTimerMessage: Timer support is only available when working with an IOmniWorker/TOmniWorker');
  oteTimerMessage := value;
end; { TOmniTaskExecutor.SetTimerMessage }

function TOmniTaskExecutor.WaitForInit: boolean;
begin
  if not (oteExecutorType in [etWorkerIntf, etWorkerObj]) then
    raise Exception.Create('TOmniTaskExecutor.WaitForInit: Wait for init is only available when working with an IOmniWorker/TOmniWorker');
  WaitForSingleObject(WorkerInitialized, INFINITE);
  Result := WorkerInitOK;
end; { TOmniTaskExecutor.WaitForInit }

{ TOmniTaskControl }

constructor TOmniTaskControl.Create(worker: IOmniWorker; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskMethod; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniTaskProcedure; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniWorker; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.Create(worker);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

destructor TOmniTaskControl.Destroy;
begin
  { TODO : Do we need wait-and-kill mechanism here to prevent shutdown locks? }
  if assigned(otcThread) then begin
    Terminate;
    FreeAndNil(otcThread);
  end;
  if otcDestroyLock then
    FreeAndNil(otcLock);
  FreeAndNil(otcExecutor);
  otcCommChannel := nil;
  DSiCloseHandleAndNull(otcTerminateEvent);
  DSiCloseHandleAndNull(otcTerminatedEvent);
  FreeAndNil(otcParameters);
  inherited Destroy;
end; { TOmniTaskControl.Destroy }

function TOmniTaskControl.Alertable: IOmniTaskControl;
begin
  Options := Options + [tcoAlertableWait];
  Result := Self;
end; { TOmniTaskControl.Alertable }

function TOmniTaskControl.CreateTask: IOmniTask;
begin
  Result := TOmniTask.Create(otcExecutor, otcParameters,
    TOmniSharedTaskInfo.Create(otcTaskName, otcCommChannel, otcUniqueID,
      otcTerminateEvent, otcTerminatedEvent, otcMonitorWindow, otcCounter, otcLock));
end; { TOmniTaskControl.CreateTask }

function TOmniTaskControl.FreeOnTerminate: IOmniTaskControl;
begin
  Options := Options + [tcoFreeOnTerminate];
  Result := Self;
end; { TOmniTaskControl.FreeOnTerminate }

function TOmniTaskControl.GetComm: IOmniCommunicationEndpoint;
begin
  Result := otcCommChannel.Endpoint1;
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
  Result := otcLock;
end; { TOmniTaskControl.GetLock }

function TOmniTaskControl.GetName: string;
begin
  Result := otcTaskName;
end; { TOmniTaskControl.GetName }

function TOmniTaskControl.GetOptions: TOmniTaskControlOptions;
begin
  Result := otcExecutor.Options;
end; { TOmniTaskControl.GetOptions }

function TOmniTaskControl.GetUniqueID: int64;
begin
  Result := otcUniqueID;
end; { TOmniTaskControl.GetUniqueID }

procedure TOmniTaskControl.Initialize;
begin
  otcUniqueID := taskUID.Increment;
  otcCommChannel := CreateTwoWayChannel;
  otcParameters := TOmniValueContainer.Create;
  otcTerminateEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcTerminateEvent <> 0);
  otcTerminatedEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcTerminatedEvent <> 0);
end; { TOmniTaskControl.Initialize }

function TOmniTaskControl.MsgWait(wakeMask: DWORD): IOmniTaskControl;
begin
  Options := Options + [tcoMessageWait];
  otcExecutor.WakeMask := wakeMask;
  Result := Self;
end; { TOmniTaskControl.MsgWait }

function TOmniTaskControl.RemoveMonitor: IOmniTaskControl;
begin
  otcMonitorWindow := 0;
  otcCommChannel.Endpoint2.RemoveMonitor;
  Result := Self;
end; { TOmniTaskControl.RemoveMonitor }

function TOmniTaskControl.Run: IOmniTaskControl;
begin
  otcParameters.Lock;
  otcThread := TOmniThread.Create(CreateTask);
  otcThread.Resume;
  Result := Self;
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Schedule(threadPool: IOmniThreadPool): IOmniTaskControl;
begin
  otcParameters.Lock;
  GlobalOmniThreadPool.Schedule(CreateTask);
  Result := Self;
end; { TOmniTaskControl.Schedule }

function TOmniTaskControl.SetTimer(interval_ms: cardinal; timerMessage: integer):
  IOmniTaskControl;
begin
  otcExecutor.TimerInterval_ms := interval_ms;
  otcExecutor.TimerMessage := timerMessage;
  Result := Self;
end; { TOmniTaskControl.SetTimer }

function TOmniTaskControl.SetMonitor(hWindow: THandle): IOmniTaskControl;
begin
  if otcParameters.IsLocked then
    raise Exception.Create('TOmniTaskControl.SetMonitor: Monitor can only be assigned while task is not running');
  otcMonitorWindow := hWindow;
  otcCommChannel.Endpoint2.SetMonitor(hWindow, COmniTaskMsg_NewMessage,
    integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi));
  Result := Self;
end; { TOmniTaskControl.SetMonitor }

procedure TOmniTaskControl.SetOptions(const value: TOmniTaskControlOptions);
begin
  otcExecutor.Options := value;
end; { TOmniTaskControl.SetOptions }

function TOmniTaskControl.SetParameter(const paramName: string;
  paramValue: TOmniValue): IOmniTaskControl; 
begin
  otcParameters.Add(paramValue, paramName);
  Result := Self;
end; { TOmniTaskControl.SetParameter }

function TOmniTaskControl.SetParameter(paramValue: TOmniValue): IOmniTaskControl;
begin
  SetParameter('', paramValue);
end; { TOmniTaskControl.SetParameter }

function TOmniTaskControl.SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
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

function TOmniTaskControl.Terminate(maxWait_ms: cardinal): boolean;
begin
{ TODO : 
reset executor and exit immediately if task was not started at all
or raise exception? }
  SetEvent(otcTerminateEvent);
  Result := WaitFor(maxWait_ms);
end; { TOmniTaskControl.Terminate }

function TOmniTaskControl.TerminateWhen(event: THandle): IOmniTaskControl;
begin
  Result := Self;
  raise Exception.Create('Not implemented: TOmniTaskControl.TerminateWhen');
end; { TOmniTaskControl.TerminateWhen }

function TOmniTaskControl.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := (WaitForSingleObject(otcTerminatedEvent, maxWait_ms) = WAIT_OBJECT_0);
end; { TOmniTaskControl.WaitFor }

function TOmniTaskControl.WaitForInit: boolean;
begin
  Result := otcExecutor.WaitForInit;
end; { TOmniTaskControl.WaitForInit }

function TOmniTaskControl.WithCounter(counter: IOmniCounter): IOmniTaskControl;
begin
  otcCounter := counter;
  Result := Self;
end; { TOmniTaskControl.WithCounter }

function TOmniTaskControl.WithLock(lock: TSynchroObject;
  autoDestroyLock: boolean): IOmniTaskControl;
begin
  otcLock := lock;
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

end.
