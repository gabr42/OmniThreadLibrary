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
///       - Defined IOmniTaskControlMonitor interface.
///       - Added IOmniTaskControl.MonitorWith method.
///       - Implemented TOmniTaskControl.TerminateWhen method.
///       - Added very basic support for task groups.
///       - Added very basic support for sequential execution (ChainTo).
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
// TODO 5 -oPrimoz Gabrijelcic : Task group? (Control them at once - terminate all, for example.)

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
  IOmniTaskControl = interface;
  
  IOmniTaskControlMonitor = interface ['{20CB3AB7-04D8-454B-AEFE-CFCFF8F27301}']
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl;
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl;
  end; { IOmniTaskControlMonitor }

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
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(const threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskControl;
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

//v1.1 extensions:
//  maybe: Comm: IOmniCommunicationEndpoint, which is actually one-to-many-to-one
//    function  Sequential: IOmniTaskGroup;
//    function  Parallel(useThreadPool: IOmniThreadPool): IOmniTaskGroup;
  IOmniTaskGroup = interface ['{B36C08B4-0F71-422C-8613-63C4D04676B7}']
    function  Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function  RunAll: IOmniTaskGroup;
    function  TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function  WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  end; { IOmniTaskGroup }

  function CreateTask(worker: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(const worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
//  function CreateTask(worker: IOmniTaskGroup; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTaskGroup: IOmniTaskGroup;

implementation

uses
  Messages,
  HVStringBuilder,
  DSiWin32,
  GpLists,
  GpStuff,
  SpinLock,
  OtlEventMonitor;

type
  TOmniTaskControlOption = (tcoAlertableWait, tcoMessageWait, tcoFreeOnTerminate);
  TOmniTaskControlOptions = set of TOmniTaskControlOption;
  TOmniExecutorType = (etNone, etMethod, etProcedure, etWorker);

  TOmniTaskExecutor = class
  strict private
    oteCommList          : TInterfaceList;
    oteCommRebuildHandles: THandle;
    oteExecutorType      : TOmniExecutorType;
    oteExitCode          : TGp4AlignedInt;
    oteExitMessage       : string;
    oteInternalLock      : TSynchroObject;
    oteMethod            : TOmniTaskMethod;
    oteOptions           : TOmniTaskControlOptions;
    otePriority          : TOTLThreadPriority;
    oteProc              : TOmniTaskProcedure;
    oteTerminateHandles  : TGpIntegerList;
    oteTimerInterval_ms  : cardinal;
    oteTimerMessage      : integer;
    oteWakeMask          : DWORD;
    oteWorkerInitialized : THandle;
    oteWorkerInitOK      : boolean;
    oteWorkerIntf        : IOmniWorker;
  strict protected
    procedure Cleanup;
    procedure Initialize;
    procedure ProcessThreadMessages;
    procedure SetOptions(const value: TOmniTaskControlOptions);
    procedure SetTimerInterval_ms(const value: cardinal);
    procedure SetTimerMessage(const value: integer);
  protected
    function GetExitCode: integer; inline;
    function GetExitMessage: string;
  public
    constructor Create(const workerIntf: IOmniWorker); overload;
    constructor Create(method: TOmniTaskMethod); overload;
    constructor Create(proc: TOmniTaskProcedure); overload;
    destructor  Destroy; override;
    procedure Asy_DispatchMessages(const task: IOmniTask);
    procedure Asy_Execute(const task: IOmniTask);
    procedure Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure Asy_SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
    procedure TerminateWhen(handle: THandle);
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
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    function  Terminated: boolean;
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
    otcParameters : TOmniValueContainer;
    otcSharedInfo : TOmniSharedTaskInfo;
    otcThread     : TOmniThread;
  strict protected
    function  CreateTask: IOmniTask;
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
    destructor  Destroy; override;
    function  Alertable: IOmniTaskControl;
    function  ChainTo(const task: IOmniTaskControl; ignoreErrors: boolean = false): IOmniTaskControl;
    function  Join(const group: IOmniTaskGroup): IOmniTaskControl;
    function  Leave(const group: IOmniTaskGroup): IOmniTaskControl;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(const threadPool: IOmniThreadPool): IOmniTaskControl;
    function  SetTimer(interval_ms: cardinal; timerMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(const paramName: string; const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameter(const paramValue: TOmniValue): IOmniTaskControl; overload;
    function  SetParameters(const parameters: array of TOmniValue): IOmniTaskControl;
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

//v1.1 extensions:
//  maybe: Comm: IOmniCommunicationEndpoint, which is actually one-to-many-to-one
//    function  Sequential: IOmniTaskGroup;
//    function  Parallel(useThreadPool: IOmniThreadPool): IOmniTaskGroup;
  TOmniTaskGroup = class(TInterfacedObject, IOmniTaskGroup)
  strict private
    otgTaskList: TInterfaceList;
  public
    constructor Create;
    destructor  Destroy; override;
    function Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
    function RunAll: IOmniTaskGroup;
    function TerminateAll(maxWait_ms: cardinal = INFINITE): boolean;
    function WaitForAll(maxWait_ms: cardinal = INFINITE): boolean;
  end; { TOmniTaskGroup }

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

function CreateTask(const worker: IOmniWorker; const taskName: string): IOmniTaskControl; overload;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTaskGroup: IOmniTaskGroup;
begin
  Result := TOmniTaskGroup.Create;
end; { CreateTaskGroup }

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
        PostMessage(otSharedInfo.MonitorWindow, COmniTaskMsg_Terminated,
          integer(Int64Rec(UniqueID).Lo), integer(Int64Rec(UniqueID).Hi));
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

procedure TOmniTask.Terminate;
begin
  SetEvent(otSharedInfo.TerminateEvent);
  if not otExecuting then //for execution so that proper cleanup can occur
    Execute;
end; { TOmniTask.Terminate }

function TOmniTask.Terminated: boolean;
begin
  Result := WaitForSingleObject(otSharedInfo.TerminatedEvent, 0) <> WAIT_TIMEOUT;
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

destructor TOmniTaskExecutor.Destroy;
begin
  oteInternalLock.Acquire;
  try
    FreeAndNil(oteCommList);
  finally oteInternalLock.Release; end;
  FreeAndNil(oteInternalLock);
  FreeAndNil(oteTerminateHandles);
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
        for intf in oteCommList do begin
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
  lastTimer_ms: int64;
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
      {if assigned(WorkerObj_ref) then begin
        WorkerObj_ref.Task := task;
        if not WorkerObj_ref.Initialize then
          Exit;
      end;}
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
        if gotMsg then begin
          if assigned(WorkerIntf) then
            WorkerIntf.DispatchMessage(msg);
          {if assigned(WorkerObj_ref) then
            WorkerObj_ref.DispatchMessage(msg)}
        end;
      end // comm handles
      else if awaited = idxRebuildHandles then
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
          {if assigned(WorkerObj_ref) then
            WorkerObj_ref.DispatchMessage(msg);}
        end
        else if assigned(WorkerIntf) then
          WorkerIntf.Timer
        {else if assigned(WorkerObj_ref) then
          WorkerObj_ref.Timer};
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
    {if assigned(WorkerObj_ref) then begin
      WorkerObj_ref.Cleanup;
      WorkerObj_ref.Task := nil;
    end;}
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
    if WaitForSingleObject(task.TerminateEvent, 0) = WAIT_TIMEOUT then begin
      case oteExecutorType of
        etMethod:
          oteMethod(task);
        etProcedure:
          oteProc(task);
        etWorker:
          Asy_DispatchMessages(task);
        else
          raise Exception.Create('TOmniTaskExecutor.Asy_Execute: Executor is not set');
      end; //case oteExecutorType
    end;
  finally Cleanup; end;
end; { TOmniTaskExecutor.Asy_Execute }

procedure TOmniTaskExecutor.Asy_RegisterComm(const comm: IOmniCommunicationEndpoint);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_RegisterComm: Additional communication support is only available when working with an IOmniWorker');
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

procedure TOmniTaskExecutor.Asy_UnregisterComm(const comm: IOmniCommunicationEndpoint);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.Asy_UnregisterComm: Additional communication support is only available when working with an IOmniWorker');
  oteInternalLock.Acquire;
  try
    oteCommList.Remove(comm);
    if oteCommList.Count = 0 then
      FreeAndNil(oteCommList);
    SetEvent(oteCommRebuildHandles);
  finally oteInternalLock.Release; end;
end; { TOmniTaskExecutor.Asy_UnregisterComm }

procedure TOmniTaskExecutor.Cleanup;
begin
  oteWorkerIntf := nil;
end; { TOmniTaskExecutor.Cleanup }

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
     (oteExecutorType <> etWorker)
  then
    raise Exception.Create('TOmniTaskExecutor.SetOptions: Trying to set IOmniWorker specific option(s)');
  oteOptions := value;
end; { TOmniTaskExecutor.SetOptions }

procedure TOmniTaskExecutor.SetTimerInterval_ms(const value: cardinal);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerInterval_ms: Timer support is only available when working with an IOmniWorker');
  oteTimerInterval_ms := value;
end; { TOmniTaskExecutor.SetTimerInterval_ms }

procedure TOmniTaskExecutor.SetTimerMessage(const value: integer);
begin
  if oteExecutorType <> etWorker then
    raise Exception.Create('TOmniTaskExecutor.SetTimerMessage: Timer support is only available when working with an IOmniWorker');
  oteTimerMessage := value;
end; { TOmniTaskExecutor.SetTimerMessage }

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
    raise Exception.Create('TOmniTaskExecutor.WaitForInit: Wait for init is only available when working with an IOmniWorker');
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
  Result := TOmniTask.Create(otcExecutor, otcParameters, otcSharedInfo);
end; { TOmniTaskControl.CreateTask }

function TOmniTaskControl.GetComm: IOmniCommunicationEndpoint;
begin
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
  otcSharedInfo := TOmniSharedTaskInfo.Create;
  otcSharedInfo.TaskName := taskName;
  otcSharedInfo.UniqueID := OtlUID.Increment;
  otcSharedInfo.CommChannel := CreateTwoWayChannel;
  otcParameters := TOmniValueContainer.Create;
  otcSharedInfo.TerminateEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminateEvent <> 0);
  otcSharedInfo.TerminatedEvent := CreateEvent(nil, true, false, nil);
  Win32Check(otcSharedInfo.TerminatedEvent <> 0);
end; { TOmniTaskControl.Initialize }

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
  (GlobalOmniThreadPool as IOmniThreadPoolScheduler).Schedule(CreateTask);
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
  otcSharedInfo.MonitorWindow := hWindow;
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
  SetParameter('', paramValue);
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

function TOmniTaskControl.Terminate(maxWait_ms: cardinal): boolean;
begin
{ TODO :
reset executor and exit immediately if task was not started at all
or raise exception? }
  SetEvent(otcSharedInfo.TerminateEvent);
  Result := WaitFor(maxWait_ms);
  // TODO 1 -oPrimoz Gabrijelcic : Kill thread if not Result
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

{ TOmniTaskGroup }

constructor TOmniTaskGroup.Create;
begin
  inherited Create;
  otgTaskList := TInterfaceList.Create;
end; { TOmniTaskGroup.Create }

destructor TOmniTaskGroup.Destroy;
begin
  FreeAndNil(otgTaskList);
  inherited Destroy;
end; { TOmniTaskGroup.Destroy }

function TOmniTaskGroup.Add(const taskControl: IOmniTaskControl): IOmniTaskGroup;
begin
  otgTaskList.Add(taskControl);
  Result := Self;
end; { TOmniTaskGroup.Add }

function TOmniTaskGroup.Remove(const taskControl: IOmniTaskControl): IOmniTaskGroup;
begin
  otgTaskList.Remove(taskControl);
  Result := Self;
end; { TOmniTaskGroup.Remove }

function TOmniTaskGroup.RunAll: IOmniTaskGroup;
var
  iIntf: IInterface;
begin
  for iIntf in otgTaskList do
    (iIntf as IOMniTaskControl).Run;
end; { TOmniTaskGroup.RunAll }

function TOmniTaskGroup.TerminateAll(maxWait_ms: cardinal): boolean;
var
  intf: IInterface;
begin
  for intf in otgTaskList do
    SetEvent((intf as IOmniTaskControlInternals).TerminateEvent);
  Result := WaitForAll(maxWait_ms);
end; { TOmniTaskGroup.TerminateAll }

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
