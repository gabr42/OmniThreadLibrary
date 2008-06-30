///<summary>Task encapsulation. Part of the OmniThreadLibrary project.</summary>
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
///   Version           : 0.1
///</para><para>
///   History:
///</para></remarks>

unit OtlTask;

interface

uses
  Windows,
  SysUtils,
  Variants,
  Classes,
  OtlCommon,
  OtlComm,
  OtlThreadPool;
  
// TODO 1 -oPrimoz Gabrijelcic : could use taskcontrol.WaitForInit: boolean (in IOmniWorker mode)

type
  IOmniTask = interface ['{958AE8A3-0287-4911-B475-F275747400E4}']
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetParam(idxParam: integer): TOmniValue;
    function  GetParamByName(const paramName: string): TOmniValue;
    function  GetTerminateEvent: THandle;
    function  GetUniqueID: cardinal;
  //
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Terminate; 
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property TerminateEvent: THandle read GetTerminateEvent;
    property UniqueID: cardinal read GetUniqueID; 
  end; { IOmniTask }

  IOmniWorker = interface ['{CA63E8C2-9B0E-4BFA-A527-31B2FCD8F413}']
    function  GetTask: IOmniTask;
    procedure SetTask(const value: IOmniTask);
  //
    procedure Cleanup;
    procedure DispatchMessage(var msg: TOmniMessage);
    procedure Idle;
    function  Initialize: boolean;
    property Task: IOmniTask read GetTask write SetTask;
  end; { IOmniWorker }

  TOmniWorker = class(TInterfacedObject, IOmniWorker)
  strict private
    owTask: IOmniTask;
  protected
    procedure DispatchMessage(var msg: TOmniMessage); virtual;
    function  GetTask: IOmniTask;
    procedure SetTask(const value: IOmniTask);
  public
    procedure Idle; virtual;
    function  Initialize: boolean; virtual;
    procedure Cleanup; virtual;
    property Task: IOmniTask read GetTask write SetTask;
  end; { TOmniWorker }

  TOmniTaskProcedure = procedure(task: IOmniTask);
  TOmniTaskMethod = procedure(task: IOmniTask) of object;

  IOmniTaskControl = interface ['{881E94CB-8C36-4CE7-9B31-C24FD8A07555}']
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetExitCode: integer;
    function  GetExitMessage: string;
    function  GetName: string;
    function  GetUniqueID: cardinal;
  //
    function  Alertable: IOmniTaskControl;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetIdle(interval_ms: cardinal; idleMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(paramValue: TOmniValue; const paramName: string = ''): IOmniTaskControl;
    function  SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  //
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Name: string read GetName;
    property UniqueID: cardinal read GetUniqueID; 
  end; { IOmniTaskControl }

  function CreateTask(executor: TOmniTaskProcedure; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(executor: TOmniTaskMethod; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: IOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;
  function CreateTask(worker: TOmniWorker; const taskName: string = ''): IOmniTaskControl; overload;

implementation

uses
  Messages,
  HVStringBuilder,
  DSiWin32,
  GpStuff,
  OtlTaskEvents;

type
  TOmniTaskExecutorType = (etNone, etMethod, etProcedure);

  TOmniTaskExecutor = record
  strict private
    oteExecutorType: TOmniTaskExecutorType;
    oteMethod      : TOmniTaskMethod;
    oteProc        : TOmniTaskProcedure;
    function GetMethod: TOmniTaskMethod;
    function GetProc  : TOmniTaskProcedure;
  public
    constructor CreateMethod(method: TOmniTaskMethod);
    constructor CreateProc(proc: TOmniTaskProcedure);
    procedure Execute(task: IOmniTask);
    property ExecutorType: TOmniTaskExecutorType read oteExecutorType;
    property Method: TOmniTaskMethod read GetMethod;
    property Proc: TOmniTaskProcedure read GetProc;
  end; { TOmniTaskExecutor }

  TOmniValueContainer = class
  strict private
    ovcCanModify: boolean;
    ovcNames    : TStringList;
    ovcValues   : array of TOmniValue;
  strict protected
    procedure Clear;
    procedure Grow;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(paramValue: TOmniValue; paramName: string = '');
    procedure Assign(parameters: array of TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    function ParamByIdx(paramIdx: integer): TOmniValue;
    function ParamByName(const paramName: string): TOmniValue;
  end; { TOmniValueContainer }

  IOmniTaskExecutor = interface ['{123F2A63-3769-4C5B-89DA-1FEB6C3421ED}']
    procedure Execute;
  end; { IOmniTaskExecutor }

  TOmniTask = class(TInterfacedObject, IOmniTask, IOmniTaskExecutor)
  strict private
    otCommChannel    : IOmniTwoWayChannel;
    otExecutor       : TOmniTaskExecutor;
    otMonitorWindow  : THandle;
    otParameters_ref : TOmniValueContainer;
    otTerminatedEvent: TDSiEventHandle;
    otTerminateEvent : TDSiEventHandle;
    otUniqueID       : cardinal;
  protected
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetParam(idxParam: integer): TOmniValue; inline;
    function  GetParamByName(const paramName: string): TOmniValue; inline;
    function  GetTerminateEvent: THandle; inline;
    function  GetUniqueID: cardinal; inline;
    procedure Terminate; inline;
  public
    constructor Create(executor: TOmniTaskExecutor; parameters: TOmniValueContainer; comm:
      IOmniTwoWayChannel; uniqueID: cardinal; terminateEvent, terminatedEvent:
      TDSiEventHandle; monitorWindow: THandle);
    procedure Execute;
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property TerminateEvent: THandle read GetTerminateEvent;
  end; { TOmniTask }

  TOmniThread = class(TThread)
  strict private
    otTask: IOmniTask;
  protected
    procedure Execute; override;
  public
    constructor Create(task: IOmniTask);
    property Task: IOmniTask read otTask;
  end; { TOmniThread }

  TOmniTaskControlOption = (tcoAlertableWait, tcoMessageWait);
  TOmniTaskControlOptions = set of TOmniTaskControlOption;

  TOmniTaskControl = class(TInterfacedObject, IOmniTaskControl)
  strict private
    otcCommChannel    : IOmniTwoWayChannel;
    otcExecutor       : TOmniTaskExecutor;
    otcExit           : integer;
    otcExitMessage    : string;
    otcIdleInterval_ms: cardinal;
    otcIdleMessage    : integer;
    otcMonitorWindow  : THandle;
    otcOptions        : TOmniTaskControlOptions;
    otcParameters     : TOmniValueContainer;
    otcTaskName       : string;
    otcTerminatedEvent: TDSiEventHandle;
    otcTerminateEvent : TDSiEventHandle;
    otcThread         : TOmniThread;
    otcUniqueID       : cardinal;
    otcWakeMask       : DWORD;
    otcWorkerIntf     : IOmniWorker;
    otcWorkerObj_ref  : TOmniWorker;
  strict protected
    procedure DispatchMessages(task: IOmniTask);
    procedure Initialize;
    procedure ProcessThreadMessages;
  protected
    function  Alertable: IOmniTaskControl;
    function  GetComm: IOmniCommunicationEndpoint; inline;
    function  GetExitCode: integer; inline;
    function  GetExitMessage: string; inline;
    function  GetName: string; inline;
    function  GetUniqueID: cardinal; inline;
  public
    constructor Create(worker: IOmniWorker; const taskName: string); overload;
    constructor Create(worker: TOmniWorker; const taskName: string); overload;
    constructor Create(executor: TOmniTaskMethod; const taskName: string); overload;
    constructor Create(executor: TOmniTaskProcedure; const taskName: string); overload;
    destructor  Destroy; override;
    function  MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
    function  RemoveMonitor: IOmniTaskControl;
    function  Run: IOmniTaskControl;
    function  Schedule(threadPool: IOmniThreadPool = nil {default pool}): IOmniTaskControl;
    function  SetIdle(interval_ms: cardinal; idleMessage: integer = -1): IOmniTaskControl;
    function  SetMonitor(hWindow: THandle): IOmniTaskControl;
    function  SetParameter(paramValue: TOmniValue; const paramName: string = ''): IOmniTaskControl;
    function  SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
    function  Terminate(maxWait_ms: cardinal = INFINITE): boolean; //will kill thread after timeout
    function  TerminateWhen(event: THandle): IOmniTaskControl;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property ExitCode: integer read GetExitCode;
    property ExitMessage: string read GetExitMessage;
    property Name: string read GetName;
    property Options: TOmniTaskControlOptions read otcOptions write otcOptions;
    property UniqueID: cardinal read GetUniqueID;
  end; { TOmniTaskControl }

var
  taskUID: TGp4AlignedInt;

{ exports }

function CreateTask(executor: TOmniTaskProcedure; const taskName: string): IOmniTaskControl; overload;
begin
  Result := TOmniTaskControl.Create(executor, taskName);
end; { CreateTask }

function CreateTask(executor: TOmniTaskMethod; const taskName: string): IOmniTaskControl; overload; 
begin
  Result := TOmniTaskControl.Create(executor, taskName);
end; { CreateTask }

function CreateTask(worker: IOmniWorker; const taskName: string): IOmniTaskControl; overload;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

function CreateTask(worker: TOmniWorker; const taskName: string): IOmniTaskControl; overload;
begin
  Result := TOmniTaskControl.Create(worker, taskName);
end; { CreateTask }

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

procedure TOmniWorker.Idle;
begin
  //do-nothing
end; { TOmniWorker.Idle }

function TOmniWorker.Initialize: boolean;
begin
  //do-nothing
  Result := true;
end; { TOmniWorker.Initialize }

procedure TOmniWorker.SetTask(const value: IOmniTask);
begin
  owTask := value;
end; { TOmniWorker.SetTask }

{ TOmniTask }

constructor TOmniTask.Create(executor: TOmniTaskExecutor; parameters:
  TOmniValueContainer; comm: IOmniTwoWayChannel; uniqueID: cardinal; terminateEvent,
  terminatedEvent: TDSiEventHandle; monitorWindow: THandle);
begin
  inherited Create;
  otExecutor := executor;
  otParameters_ref := parameters;
  otCommChannel := comm;
  otUniqueID := uniqueID;
  otMonitorWindow := monitorWindow;
  otTerminateEvent := terminateEvent;
  otTerminatedEvent := terminatedEvent;
end; { TOmniTask.Create }

procedure TOmniTask.Execute;
begin
  otExecutor.Execute(Self);
  if otMonitorWindow <> 0 then
    PostMessage(otMonitorWindow, COmniTaskMsg_Terminated, integer(otUniqueID), 0);
  SetEvent(otTerminatedEvent);
end; { TOmniTask.Execute }

function TOmniTask.GetComm: IOmniCommunicationEndpoint;
begin
  Result := otCommChannel.Endpoint2;
end; { TOmniTask.GetComm }

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
  Result := otTerminateEvent;
end; { TOmniTask.GetTerminateEvent }

function TOmniTask.GetUniqueID: cardinal;
begin
  Result := otUniqueID;
end; { TOmniTask.GetUniqueID }

procedure TOmniTask.SetExitStatus(exitCode: integer; const exitMessage: string);
begin
  raise Exception.Create('Not implemented: TOmniTask.SetExitStatus');
end; { TOmniTask.SetExitStatus }

procedure TOmniTask.Terminate;
begin
  SetEvent(otTerminateEvent);
end; { TOmniTask.Terminate }

{ TOmniValueContainer }

constructor TOmniValueContainer.Create;
begin
  inherited Create;
  ovcNames := TStringList.Create;
  ovcCanModify := true;
end; { TOmniValueContainer.Create }

destructor TOmniValueContainer.Destroy;
begin
  FreeAndNil(ovcNames);
  inherited Destroy;
end; { TOmniValueContainer.Destroy }

procedure TOmniValueContainer.Add(paramValue: TOmniValue; paramName: string);
var
  idxParam: integer;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  if paramName = '' then
    paramName := IntToStr(ovcNames.Count);
  idxParam := ovcNames.IndexOf(paramName); 
  if idxParam < 0 then begin
    idxParam := ovcNames.Add(paramName);
    if ovcNames.Count > Length(ovcValues) then
      Grow;
  end;
  ovcValues[idxParam] := paramValue;
end; { TOmniValueContainer.Add }

procedure TOmniValueContainer.Assign(parameters: array of TOmniValue);
var
  value: TOmniValue;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  Clear;
  SetLength(ovcValues, Length(parameters));
  for value in parameters do
    Add(value);
end; { TOmniValueContainer.Assign }

procedure TOmniValueContainer.Clear;
begin
  SetLength(ovcValues, 0);
  ovcNames.Clear;
end; { TOmniValueContainer.Clear }

procedure TOmniValueContainer.Grow;
var
  iValue   : integer;
  tmpValues: array of TOmniValue;
begin
  SetLength(tmpValues, Length(ovcValues));
  for iValue := 0 to High(ovcValues) - 1 do
    tmpValues[iValue] := ovcValues[iValue];
  SetLength(ovcValues, 2*Length(ovcValues)+1);
  for iValue := 0 to High(tmpValues) - 1 do
    ovcValues[iValue] := tmpValues[iValue];
end; { TOmniValueContainer.Grow }

function TOmniValueContainer.IsLocked: boolean;
begin
  Result := not ovcCanModify;
end; { TOmniValueContainer.IsLocked }

procedure TOmniValueContainer.Lock;
begin
  ovcCanModify := false;
end; { TOmniValueContainer.Lock }

function TOmniValueContainer.ParamByIdx(paramIdx: integer): TOmniValue;
begin
  Result := ovcValues[paramIdx];
end; { TOmniValueContainer.ParamByIdx }

function TOmniValueContainer.ParamByName(const paramName: string): TOmniValue;
begin
  Result := ovcValues[ovcNames.IndexOf(paramName)];
end; { TOmniValueContainer.ParamByName }

{ TOmniTaskExecutor }

constructor TOmniTaskExecutor.CreateMethod(method: TOmniTaskMethod);
begin
  oteExecutorType := etMethod;
  oteMethod := method;
end; { TOmniTaskExecutor.CreateMethod }

constructor TOmniTaskExecutor.CreateProc(proc: TOmniTaskProcedure);
begin
  oteExecutorType := etProcedure;
  oteProc := proc;
end; { TOmniTaskExecutor.CreateProc }

procedure TOmniTaskExecutor.Execute(task: IOmniTask);
begin
  case ExecutorType of
    etMethod:
      Method(task);
    etProcedure:
      Proc(task);
    else
      raise Exception.Create('TOmniTaskExecutor.Execute: Executor is not set');
  end;
end; { TOmniTaskExecutor.Execute }

function TOmniTaskExecutor.GetMethod: TOmniTaskMethod;
begin
  if oteExecutorType <> etMethod then
    raise Exception.Create('TOmniTaskExecutor: Executor is not a method');
  Result := oteMethod;
end; { TOmniTaskExecutor.GetMethod }

function TOmniTaskExecutor.GetProc: TOmniTaskProcedure;
begin
  if oteExecutorType <> etProcedure then
    raise Exception.Create('TOmniTaskExecutor: Executor is not a procedure');
  Result := oteProc;
end; { TOmniTaskExecutor.GetProc }

{ TOmniTaskControl }

constructor TOmniTaskControl.Create(worker: IOmniWorker; const taskName: string);
begin
  otcWorkerIntf := worker;
  otcExecutor := TOmniTaskExecutor.CreateMethod(DispatchMessages);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(executor: TOmniTaskMethod; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.CreateMethod(executor);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(executor: TOmniTaskProcedure; const taskName: string);
begin
  otcExecutor := TOmniTaskExecutor.CreateProc(executor);
  otcTaskName := taskName;
  Initialize;
end; { TOmniTaskControl.Create }

constructor TOmniTaskControl.Create(worker: TOmniWorker; const taskName: string);
begin
  otcWorkerObj_ref := worker;
  otcExecutor := TOmniTaskExecutor.CreateMethod(DispatchMessages);
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
  otcCommChannel := nil;
  DSiCloseHandleAndNull(otcTerminateEvent);
  DSiCloseHandleAndNull(otcTerminatedEvent);
  FreeAndNil(otcParameters);
  inherited Destroy;
end; { TOmniTaskControl.Destroy }

function TOmniTaskControl.Alertable: IOmniTaskControl;
begin
  if not (assigned(otcWorkerIntf) or assigned(otcWorkerObj_ref)) then
    raise Exception.Create('TOmniTaskControl.Alertable: Alertable wait is only available when working with an OmniWorker');
  Include(otcOptions, tcoAlertableWait);
  Result := Self;
end; { TOmniTaskControl.Alertable }

///<summary>Message dispatching loop. *** Executed from the task thread *** </summary>
procedure TOmniTaskControl.DispatchMessages(task: IOmniTask);
var
  flags      : DWORD;
  lastIdle_ms: int64;
  msg        : TOmniMessage;
  timeout_ms : int64;
  wakeMask   : DWORD;
begin
  if assigned(otcWorkerIntf) and assigned(otcWorkerObj_ref) then
    raise Exception.Create('TOmniTaskControl: Internal error, both otcWorkerIntf and otcWorkerObj are assigned');
  if assigned(otcWorkerIntf) then begin
    otcWorkerIntf.Task := task;
    if not otcWorkerIntf.Initialize then
      Exit;
  end;
  if assigned(otcWorkerObj_ref) then begin
    otcWorkerObj_ref.Task := task;
    if not otcWorkerObj_ref.Initialize then
      Exit;
  end;
  try
    if tcoMessageWait in Options then
      wakeMask := otcWakeMask
    else
      wakeMask := 0;
    if tcoAlertableWait in Options then
      flags := MWMO_ALERTABLE
    else
      flags := 0;
    lastIdle_ms := DSiTimeGetTime64;
    repeat
      if otcIdleInterval_ms <= 0 then
        timeout_ms := INFINITE
      else begin
        timeout_ms := otcIdleInterval_ms - (DSiTimeGetTime64 - lastIdle_ms);
        if timeout_ms < 0 then
          timeout_ms := 0;
      end;
      case DSiMsgWaitForTwoObjectsEx(task.TerminateEvent, task.Comm.NewMessageEvent,
             cardinal(timeout_ms), wakeMask, flags)
      of
        WAIT_OBJECT_1:
          if task.Comm.Receive(msg) then begin
            if assigned(otcWorkerIntf) then
              otcWorkerIntf.DispatchMessage(msg);
            if assigned(otcWorkerObj_ref) then
              otcWorkerObj_ref.DispatchMessage(msg)
          end;
        WAIT_OBJECT_2: //message
          ProcessThreadMessages;
        WAIT_IO_COMPLETION:
          ; // do-nothing
        WAIT_TIMEOUT:
          begin
            if otcIdleMessage >= 0 then begin
              msg.MsgID := otcIdleMessage;
              msg.MsgData := Null;
              if assigned(otcWorkerIntf) then
                otcWorkerIntf.DispatchMessage(msg);
              if assigned(otcWorkerObj_ref) then
                otcWorkerObj_ref.DispatchMessage(msg);
            end
            else if assigned(otcWorkerIntf) then
              otcWorkerIntf.Idle
            else if assigned(otcWorkerObj_ref) then
              otcWorkerObj_ref.Idle;
            lastIdle_ms := DSiTimeGetTime64;
          end; //WAIT_TIMEOUT
        else
          break; //repeat
      end; //case
    until false;
  finally
    if assigned(otcWorkerIntf) then begin
      otcWorkerIntf.Cleanup;
      otcWorkerIntf.Task := nil;
    end;
    if assigned(otcWorkerObj_ref) then begin
      otcWorkerObj_ref.Cleanup;
      otcWorkerObj_ref.Task := nil;
    end;
  end;
  otcWorkerIntf := nil;
  otcWorkerObj_ref := nil;
end; { TOmniTaskControl.DispatchMessages }

function TOmniTaskControl.GetComm: IOmniCommunicationEndpoint;
begin
  Result := otcCommChannel.Endpoint1;
end; { TOmniTaskControl.GetComm }

function TOmniTaskControl.GetExitCode: integer;
begin
  Result := otcExit;
end; { TOmniTaskControl.GetExitCode }

function TOmniTaskControl.GetExitMessage: string;
begin
  Result := otcExitMessage;
end; { TOmniTaskControl.GetExitMessage }

function TOmniTaskControl.GetName: string;
begin
  Result := otcTaskName;
end; { TOmniTaskControl.GetName }

function TOmniTaskControl.GetUniqueID: cardinal;
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

function TOmniTaskControl.MsgWait(wakeMask: DWORD = QS_ALLEVENTS): IOmniTaskControl;
begin
  if not (assigned(otcWorkerIntf) or assigned(otcWorkerObj_ref)) then
    raise Exception.Create('TOmniTaskControl.MsgWait: Message wait is only available when working with an OmniWorker');
  Include(otcOptions, tcoMessageWait);
  otcWakeMask := wakeMask;
  Result := Self;
end; { TOmniTaskControl.MsgWait }

procedure TOmniTaskControl.ProcessThreadMessages;
var
  msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) and (Msg.Message <> WM_QUIT) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end; { TOmniTaskControl.ProcessThreadMessages }

function TOmniTaskControl.RemoveMonitor: IOmniTaskControl;
begin
  otcMonitorWindow := 0;
  otcCommChannel.Endpoint2.RemoveMonitor;
  Result := Self;
end; { TOmniTaskControl.RemoveMonitor }

function TOmniTaskControl.Run: IOmniTaskControl;
var
  task: IOmniTask;
begin
  otcParameters.Lock;
  task := TOmniTask.Create(otcExecutor, otcParameters, otcCommChannel, otcUniqueID,
    otcTerminateEvent, otcTerminatedEvent, otcMonitorWindow);
  otcThread := TOmniThread.Create(task);
  otcThread.Resume;
  Result := Self;
end; { TOmniTaskControl.Run }

function TOmniTaskControl.Schedule(threadPool: IOmniThreadPool): IOmniTaskControl;
//var
//  task: IOmniTask;
begin
//  otcParameters.Lock;
//  task := TOmniTask.Create(otcExecutor, otcParameters, otcComm);
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniTaskControl.Schedule
  raise Exception.Create('Thread pools are not implemented - yet ...');
//  Result := Self;
end; { TOmniTaskControl.Schedule }

function TOmniTaskControl.SetIdle(interval_ms: cardinal; idleMessage: integer):
  IOmniTaskControl;
begin
  if not assigned(otcWorkerIntf) then
    raise Exception.Create('TOmniTaskControl.SetIdle: Idle mode is only available when working with an IOmniWorker');
  otcIdleInterval_ms := interval_ms;
  otcIdleMessage := idleMessage;
  Result := Self;
end; { TOmniTaskControl.SetIdle }

function TOmniTaskControl.SetMonitor(hWindow: THandle): IOmniTaskControl;
begin
  if otcParameters.IsLocked then
    raise Exception.Create('TOmniTaskControl.SetMonitor: Monitor can only be assigned while task is not running');
  otcMonitorWindow := hWindow;
  otcCommChannel.Endpoint2.SetMonitor(hWindow, integer(UniqueID), 0);
  Result := Self;
end; { TOmniTaskControl.SetMonitor }

function TOmniTaskControl.SetParameter(paramValue: TOmniValue;
  const paramName: string): IOmniTaskControl;
begin
  otcParameters.Add(paramValue, paramName);
  Result := Self;
end; { TOmniTaskControl.SetParameter }

function TOmniTaskControl.SetParameters(parameters: array of TOmniValue): IOmniTaskControl;
begin
  otcParameters.Assign(parameters);
  Result := Self;
end; { TOmniTaskControl.SetParameters }

function TOmniTaskControl.Terminate(maxWait_ms: cardinal): boolean;
begin
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
