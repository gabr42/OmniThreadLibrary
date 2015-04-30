///<summary>Event dispatching component. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2012, Primoz Gabrijelcic
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
///   Last modification : 2012-10-02
///   Version           : 1.07e
///</para><para>
///   History:
///     1.07e: 2012-10-02
///       - TOmniEventMonitor is marked for 64-bit support.
///     1.07d: 2012-10-01
///       - COmniTaskMsg_NewMessage messages must be processed even if OnTaskMessage event
///         is not assigned. Otherwise internal messages can get lost.
///     1.07c: 2012-09-27
///       - Calls task controller's FilterMessage method to remove internal (Invoke)
///         messages before passing messages to the event handler.
///     1.07b: 2011-12-19
///       - COmniTaskMsg_Terminated is processed even if OnTaskTerminated handler is not set.
///     1.07a: 2011-07-27
///       - Removed 'FreeAndNil(uninitialized variable)' which was leftover from
///         incorrectly removed code in version 1.06.
///     1.07: 2011-07-26
///       - TOmniTaskEvent, TOmniTaskMessageEvent, TOmniPoolThreadEvent, and
///         TOmniPoolWorkItemEvent renamed to TOmniMonitorTaskEvent,
///         TOmniMonitorTaskMessageEvent, TOmniMonitorPoolThreadEvent and
///         TOmniMonitorPoolWorkItemEvent, respectively.
///     1.06: 2011-07-14
///       - Removed task exception object parameter from OnPoolWorkItemCompleted.
///     1.05: 2011-07-04
///       - OnPoolWorkItemCompleted event handler got new parameter - task exception object.
///     1.04b: 2011-02-15
///       - Don't rearm self if message window was already destroyed.
///       - Safely destroy message window.
///     1.04a: 2010-09-23
///       - Destroy internal monitor in Terminate.
///       - Signal termination (in Execute) before 'Terminated' is set (which may cause
///         Monitor to be immediately destroyed.
///     1.04: 2010-07-22
///       - Implemented ProcessMessages.
///     1.03: 2010-07-07
///       - Internal message window is exposed via the MessageWindow property.
///     1.02: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.01a: 2010-05-30
///       - Message retrieving loop destroys interface immediately, not when the next
///         message is received.
///     1.01: 2010-03-03
///       - Implemented TOmniEventMonitorPool, per-thread TOmniEventMonitor allocator.
///     1.0a: 2009-01-26
///       - Pass correct task ID to the OnPoolWorkItemCompleted handler.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

unit OtlEventMonitor;

{$I OtlOptions.inc}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  SysUtils,
  Messages,
  Classes,
  GpStuff,
  GpLists,
  OtlCommon,
  OtlComm,
  OtlSync,
  OtlTaskControl,
  OtlThreadPool;

type
  TOmniMonitorTaskEvent = procedure(const task: IOmniTaskControl) of object;
  TOmniMonitorTaskMessageEvent = procedure(const task: IOmniTaskControl; const msg: TOmniMessage) of object;
  TOmniMonitorPoolThreadEvent = procedure(const pool: IOmniThreadPool; threadID: integer) of object;
  TOmniMonitorPoolWorkItemEvent = procedure(const pool: IOmniThreadPool; taskID: int64) of object;

  {$IFDEF OTL_Supports64Bit}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF OTL_Supports64Bit}
  TOmniEventMonitor = class(TComponent, IOmniTaskControlMonitor, IOmniThreadPoolMonitor)
  strict private
    emMessageWindow           : THandle;
    emMonitoredPools          : IOmniInterfaceDictionary;
    emMonitoredTasks          : IOmniInterfaceDictionary;
    emOnPoolThreadCreated     : TOmniMonitorPoolThreadEvent;
    emOnPoolThreadDestroying  : TOmniMonitorPoolThreadEvent;
    emOnPoolThreadKilled      : TOmniMonitorPoolThreadEvent;
    emOnPoolWorkItemEvent     : TOmniMonitorPoolWorkItemEvent;
    emOnTaskMessage           : TOmniMonitorTaskMessageEvent;
    emOnTaskUndeliveredMessage: TOmniMonitorTaskMessageEvent;
    emOnTaskTerminated        : TOmniMonitorTaskEvent;
    emCurrentMsg              : TOmniMessage;
  strict protected
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Detach(const pool: IOmniThreadPool): IOmniThreadPool; overload;
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Monitor(const pool: IOmniThreadPool): IOmniThreadPool; overload;
    procedure ProcessMessages;
  published
    property MessageWindow: THandle read emMessageWindow;
    property OnPoolThreadCreated: TOmniMonitorPoolThreadEvent read emOnPoolThreadCreated
      write emOnPoolThreadCreated;
    property OnPoolThreadDestroying: TOmniMonitorPoolThreadEvent read emOnPoolThreadDestroying
      write emOnPoolThreadDestroying;
    property OnPoolThreadKilled: TOmniMonitorPoolThreadEvent read emOnPoolThreadKilled
      write emOnPoolThreadKilled;
    property OnPoolWorkItemCompleted: TOmniMonitorPoolWorkItemEvent read emOnPoolWorkItemEvent
      write emOnPoolWorkItemEvent;
    property OnTaskMessage: TOmniMonitorTaskMessageEvent read emOnTaskMessage
      write emOnTaskMessage;
    property OnTaskTerminated: TOmniMonitorTaskEvent read emOnTaskTerminated
      write emOnTaskTerminated;
    property OnTaskUndeliveredMessage: TOmniMonitorTaskMessageEvent
      read emOnTaskUndeliveredMessage write emOnTaskUndeliveredMessage;
  end; { TOmniEventMonitor }

  TOmniEventMonitorClass = class of TOmniEventMonitor;

  ///<summary>A pool of per-thread event monitors.</summary>
  ///<since>2010-03-03</since>
  TOmniEventMonitorPool = class
  strict private
    empListLock    : TOmniCS;
    empMonitorClass: TOmniEventMonitorClass;
    empMonitorList : TGpIntegerObjectList;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Allocate: TOmniEventMonitor;
    procedure Release(monitor: TOmniEventMonitor);
    property MonitorClass: TOmniEventMonitorClass read empMonitorClass write empMonitorClass;
  end; { TOmniEventMonitorPool }

var
  COmniTaskMsg_NewMessage: cardinal;
  COmniTaskMsg_Terminated: cardinal;
  COmniPoolMsg           : cardinal;
  
implementation

uses
  Windows,
  DSiWin32;

const
  CMaxReceiveLoop_ms = 5;

type
  ///<summary>Reference counted TOmniEventMonitor.</summary>
  TOmniCountedEventMonitor = class
  strict private
    cemMonitor : TOmniEventMonitor;
    cemRefCount: integer;
  public
    constructor Create(monitor: TOmniEventMonitor);
    destructor  Destroy; override;
    function  Allocate: TOmniEventMonitor;
    procedure Release;
    property Monitor: TOmniEventMonitor read cemMonitor;
    property RefCount: integer read cemRefCount;
  end; { TOmniCountedEventMonitor }

{ TOmniEventMonitor }

constructor TOmniEventMonitor.Create(AOwner: TComponent);
begin
  inherited;
  emMessageWindow := DSiAllocateHWnd(WndProc);
  Win32Check(emMessageWindow <> 0);
  emMonitoredTasks := CreateInterfaceDictionary;
  emMonitoredPools := CreateInterfaceDictionary;
end; { TOmniEventMonitor.Create }

destructor TOmniEventMonitor.Destroy;
var
  intfKV   : TOmniInterfaceDictionaryPair;
  winHandle: THandle;
begin
  for intfKV in emMonitoredTasks do
    (intfKV.Value as IOmniTaskControl).RemoveMonitor;
  emMonitoredTasks.Clear;
  for intfKV in emMonitoredPools do
    (intfKV.Value as IOmniThreadPool).RemoveMonitor;
  emMonitoredPools.Clear;
  if emMessageWindow <> 0 then begin
    winHandle := emMessageWindow;
    emMessageWindow := 0;
    DSiDeallocateHWnd(winHandle);
  end;
  inherited;
end; { TOmniEventMonitor.Destroy }

function TOmniEventMonitor.Detach(const task: IOmniTaskControl): IOmniTaskControl;
begin
  emMonitoredTasks.Remove(task.UniqueID);
  Result := task.RemoveMonitor;
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.Detach(const pool: IOmniThreadPool): IOmniThreadPool;
begin
  emMonitoredPools.Remove(pool.UniqueID);
  Result := pool.RemoveMonitor;
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.Monitor(const task: IOmniTaskControl): IOmniTaskControl;
begin
  Assert(emMessageWindow <> 0);
  emMonitoredTasks.Add(task.UniqueID, task);
  Result := task.SetMonitor(emMessageWindow);
end; { TOmniEventMonitor.Monitor }

function TOmniEventMonitor.Monitor(const pool: IOmniThreadPool): IOmniThreadPool;
begin
  Assert(emMessageWindow <> 0);
  emMonitoredPools.Add(pool.UniqueID, pool);
  Result := pool.SetMonitor(emMessageWindow);
end; { TOmniEventMonitor.Monitor }

procedure TOmniEventMonitor.ProcessMessages;
var
  msg: TMsg;
begin
  while (emMessageWindow <> 0) and
        PeekMessage(Msg, emMessageWindow, 0, 0, PM_REMOVE) and
        (Msg.Message <> WM_QUIT) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end; { TOmniEventMonitor.ProcessMessages }

procedure TOmniEventMonitor.WndProc(var msg: TMessage);
var
  endpoint     : IOmniCommunicationEndpoint;
  pool         : IOmniThreadPool;
  task         : IOmniTaskControl;
  timeStart    : int64;
  tpMonitorInfo: TOmniThreadPoolMonitorInfo;

  function ProcessMessages(timeout_ms: integer = CMaxReceiveLoop_ms;
    rearmSelf: boolean = true): boolean;
  begin
    Result := true;
    while task.Comm.Receive(emCurrentMsg) do begin
      if (not (task as IOmniTaskControlInternals).FilterMessage(emCurrentMsg)) and
         assigned(emOnTaskMessage)
      then
        emOnTaskMessage(task, emCurrentMsg);
      if (DSiElapsedSince(GetTickCount, timeStart) > timeout_ms) and (emMessageWindow <> 0) then begin
        if rearmSelf then
          Win32Check(PostMessage(emMessageWindow, COmniTaskMsg_NewMessage, msg.WParam, msg.LParam));
        Result := false;
        break; //while
      end;
    end; //while
    emCurrentMsg.MsgData._ReleaseAndClear;
  end; { ProcessMessages }

begin { TOmniEventMonitor.WndProc }
  if msg.Msg = COmniTaskMsg_NewMessage then begin
    task := emMonitoredTasks.ValueOf(Pint64(@msg.WParam)^) as IOmniTaskControl;
    if assigned(task) then begin
      timeStart := GetTickCount;
      ProcessMessages;
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniTaskMsg_Terminated then begin
    task := emMonitoredTasks.ValueOf(Pint64(@msg.WParam)^) as IOmniTaskControl;
    if assigned(task) then begin
      endpoint := (task as IOmniTaskControlSharedInfo).SharedInfo.CommChannel.Endpoint1;
      while endpoint.Receive(emCurrentMsg) do
        if Assigned(emOnTaskMessage) then
          emOnTaskMessage(task, emCurrentMsg);
      endpoint := (task as IOmniTaskControlSharedInfo).SharedInfo.CommChannel.Endpoint2;
      while endpoint.Receive(emCurrentMsg) do
        if Assigned(emOnTaskUndeliveredMessage) then
          emOnTaskUndeliveredMessage(task, emCurrentMsg);
      emCurrentMsg.MsgData._ReleaseAndClear;
      if Assigned(emOnTaskTerminated) then
        OnTaskTerminated(task);
      Detach(task);
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniPoolMsg then begin
    tpMonitorInfo := TOmniThreadPoolMonitorInfo(msg.LParam);
    try
      pool := emMonitoredPools.ValueOf(tpMonitorInfo.UniqueID) as IOmniThreadPool;
      if assigned(pool) then begin
        if tpMonitorInfo.ThreadPoolOperation = tpoCreateThread then begin
          if assigned(OnPoolThreadCreated) then
            OnPoolThreadCreated(pool, tpMonitorInfo.ThreadID);
        end
        else if tpMonitorInfo.ThreadPoolOperation = tpoDestroyThread then begin
          if assigned(OnPoolThreadDestroying) then
            OnPoolThreadDestroying(pool, tpMonitorInfo.ThreadID);
        end
        else if tpMonitorInfo.ThreadPoolOperation = tpoKillThread then begin
          if assigned(OnPoolThreadKilled) then
            OnPoolThreadKilled(pool, tpMonitorInfo.ThreadID);
        end
        else if tpMonitorInfo.ThreadPoolOperation = tpoWorkItemCompleted then begin
          if assigned(OnPoolWorkItemCompleted) then
            OnPoolWorkItemCompleted(pool, tpMonitorInfo.TaskID);
        end;
      end;
    finally FreeAndNil(tpMonitorInfo); end;
  end
  else
    msg.Result := DefWindowProc(emMessageWindow, msg.Msg, msg.WParam, msg.LParam);
end; { TOmniEventMonitor.WndProc }

{ TOmniCountedEventMonitor }

constructor TOmniCountedEventMonitor.Create(monitor: TOmniEventMonitor);
begin
  inherited Create;
  cemMonitor := monitor;
  cemRefCount := 1;
end; { TOmniCountedEventMonitor.Create }

destructor TOmniCountedEventMonitor.Destroy;
begin
  FreeAndNil(cemMonitor);
  inherited;
end; { TOmniCountedEventMonitor.Destroy }

function TOmniCountedEventMonitor.Allocate: TOmniEventMonitor;
begin
  if cemRefCount = 0 then
    cemMonitor := TOmniEventMonitor.Create(nil);
  Inc(cemRefCount);
  Result := cemMonitor;
end; { TOmniCountedEventMonitor.Allocate }

procedure TOmniCountedEventMonitor.Release;
begin
  Assert(cemRefCount > 0);
  Dec(cemRefCount);
  if cemRefCount = 0 then
    FreeAndNil(cemMonitor);
end; { TOmniCountedEventMonitor.Release }

{ TOmniEventMonitorPool }

constructor TOmniEventMonitorPool.Create;
begin
  inherited Create;
  empMonitorList := TGpIntegerObjectList.Create;
  empMonitorList.Sorted := true;
end; { TOmniEventMonitorPool.Create }

destructor TOmniEventMonitorPool.Destroy;
begin
  FreeAndNil(empMonitorList);
  inherited;
end; { TOmniEventMonitorPool.Destroy }

///<summary>Returns monitor associated with the current thread. Allocates new monitor if
///    no monitor has been associated with this thread.</summary>
function TOmniEventMonitorPool.Allocate: TOmniEventMonitor;
var
  monitorInfo: TOmniCountedEventMonitor;
begin
  empListLock.Acquire;
  try
    monitorInfo := TOmniCountedEventMonitor(empMonitorList.FetchObject(integer(GetCurrentThreadID)));
    if assigned(monitorInfo) then
      monitorInfo.Allocate
    else begin
      monitorInfo := TOmniCountedEventMonitor.Create(MonitorClass.Create(nil));
      empMonitorList.AddObject(integer(GetCurrentThreadID), monitorInfo);
    end;
    Result := monitorInfo.Monitor;
  finally empListLock.Release; end;
end; { TOmniEventMonitorPool.Allocate }

///<summary>Releases monitor from the current thread. If monitor is no longer in use,
///    destroys the monitor.</summary>
///<since>2010-03-03</since>
procedure TOmniEventMonitorPool.Release(monitor: TOmniEventMonitor);
var
  idxMonitor : integer;
  monitorInfo: TOmniCountedEventMonitor;
begin
  empListLock.Acquire;
  try
    idxMonitor := empMonitorList.IndexOf(integer(GetCurrentThreadID));
    if idxMonitor < 0 then
      raise Exception.CreateFmt(
        'TOmniEventMonitorPool.Release: Monitor is not allocated for thread %d',
        [GetCurrentThreadID]);
    monitorInfo := TOmniCountedEventMonitor(empMonitorList.Objects[idxMonitor]);
    Assert(monitorInfo.Monitor = monitor);
    monitorInfo.Release;
    if monitorInfo.RefCount = 0 then begin
      empMonitorList.Delete(idxMonitor);
    end;
  finally empListLock.Release; end;
end; { TOmniEventMonitorPool.Release }

initialization
  COmniTaskMsg_NewMessage := RegisterWindowMessage('Gp/OtlTaskEvents/NewMessage');
  Win32Check(COmniTaskMsg_NewMessage <> 0);
  COmniTaskMsg_Terminated := RegisterWindowMessage('Gp/OtlTaskEvents/Terminated');
  Win32Check(COmniTaskMsg_Terminated <> 0);
  COmniPoolMsg := RegisterWindowMessage('Gp/OtlThreadPool');
  Win32CHeck(COmniPoolMsg <> 0);
end.
