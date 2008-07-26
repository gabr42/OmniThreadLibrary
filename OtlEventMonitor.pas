///<summary>Event dispatching component. Part of the OmniThreadLibrary project.</summary>
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
///   Last modification : 2008-07-26
///   Version           : 0.3
///</para><para>
///   History:
///     0.3: 2008-07-26
///       - Unit renamed to OltEventMonitor.
///       - Class TOmniTaskEventDispatch renamed to TOmniEventMonitor.
///       - Added support for thread pool monitoring. 
///     0.2: 2008-07-24
///       - Implements IOmniTaskControlMonitor.
///</para></remarks>

unit OtlEventMonitor;

interface

uses
  Messages,
  Classes,
  GpStuff,
  OtlTaskControl,
  OtlThreadPool;

type
  TOmniTaskEvent = procedure(task: IOmniTaskControl) of object;
  TOmniPoolThreadEvent = procedure(pool: IOmniThreadPool; threadID: integer) of object;
  TOmniPoolWorkItemEvent = procedure(pool: IOmniThreadPool; taskID: int64) of object;

// TODO 1 -oPrimoz Gabrijelcic : tedMonitoredTasks list will be too slow, replace it with a sorted list of (task.UniqueID, task) pairs; same goes for tedMonitoredPools

  TOmniEventMonitor = class(TComponent, IOmniTaskControlMonitor, IOmniThreadPoolMonitor)
  strict private
    tedMessageWindow         : THandle;
    tedMonitoredPools        : TInterfaceList;
    tedMonitoredTasks        : TInterfaceList;
    tedOnPoolThreadCreated   : TOmniPoolThreadEvent;
    tedOnPoolThreadDestroying: TOmniPoolThreadEvent;
    tedOnPoolThreadKilled    : TOmniPoolThreadEvent;
    tedOnPoolWorkItemEvent   : TOmniPoolWorkItemEvent;
    tedOnTaskMessage         : TOmniTaskEvent;
    tedOnTaskTerminated      : TOmniTaskEvent;
  strict protected
    function  LocatePool(poolUniqueID: int64): IOmniThreadPool;
    function  LocateTask(taskUniqueID: int64): IOmniTaskControl;
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Detach(task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Detach(pool: IOmniThreadPool): IOmniThreadPool; overload;
    function  Monitor(task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Monitor(pool: IOmniThreadPool): IOmniThreadPool; overload;
  published
    property OnPoolThreadCreated: TOmniPoolThreadEvent read tedOnPoolThreadCreated
      write tedOnPoolThreadCreated;
    property OnPoolThreadDestroying: TOmniPoolThreadEvent read tedOnPoolThreadDestroying
      write tedOnPoolThreadDestroying;
    property OnPoolThreadKilled: TOmniPoolThreadEvent read tedOnPoolThreadKilled
      write tedOnPoolThreadKilled;
    property OnPoolWorkItemCompleted: TOmniPoolWorkItemEvent read tedOnPoolWorkItemEvent
      write tedOnPoolWorkItemEvent;
    property OnTaskMessage: TOmniTaskEvent read tedOnTaskMessage write tedOnTaskMessage;
    property OnTaskTerminated: TOmniTaskEvent read tedOnTaskTerminated write
      tedOnTaskTerminated;
  end; { TOmniEventMonitor }

var
  COmniTaskMsg_NewMessage: cardinal;
  COmniTaskMsg_Terminated: cardinal;
  COmniPoolMsg           : cardinal;

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

{ TOmniEventMonitor }

constructor TOmniEventMonitor.Create(AOwner: TComponent);
begin
  inherited;
  tedMessageWindow := DSiAllocateHWnd(WndProc);
  Win32Check(tedMessageWindow <> 0);
  tedMonitoredTasks := TInterfaceList.Create;
  tedMonitoredPools := TInterfaceList.Create;
end; { TOmniEventMonitor.Create }

destructor TOmniEventMonitor.Destroy;
begin
  while tedMonitoredTasks.Count > 0 do
    Detach(tedMonitoredTasks[tedMonitoredTasks.Count - 1] as IOmniTaskControl);
  FreeAndNil(tedMonitoredTasks);
  while tedMonitoredPools.Count > 0 do
    Detach(tedMonitoredPools[tedMonitoredPools.Count - 1] as IOmniThreadPool);
  if tedMessageWindow <> 0 then begin
    DSiDeallocateHWnd(tedMessageWindow);
    tedMessageWindow := 0;
  end;
  inherited;
end; { TOmniEventMonitor.Destroy }

function TOmniEventMonitor.Detach(task: IOmniTaskControl): IOmniTaskControl;
begin
  Result := task.RemoveMonitor;
  tedMonitoredTasks.Remove(task);
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.Detach(pool: IOmniThreadPool): IOmniThreadPool;
begin
  Result := pool.RemoveMonitor;
  tedMonitoredPools.Remove(pool);
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.LocatePool(poolUniqueID: int64): IOmniThreadPool;
var
  intf: IInterface;
begin
  // TODO 1 -oPrimoz Gabrijelcic : This is too slow!
  for intf in tedMonitoredPools do begin
    Result := intf as IOmniThreadPool;
    if Result.UniqueID = poolUniqueID then
      Exit;
  end;
  Result := nil;
end; { TOmniEventMonitor.LocatePool }

function TOmniEventMonitor.LocateTask(taskUniqueID: int64): IOmniTaskControl;
var
  intf: IInterface;
begin
  // TODO 1 -oPrimoz Gabrijelcic : This is too slow!
  for intf in tedMonitoredTasks do begin
    Result := intf as IOmniTaskControl;
    if Result.UniqueID = taskUniqueID then
      Exit;
  end;
  Result := nil;
end; { TOmniEventMonitor.LocateTask }

function TOmniEventMonitor.Monitor(task: IOmniTaskControl): IOmniTaskControl;
begin
  tedMonitoredTasks.Add(task);
  Result := task.SetMonitor(tedMessageWindow);
end; { TOmniEventMonitor.Monitor }

function TOmniEventMonitor.Monitor(pool: IOmniThreadPool): IOmniThreadPool;
begin
  tedMonitoredPools.Add(pool);
  Result := pool.SetMonitor(tedMessageWindow);
end; { TOmniEventMonitor.Monitor }

procedure TOmniEventMonitor.WndProc(var msg: TMessage);
var
  pool         : IOmniThreadPool;
  task         : IOmniTaskControl;
  taskID       : int64;
  tpMonitorInfo: TOmniThreadPoolMonitorInfo;
begin
  if msg.Msg = COmniTaskMsg_NewMessage then begin
    if assigned(OnTaskMessage) then begin
      Int64Rec(taskID).Lo := cardinal(msg.WParam);
      Int64Rec(taskID).Hi := cardinal(msg.LParam);
      task := LocateTask(taskID);
      if assigned(task) then
        OnTaskMessage(task);
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniTaskMsg_Terminated then begin
    if assigned(OnTaskTerminated) then begin
      Int64Rec(taskID).Lo := cardinal(msg.WParam);
      Int64Rec(taskID).Hi := cardinal(msg.LParam);
      task := LocateTask(taskID);
      if assigned(task) then begin
        OnTaskTerminated(task);
        Detach(task);
      end;
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniPoolMsg then begin
    tpMonitorInfo := TOmniThreadPoolMonitorInfo(msg.LParam);
    try
      pool := LocatePool(tpMonitorInfo.UniqueID);
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
            OnPoolWorkItemCompleted(pool, taskID);
        end;
      end;
    finally FreeAndNil(tpMonitorInfo); end;
  end
  else
    msg.Result := DefWindowProc(tedMessageWindow, msg.Msg, msg.WParam, msg.LParam);
end; { TOmniEventMonitor.WndProc }

initialization
  COmniTaskMsg_NewMessage := RegisterWindowMessage('Gp/OtlTaskEvents/NewMessage');
  Win32Check(COmniTaskMsg_NewMessage <> 0);
  COmniTaskMsg_Terminated := RegisterWindowMessage('Gp/OtlTaskEvents/Terminated');
  Win32Check(COmniTaskMsg_Terminated <> 0);
  COmniPoolMsg := RegisterWindowMessage('Gp/OtlThreadPool');
  Win32CHeck(COmniPoolMsg <> 0);
end.
