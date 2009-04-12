///<summary>Event dispatching component. Part of the OmniThreadLibrary project.</summary>
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
///   Last modification : 2009-01-26
///   Version           : 1.0a
///</para><para>
///   History:
///     1.0a: 2009-01-26
///       - Pass correct task ID to the OnPoolWorkItemCompleted handler.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

{$WARN SYMBOL_PLATFORM OFF}

unit OtlEventMonitor;

interface

uses
  Messages,
  Classes,
  GpStuff,
  GpLists,
  OtlComm,
  OtlCommon,
  OtlTaskControl,
  OtlThreadPool;

type
  TOmniTaskEvent = procedure(const task: IOmniTaskControl) of object;
  TOmniTaskMessageEvent = procedure(const task: IOmniTaskControl; const msg: TOmniMessage) of object;
  TOmniPoolThreadEvent = procedure(const pool: IOmniThreadPool; threadID: integer) of object;
  TOmniPoolWorkItemEvent = procedure(const pool: IOmniThreadPool; taskID: int64) of object;

  TOmniEventMonitor = class(TComponent, IOmniTaskControlMonitor, IOmniThreadPoolMonitor)
  strict private
    tedMessageWindow           : THandle;
    tedMonitoredPools          : IInterfaceDictionary;
    tedMonitoredTasks          : IInterfaceDictionary;
    tedOnPoolThreadCreated     : TOmniPoolThreadEvent;
    tedOnPoolThreadDestroying  : TOmniPoolThreadEvent;
    tedOnPoolThreadKilled      : TOmniPoolThreadEvent;
    tedOnPoolWorkItemEvent     : TOmniPoolWorkItemEvent;
    tedOnTaskMessage           : TOmniTaskMessageEvent;
    tedOnTaskUndeliveredMessage: TOmniTaskEvent;
    tedOnTaskTerminated        : TOmniTaskEvent;
    tedCurrentMsg              : TOmniMessage;
  strict protected
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Detach(const task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Detach(const pool: IOmniThreadPool): IOmniThreadPool; overload;
    function  Monitor(const task: IOmniTaskControl): IOmniTaskControl; overload;
    function  Monitor(const pool: IOmniThreadPool): IOmniThreadPool; overload;
  published
    property OnPoolThreadCreated: TOmniPoolThreadEvent read tedOnPoolThreadCreated
      write tedOnPoolThreadCreated;
    property OnPoolThreadDestroying: TOmniPoolThreadEvent read tedOnPoolThreadDestroying
      write tedOnPoolThreadDestroying;
    property OnPoolThreadKilled: TOmniPoolThreadEvent read tedOnPoolThreadKilled
      write tedOnPoolThreadKilled;
    property OnPoolWorkItemCompleted: TOmniPoolWorkItemEvent read tedOnPoolWorkItemEvent
      write tedOnPoolWorkItemEvent;
    property OnTaskMessage: TOmniTaskMessageEvent read tedOnTaskMessage write tedOnTaskMessage;
    property OnTaskTerminated: TOmniTaskEvent read tedOnTaskTerminated write
      tedOnTaskTerminated;
    property OnTaskUndeliveredMessage: TOmniTaskEvent read tedOnTaskUndeliveredMessage
      write tedOnTaskUndeliveredMessage;
  end; { TOmniEventMonitor }

var
  COmniTaskMsg_NewMessage: cardinal;
  COmniTaskMsg_Terminated: cardinal;
  COmniPoolMsg           : cardinal;
  AllMonitoredTasks      : TList = nil;
  
implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

const
  CMaxReceiveLoop_ms = 5;

{ TOmniEventMonitor }               

constructor TOmniEventMonitor.Create(AOwner: TComponent);
begin
  inherited;
  tedMessageWindow := DSiAllocateHWnd(WndProc);
  Win32Check(tedMessageWindow <> 0);
  tedMonitoredTasks := CreateInterfaceDictionary;
  tedMonitoredPools := CreateInterfaceDictionary;
end; { TOmniEventMonitor.Create }

destructor TOmniEventMonitor.Destroy;
var
  intfKV: TInterfaceDictionaryPair;
begin
  for intfKV in tedMonitoredTasks do 
    (intfKV.Value as IOmniTaskControl).RemoveMonitor;
  tedMonitoredTasks.Clear;
  for intfKV in tedMonitoredPools do
    (intfKV.Value as IOmniThreadPool).RemoveMonitor;
  tedMonitoredPools.Clear;
  if tedMessageWindow <> 0 then begin
    DSiDeallocateHWnd(tedMessageWindow);
    tedMessageWindow := 0;
  end;
  inherited;
end; { TOmniEventMonitor.Destroy }

function TOmniEventMonitor.Detach(const task: IOmniTaskControl): IOmniTaskControl;
begin
  Result := task.RemoveMonitor;
  tedMonitoredTasks.Remove(task.UniqueID);
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.Detach(const pool: IOmniThreadPool): IOmniThreadPool;
begin
  Result := pool.RemoveMonitor;
  tedMonitoredPools.Remove(pool.UniqueID);
end; { TOmniEventMonitor.Detach }

function TOmniEventMonitor.Monitor(const task: IOmniTaskControl): IOmniTaskControl;
begin
  tedMonitoredTasks.Add(task.UniqueID, task);
  Result := task.SetMonitor(tedMessageWindow);
end; { TOmniEventMonitor.Monitor }

function TOmniEventMonitor.Monitor(const pool: IOmniThreadPool): IOmniThreadPool;
begin
  tedMonitoredPools.Add(pool.UniqueID, pool);
  Result := pool.SetMonitor(tedMessageWindow);
end; { TOmniEventMonitor.Monitor }

procedure TOmniEventMonitor.WndProc(var msg: TMessage);
var
  pool         : IOmniThreadPool;
  task         : IOmniTaskControl;
  timeStart    : int64;
  tpMonitorInfo: TOmniThreadPoolMonitorInfo;

  function ProcessMessages(timeout_ms: integer = CMaxReceiveLoop_ms;
    rearmSelf: boolean = true): boolean;
  begin
    Result := true;
    while task.Comm.Receive(tedCurrentMsg) do begin
      if assigned(tedOnTaskMessage) then
        tedOnTaskMessage(task, tedCurrentMsg);
      if DSiElapsedSince(GetTickCount, timeStart) > timeout_ms then begin
        if rearmSelf then
          Win32Check(PostMessage(tedMessageWindow, COmniTaskMsg_NewMessage, msg.WParam, msg.LParam));
        Result := false;
        break; //while
      end;
    end; //while
  end; { ProcessMessages }

begin { TOmniEventMonitor.WndProc }
  if msg.Msg = COmniTaskMsg_NewMessage then begin
    if assigned(OnTaskMessage) then begin
      task := tedMonitoredTasks.ValueOf(Pint64(@msg.WParam)^) as IOmniTaskControl;
      if assigned(task) then begin
        timeStart := GetTickCount;
        if ProcessMessages then begin
          // all messages processed
          task.SetMonitor(tedMessageWindow);
          ProcessMessages(1, false);
        end;
      end;
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniTaskMsg_Terminated then begin
    if assigned(OnTaskTerminated) then begin
      task := tedMonitoredTasks.ValueOf(Pint64(@msg.WParam)^) as IOmniTaskControl;
      if assigned(task) then begin
        while task.SharedInfo.CommChannel.Endpoint1.Receive(tedCurrentMsg) do
          if Assigned(tedOnTaskMessage) then
            tedOnTaskMessage(task, tedCurrentMsg);
        while task.SharedInfo.CommChannel.Endpoint2.Receive(tedCurrentMsg) do
          if Assigned(tedOnTaskUndeliveredMessage) then
            tedOnTaskUndeliveredMessage(task);
        if Assigned(tedOnTaskTerminated) then
          OnTaskTerminated(task);
        AllMonitoredTasks.Remove(pointer(task.SharedInfo));
      end;
    end;
    Detach(task);
    msg.Result := 0;
  end
  else if msg.Msg = COmniPoolMsg then begin
    tpMonitorInfo := TOmniThreadPoolMonitorInfo(msg.LParam);
    try
      pool := tedMonitoredPools.ValueOf(tpMonitorInfo.UniqueID) as IOmniThreadPool;
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
