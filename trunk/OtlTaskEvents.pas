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
///   Last modification : 2008-06-30
///   Version           : 0.1
///</para><para>
///   History:
///</para></remarks>

unit OtlTaskEvents;

interface

{$R OtlTaskEvents.dcr}

uses
  Messages,
  Classes,
  OtlTask;

type
  TOmniTaskEvent = procedure(task: IOmniTaskControl) of object;

// TODO 1 -oPrimoz Gabrijelcic : tedMonitoredTasks list will be too slow, replace it with a sorted list of (task.UniqueID, task) pairs

  TOmniTaskEventDispatch = class(TComponent)
  strict private
    tedMessageWindow   : THandle;
    tedMonitoredTasks  : TInterfaceList;
    tedOnTaskTerminated: TOmniTaskEvent;
    tedOnTaskMessage   : TOmniTaskEvent;
  strict protected
    function  LocateTask(taskUniqueID: cardinal): IOmniTaskControl;
    procedure WndProc(var msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Detach(task: IOmniTaskControl): IOmniTaskControl;
    function  Monitor(task: IOmniTaskControl): IOmniTaskControl;
  published
    property OnTaskTerminated: TOmniTaskEvent read tedOnTaskTerminated write
      tedOnTaskTerminated;
    property OnTaskMessage: TOmniTaskEvent read tedOnTaskMessage write tedOnTaskMessage;
  end; { TOmniTaskEventDispatch }

var
  COmniTaskMsg_NewMessage: cardinal;
  COmniTaskMsg_Terminated: cardinal;

implementation

uses
  Windows,
  SysUtils,
  DSiWin32;

{ TOmniTaskEventDispatch }

constructor TOmniTaskEventDispatch.Create(AOwner: TComponent);
begin
  inherited;
  tedMessageWindow := DSiAllocateHWnd(WndProc);
  Win32Check(tedMessageWindow <> 0);
  tedMonitoredTasks := TInterfaceList.Create;
end; { TOmniTaskEventDispatch.Create }

destructor TOmniTaskEventDispatch.Destroy;
begin
  while tedMonitoredTasks.Count > 0 do
    Detach(tedMonitoredTasks[tedMonitoredTasks.Count - 1] as IOmniTaskControl);
  FreeAndNil(tedMonitoredTasks);
  if tedMessageWindow <> 0 then begin
    DSiDeallocateHWnd(tedMessageWindow);
    tedMessageWindow := 0;                                
  end;
  inherited;
end; { TOmniTaskEventDispatch.Destroy }

function TOmniTaskEventDispatch.Detach(task: IOmniTaskControl): IOmniTaskControl;
begin
  Result := task.RemoveMonitor;
  tedMonitoredTasks.Remove(task);
end; { TOmniTaskEventDispatch.Detach }

function TOmniTaskEventDispatch.LocateTask(taskUniqueID: cardinal): IOmniTaskControl;
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
end; { TOmniTaskEventDispatch.LocateTask }

function TOmniTaskEventDispatch.Monitor(task: IOmniTaskControl): IOmniTaskControl;
begin
  tedMonitoredTasks.Add(task);
  Result := task.SetMonitor(tedMessageWindow);
end; { TOmniTaskEventDispatch.Monitor }

procedure TOmniTaskEventDispatch.WndProc(var msg: TMessage);
var
  task: IOmniTaskControl;
begin
  if msg.Msg = COmniTaskMsg_NewMessage then begin
    if assigned(OnTaskMessage) then begin
      task := LocateTask(cardinal(msg.WParam));
      if assigned(task) then
        OnTaskMessage(task);
    end;
    msg.Result := 0;
  end
  else if msg.Msg = COmniTaskMsg_Terminated then begin
    if assigned(OnTaskTerminated) then begin
      task := LocateTask(cardinal(msg.WParam));
      if assigned(task) then
        OnTaskTerminated(task);
    end;
    msg.Result := 0;
  end
  else
    msg.Result := DefWindowProc(tedMessageWindow, msg.Msg, msg.WParam, msg.LParam);
end; { TOmniTaskEventDispatch.WndProc }

initialization
  COmniTaskMsg_NewMessage := RegisterWindowMessage('Gp/OtlTaskEvents/NewMessage');
  Win32Check(COmniTaskMsg_NewMessage <> 0);
  COmniTaskMsg_Terminated := RegisterWindowMessage('Gp/OtlTaskEvents/Terminated');
  Win32Check(COmniTaskMsg_Terminated <> 0);
end.
