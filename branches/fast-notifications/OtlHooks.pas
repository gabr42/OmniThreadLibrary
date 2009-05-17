///<summary>OmniThreadLibrary hooks.</summary>
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
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2009-05-17
///   Last modification : 2009-05-17
///   Version           : 1.0
///</para><para>
///   History:
///</para></remarks>

unit OtlHooks;

interface

type
  TThreadNotificationType = (tntCreate, tntDestroy);
  TThreadNotificationProc = procedure(notifyType: TThreadNotificationType;
                              const threadName: string);
  TThreadNotificationMeth = procedure(notifyType: TThreadNotificationType;
                              const threadName: string) of object;

procedure RegisterThreadNotification(notifyProc: TThreadNotificationProc); overload;
procedure RegisterThreadNotification(notifyMethod: TThreadNotificationMeth); overload;
procedure UnregisterThreadNotification(notifyProc: TThreadNotificationProc); overload;
procedure UnregisterThreadNotification(notifyMethod: TThreadNotificationMeth); overload;

procedure SendThreadNotifications(notifyType: TThreadNotificationType;
  const threadName: string);

implementation

uses
  Windows,
  SysUtils,
  Classes,
  OtlSync;

type
  TThreadNotifications = class
  strict private
    tnList: TList;
    tnLock: TOmniMREW;
  strict protected
    function  Find(data, code: pointer): integer;
    procedure Register(data, code: pointer); overload;
    procedure Unregister(data, code: pointer); overload;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Notify(notifyType: TThreadNotificationType; const threadName: string);
    procedure Register(notifyProc: TThreadNotificationProc); overload;
    procedure Register(notifyMethod: TThreadNotificationMeth); overload;
    procedure Unregister(notifyProc: TThreadNotificationProc); overload;
    procedure Unregister(notifyMethod: TThreadNotificationMeth); overload;
  end; { TThreadNotificationProc }

var
  GThreadNotifications: TThreadNotifications;

procedure RegisterThreadNotification(notifyProc: TThreadNotificationProc); overload;
begin
  GThreadNotifications.Register(notifyProc);
end; { RegisterThreadNotification }

procedure RegisterThreadNotification(notifyMethod: TThreadNotificationMeth); overload;
begin
  GThreadNotifications.Register(notifyMethod);
end; { RegisterThreadNotification }

procedure UnregisterThreadNotification(notifyProc: TThreadNotificationProc); overload;
begin
  GThreadNotifications.Unregister(notifyProc);
end; { UnregisterThreadNotification }

procedure UnregisterThreadNotification(notifyMethod: TThreadNotificationMeth); overload;
begin
  GThreadNotifications.Unregister(notifyMethod);
end; { UnregisterThreadNotification }

procedure SendThreadNotifications(notifyType: TThreadNotificationType;
  const threadName: string);
begin
  GThreadNotifications.Notify(notifyType, threadName);
end; { SendThreadNotifications }

constructor TThreadNotifications.Create;
begin
  inherited Create;
  tnList := TList.Create;
end; { TThreadNotifications.Create }

destructor TThreadNotifications.Destroy;
begin
  FreeAndNil(tnList);
  inherited Destroy;
end; { TThreadNotifications.Destroy }

function TThreadNotifications.Find(data, code: pointer): integer;
begin
  Result := 0;
  while Result < tnList.Count do begin
    if (tnList[Result] = data) and (tnList[Result+1] = code) then
      Exit;
    Inc(Result, 2);
  end;
  Result := -1;
end; { TThreadNotifications.Find }

procedure TThreadNotifications.Notify(notifyType: TThreadNotificationType;
  const threadName: string);
var
  iObserver: integer;
  meth     : TMethod;
begin
  tnLock.EnterReadLock;
  try
    iObserver := 0;
    while iObserver < tnList.Count do begin
      if tnList[iObserver] = nil then
        TThreadNotificationProc(tnList[iObserver+1])(notifyType, threadName)
      else begin
        meth.Data := tnList[iObserver];
        meth.Code := tnList[iObserver+1];
        TThreadNotificationMeth(meth)(notifyType, threadName);
      end;
      Inc(iObserver, 2);
    end;
  finally tnLock.ExitReadLock; end;
end; { TThreadNotifications.Notify }

procedure TThreadNotifications.Register(notifyProc: TThreadNotificationProc);
begin
  Register(nil, pointer(@notifyProc));
end; { TThreadNotifications.Register }

procedure TThreadNotifications.Register(notifyMethod: TThreadNotificationMeth);
begin
  Register(TMethod(notifyMethod).Data, TMethod(notifyMethod).Code);
end; { TThreadNotifications.Register }

procedure TThreadNotifications.Register(data, code: pointer);
begin
  tnLock.EnterWriteLock;
  try
    tnList.Add(data);
    tnList.Add(code);
  finally tnLock.ExitWriteLock; end;
end; { TThreadNotifications.Register }

procedure TThreadNotifications.Unregister(notifyProc: TThreadNotificationProc);
begin
  Unregister(nil, pointer(@notifyProc));
end; { TThreadNotifications.Unregister }

procedure TThreadNotifications.Unregister(notifyMethod: TThreadNotificationMeth);
begin
  Unregister(TMethod(notifyMethod).Data, TMethod(notifyMethod).Code);
end; { TThreadNotifications.Unregister }

procedure TThreadNotifications.Unregister(data, code: pointer);
var
  idxObserver: integer;
begin
  tnLock.EnterWriteLock;
  try
    idxObserver := Find(data, code);
    if idxObserver >= 0 then begin
      tnList.Delete(idxObserver);
      tnList.Delete(idxObserver);
    end;
  finally tnLock.ExitWriteLock; end;
end; { TThreadNotifications.Unregister }

initialization
  GThreadNotifications := TThreadNotifications.Create;
finalization
  FreeAndNil(GThreadNotifications);
end.
