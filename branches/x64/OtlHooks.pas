///<summary>OmniThreadLibrary hooks.</summary>
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
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2009-05-17
///   Last modification : 2011-07-14
///   Version           : 1.02
///</para><para>
///   History:
///     1.02: 2011-07-14
///       - Support for non-silent exceptions removed.
///       - Changed signature for exception filters.
///     1.01: 2010-07-01
///       - Includes OTLOptions.inc.
///</para></remarks>

unit OtlHooks;

{$I OtlOptions.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  OtlSync;

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

type
  TExceptionFilterProc = procedure(var e: Exception; var continueProcessing: boolean);
  TExceptionFilterMeth = procedure(var e: Exception; var continueProcessing: boolean) of object;

procedure RegisterExceptionFilter(filterProc: TExceptionFilterProc); overload;
procedure RegisterExceptionFilter(filterMethod: TExceptionFilterMeth); overload;
procedure UnregisterExceptionFilter(filterProc: TExceptionFilterProc); overload;
procedure UnregisterExceptionFilter(filterMethod: TExceptionFilterMeth); overload;

procedure FilterException(var e: Exception);

implementation

type
  TProcMethodList = class(TList)
  strict private
    pmlLock: TOmniMREW;
  strict protected
    procedure Add(data, code: pointer); reintroduce; overload;
    function  Find(data, code: pointer): integer;
    procedure Remove(data, code: pointer); overload;
  public
    procedure Add(method: TMethod); reintroduce; overload;
    procedure Add(proc: pointer); reintroduce; overload;
    procedure EnterReadLock; inline;
    procedure ExitReadLock; inline;
    procedure Remove(method: TMethod); overload;
    procedure Remove(proc: pointer); overload;
  end; { TProcMethodList }

  TThreadNotifications = class
  strict private
    tnList: TProcMethodList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Notify(notifyType: TThreadNotificationType; const threadName: string);
    procedure Register(notifyProc: TThreadNotificationProc); overload;
    procedure Register(notifyMethod: TThreadNotificationMeth); overload;
    procedure Unregister(notifyProc: TThreadNotificationProc); overload;
    procedure Unregister(notifyMethod: TThreadNotificationMeth); overload;
  end; { TThreadNotificationProc }

  TExceptionFilters = class
  strict private
    efList: TProcMethodList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Register(filterProc: TExceptionFilterProc); overload;
    procedure Register(filterMethod: TExceptionFilterMeth); overload;
    procedure Unregister(filterProc: TExceptionFilterProc); overload;
    procedure Unregister(filterMethod: TExceptionFilterMeth); overload;
    procedure Filter(var e: Exception);
  end; { TExceptionFilters }

var
  GExceptionFilters   : TExceptionFilters;
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

procedure RegisterExceptionFilter(filterProc: TExceptionFilterProc); overload;
begin
  GExceptionFilters.Register(filterProc);
end; { RegisterExceptionFilter }

procedure RegisterExceptionFilter(filterMethod: TExceptionFilterMeth); overload;
begin
  GExceptionFilters.Register(filterMethod);
end; { RegisterExceptionFilter }

procedure UnregisterExceptionFilter(filterProc: TExceptionFilterProc); overload;
begin
  GExceptionFilters.Unregister(filterProc);
end; { UnregisterExceptionFilter }

procedure UnregisterExceptionFilter(filterMethod: TExceptionFilterMeth); overload;
begin
  GExceptionFilters.Unregister(filterMethod);
end; { UnregisterExceptionFilter }

procedure FilterException(var e: Exception);
begin
  GExceptionFilters.Filter(e);
end; { FilterException }

{ TProcMethodList }

procedure TProcMethodList.Add(data, code: pointer);
begin
  pmlLock.EnterWriteLock;
  try
    inherited Add(data);
    inherited Add(code);
  finally pmlLock.ExitWriteLock; end;
end; { TProcMethodList.Add }

procedure TProcMethodList.Add(method: TMethod);
begin
  Add(TMethod(method).Data, TMethod(method).Code);
end; { TProcMethodList.Add }

procedure TProcMethodList.Add(proc: pointer);
begin
  Add(nil, proc);
end; { TProcMethodList.Add }

procedure TProcMethodList.EnterReadLock;
begin
  pmlLock.EnterReadLock;
end; { TProcMethodList.EnterReadLock }

procedure TProcMethodList.ExitReadLock;
begin
  pmlLock.ExitReadLock;
end; { TProcMethodList.ExitReadLock }

function TProcMethodList.Find(data, code: pointer): integer;
begin
  Result := 0;
  while Result < Count do begin
    if (Items[Result] = data) and (Items[Result+1] = code) then
      Exit;
    Inc(Result, 2);
  end;
  Result := -1;
end; { TProcMethodList.Find }

procedure TProcMethodList.Remove(method: TMethod);
begin
  Remove(TMethod(method).Data, TMethod(method).Code);
end; { TProcMethodList.Remove }

procedure TProcMethodList.Remove(proc: pointer);
begin
  Remove(nil, proc);
end; { TProcMethodList.Remove }

procedure TProcMethodList.Remove(data, code: pointer);
var
  idxMeth: integer;
begin
  pmlLock.EnterWriteLock;
  try
    idxMeth := Find(data, code);
    if idxMeth >= 0 then begin
      Delete(idxMeth);
      Delete(idxMeth);
    end;
  finally pmlLock.ExitWriteLock; end;
end; { TProcMethodList.Remove }

{ TThreadNotifications }

constructor TThreadNotifications.Create;
begin
  inherited Create;
  tnList := TProcMethodList.Create;
end; { TThreadNotifications.Create }

destructor TThreadNotifications.Destroy;
begin
  FreeAndNil(tnList);
  inherited Destroy;
end; { TThreadNotifications.Destroy }

procedure TThreadNotifications.Notify(notifyType: TThreadNotificationType;
  const threadName: string);
var
  iObserver: integer;
  meth     : TMethod;
begin
  if not assigned(Self) then
    Exit; //finalization
  tnList.EnterReadLock;
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
  finally tnList.ExitReadLock; end;
end; { TThreadNotifications.Notify }

procedure TThreadNotifications.Register(notifyProc: TThreadNotificationProc);
begin
  tnList.Add(pointer(@notifyProc));
end; { TThreadNotifications.Register }

procedure TThreadNotifications.Register(notifyMethod: TThreadNotificationMeth);
begin
  tnList.Add(TMethod(notifyMethod));
end; { TThreadNotifications.Register }

procedure TThreadNotifications.Unregister(notifyProc: TThreadNotificationProc);
begin
  tnList.Remove(pointer(@notifyProc));
end; { TThreadNotifications.Unregister }

procedure TThreadNotifications.Unregister(notifyMethod: TThreadNotificationMeth);
begin
  tnList.Remove(TMethod(notifyMethod));
end; { TThreadNotifications.Unregister }

{ TExceptionFilters }

constructor TExceptionFilters.Create;
begin
  inherited Create;
  efList := TProcMethodList.Create;
end; { TExceptionFilters.Create }

destructor TExceptionFilters.Destroy;
begin
  FreeAndNil(efList);
  inherited;
end; { TExceptionFilters.Destroy }

procedure TExceptionFilters.Filter(var e: Exception);
var
  continueProcessing: boolean;
  iObserver         : integer;
  meth              : TMethod;
begin
  efList.EnterReadLock;
  try
    iObserver := 0;
    continueProcessing := true;
    while continueProcessing and (iObserver < efList.Count) do begin
      if efList[iObserver] = nil then
        TExceptionFilterProc(efList[iObserver+1])(e, continueProcessing)
      else begin
        meth.Data := efList[iObserver];
        meth.Code := efList[iObserver+1];
        TExceptionFilterMeth(meth)(e, continueProcessing);
      end;
      Inc(iObserver, 2);
    end;
  finally efList.ExitReadLock; end;
end; { TExceptionFilters.Filter }

procedure TExceptionFilters.Register(filterMethod: TExceptionFilterMeth);
begin
  efList.Add(TMethod(filterMethod));
end; { TExceptionFilters.Register }

procedure TExceptionFilters.Register(filterProc: TExceptionFilterProc);
begin
  efList.Add(pointer(@filterProc));
end; { TExceptionFilters.Register }

procedure TExceptionFilters.Unregister(filterMethod: TExceptionFilterMeth);
begin
  efList.Remove(TMethod(filterMethod));
end; { TExceptionFilters.Unregister }

procedure TExceptionFilters.Unregister(filterProc: TExceptionFilterProc);
begin
  efList.Remove(pointer(@filterProc));
end; { TExceptionFilters.Unregister }

initialization
  GExceptionFilters := TExceptionFilters.Create;
  GThreadNotifications := TThreadNotifications.Create;
finalization
  FreeAndNil(GExceptionFilters);
  FreeAndNil(GThreadNotifications);
end.
