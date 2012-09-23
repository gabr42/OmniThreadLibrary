///<summary>Observer pattern interface for the containers unit.
///    Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2009 Primoz Gabrijelcic
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
///   Creation date     : 2009-02-19
///   Last modification : 2010-07-01
///   Version           : 1.04
///</para><para>
///   History:
///     1.04: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.03: 2009-12-22
///       - TOmniContainerSubject moved here from OtlContainers because it will also be
///         used in OtlCollections.
///     1.02: 2009-11-15
///       - Windows message observer exposes some of its internals.
///     1.01: 2009-04-06
///       - External event can be provided in the TOmniContainerWindowsEventObserverImpl
///         constructor.
///       - Event is created in TOmniContainerWindowsEventObserverImpl constructor if external
///         event is not provided.
///     1.0: 2009-03-30
///       - First official release.
///</para></remarks>

unit OtlContainerObserver;

{$I OtlOptions.inc}
{$WARN SYMBOL_PLATFORM OFF} // Win32Check

interface

uses
  Classes,
  OtlSync,
  GpStuff;

type
  ///<summary>All possible actions observer can take interest in.</summary>
  TOmniContainerObserverInterest = (
    //Interests with permanent subscription:
    coiNotifyOnAllInserts, coiNotifyOnAllRemoves,
    //Interests with one-shot subscription:
    coiNotifyOnPartlyEmpty, coiNotifyOnAlmostFull
  );

  ///<summary>Container observer. Class based for performance.</summary>
  TOmniContainerObserver = class
  strict private
    coIsActivated: TGp4AlignedInt;
  public
    constructor Create;
    procedure Activate; inline;
    function  CanNotify: boolean; inline;
    procedure Deactivate; inline;
    procedure Notify; virtual; abstract;
  end; { TOmniContainerObserver }

  TOmniContainerWindowsEventObserver = class(TOmniContainerObserver)
  public
    function  GetEvent: THandle; virtual; abstract;
  end; { TOmniContainerWindowsEventObserver }

  TOmniContainerWindowsMessageObserver = class(TOmniContainerObserver)
  strict protected
    function  GetHandle: THandle; virtual; abstract;
  public
    procedure Send(aMessage: cardinal; wParam, lParam: integer); virtual; abstract;
    property Handle: THandle read GetHandle;
  end; { TOmniContainerWindowsMessageObserver }

  TOmniContainerSubject = class
  strict private
    csListLocks    : array [TOmniContainerObserverInterest] of TOmniMREW;
    csObserverLists: array [TOmniContainerObserverInterest] of TList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Attach(const observer: TOmniContainerObserver;
      interest: TOmniContainerObserverInterest);
    procedure Detach(const observer: TOmniContainerObserver;
      interest: TOmniContainerObserverInterest);
    procedure Notify(interest: TOmniContainerObserverInterest);
    procedure NotifyOnce(interest: TOmniContainerObserverInterest);
    procedure Rearm(interest: TOmniContainerObserverInterest);
  end; { TOmniContainerSubject }

  function CreateContainerWindowsEventObserver(externalEvent: THandle = 0):
    TOmniContainerWindowsEventObserver;
  function CreateContainerWindowsMessageObserver(hWindow: THandle; msg: cardinal;
    wParam, lParam: integer): TOmniContainerWindowsMessageObserver;

implementation

uses
  Windows,
  SysUtils,
  {$IFDEF OTL_HasSystemTypes}
  System.Types,
  {$ENDIF}
  DSiWin32,
  OtlCommon;

type
  TOmniContainerWindowsEventObserverImpl = class(TOmniContainerWindowsEventObserver)
  strict private
    cweoEvent          : THandle;
    cweoEventIsExternal: boolean;
  public
    constructor Create(externalEvent: THandle = 0);
    destructor  Destroy; override;
    function  GetEvent: THandle; override;
    procedure Notify; override;
  end; { TOmniContainerWindowsEventObserverImpl }

  TOmniContainerWindowsMessageObserverImpl = class(TOmniContainerWindowsMessageObserver)
  strict private
    cwmoHandle  : THandle;
    cwmoLParam  : integer;
    cwmoMessage : cardinal;
    cwmoWParam  : integer;
  strict protected
    function  GetHandle: THandle; override;
  public
    constructor Create(handle: THandle; aMessage: cardinal; wParam, lParam: integer);
    procedure Send(aMessage: cardinal; wParam, lParam: integer); override;
    procedure Notify; override;
  end; { TOmniContainerWindowsMessageObserver }

{ exports }

function CreateContainerWindowsEventObserver(externalEvent: THandle):
  TOmniContainerWindowsEventObserver;
begin
  Result := TOmniContainerWindowsEventObserverImpl.Create(externalEvent);
end; { CreateContainerWindowsEventObserver }

function CreateContainerWindowsMessageObserver(hWindow: THandle; msg: cardinal; wParam,
  lParam: integer): TOmniContainerWindowsMessageObserver;
begin
  Result := TOmniContainerWindowsMessageObserverImpl.Create(hWindow, msg, wParam, lParam);
end; { CreateContainerWindowsMessageObserver }

{ TOmniContainerObserver }

constructor TOmniContainerObserver.Create;
begin
  inherited;
  Activate;
end; { TOmniContainerObserver.Create }

procedure TOmniContainerObserver.Activate;
begin
  coIsActivated.Value := 1;
end; { TOmniContainerObserver.Activate }

function TOmniContainerObserver.CanNotify: boolean;
begin
  Result := (InterlockedCompareExchange(PInteger(coIsActivated.Addr)^, 0, 1) = 1);
end; { TOmniContainerObserver.CanNotify }

procedure TOmniContainerObserver.Deactivate;
begin
  coIsActivated.Value := 0;
end; { TOmniContainerObserver.Deactivate }

{ TOmniContainerWindowsEventObserverImpl }

constructor TOmniContainerWindowsEventObserverImpl.Create(externalEvent: THandle);
begin
  if externalEvent <> 0 then begin
    cweoEvent := externalEvent;
    cweoEventIsExternal := true;                               
  end
  else begin
    cweoEvent := Windows.CreateEvent(nil, false, false, nil);
    cweoEventIsExternal := false;
  end;
end; { TOmniContainerWindowsEventObserverImpl.Create }

destructor TOmniContainerWindowsEventObserverImpl.Destroy;
begin
  if not cweoEventIsExternal then
    DSiCloseHandleAndNull(cweoEvent);
  cweoEvent := 0;
  inherited;
end; { TOmniContainerWindowsEventObserverImpl.Destroy }

function TOmniContainerWindowsEventObserverImpl.GetEvent: THandle;
begin
  Result := cweoEvent;
end; { TOmniContainerWindowsEventObserverImpl.GetEvent }

procedure TOmniContainerWindowsEventObserverImpl.Notify;
begin
  Win32Check(SetEvent(GetEvent));
end; { TOmniContainerWindowsEventObserverImpl.Notify }

{ TOmniContainerWindowsMessageObserver }

constructor TOmniContainerWindowsMessageObserverImpl.Create(handle: THandle; aMessage:
  cardinal; wParam, lParam: integer);
begin
  inherited Create;
  cwmoHandle := handle;
  cwmoMessage := aMessage;
  cwmoWParam := wParam;
  cwmoLParam := lParam;
end; { TOmniContainerWindowsMessageObserver.Create }

function TOmniContainerWindowsMessageObserverImpl.GetHandle: THandle;
begin
  Result := cwmoHandle;
end; { TOmniContainerWindowsMessageObserverImpl.GetHandle }

procedure TOmniContainerWindowsMessageObserverImpl.Notify;
begin
  Win32Check(PostMessage(cwmoHandle, cwmoMessage, cwmoWParam, cwmoLParam));
end; { TOmniContainerWindowsMessageObserver.Notify }

procedure TOmniContainerWindowsMessageObserverImpl.Send(aMessage: cardinal;
  wParam, lParam: integer);
begin
  Win32Check(PostMessage(cwmoHandle, aMessage, wParam, lParam));
end; { TOmniContainerWindowsMessageObserverImpl.Send }

{ TOmniContainerSubject }

constructor TOmniContainerSubject.Create;
var
  interest: TOmniContainerObserverInterest;
begin
  inherited Create;
  for interest := Low(TOmniContainerObserverInterest) to High(TOmniContainerObserverInterest) do
    csObserverLists[interest] := TList.Create;
end; { TOmniContainerSubject.Create }

destructor TOmniContainerSubject.Destroy;
var
  interest: TOmniContainerObserverInterest;
begin
  for interest := Low(TOmniContainerObserverInterest) to High(TOmniContainerObserverInterest) do begin
    csObserverLists[interest].Free;
    csObserverLists[interest] := nil;
  end;
  inherited;
end; { TOmni ContainerSubject.Destroy }

procedure TOmniContainerSubject.Attach(const observer: TOmniContainerObserver;
  interest: TOmniContainerObserverInterest);
begin
  csListLocks[interest].EnterWriteLock;
  try
    if csObserverLists[interest].IndexOf(observer) < 0 then
      csObserverLists[interest].Add(observer);
  finally csListLocks[interest].ExitWriteLock; end;
end; { TOmniContainerSubject.Attach }

procedure TOmniContainerSubject.Detach(const observer: TOmniContainerObserver;
  interest: TOmniContainerObserverInterest);
begin
  csListLocks[interest].EnterWriteLock;
  try
    csObserverLists[interest].Remove(observer);
  finally csListLocks[interest].ExitWriteLock; end;
end; { TOmniContainerSubject.Detach }

procedure TOmniContainerSubject.Notify(interest: TOmniContainerObserverInterest);
var
  iObserver: integer;
  list     : TList;
begin
  {$R-}
  csListLocks[interest].EnterReadLock;
  try
    list := csObserverLists[interest];
    for iObserver := 0 to list.Count - 1 do begin
      TOmniContainerObserver(list[iObserver]).Notify;
    end;
  finally csListLocks[interest].ExitReadLock; end;
  {$R+}
end; { TOmniContainerSubject.Notify }

procedure TOmniContainerSubject.NotifyOnce(interest: TOmniContainerObserverInterest);
var
  iObserver: integer;
  list     : TList;
  observer : TOmniContainerObserver;
begin
  {$R-}
  csListLocks[interest].EnterReadLock;
  try
    list := csObserverLists[interest];
    for iObserver := 0 to list.Count - 1 do begin
      observer := TOmniContainerObserver(list[iObserver]);
      if observer.CanNotify then begin 
        observer.Notify;
        observer.Deactivate;
      end;
    end;
  finally csListLocks[interest].ExitReadLock; end;
  {$R+}
end; { TOmniContainerSubject.NotifyAndRemove }

procedure TOmniContainerSubject.Rearm(interest: TOmniContainerObserverInterest);
var
  iObserver: integer;
  list     : TList;
begin
  {$R-}
  csListLocks[interest].EnterReadLock;
  try
    list := csObserverLists[interest];
    for iObserver := 0 to list.Count - 1 do
      TOmniContainerObserver(list[iObserver]).Activate;
  finally csListLocks[interest].ExitReadLock; end;
  {$R+}
end; { TOmniContainerSubject.Rearm }

end.

