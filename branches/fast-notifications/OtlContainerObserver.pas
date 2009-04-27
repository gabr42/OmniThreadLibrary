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
///   Last modification : 2009-04-06
///   Version           : 1.0
///</para><para>
///   History:
///     1.01: 2009-04-06
///       - External event can be provided in the TOmniContainerWindowsEventObserverImpl
///         constructor.
///       - Event is created in TOmniContainerWindowsEventObserverImpl constructor if external
///         event is not provided.
///     1.0: 2009-03-30
///       - First official release.
///</para></remarks>

unit OtlContainerObserver;

interface

uses
  Classes,
  SyncObjs,
  GpStuff;

type
  ///<summary>All possible actions observer can take interest in.</summary>
  TOmniContainerObserverInterest = (
    //Interests with permanent subscription:
    coiNotifyOnAllInserts, coiNotifyOnAllRemoves, coiNotifyOnLastRemove,
    //Interests with one-shot subscription:
    coiNotifyOnFirstInsert, coiNotifyOnPartlyEmpty, coiNotifyOnAlmostFull
  );

  ///<summary>Container observer. Class based for performance.</summary>
  TOmniContainerObserver = class 
  public
    procedure Notify; virtual; abstract;
  end; { TOmniContainerObserver }

  TOmniContainerWindowsEventObserver = class(TOmniContainerObserver)
  public
    function  GetEvent: THandle; virtual; abstract;
  end; { TOmniContainerWindowsEventObserver }

  ///<summary>Container as a subject.</summary>
  IOmniContainerSubject = interface ['{F66DD79E-230A-4246-B740-C0A7665549EC}']
    procedure Attach(const observer: TOmniContainerObserver;
      interest: TOmniContainerObserverInterest);
    procedure Detach(const observer: TOmniContainerObserver);
    procedure Notify(interest: TOmniContainerObserverInterest);
    procedure NotifyOnce(interest: TOmniContainerObserverInterest);
  end; { IOmniContainerSubject }

  ///<summary>Observer sublist enumerator.</summary>
  IOmniContainerObserverEnum = interface ['{88F35ADE-16F7-427B-80E3-4686007F7B42}']
    function  GetCurrent: TOmniContainerObserver;
    function  MoveNext: boolean;
    property Current: TOmniContainerObserver read GetCurrent;
  end; { IOmniContainerObserverEnum }

  ///<summary>Observer sublist enumerator factory.</summary>
  TOmniContainerObserverEnumFactory = record
  strict private
    coefObserverList_ref: TList;
    coefSyncObj_ref     : TSynchroObject;
  public
    constructor Create(observerList: TList; syncObj: TSynchroObject);
    function  GetEnumerator: IOmniContainerObserverEnum;
  end; { TOmniContainerObserverEnumFactory }

  ///<summary>List of observers and their interests.</summary>
  IOmniContainerObserverList = interface ['{7FFE4895-C0A5-4AC2-9048-6D125BC64A39}']
    procedure Add(const observer: TOmniContainerObserver;
      interest: TOmniContainerObserverInterest);
    function  Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
    procedure Remove(const observer: TOmniContainerObserver);
  end; { IOmniContainerObserverList }

  function CreateContainerObserverList: IOmniContainerObserverList;
  function CreateContainerWindowsEventObserver(externalEvent: THandle = 0):
    TOmniContainerWindowsEventObserver;
  function CreateContainerWindowsMessageObserver(hWindow: THandle; msg: cardinal;
    wParam, lParam: integer): TOmniContainerObserver;

implementation

uses
  Windows,
  SysUtils,
  DSiWin32,
  OtlCommon,
  OtlSync;

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

  TOmniContainerWindowsMessageObserver = class(TOmniContainerObserver)
  strict private
    cwmoHandle  : THandle;
    cwmoLParam  : integer;
    cwmoMessage : cardinal;
    cwmoWParam  : integer;
  public
    constructor Create(handle: THandle; aMessage: cardinal; wParam, lParam: integer);
    procedure Notify; override;
  end; { TOmniContainerWindowsMessageObserver }

  ///<summary>Observer sublist enumerator.</summary>
  TOmniContainerObserverEnum = class(TInterfacedObject, IOmniContainerObserverEnum)
  strict private
    coeObserverEnum: TListEnumerator;
    coeSyncObj_ref : TSynchroObject;
  public
    constructor Create(observerList: TList; syncObj: TSynchroObject);
    destructor  Destroy; override;
    function  GetCurrent: TOmniContainerObserver; inline;
    function  MoveNext: boolean; inline;
    property Current: TOmniContainerObserver read GetCurrent;
  end; { TOmniContainerObserverEnum }

  ///<summary>List of observers and their interests. Implements separate lists for all
  ///    container actions for faster enumeration.</summary>
  TOmniContainerObserverList = class(TInterfacedObject, IOmniContainerObserverList)
  strict private
    colInterestLists: array [TOmniContainerObserverInterest] of TList;
    colLock         : TOmniCS; // TODO 3 -oPrimoz Gabrijelcic : Should be SWMR
    colObserverList : TList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const observer: TOmniContainerObserver; interest:
      TOmniContainerObserverInterest);
    function  Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
    procedure Remove(const observer: TOmniContainerObserver);
  end; { TOmniContainerObserverList }

{ exports }

function CreateContainerObserverList: IOmniContainerObserverList;
begin
  Result := TOmniContainerObserverList.Create;
end; { CreateContainerObserverList }

function CreateContainerWindowsEventObserver(externalEvent: THandle):
  TOmniContainerWindowsEventObserver;
begin
  Result := TOmniContainerWindowsEventObserverImpl.Create(externalEvent);
end; { CreateContainerWindowsEventObserver }

function CreateContainerWindowsMessageObserver(hWindow: THandle; msg: cardinal; wParam,
  lParam: integer): TOmniContainerObserver;
begin
  Result := TOmniContainerWindowsMessageObserver.Create(hWindow, msg, wParam, lParam);
end; { CreateContainerWindowsMessageObserver }

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

constructor TOmniContainerWindowsMessageObserver.Create(handle: THandle; aMessage:
  cardinal; wParam, lParam: integer);
begin
  inherited Create;
  cwmoHandle := handle;
  cwmoMessage := aMessage;
  cwmoWParam := wParam;
  cwmoLParam := lParam;
end; { TOmniContainerWindowsMessageObserver.Create }

procedure TOmniContainerWindowsMessageObserver.Notify;
begin
  Win32Check(PostMessage(cwmoHandle, cwmoMessage, cwmoWParam, cwmoLParam));
end; { TOmniContainerWindowsMessageObserver.Notify }

{ TOmniContainerObserverEnum }

constructor TOmniContainerObserverEnum.Create(observerList: TList; syncObj:
  TSynchroObject);
begin
  coeObserverEnum := observerList.GetEnumerator;
  coeSyncObj_ref := syncObj;
end; { TOmniContainerObserverEnum.Create }

destructor TOmniContainerObserverEnum.Destroy;
begin
  coeSyncObj_ref.Release;
  FreeAndNil(coeObserverEnum);
  inherited;
end; { TOmniContainerObserverEnum.Destroy }

function TOmniContainerObserverEnum.GetCurrent: TOmniContainerObserver;
begin
  Result := coeObserverEnum.Current;
end; { TOmniContainerObserverEnum.GetCurrent }

function TOmniContainerObserverEnum.MoveNext: boolean;
begin
  Result := coeObserverEnum.MoveNext;
end; { TOmniContainerObserverEnum.MoveNext }

{ TOmniContainerObserverEnumFactory }

constructor TOmniContainerObserverEnumFactory.Create(observerList: TList; syncObj:
  TSynchroObject);
begin
  coefObserverList_ref := observerList;
  coefSyncObj_ref := syncObj;
end; { TOmniContainerObserverEnumFactory.Create }

function TOmniContainerObserverEnumFactory.GetEnumerator: IOmniContainerObserverEnum;
begin
  Result := TOmniContainerObserverEnum.Create(coefObserverList_ref, coefSyncObj_ref);
end; { TOmniContainerObserverEnumFactory.GetEnumerator }

{ TOmniContainerObserverList }

constructor TOmniContainerObserverList.Create;
var
  interest: TOmniContainerObserverInterest;
begin
  inherited Create;
  colObserverList := TList.Create;
  for interest := Low(interest) to High(interest) do
    colInterestLists[interest] := TList.Create;
end; { TOmniContainerObserverList.Create }

destructor TOmniContainerObserverList.Destroy;
var
  interest: TOmniContainerObserverInterest;
begin
  for interest := Low(interest) to High(interest) do begin
    colInterestLists[interest].Free;
    colInterestLists[interest] := nil;
  end;
  FreeAndNil(colObserverList);
  inherited Destroy;
end; { TOmniContainerObserverList.Destroy }

procedure TOmniContainerObserverList.Add(const observer: TOmniContainerObserver;
  interest: TOmniContainerObserverInterest);
begin
  Remove(observer);
  colLock.Acquire;
  try
    colObserverList.Add(observer);
    colInterestLists[interest].Add(pointer(observer));
  finally colLock.Release; end;
end; { TOmniContainerObserverList.Add }

function TOmniContainerObserverList.Enumerate(interest: TOmniContainerObserverInterest):
  TOmniContainerObserverEnumFactory;
begin
  colLock.Acquire; //will be released when the enumerator is destroyed
  Result := TOmniContainerObserverEnumFactory.Create(colInterestLists[interest], colLock.SyncObj);
end; { TOmniContainerObserverList.Enumerate }

procedure TOmniContainerObserverList.Remove(const observer: TOmniContainerObserver);
var
  idxObserver: integer;
  interest   : TOmniContainerObserverInterest;
begin
  if not assigned(observer) then
    Exit;
  colLock.Acquire;
  try
    idxObserver := colObserverList.IndexOf(observer);
    if idxObserver < 0 then
      Exit;
    colObserverList.Delete(idxObserver);
    for interest := Low(interest) to High(interest) do
      colInterestLists[interest].Remove(pointer(observer));
  finally colLock.Release; end;
end; { TOmniContainerObserverList.Remove }

end.

