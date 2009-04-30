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
  OtlSync,
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
  strict private
    coIsActivated: boolean;
  public
    constructor Create;
    procedure Activate; inline;
    procedure Deactivate; inline;
    procedure Notify; virtual; abstract;
    property IsActive: boolean read coIsActivated;
  end; { TOmniContainerObserver }

  TOmniContainerWindowsEventObserver = class(TOmniContainerObserver)
  public
    function  GetEvent: THandle; virtual; abstract;
  end; { TOmniContainerWindowsEventObserver }

  function CreateContainerWindowsEventObserver(externalEvent: THandle = 0):
    TOmniContainerWindowsEventObserver;
  function CreateContainerWindowsMessageObserver(hWindow: THandle; msg: cardinal;
    wParam, lParam: integer): TOmniContainerObserver;

implementation

uses
  Windows,
  SysUtils,
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

{ exports }

//function CreateContainerObserverList: IOmniContainerObserverList;
//begin
//  Result := TOmniContainerObserverList.Create;
//end; { CreateContainerObserverList }

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

{ TOmniContainerObserver }

constructor TOmniContainerObserver.Create;
begin
  inherited;
  coIsActivated := true;
end; { TOmniContainerObserver.Create }

procedure TOmniContainerObserver.Activate;
begin
  coIsActivated := true;
end; { TOmniContainerObserver.Activate }

procedure TOmniContainerObserver.Deactivate;
begin
  coIsActivated := false;
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

end.

