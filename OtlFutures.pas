/// <summary>Futures implementation. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010, Primoz Gabrijelcic
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
///
///   Creation date     : 2008-06-17
///   Last modification : 2010-07-01
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.0: 2010-06-17
///       - Initial implementation.
/// </para></remarks>

unit OtlFutures;

{$I OTLOptions.inc}

interface

uses
  Windows,
  SysUtils,
  OtlTask,
  OtlTaskControl;

type
  TOmniFutureDelegate<T> = reference to function: T;
  TOmniFutureDelegateEx<T> = reference to function(const task: IOmniTask): T;

  IOmniFuture<T> = interface
    procedure Cancel;
    function  IsCancelled: boolean;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { IOmniFuture<T> }

  { TODO : maybe add Value(timeout_ms) }
  TOmniFuture<T> = class(TInterfacedObject, IOmniFuture<T>)
  strict private
    ofCancellable: boolean;
    ofCancelled  : boolean;
    ofResult     : T;
    ofTask       : IOmniTaskControl;
  strict protected
    procedure DestroyTask;
    procedure Execute(action: TOmniTaskDelegate);
  public
    constructor Create(action: TOmniFutureDelegate<T>); // sadly, those two Creates cannot be overloaded as this crashes the compiler (internal error T888)
    constructor CreateEx(action: TOmniFutureDelegateEx<T>);
    destructor  Destroy; override;
    procedure Cancel;
    function  IsCancelled: boolean; inline;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { TOmniFuture<T> }

  EFutureError = class(Exception);
  EFutureCancelled = class(Exception);

implementation

{ TOmniFuture<T> }

constructor TOmniFuture<T>.Create(action: TOmniFutureDelegate<T>);
begin
  inherited Create;
  ofCancellable := false;
  Execute(procedure (const task: IOmniTask)
    begin
      ofResult := action();
    end);
end; { TOmniFuture<T>.Create }

constructor TOmniFuture<T>.CreateEx(action: TOmniFutureDelegateEx<T>);
begin
  inherited Create;
  ofCancellable := true;
  Execute(procedure (const task: IOmniTask)
    begin
      ofResult := action(task);
    end);
end; { TOmniFuture<T>.CreateEx }

destructor TOmniFuture<T>.Destroy;
begin
  DestroyTask;
  inherited;
end; { TOmniFuture<T>.Destroy }

procedure TOmniFuture<T>.Cancel;
begin
  if not ofCancellable then
    raise EFutureError.Create('Action cannot be cancelled');
  if not IsCancelled then begin
    ofCancelled := true;
    if assigned(ofTask) then
      ofTask.CancellationToken.Signal;
  end;
end; { TOmniFuture<T>.Cancel }

procedure TOmniFuture<T>.DestroyTask;
begin
  if assigned(ofTask) then begin
    ofTask.Terminate;
    ofTask := nil;
  end;
end; { TOmniFuture<T>.DestroyTask }

procedure TOmniFuture<T>.Execute(action: TOmniTaskDelegate);
begin
  ofTask := CreateTask(action, 'TOmniFuture action').Schedule;
end; { TOmniFuture<T>.Execute }

function TOmniFuture<T>.IsCancelled: boolean;
begin
  Result := ofCancelled;
end; { TOmniFuture<T>.IsCancelled }

function TOmniFuture<T>.IsDone: boolean;
begin
  Result := (not assigned(ofTask)) or ofTask.WaitFor(0);
end; { TOmniFuture<T>.IsDone }

function TOmniFuture<T>.TryValue(timeout_ms: cardinal; var value: T): boolean;
begin
  Result := false;
  if ofCancelled then
    raise EFutureCancelled.Create('Action was cancelled');
  if assigned(ofTask) then begin
    if not ofTask.WaitFor(timeout_ms) then
      Exit;
    DestroyTask;
  end;
  value := ofResult;
  Result := true;
end; { TOmniFuture<T>.TryValue }

function TOmniFuture<T>.Value: T;
begin
  TryValue(INFINITE, Result);
end; { TOmniFuture<T>.Value }

end.
