///<summary>Simple log collector, used internally for OTL debugging.</summary>
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
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2010-07-08
///   Last modification : 2010-07-21
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2010-07-21
///       - SaveToFile will append original file.
///</para></remarks>

unit OtlLogger;

{$I OtlOptions.inc}

interface

uses
  Classes,
  OtlContainers;

type
  TOmniLogger = class
  strict private
    eventList: TOmniBaseQueue;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    procedure GetEventList(sl: TStringList);
    procedure Log(const msg: string; const params: array of const); overload;
    procedure Log(const msg: string); overload;
    procedure SaveEventList(const fileName: string);
  end; { TOmniLogger }

var
  GLogger: TOmniLogger;

implementation

uses
  Windows,
  SysUtils,
  DSiWin32,
  OtlCommon;

{ TOmniLogger }

constructor TOmniLogger.Create;
begin
  inherited Create;
  eventList := TOmniBaseQueue.Create;
end; { TOmniLogger.Create }

destructor TOmniLogger.Destroy;
begin
  FreeAndNil(eventList);
  inherited;
end; { TOmniLogger.Destroy }

procedure TOmniLogger.Clear;
var
  tmp: TOmniValue;
begin
  while eventList.TryDequeue(tmp) do begin
    tmp := '';
    ;
  end;
end; { TOmniLogger.Clear }

procedure TOmniLogger.GetEventList(sl: TStringList);
var
  tmp: TOmniValue;
begin
  while eventList.TryDequeue(tmp) do
    sl.Add(tmp);
end; { TOmniLogger.GetEventList }

procedure TOmniLogger.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end; { TOmniLogger.Log }

procedure TOmniLogger.Log(const msg: string);
begin
  eventList.Enqueue(Format('[%d] %d %s', [GetCurrentThreadID, DSiTimeGetTime64, msg]));
end; { TOmniLogger.Log }

procedure TOmniLogger.SaveEventList(const fileName: string);
var
  s    : string;
  slNew: TStringList;
  slOld: TStringList;
begin
  slOld := TStringList.Create;
  try
    if FileExists(fileName) then
      slOld.LoadFromFile(fileName);
    slNew := TStringList.Create;
    try
      GetEventList(slNew);
      for s in slNew do
        slOld.Add(s);
      slOld.SaveToFile(fileName);
    finally FreeAndNil(slNew); end;
  finally FreeAndNil(slOld); end;
end;

initialization
  GLogger := TOmniLogger.Create;
finalization
  FreeAndNil(GLogger);
end.
