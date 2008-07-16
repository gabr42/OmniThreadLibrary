///<summary>Stuff common for the OmniThreadLibrary project.</summary>
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
///   Last modification : 2008-07-15
///   Version           : 0.1
///</para><para>
///   History:
///     0.1: 2008-07-15
///       - Moved in TOmniValueContainer from OtlTask. 
///</para></remarks>

unit OtlCommon;

interface

uses
  Classes,
  Variants;

type
  TOmniValue = type Variant; // maybe we should use own record type with implicit overloaded for parameters instead of TOmniValue

  TOmniValueContainer = class
  strict private
    ovcCanModify: boolean;
    ovcNames    : TStringList;
    ovcValues   : array of TOmniValue;
  strict protected
    procedure Clear;
    procedure Grow;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(paramValue: TOmniValue; paramName: string = '');
    procedure Assign(parameters: array of TOmniValue);
    function  IsLocked: boolean; inline;
    procedure Lock; inline;
    function ParamByIdx(paramIdx: integer): TOmniValue;
    function ParamByName(const paramName: string): TOmniValue;
  end; { TOmniValueContainer }

implementation

uses
  SysUtils;

{ TOmniValueContainer }

constructor TOmniValueContainer.Create;
begin
  inherited Create;
  ovcNames := TStringList.Create;
  ovcCanModify := true;
end; { TOmniValueContainer.Create }

destructor TOmniValueContainer.Destroy;
begin
  FreeAndNil(ovcNames);
  inherited Destroy;
end; { TOmniValueContainer.Destroy }

procedure TOmniValueContainer.Add(paramValue: TOmniValue; paramName: string);
var
  idxParam: integer;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  if paramName = '' then
    paramName := IntToStr(ovcNames.Count);
  idxParam := ovcNames.IndexOf(paramName); 
  if idxParam < 0 then begin
    idxParam := ovcNames.Add(paramName);
    if ovcNames.Count > Length(ovcValues) then
      Grow;
  end;
  ovcValues[idxParam] := paramValue;
end; { TOmniValueContainer.Add }

procedure TOmniValueContainer.Assign(parameters: array of TOmniValue);
var
  value: TOmniValue;
begin
  if not ovcCanModify then
    raise Exception.Create('TOmniValueContainer: Already locked');
  Clear;
  SetLength(ovcValues, Length(parameters));
  for value in parameters do
    Add(value);
end; { TOmniValueContainer.Assign }

procedure TOmniValueContainer.Clear;
begin
  SetLength(ovcValues, 0);
  ovcNames.Clear;
end; { TOmniValueContainer.Clear }

procedure TOmniValueContainer.Grow;
var
  iValue   : integer;
  tmpValues: array of TOmniValue;
begin
  SetLength(tmpValues, Length(ovcValues));
  for iValue := 0 to High(ovcValues) - 1 do
    tmpValues[iValue] := ovcValues[iValue];
  SetLength(ovcValues, 2*Length(ovcValues)+1);
  for iValue := 0 to High(tmpValues) - 1 do
    ovcValues[iValue] := tmpValues[iValue];
end; { TOmniValueContainer.Grow }

function TOmniValueContainer.IsLocked: boolean;
begin
  Result := not ovcCanModify;
end; { TOmniValueContainer.IsLocked }

procedure TOmniValueContainer.Lock;
begin
  ovcCanModify := false;
end; { TOmniValueContainer.Lock }

function TOmniValueContainer.ParamByIdx(paramIdx: integer): TOmniValue;
begin
  Result := ovcValues[paramIdx];
end; { TOmniValueContainer.ParamByIdx }

function TOmniValueContainer.ParamByName(const paramName: string): TOmniValue;
begin
  Result := ovcValues[ovcNames.IndexOf(paramName)];
end; { TOmniValueContainer.ParamByName }

end.
