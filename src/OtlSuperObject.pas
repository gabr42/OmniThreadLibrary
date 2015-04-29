///<summary>TOmniValue to SuperObject [superobject.googlecode.com] converters.
///    Part of the OmniThreadLibrary project. Requires Delphi 2009 or newer.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2013 Primoz Gabrijelcic
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
///   Author            : Lee_Nover
///   Creation date     : 2013-06-05
///   Last modification : 2013-06-05
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2013-06-05
///       - Created.

unit OtlSuperObject;

interface

uses
  RTTI,
  SuperObject;

type
  TOtlSuperRttiContext = class(TSuperRttiContext)
  public
    constructor Create; override;
  end;

function OmniValToSO(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;
function SOToOmniVal(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;

implementation

uses
  SysUtils,
  OtlCommon;

function OmniValToSO(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;
var
  ov: TOmniValue;
  v: TValue;
begin
  ov := value.AsType<TOmniValue>();
  // todo: add support for copying arrays
  v := ov;
  Result := ctx.ToJson(v, index);
end;

function SOToOmniVal(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;
var
  ov: TOmniValue;
  stArr: TSuperArray;
//  vArr: TArray<TValue>;
//  arrItem: ISuperObject;
  idxItem: Integer;
  ovc: TOmniValueContainer;
begin
  Result := True;
  ov := nil;
  case obj.DataType of
    stNull: ov := nil;
    stBoolean: ov := obj.AsBoolean;
    stDouble: ov := obj.AsDouble;
    stCurrency: ov := obj.AsCurrency;
    stInt: ov := obj.AsInteger;
    stObject: ov := TObject(obj.DataPtr);
    stArray:
      begin
        stArr := obj.AsArray;
        ov := TOmniValue.Create([]);
        ovc := ov.AsArray;

        for idxItem := 0 to stArr.Length-1 do
          case stArr[idxItem].DataType of
            stNull: ovc.Add(nil);
            stBoolean: ovc.Add(stArr.B[idxItem]);
            stDouble: ovc.Add(stArr.D[idxItem]);
            stCurrency: ovc.Add(stArr.C[idxItem]);
            stInt: ovc.Add(stArr.I[idxItem]);
            stObject: ovc.Add(stArr.O[idxItem]);
//            stArray: vArr[idxItem] := stArr;
            stString: ovc.Add(stArr.S[idxItem]);
          end;

      end;
    stString: ov := obj.AsString;
    stMethod: Result := False;
  end;
  Value := TValue.From<TOmniValue>(ov);
end;

{ TOtlSuperRttiContext }

constructor TOtlSuperRttiContext.Create;
begin
  inherited;
  SerialToJson.AddOrSetValue(TypeInfo(TOmniValue), OmniValToSO);
  SerialFromJson.AddOrSetValue(TypeInfo(TOmniValue), SOToOmniVal);
end;

end.
