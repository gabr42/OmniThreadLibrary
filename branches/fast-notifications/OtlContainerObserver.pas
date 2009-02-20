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
///   Last modification : 2009-02-19
///   Version           : 0.1
///</para><para>
///   History:
///     0.1: 2009-02-19
///       - Created.
///</para></remarks>

unit OtlContainerObserver;

interface

uses
  Classes;

type
  ///<summary>All actions container can report.</summary>
  TOmniContainerAction = (caInsert, caRemove, caAlmostFull, caPartlyEmpty);

  ///<summary>Container observer.</summary>
  IOmniContainerObserver = interface ['{79288268-0B69-45C2-8E2B-50B1C5757172}']
    procedure Notify(action: TOmniContainerAction);
  end; { IOmniContainerObserver }

  ///<summary>All possible actions observer can take interest in.</summary>
  TOmniContainerObserverInterest = (coiNotifyOnFirstInsert, coiNotifyOnAllInserts,
    coiNotifyOnLastRemove, coiNotifyOnAllRemoves, coiNotifyOnAlmostFull,
    coiNotifyOnPartlyEmpty);
  TOmniContainerObserverInterests = set of TOmniContainerObserverInterest;

  ///<summary>Container as a subject.</summary>
  IOmniContainerSubject = interface ['{F66DD79E-230A-4246-B740-C0A7665549EC}']
    procedure Attach(observer: IOmniContainerObserver;
      interests: TOmniContainerObserverInterests);
    procedure Detach(observer: IOmniContainerObserver);
  end; { IOmniContainerSubject }

  ///<summary>Observer sublist enumerator.</summary>
  IOmniContainerObserverEnum = interface ['{88F35ADE-16F7-427B-80E3-4686007F7B42}']
    function  GetCurrent: IOmniContainerObserver;
    function  MoveNext: boolean;
    property Current: IOmniContainerObserver read GetCurrent;
  end; { IOmniContainerObserverEnum }

  ///<summary>Observer sublist enumerator factory.</summary>
  TOmniContainerObserverEnumFactory = record
  strict private
    coefObserverList_ref: TList;
  public
    constructor Create(observerList: TList);
    function  GetEnumerator: IOmniContainerObserverEnum;
  end; { TOmniContainerObserverEnumFactory }

  ///<summary>List of observers and their interests.</summary>
  IOmniContainerObserverList = interface ['{7FFE4895-C0A5-4AC2-9048-6D125BC64A39}']
    procedure Add(observer: IOmniContainerObserver;
      interests: TOmniContainerObserverInterests);
    function Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
    procedure Remove(observer: IOmniContainerObserver);
  end; { IOmniContainerObserverList }

  function CreateContainerObserverList: IOmniContainerObserverList;

implementation

uses
  SysUtils;

type
  ///<summary>Observer sublist enumerator.</summary>
  TOmniContainerObserverEnum = class(TInterfacedObject, IOmniContainerObserverEnum)
  strict private
    coeObserverEnum: TListEnumerator;
  public
    constructor Create(observerList: TList);
    destructor Destroy; override;
    function  GetCurrent: IOmniContainerObserver;
    function  MoveNext: boolean;
    property Current: IOmniContainerObserver read GetCurrent;
  end; { TOmniContainerObserverEnum }

  ///<summary>List of observers and their interests. Implements separate lists for all
  ///    container actions for faster enumeration.</summary>
  TOmniContainerObserverList = class(TInterfacedObject, IOmniContainerObserverList)
  strict private
    colInterestLists: array [TOmniContainerObserverInterest] of TList;
    colObserverList: TInterfaceList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(observer: IOmniContainerObserver; interests:
      TOmniContainerObserverInterests);
    function  Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
    procedure Remove(observer: IOmniContainerObserver);
  end; { TOmniContainerObserverList }

{ exports }

function CreateContainerObserverList: IOmniContainerObserverList;
begin
  Result := TOmniContainerObserverList.Create;
end; { CreateContainerObserverList }

{ TOmniContainerObserverEnum }

constructor TOmniContainerObserverEnum.Create(observerList: TList);
begin
  coeObserverEnum := observerList.GetEnumerator;
end; { TOmniContainerObserverEnum.Create }

destructor TOmniContainerObserverEnum.Destroy;
begin
  FreeAndNil(coeObserverEnum);
  inherited;
end; { TOmniContainerObserverEnum.Destroy }

function TOmniContainerObserverEnum.GetCurrent: IOmniContainerObserver;
begin
  pointer(Result) := coeObserverEnum.Current;
end; { TOmniContainerObserverEnum.GetCurrent }

function TOmniContainerObserverEnum.MoveNext: boolean;
begin
  Result := coeObserverEnum.MoveNext;
end; { TOmniContainerObserverEnum.MoveNext }

{ TOmniContainerObserverEnumFactory }

constructor TOmniContainerObserverEnumFactory.Create(observerList: TList);
begin
  coefObserverList_ref := observerList;
end; { TOmniContainerObserverEnumFactory.Create }

function TOmniContainerObserverEnumFactory.GetEnumerator: IOmniContainerObserverEnum;
begin
  Result := TOmniContainerObserverEnum.Create(coefObserverList_ref);
end; { TOmniContainerObserverEnumFactory.GetEnumerator }

{ TOmniContainerObserverList }

constructor TOmniContainerObserverList.Create;
var
  interest: TOmniContainerObserverInterest;
begin
  inherited Create;
  colObserverList := TInterfaceList.Create;
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

procedure TOmniContainerObserverList.Add(observer: IOmniContainerObserver; interests:
  TOmniContainerObserverInterests);
var
  interest: TOmniContainerObserverInterest;
begin
  Remove(observer);
  colObserverList.Add(observer);
  for interest := Low(interest) to High(interest) do
    if interest in interests then
      colInterestLists[interest].Add(pointer(observer));
end; { TOmniContainerObserverList.Add }

function TOmniContainerObserverList.Enumerate(interest: TOmniContainerObserverInterest):
  TOmniContainerObserverEnumFactory;
begin
  Result := TOmniContainerObserverEnumFactory.Create(colInterestLists[interest]);
end; { TOmniContainerObserverList.Enumerate }

procedure TOmniContainerObserverList.Remove(observer: IOmniContainerObserver);
var
  idxObserver: integer;
  interest   : TOmniContainerObserverInterest;
begin
  idxObserver := colObserverList.IndexOf(observer);
  if idxObserver < 0 then
    Exit;
  colObserverList.Delete(idxObserver);
  for interest := Low(interest) to High(interest) do
    colInterestLists[interest].Remove(pointer(observer));
end; { TOmniContainerObserverList.Remove }

end.

