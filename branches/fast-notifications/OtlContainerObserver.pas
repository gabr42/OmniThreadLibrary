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
  public
    constructor Create(observerList: TList);
    function  GetEnumerator: IOmniContainerObserverEnum;
  end; { TOmniContainerObserverEnumFactory }

  ///<summary>List of observers and their interests. Implements separate lists for all
  ///    container actions for faster enumeration.</summary>
  IOmniContainerObserverList = interface ['{7FFE4895-C0A5-4AC2-9048-6D125BC64A39}']
    function Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
  end; { IOmniContainerObserverList }

implementation

type
  TOmniContainerObserverList = class(TInterfacedObject, IOmniContainerObserverList)
  public
    function Enumerate(interest: TOmniContainerObserverInterest):
      TOmniContainerObserverEnumFactory;
  end; { TOmniContainerObserverList }

end.
