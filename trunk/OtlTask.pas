///<summary>Task interface. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2009, Primoz Gabrijelcic
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
///   Creation date     : 2008-06-12
///   Last modification : 2009-02-06
///   Version           : 1.05
///</para><para>
///   History:
///     1.05: 2009-02-06
///       - Implemented per-thread data storage.
///     1.04: 2009-01-26
///       - Implemented IOmniTask.Enforced decorator.
///     1.03: 2008-11-01
///       - *** Breaking interface change ***
///         - IOmniTask.Terminated renamed to IOmniTask.Stopped.
///         - New IOmniTask.Terminated that check whether the task
///           *has been requested to terminate*.
///     1.02: 2008-10-05
///       - Added two overloaded SetTimer methods using string/pointer invocation.
///     1.01: 2008-09-18
///       - Exposed SetTimer interface.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

unit OtlTask;

interface

uses
  Windows,
  SysUtils,
  Variants,
  Classes,
  SyncObjs,
  OtlCommon,
  OtlComm;

type
  IOmniTask = interface ['{958AE8A3-0287-4911-B475-F275747400E4}']
    function  GetComm: IOmniCommunicationEndpoint;
    function  GetCounter: IOmniCounter;
    function  GetLock: TSynchroObject;
    function  GetName: string;
    function  GetParam(idxParam: integer): TOmniValue;
    function  GetParamByName(const paramName: string): TOmniValue;
    function  GetTerminateEvent: THandle;
    function  GetThreadData: IInterface;
    function  GetUniqueID: int64;
  //
    procedure Enforced(forceExecution: boolean = true);
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure SetTimer(interval_ms: cardinal; timerMessageID: integer = -1); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMethod: pointer); overload;
    procedure SetTimer(interval_ms: cardinal; const timerMessageName: string); overload;
    procedure StopTimer;
    procedure Terminate;
    function  Terminated: boolean;
    function  Stopped: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property TerminateEvent: THandle read GetTerminateEvent; //use Terminate to terminate a task, don't just set TerminateEvent
    property ThreadData: IInterface read GetThreadData;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTask }

  IOmniTaskExecutor = interface ['{123F2A63-3769-4C5B-89DA-1FEB6C3421ED}']
    procedure Execute;
    procedure SetThreadData(const value: IInterface);
  end; { IOmniTaskExecutor }

implementation

end.
