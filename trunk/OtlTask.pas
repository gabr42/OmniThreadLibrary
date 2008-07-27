///<summary>Task interface. Part of the OmniThreadLibrary project.</summary>
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
///   Last modification : 2008-07-23
///   Version           : 0.6
///</para><para>
///   History:
///     0.6: 2008-07-23
///       - Added reserved exit statuses.
///     0.5: 2008-07-22
///       - Added Lock property. Lock is only available if WithLock method is run on the
///         task control object.
///     0.4: 2008-07-15
///       - Everything but the IOmniTask interface declaration moved into the
///         OtlTaskControl unit.
///     0.3: 2008-07-10
///       - Implemented ExitCode/ExitMessage/SetExitStatus.
///     0.2: 2008-07-09
///       - TOmniTaskExcecutor changed from a record to a class.
///       - IOmniWorker message dispatcher extracted into the
///         TOmniTaskExecutor class.
///       - Added support for dispatching additional communication channel
///         messages.
///       - Replaced spinlocks with ticket spinlocks. There seems to be a
///         problem with the SpinLock code and ticket spinlocks should be faster
///         in our scenario anyway.
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
    function  GetUniqueID: int64;
  //
    procedure RegisterComm(const comm: IOmniCommunicationEndpoint);
    procedure SetExitStatus(exitCode: integer; const exitMessage: string);
    procedure Terminate;
    function  Terminated: boolean;
    procedure UnregisterComm(const comm: IOmniCommunicationEndpoint);
    property Comm: IOmniCommunicationEndpoint read GetComm;
    property Counter: IOmniCounter read GetCounter;
    property Lock: TSynchroObject read GetLock;
    property Name: string read GetName;
    property Param[idxParam: integer]: TOmniValue read GetParam;
    property ParamByName[const paramName: string]: TOmniValue read GetParamByName;
    property TerminateEvent: THandle read GetTerminateEvent;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniTask }

  IOmniTaskExecutor = interface ['{123F2A63-3769-4C5B-89DA-1FEB6C3421ED}']
    procedure Execute;
  end; { IOmniTaskExecutor }

implementation

{ TOmniTask }

end.
