///<summary>Thread pool implementation. Part of the OmniThreadLibrary project.</summary>
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
///   Last modification : 2008-06-30
///   Version           : 0.0
///</para><para>
///   History:
///</para></remarks>

unit OtlThreadPool;

interface

uses
  OtlTask;

type
  IOmniThreadPool = interface ['{1FA74554-1866-46DD-AC50-F0403E378682}']
    function  GetName: string;
    procedure SetName(const value: string);
  //
    procedure Schedule(task: IOmniTask); 
    property Name: string read GetName write SetName;
  //thrown in from my private thread pool unit; to be cleaned up
  {
    procedure CancelAll;
    procedure Cancel(workItemID: int64); <-- task?
    //function  GetActiveWorkItemDescriptions: string; <-- debugging interface, does not belong here
    function  IsIdle: boolean;
    property CountExecuting: integer read GetExecutingCount;
    property CountQueued: integer read GetQueuedCount;
    property IdleWorkerThreadTimeout_sec: integer read tpIdleWorkerThreadTimeout_sec write
      tpIdleWorkerThreadTimeout_sec default CDefaultIdleWorkerThreadTimeout_sec;
    property MinWorkers: integer read tpMinWorkers write tpMinWorkers;
    property MaxExecuting: integer read tpMaxExecuting write tpMaxExecuting
      (*default <number of CPUs in the thread affinity mask>*);
    property MaxQueued: integer read tpMaxQueueLength write SetMaxQueueLength;
    property MaxQueuedTime_sec: integer read tpMaxQueuedTime_sec write SetMaxQueuedTime_sec;
    property WaitOnTerminate_sec: integer read tpWaitOnTerminate_sec write
      tpWaitOnTerminate_sec default 30;
    property OnError: TGpTPError read tpOnError write tpOnError;
    //:Thread created event. Will be called from the context of the worker thread.
    property OnWorkerThreadCreated_Asy: TGpTPWorkerThreadEvent read tpOnWorkerThreadCreated
      write tpOnWorkerThreadCreated;
    //:Thread destroying event. Will be called from the context of the worker thread.
    property OnWorkerThreadDestroying_Asy: TGpTPWorkerThreadEvent read
      tpOnWorkerThreadDestroying write tpOnWorkerThreadDestroying;
    property OnWorkItemDone: TGpTPWorkItemDone read tpOnWorkItemDone write tpOnWorkItemDone;
  }
  end;

  function GlobalOmniThreadPool: IOmniThreadPool;
  function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;

implementation

uses
  SysUtils;

type
  TOmniThreadPool = class(TInterfacedObject, IOmniThreadPool)
  strict private
    otpName: string;
  protected
    function  GetName: string;
    procedure SetName(const value: string);
  public
    constructor Create(const name: string);
    procedure Schedule(task: IOmniTask);
    property Name: string read GetName write SetName;
  end; { TOmniThreadPool }

var
  GOmniThreadPool: IOmniThreadPool = nil;

{ exports }

function GlobalOmniThreadPool: IOmniThreadPool;
begin
  if not assigned(GOmniThreadPool) then
    GOmniThreadPool := CreateThreadPool('GlobalOmniThreadPool');
  Result := GOmniThreadPool;
end; { GlobalOmniThreadPool }

function CreateThreadPool(const threadPoolName: string): IOmniThreadPool;
begin
  Result := TOmniThreadPool.Create(threadPoolName);
end; { CreateThreadPool }

constructor TOmniThreadPool.Create(const name: string);
begin
  inherited Create;
  otpName := name;
end; { TOmniThreadPool.Create }

function TOmniThreadPool.GetName: string;
begin
  Result := otpName;
end; { TOmniThreadPool.GetName }

procedure TOmniThreadPool.Schedule(task: IOmniTask);
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniThreadPool.Schedule
  raise Exception.Create('TOmniThreadPool.Schedule: not implemented');
end; { TOmniThreadPool.Schedule }

procedure TOmniThreadPool.SetName(const value: string);
begin
  otpName := value;
end; { TOmniThreadPool.SetName }

end.
