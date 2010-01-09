///<summary>High-level parallel execution management.
///    Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010 Primoz Gabrijelcic
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
///   Creation date     : 2010-01-08
///   Last modification : 2010-01-08
///   Version           : 0.1
///</para><para>
///   History:
///     0.1: 2010-01-08
///       - Created.
///</para></remarks>

// http://msdn.microsoft.com/en-us/magazine/cc163340.aspx

unit OtlParallel;

interface

uses
  OtlCommon;

type
  IOmniParallelLoop = interface;

  TOmniLoopDelegate = reference to procedure(const loop: IOmniParallelLoop;
    const value: TOmniValue);

  TOmniSimpleLoopDelegate = reference to procedure(const value: TOmniValue);

  IOmniParallelLoop = interface ['{ACBFF35A-FED9-4630-B442-1C8B6AD2AABF}']
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
    procedure Execute(loopBody: TOmniSimpleLoopDelegate); overload;
    procedure Execute(loopBody: TOmniLoopDelegate); overload;
    procedure Stop;
    function  Timeout(timeout_ms: integer): IOmniParallelLoop;
  end; { IOmniParallelLoop }

  Parallel = class
    class function ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop;
  end; { Parallel }

implementation

type
  TOmniParallelLoop = class(TInterfacedObject, IOmniParallelLoop)
  strict private
    oplEnumerator: IOmniValueEnumerator;
    oplNumTasks  : integer;
  public
    constructor Create(enum: IOmniValueEnumerator);
    procedure Execute(loopBody: TOmniSimpleLoopDelegate); overload;
    procedure Execute(loopBody: TOmniLoopDelegate); overload;
    function  NumTasks(taskCount: integer): IOmniParallelLoop;
    procedure Stop;
    function  Timeout(timeout_ms: Integer): IOmniParallelLoop;
  end; { TOmniParallelLoop }

{ Parallel }

class function Parallel.ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(enum);
end; { Parallel.ForEach }

{ TOmniParallelLoop }

constructor TOmniParallelLoop.Create(enum: IOmniValueEnumerator);
begin
  inherited Create;
  oplEnumerator := enum;
  oplNumTasks := Environment.Process.Affinity.Count;
end; { TOmniParallelLoop.Create }

procedure TOmniParallelLoop.Execute(loopBody: TOmniLoopDelegate);
begin
  { TODO : Implement }
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniSimpleLoopDelegate);
begin
  { TODO : Implement }
end; { TOmniParallelLoop.Execute }

function TOmniParallelLoop.NumTasks(taskCount: integer): IOmniParallelLoop;
begin
  oplNumTasks := taskCount;
  Result := Self;
end; { TOmniParallelLoop.taskCount }

procedure TOmniParallelLoop.Stop;
begin
  { TODO : Implement }
end; { TOmniParallelLoop.Stop }

function TOmniParallelLoop.Timeout(timeout_ms: Integer): IOmniParallelLoop;
begin
  { TODO : Implement }
  Result := Self;
end; { TOmniParallelLoop.Timeout }

end.
