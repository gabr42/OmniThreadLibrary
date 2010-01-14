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
///   Last modification : 2010-01-14
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2010-01-14
///       - Released.
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

  // *** assumes that the enumerator's Take method is threadsafe ***
  IOmniParallelLoop = interface ['{ACBFF35A-FED9-4630-B442-1C8B6AD2AABF}']
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
    procedure Execute(loopBody: TOmniSimpleLoopDelegate); overload;
    procedure Execute(loopBody: TOmniLoopDelegate); overload;
    procedure Stop;
    function  Timeout(timeout_ms: integer): IOmniParallelLoop;
  end; { IOmniParallelLoop }

  Parallel = class
    class function ForEach(const enumGen: IOmniValueEnumerable): IOmniParallelLoop;
  end; { Parallel }

implementation

uses
  Windows,
  SysUtils,
  GpStuff,
  OtlSync,
  OtlTask,
  OtlTaskControl;

{ TODO 3 -ogabr : Should work with thread-unsafe enumerators }

type
  TOmniParallelLoop = class(TInterfacedObject, IOmniParallelLoop)
  strict private
    oplBody    : TOmniLoopDelegate;
    oplEnumGen : IOmniValueEnumerable;
    oplNumTasks: integer;
    oplStopped : boolean;
  public
    constructor Create(const enumGen: IOmniValueEnumerable);
    procedure Execute(loopBody: TOmniSimpleLoopDelegate); overload;
    procedure Execute(loopBody: TOmniLoopDelegate); overload;
    function  NumTasks(taskCount: integer): IOmniParallelLoop;
    procedure Stop;
    function  Timeout(timeout_ms: Integer): IOmniParallelLoop;
  end; { TOmniParallelLoop }

{ Parallel }

class function Parallel.ForEach(const enumGen: IOmniValueEnumerable): IOmniParallelLoop;
begin
  { TODO 3 -ogabr : In Delphi 2010 RTTI to be used to get to the GetEnumerator from base object }
  Result := TOmniParallelLoop.Create(enumGen);
end; { Parallel.ForEach }

{ TOmniParallelLoop }

constructor TOmniParallelLoop.Create(const enumGen: IOmniValueEnumerable);
begin
  inherited Create;
  oplEnumGen := enumGen;
  oplNumTasks := Environment.Process.Affinity.Count;
end; { TOmniParallelLoop.Create }

procedure TOmniParallelLoop.Execute(loopBody: TOmniLoopDelegate);
var
  countStopped: IOmniResourceCount;
  iTask       : integer;
begin
  oplStopped := false;
  oplBody := loopBody;
  countStopped := TOmniResourceCount.Create(oplNumTasks);
  for iTask := 1 to oplNumTasks do
    CreateTask(
      procedure (const task: IOmniTask)
      var
        value     : TOmniValue;
        enumerator: IOmniValueEnumerator;
      begin
        enumerator := oplEnumGen.GetEnumerator;
        while (not oplStopped) and enumerator.Take(value) do
          oplBody(Self, value);
        countStopped.Allocate;
      end,
      'Parallel.ForEach worker #' + IntToStr(iTask)
    ).Unobserved
     .Run;
  WaitForSingleObject(countStopped.Handle, INFINITE);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniSimpleLoopDelegate);
begin
  Execute(
    procedure (const loop: IOmniParallelLoop; const elem: TOmniValue)
    begin
      loopBody(elem);
    end);
end; { TOmniParallelLoop.Execute }

function TOmniParallelLoop.NumTasks(taskCount: integer): IOmniParallelLoop;
begin
  Assert(taskCount > 0);
  oplNumTasks := taskCount;
  Result := Self;
end; { TOmniParallelLoop.taskCount }

procedure TOmniParallelLoop.Stop;
begin
  oplStopped := true;
end; { TOmniParallelLoop.Stop }

function TOmniParallelLoop.Timeout(timeout_ms: Integer): IOmniParallelLoop;
begin
  { TODO : Implement }
  Result := Self;
end; { TOmniParallelLoop.Timeout }

end.
