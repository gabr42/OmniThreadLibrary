///<summary>High-level parallel execution management.
///    Part of the OmniThreadLibrary project. Requires Delphi 2009 or newer.</summary>
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
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2010-02-02
///       - Implemented ForEach(rangeLow, rangeHigh).
///       - Implemented ForEach.Aggregate.
///       - ForEach optimized for execution on single-core computer.
///       - Implemented Parallel.Join.
///       - Removed Stop method. Loop can be cancelled with a cancellation token.
///     1.0: 2010-01-14
///       - Released.
///</para></remarks>

// http://msdn.microsoft.com/en-us/magazine/cc163340.aspx
// http://blogs.msdn.com/pfxteam/archive/2007/11/29/6558543.aspx
// http://cis.jhu.edu/~dsimcha/parallelFuture.html

(* Things to consider:
   - Support for simpler IEnumerable source (with "lock and fetch a packet" approach).
   - All Parallel stuff should have a "chunk" option (or default).
       int P = 2 * Environment.ProcessorCount; // assume twice the procs for
                                               // good work distribution
       int Chunk = N / P;                      // size of a work chunk
   - Probably we need Parallel.Join.MonitorWith or something like that.

   Can something like this be implemented?:
     "To scale well on multiple processors, TPL uses work-stealing techniques to
      dynamically adapt and distribute work items over the worker threads. The library
      has a task manager that, by default, uses one worker thread per processor. This
      ensures minimal thread switching by the OS. Each worker thread has its own local
      task queue of work to be done. Each worker usually just pushes new tasks onto its
      own queue and pops work whenever a task is done. When its local queue is empty,
      a worker looks for work itself and tries to "steal" work from the queues of
      other workers."
*)

unit OtlParallel;

interface

// TODO 3 -oPrimoz Gabrijelcic : How to enumerate over TList<T>?
// TODO 5 -oPrimoz Gabrijelcic : Do we need separate thread (or task?) pool for Parallel.For?

uses
  SysUtils,
  OtlCommon,
  OtlSync,
  OtlTask,
  OtlDataManager;

type
  IOmniParallelLoop = interface;

  TOmniAggregatorDelegate = reference to procedure(var aggregate: TOmniValue; const value: TOmniValue);
  TOmniAggregatorIntDelegate = reference to procedure(var aggregate: int64; value: int64);

  TOmniIteratorDelegate = reference to procedure(const value: TOmniValue);
  TOmniIteratorDelegate<T> = reference to procedure(const value: T);
  TOmniIteratorIntDelegate = reference to procedure(const value: int64);

  TOmniIteratorAggregateDelegate = reference to function(const value: TOmniValue): TOmniValue;
  TOmniIteratorAggregateDelegate<T> = reference to function(const value: T): TOmniValue;
  TOmniIteratorAggregateIntDelegate = reference to function(const value: int64): int64;
  TOmniIteratorAggregateIntDelegate<T> = reference to function(const value: T): int64;

  IOmniParallelAggregatorLoop = interface
    function  Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue; overload;
    function  Execute(loopBody: TOmniIteratorAggregateIntDelegate): int64; overload;
  end; { IOmniParallelAggregatorLoop }

  IOmniParallelAggregatorLoop<T> = interface
    function  Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue; overload;
    function  Execute(loopBody: TOmniIteratorAggregateIntDelegate<T>): TOmniValue; overload;
  end; { IOmniParallelAggregatorLoop<T> }

  IOmniParallelLoop = interface
    function  Aggregate(aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorIntDelegate): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorDelegate;
      defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorIntDelegate;
      defaultAggregateValue: int64): IOmniParallelAggregatorLoop; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntDelegate); overload;
    function  CancelWith(token: IOmniCancellationToken): IOmniParallelLoop;
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
  end; { IOmniParallelLoop }

  IOmniParallelLoop<T> = interface
    function  Aggregate(aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>; overload;
    function  Aggregate(aggregator: TOmniAggregatorDelegate;
      defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop<T>; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    function  CancelWith(token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
  end; { IOmniParallelLoop<T> }

  Parallel = class
    class function  ForEach(const enumGen: IOmniValueEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop; overload;
    class function  ForEach(low, high: integer; step: integer = 1): IOmniParallelLoop; overload;
    class function  ForEach<T>(const enumGen: IOmniValueEnumerable):
      IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IOmniValueEnumerator):
      IOmniParallelLoop<T>; overload;
    class procedure Join(const task1, task2: TOmniTaskFunction); overload;
    class procedure Join(const task1, task2: TProc); overload;
    class procedure Join(const tasks: array of TOmniTaskFunction); overload;
    class procedure Join(const tasks: array of TProc); overload;
  end; { Parallel }

type
  TOmniParallelLoop = class(TInterfacedObject, IOmniParallelLoop, IOmniParallelAggregatorLoop)
  strict private
    oplAggregate        : TOmniValue;
    oplAggregator       : TOmniAggregatorDelegate;
    oplCancellationToken: IOmniCancellationToken;
    oplManagedProvider  : boolean;
    oplNumTasks         : integer;
    oplSourceProvider   : TOmniSourceProvider;
  strict protected
    function  Stopped: boolean;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean);
    destructor  Destroy; override;
    function  Aggregate(aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorDelegate; defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorIntDelegate): IOmniParallelAggregatorLoop; overload;
    function  Aggregate(aggregator: TOmniAggregatorIntDelegate; defaultAggregateValue: int64): IOmniParallelAggregatorLoop; overload;
    function  CancelWith(token: IOmniCancellationToken): IOmniParallelLoop;
    function  Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue; overload;
    function  Execute(loopBody: TOmniIteratorAggregateIntDelegate): int64; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntDelegate); overload;
    function  NumTasks(taskCount: integer): IOmniParallelLoop;
  end; { TOmniParallelLoop }

  TOmniParallelLoop<T> = class(TInterfacedObject, IOmniParallelLoop<T>,
                                 IOmniParallelAggregatorLoop<T>)
  strict private
    oplAggregator     : IOmniParallelAggregatorLoop;
    oplManagedProvider: boolean;
    oplParallel       : IOmniParallelLoop;
    oplSourceProvider : TOmniSourceProvider;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean);
    destructor  Destroy; override;
    function  Aggregate(aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>; overload;
    function  Aggregate(aggregator: TOmniAggregatorDelegate;
      defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop<T>; overload;
    function  CancelWith(token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue; overload;
    function  Execute(loopBody: TOmniIteratorAggregateIntDelegate<T>): TOmniValue; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
  end; { TOmniParallelLoop<T> }

implementation

uses
  Windows,
  GpStuff,
  OtlTaskControl;

{ Parallel }

class function Parallel.ForEach(const enumGen: IOmniValueEnumerable): IOmniParallelLoop;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach(enumGen.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach(low, high: integer; step: integer): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(CreateSourceProvider(low, high, step), true);
end; { Parallel.ForEach }

class function Parallel.ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(sourceProvider, false);
end; { Parallel.ForEach }

class function Parallel.ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := TOmniParallelLoop.Create(CreateSourceProvider(enum), true);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enumGen: IOmniValueEnumerable):
  IOmniParallelLoop<T>;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach<T>(enuMGen.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enum: IOmniValueEnumerator):
  IOmniParallelLoop<T>;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := TOmniParallelLoop<T>.Create(CreateSourceProvider(enum), true);
end; { Parallel.ForEach }

class procedure Parallel.Join(const task1, task2: TOmniTaskFunction);
begin
  Join([task1, task2]);
end; { Parallel.Join }

class procedure Parallel.Join(const tasks: array of TOmniTaskFunction);
var
  countStopped: TOmniResourceCount;
  firstTask   : IOmniTaskControl;
  prevTask    : IOmniTaskControl;
  proc        : TOmniTaskFunction;
  task        : IOmniTaskControl;
begin
  if (Environment.Process.Affinity.Count = 1) or (Length(tasks) = 1) then begin
    prevTask := nil;
    for proc in tasks do begin
      task := CreateTask(proc).Unobserved;
      if assigned(prevTask) then
        prevTask.ChainTo(task);
      prevTask := task;
      if not assigned(firstTask) then
        firstTask := task;
    end;
    if assigned(firstTask) then begin
      firstTask.Run;
      prevTask.WaitFor(INFINITE);
    end;
  end
  else begin
    countStopped := TOmniResourceCount.Create(Length(tasks));
    for proc in tasks do
      CreateTask(
        procedure (const task: IOmniTask) begin
          proc(task);
          countStopped.Allocate;
        end
      ).Unobserved
       .Schedule;
    WaitForSingleObject(countStopped.Handle, INFINITE);
  end;
end; { Parallel.Join }

class procedure Parallel.Join(const task1, task2: TProc);
begin
  Join([task1, task2]);
end; { Parallel.Join }

class procedure Parallel.Join(const tasks: array of TProc);
var
  countStopped: TOmniResourceCount;
  proc        : TProc;
begin
  if (Environment.Process.Affinity.Count = 1) or (Length(tasks) = 1) then begin
    for proc in tasks do
      proc;
  end
  else begin
    countStopped := TOmniResourceCount.Create(Length(tasks));
    for proc in tasks do
      CreateTask(
        procedure (const task: IOmniTask) begin
          proc;
          countStopped.Allocate;
        end
      ).Unobserved
       .Schedule;
    WaitForSingleObject(countStopped.Handle, INFINITE);
  end;
end; { Parallel.Join }

{ TOmniParallelLoop }

constructor TOmniParallelLoop.Create(const sourceProvider: TOmniSourceProvider;
  managedProvider: boolean);
begin
  inherited Create;
  oplSourceProvider := sourceProvider;
  oplNumTasks := Environment.Process.Affinity.Count;
  oplManagedProvider := managedProvider;
end; { TOmniParallelLoop.Create }

destructor TOmniParallelLoop.Destroy;
begin
  if oplManagedProvider then
    FreeAndNil(oplSourceProvider);
  inherited;
end; { TOmniParallelLoop.Destroy }

function TOmniParallelLoop.Aggregate(aggregator: TOmniAggregatorDelegate):
  IOmniParallelAggregatorLoop;
begin
  Result := Aggregate(aggregator, TOmniValue.Null);
end; { TOmniParallelLoop.Aggregate }

function TOmniParallelLoop.Aggregate(aggregator: TOmniAggregatorDelegate;
  defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop;
begin
  oplAggregator := aggregator;
  oplAggregate := defaultAggregateValue;
  Result := Self;
end; { TOmniParallelLoop.Aggregate }

function TOmniParallelLoop.Aggregate(aggregator: TOmniAggregatorIntDelegate):
  IOmniParallelAggregatorLoop;
begin
  Result := Aggregate(aggregator, 0);
end; { TOmniParallelLoop.Aggregate }

function TOmniParallelLoop.Aggregate(aggregator: TOmniAggregatorIntDelegate;
  defaultAggregateValue: int64): IOmniParallelAggregatorLoop;
begin
  Result := Aggregate(
    procedure (var aggregate: TOmniValue; const value: TOmniValue)
    var
      aggregateInt: int64;
    begin
      aggregateInt := aggregate.AsInt64;
      aggregator(aggregateInt, value);
      aggregate.AsInt64 := aggregateInt;
    end,
    defaultAggregateValue);
end; { TOmniParallelLoop.Aggregate }

function TOmniParallelLoop.CancelWith(token: IOmniCancellationToken): IOmniParallelLoop;
begin
  oplCancellationToken := token;
  Result := Self;
end; { TOmniParallelLoop.CancelWith }

function TOmniParallelLoop.Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue;
var
  countStopped : IOmniResourceCount;
  dataManager  : TOmniDataManager;
  iTask        : integer;
  localQueue   : TOmniLocalQueue;
  lockAggregate: IOmniCriticalSection;
  value        : TOmniValue;
begin
  if ((oplNumTasks = 1) or (Environment.Thread.Affinity.Count = 1)) then begin
    dataManager := CreateDataManager(oplSourceProvider, oplNumTasks);
    try
      localQueue := dataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          oplAggregator(oplAggregate, loopBody(value));
      finally FreeAndNil(localQueue); end;
    finally FreeAndNil(dataManager); end;
    Result := oplAggregate;
  end
  else begin
    countStopped := TOmniResourceCount.Create(oplNumTasks);
    lockAggregate := CreateOmniCriticalSection;
    dataManager := CreateDataManager(oplSourceProvider, oplNumTasks);
    try
      for iTask := 1 to oplNumTasks do
        CreateTask(
          procedure (const task: IOmniTask)
          var
            aggregate : TOmniValue;
            localQueue: TOmniLocalQueue;
            value     : TOmniValue;
          begin
            aggregate := TOmniValue.Null;
            localQueue := dataManager.CreateLocalQueue;
            try
              while (not Stopped) and localQueue.GetNext(value) do
                oplAggregator(aggregate, loopBody(value));
            finally FreeAndNil(localQueue); end;
            task.Lock.Acquire;
            try
              oplAggregator(oplAggregate, aggregate);
            finally task.Lock.Release; end;
            countStopped.Allocate;
          end,
          'Parallel.ForEach worker #' + IntToStr(iTask)
        ).WithLock(lockAggregate)
         .Unobserved
         .Schedule;
      WaitForSingleObject(countStopped.Handle, INFINITE);
    finally FreeAndNil(dataManager); end;
    Result := oplAggregate;
  end;
end; { TOmniParallelLoop.Execute }

function TOmniParallelLoop.Execute(loopBody: TOmniIteratorAggregateIntDelegate): int64;
begin
  Result := Execute(
    function(const value: TOmniValue): TOmniValue
    begin
      Result := loopBody(value);
    end);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorDelegate);
var
  countStopped: IOmniResourceCount;
  dataManager : TOmniDataManager;
  iTask       : integer;
  localQueue  : TOmniLocalQueue;
  value       : TOmniValue;
begin
  if ((oplNumTasks = 1) or (Environment.Thread.Affinity.Count = 1)) then begin
    dataManager := CreateDataManager(oplSourceProvider, oplNumTasks);
    try
      localQueue := dataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          loopBody(value);
      finally FreeAndNil(localQueue); end;
    finally FreeAndNil(dataManager); end;
  end
  else begin
    // TODO 3 -oPrimoz Gabrijelcic : Replace this with a task pool?
    dataManager := CreateDataManager(oplSourceProvider, oplNumTasks);
    try
      countStopped := TOmniResourceCount.Create(oplNumTasks);
      for iTask := 1 to oplNumTasks do
        CreateTask(
          procedure (const task: IOmniTask)
          var
            localQueue: TOmniLocalQueue;
            value     : TOmniValue;
          begin
            localQueue := dataManager.CreateLocalQueue;
            try
              while (not Stopped) and localQueue.GetNext(value) do
                loopBody(value);
            finally FreeAndNil(localQueue); end;
            countStopped.Allocate;
          end,
          'Parallel.ForEach worker #' + IntToStr(iTask)
        ).Unobserved
         .Schedule;
      WaitForSingleObject(countStopped.Handle, INFINITE);
    finally FreeAndNil(dataManager); end;
  end;
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorIntDelegate);
begin
  Execute(
    procedure (const elem: TOmniValue)
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

function TOmniParallelLoop.Stopped: boolean;
begin
  Result := (assigned(oplCancellationToken) and oplCancellationToken.IsSignaled);
end; { TOmniParallelLoop.Stopped }

{ TOmniParalleLoop<T> }

constructor TOmniParallelLoop<T>.Create(const sourceProvider: TOmniSourceProvider;
  managedProvider: boolean);
begin
  inherited Create;
  oplSourceProvider := sourceProvider;
  oplManagedProvider := managedProvider;
  oplParallel := Parallel.ForEach(sourceProvider);
end; { TOmniParallelLoop<T>.Create }

destructor TOmniParallelLoop<T>.Destroy;
begin
  if oplManagedProvider then
    FreeAndNil(oplSourceProvider);
  inherited;
end; { TOmniParallelLoop }

function TOmniParallelLoop<T>.Aggregate(aggregator: TOmniAggregatorDelegate):
  IOmniParallelAggregatorLoop<T>;
begin
  Result := Aggregate(aggregator, TOmniValue.Null);
end; { TOmniParallelLoop<T>.Aggregate }

function TOmniParallelLoop<T>.Aggregate(aggregator: TOmniAggregatorDelegate;
  defaultAggregateValue: TOmniValue): IOmniParallelAggregatorLoop<T>;
begin
  oplAggregator := oplParallel.Aggregate(aggregator, defaultAggregateValue);
  Result := Self;
end; { TOmniParallelLoop<T>.Aggregate }

function TOmniParallelLoop<T>.CancelWith(token: IOmniCancellationToken): IOmniParallelLoop<T>;
begin
  oplParallel.CancelWith(token);
  Result := Self;
end; { TOmniParallelLoop<T>.CancelWith }

function TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue;
begin
  Result := oplAggregator.Execute(
    function (const value: TOmniValue): TOmniValue
    begin
      Result := loopBody(T(value.AsObject));
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorDelegate<T>);
begin
  oplParallel.Execute(
    procedure (const value: TOmniValue)
    begin
      loopBody(T(value.AsObject));
    end
  );
end; { TOmniParallelLoop<T>.Execute }

function TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorAggregateIntDelegate<T>):
  TOmniValue;
begin
  Result := oplAggregator.Execute(
    function (const value: TOmniValue): TOmniValue
    begin
      Result := loopBody(T(value.AsObject));
    end
  );
end; { TOmniParallelLoop }

function TOmniParallelLoop<T>.NumTasks(taskCount: integer): IOmniParallelLoop<T>;
begin
  oplParallel.NumTasks(taskCount);
  Result := Self;
end; { TOmniParallelLoop<T>.taskCount }

end.
