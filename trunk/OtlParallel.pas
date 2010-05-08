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
  - Probably we need Parallel.Join.MonitorWith or something like that.
*)

unit OtlParallel;

{$IF CompilerVersion >= 21}
  {$DEFINE OTL_ERTTI}
{$IFEND}

interface

// TODO 3 -oPrimoz Gabrijelcic : How to enumerate over TList<T>?
// TODO 5 -oPrimoz Gabrijelcic : Do we need separate thread (or task?) pool for Parallel.For?
// TODO 3 -oPrimoz Gabrijelcic : ForEach chaining (output of one ForEach goes into the next ForEach); must have a simple syntax and good task scheduler.

uses
  SysUtils,
  {$IFDEF OTL_ERTTI}
  TypInfo,
  RTTI,
  {$ENDIF OTL_ERTTI}
  Generics.Collections,
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
  TOmniIteratorIntDelegate = reference to procedure(value: int64);

  TOmniIteratorAggregateDelegate = reference to function(const value: TOmniValue): TOmniValue;
  TOmniIteratorAggregateDelegate<T> = reference to function(const value: T): TOmniValue;
  TOmniIteratorAggregateIntDelegate = reference to function(value: int64): int64;
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

  TEnumeratorDelegate = reference to function(var next: TOmniValue): boolean;
  TEnumeratorDelegate<T> = reference to function(var next: T): boolean;

  Parallel = class
    class function  ForEach(const enumerable: IOmniValueEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const enumerable: IEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop; overload;
    class function  ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop; overload;
    class function  ForEach(low, high: integer; step: integer = 1): IOmniParallelLoop; overload;
    class function  ForEach<T>(const enumerable: IOmniValueEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IOmniValueEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>; overload;
    {$IFDEF OTL_ERTTI}
    class function  ForEach(const enumerable: TObject): IOmniParallelLoop; overload;
    class function  ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>; overload;
    {$ENDIF OTL_ERTTI}
//    class function  ForEach<T>(enumerator: TEnumeratorDelegate<T>): IOmniParallelLoop<T>; overload;
    class procedure Join(const task1, task2: TOmniTaskFunction); overload;
    class procedure Join(const task1, task2: TProc); overload;
    class procedure Join(const tasks: array of TOmniTaskFunction); overload;
    class procedure Join(const tasks: array of TProc); overload;
  end; { Parallel }

  TOmniDelegateEnumerator = class(TOmniValueEnumerator)
  strict private
    odeDelegate: TEnumeratorDelegate;
    odeValue   : TOmniValue;
  public
    constructor Create(delegate: TEnumeratorDelegate);
    function  GetCurrent: TOmniValue; override;
    function  MoveNext: boolean; override;
  end; { TOmniDelegateEnumerator }

  TOmniParallelLoopBase = class(TInterfacedObject)
  {$IFDEF OTL_ERTTI}
  strict private
    oplDestroy    : TRttiMethod;
    oplEnumerable : TValue;
    oplGetCurrent : TRttiMethod;
    oplMoveNext   : TRttiMethod;
    oplRttiContext: TRttiContext;
  public
    constructor Create(enumerable: TObject); overload;
  {$ENDIF OTL_ERTTI}
  private
    oplDelegateEnum     : TOmniDelegateEnumerator;
    oplManagedProvider  : boolean;
    oplNumTasks         : integer;
    oplSourceProvider   : TOmniSourceProvider;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean); overload;
    constructor Create(const enumerator: TEnumeratorDelegate); overload;
    destructor  Destroy; override;
  end; { TOmniParallelLoopBase }

  TOmniParallelLoop = class(TOmniParallelLoopBase, IOmniParallelLoop,
                                                   IOmniParallelAggregatorLoop)
  strict private
    oplAggregate        : TOmniValue;
    oplAggregator       : TOmniAggregatorDelegate;
    oplCancellationToken: IOmniCancellationToken;
  strict protected
    function  Stopped: boolean;
  public
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

  TOmniParallelLoop<T> = class(TOmniParallelLoopBase, IOmniParallelLoop<T>,
                                                      IOmniParallelAggregatorLoop<T>)
  strict private
    oplAggregator: IOmniParallelAggregatorLoop;
    oplParallel  : IOmniParallelLoop;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean); overload;
    constructor Create(const enumerator: TEnumeratorDelegate); overload;
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

class function Parallel.ForEach(const enumerable: IOmniValueEnumerable):
  IOmniParallelLoop;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach(low, high: integer; step: integer): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(CreateSourceProvider(low, high, step), true);
end; { Parallel.ForEach }

class function Parallel.ForEach(const enumerable: IEnumerable): IOmniParallelLoop;
begin
  Result := Parallel.ForEach(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach(const enum: IEnumerator): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(CreateSourceProvider(enum), true);
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

class function Parallel.ForEach(const enumerable: TObject): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(enumerable);
end; { Parallel.ForEach }

class function Parallel.ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(enumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enumerable: IOmniValueEnumerable):
  IOmniParallelLoop<T>;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach<T>(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enum: IOmniValueEnumerator):
  IOmniParallelLoop<T>;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := TOmniParallelLoop<T>.Create(CreateSourceProvider(enum), true);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>;
begin
  Result := Parallel.ForEach<T>(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>;
begin
  // TODO 1 -oPrimoz Gabrijelcic : don't know yet how to pull this through without rebuilding OtlDataManager from scratch
  Result := nil;
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>;
begin
  Result := Parallel.ForEach<T>(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(CreateSourceProvider(enum), true );
end; { Parallel.ForEach }

class function Parallel.ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enumerable);
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

{ TOmniParallelLoopBase }

constructor TOmniParallelLoopBase.Create(const sourceProvider: TOmniSourceProvider;
  managedProvider: boolean);
begin
  inherited Create;
  oplSourceProvider := sourceProvider;
  oplNumTasks := Environment.Process.Affinity.Count;
  oplManagedProvider := managedProvider;
end; { TOmniParallelLoopBase.Create }

constructor TOmniParallelLoopBase.Create(const enumerator: TEnumeratorDelegate);
begin
  oplDelegateEnum := TOmniDelegateEnumerator.Create(enumerator);
  Create(CreateSourceProvider(oplDelegateEnum), true);
end; { TOmniParallelLoopBase.Create }

{$IFDEF OTL_ERTTI}
constructor TOmniParallelLoopBase.Create(enumerable: TObject);
var
  rm: TRttiMethod;
  rt: TRttiType;
begin
  oplRttiContext := TRttiContext.Create;
  rt := oplRttiContext.GetType(enumerable.ClassType);
  Assert(assigned(rt));
  rm := rt.GetMethod('GetEnumerator');
  Assert(assigned(rm));
  Assert(assigned(rm.ReturnType) and (rm.ReturnType.TypeKind = tkClass));
  oplEnumerable := rm.Invoke(enumerable, []);
  Assert(oplEnumerable.AsObject <> nil);
  rt := oplRttiContext.GetType(oplEnumerable.TypeInfo);
  oplMoveNext := rt.GetMethod('MoveNext');
  Assert(assigned(oplMoveNext));
  Assert((oplMoveNext.ReturnType.TypeKind = tkEnumeration) and SameText(oplMoveNext.ReturnType.Name, 'Boolean'));
  oplGetCurrent := rt.GetMethod('GetCurrent');
  Assert(assigned(oplGetCurrent));
  oplDestroy := rt.GetMethod('Destroy');
  Assert(assigned(oplDestroy));
  Create(
    function (var next: TOmniValue): boolean begin
      Result := oplMoveNext.Invoke(oplEnumerable, []).AsBoolean;
      if Result then
        next := oplGetCurrent.Invoke(oplEnumerable, []);
    end
  );
end; { TOmniParallelLoopBase.Create }
{$ENDIF OTL_ERTTI}

destructor TOmniParallelLoopBase.Destroy;
begin
  if oplManagedProvider then
    FreeAndNil(oplSourceProvider);
  FreeAndNil(oplDelegateEnum);
  {$IFDEF OTL_ERTTI}
  if oplEnumerable.AsObject <> nil then begin
    oplDestroy.Invoke(oplEnumerable, []);
    oplRttiContext.Free;
  end;
  {$ENDIF OTL_ERTTI}
  inherited;
end; { TOmniParallelLoopBase.Destroy }

{ TOmniParallelLoop }

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

constructor TOmniParallelLoop<T>.Create(const enumerator: TEnumeratorDelegate);
begin
  oplDelegateEnum := TOmniDelegateEnumerator.Create(enumerator);
  Create(CreateSourceProvider(oplDelegateEnum), true);
end; { TOmniParallelLoop }

destructor TOmniParallelLoop<T>.Destroy;
begin
  if oplManagedProvider then
    FreeAndNil(oplSourceProvider);
  FreeAndNil(oplDelegateEnum);
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

{ TOmniDelegateEnumerator }

constructor TOmniDelegateEnumerator.Create(delegate: TEnumeratorDelegate);
begin
  odeDelegate := delegate;
end; { TOmniDelegateEnumerator.Create }

function TOmniDelegateEnumerator.GetCurrent: TOmniValue;
begin
  Result := odeValue;
end; { TOmniDelegateEnumerator.GetCurrent }

function TOmniDelegateEnumerator.MoveNext: boolean;
begin
  Result := odeDelegate(odeValue);
end; { TOmniDelegateEnumerator.MoveNext }

end.
