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

// TODO 1 -oPrimoz Gabrijelcic : Check compilation with D2009.
// TODO 5 -oPrimoz Gabrijelcic : Do we need separate thread (or task?) pool for Parallel.For?
// TODO 5 -oPrimoz Gabrijelcic : Simple way to access Parallel.ForEach output? something like "for xxx in Parallel.ForEach(...)..."? Better: Parallel.ForEach.NoWait and use for over the blocking collection. Add a demo.
// TODO 3 -oPrimoz Gabrijelcic : Do we need another Int delegate using 'integer' instead of 'int64'?
// TODO 3 -oPrimoz Gabrijelcic : Maybe we could use .Aggregate<T> where T is the aggregate type?
// TODO 1 -oPrimoz Gabrijelcic : How to combine Futures and NoWait version of Aggregate?

uses
  SysUtils,
  {$IFDEF OTL_ERTTI}
  TypInfo,
  RTTI,
  {$ENDIF OTL_ERTTI}
  Generics.Collections,
  OtlCommon,
  OtlSync,
  OtlCollections,
  OtlTask,
  OtlDataManager;

type
  IOmniParallelLoop = interface;
  IOmniParallelLoop<T> = interface;

  TOmniAggregatorDelegate = reference to procedure(var aggregate: TOmniValue; const value: TOmniValue);

  TOmniIteratorDelegate = reference to procedure(const value: TOmniValue);
  TOmniIteratorDelegate<T> = reference to procedure(const value: T);

  TOmniIteratorAggregateDelegate = reference to function(const value: TOmniValue): TOmniValue;
  TOmniIteratorAggregateDelegate<T> = reference to function(const value: T): TOmniValue;

  TOmniIteratorIntoDelegate = reference to procedure(const value: TOmniValue; var result: TOmniValue);
  TOmniIteratorIntoDelegate<T> = reference to procedure(const value: T; var result: TOmniValue);

  IOmniParallelAggregatorLoop = interface
    function  Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue; overload;
  end; { IOmniParallelAggregatorLoop }

  IOmniParallelAggregatorLoop<T> = interface
    function  Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue; overload;
  end; { IOmniParallelAggregatorLoop<T> }

  IOmniParallelIntoLoop = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate);
  end; { IOmniParallelIntoLoop }

  IOmniParallelIntoLoop<T> = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate<T>);
  end; { IOmniParallelIntoLoop<T> }

  IOmniParallelLoop = interface
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
    function  NoWait: IOmniParallelLoop;
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
    function  OnStop(stopCode: TProc): IOmniParallelLoop;
    function  PreserveOrder: IOmniParallelLoop;
  end; { IOmniParallelLoop }

  IOmniParallelLoop<T> = interface
    function  Aggregate(defaultAggregateValue: T;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop<T>; overload;
    function  NoWait: IOmniParallelLoop<T>;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>;
    function  PreserveOrder: IOmniParallelLoop<T>;
  end; { IOmniParallelLoop<T> }

  TEnumeratorDelegate = reference to function(var next: TOmniValue): boolean;
  TEnumeratorDelegate<T> = reference to function(var next: T): boolean;

  Parallel = class
    class function  ForEach(const enumerable: IOmniValueEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const enumerable: IEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const source: IOmniBlockingCollection): IOmniParallelLoop; overload;
    class function  ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop; overload;
    class function  ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop; overload;
    class function  ForEach(low, high: integer; step: integer = 1): IOmniParallelLoop<integer>; overload;
    class function  ForEach<T>(const enumerable: IOmniValueEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IOmniValueEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(enumerator: TEnumeratorDelegate<T>): IOmniParallelLoop<T>; overload;
    {$IFDEF OTL_ERTTI}
    class function  ForEach(const enumerable: TObject): IOmniParallelLoop; overload;
    class function  ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>; overload;
    {$ENDIF OTL_ERTTI}
    class procedure Join(const task1, task2: TOmniTaskDelegate); overload;
    class procedure Join(const task1, task2: TProc); overload;
    class procedure Join(const tasks: array of TOmniTaskDelegate); overload;
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

  TOmniDelegateEnumerator<T> = class(TOmniValueEnumerator)
  strict private
    odeDelegate: TEnumeratorDelegate<T>;
    odeValue   : T;
  public
    constructor Create(delegate: TEnumeratorDelegate<T>);
    function  GetCurrent: TOmniValue; override;
    function  MoveNext: boolean; override;
  end; { TOmniDelegateEnumerator }

  TOmniParallelLoopOption = (ploNoWait, ploPreserveOrder);
  TOmniParallelLoopOptions = set of TOmniParallelLoopOption;

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
  strict private
    oplAggregate        : TOmniValue;
    oplAggregator       : TOmniAggregatorDelegate;
    oplCancellationToken: IOmniCancellationToken;
    oplCountStopped     : IOmniResourceCount;
    oplDataManager      : TOmniDataManager;
    oplDelegateEnum     : TOmniDelegateEnumerator;
    oplIntoQueueIntf    : IOmniBlockingCollection;
    oplManagedProvider  : boolean;
    oplNumTasks         : integer;
    oplOnStop           : TProc;
    oplOptions          : TOmniParallelLoopOptions;
    oplSourceProvider   : TOmniSourceProvider;
  strict protected
    procedure DoOnStop;
    procedure InternalExecute(loopBody: TOmniIteratorDelegate);
    procedure InternalExecuteInto(loopBody: TOmniIteratorIntoDelegate);
    function  InternalExecuteAggregate(loopBody: TOmniIteratorAggregateDelegate): TOmniValue;
    procedure InternalExecuteTask(task: TOmniTaskDelegate);
    procedure SetAggregator(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate);
    procedure SetCancellationToken(const token: IOmniCancellationToken);
    procedure SetIntoQueue(const queue: IOmniBlockingCollection); overload;
    procedure SetNumTasks(taskCount: integer);
    procedure SetOnStop(stopDelegate: TProc);
    function  Stopped: boolean; inline;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean); overload;
    constructor Create(const enumerator: TEnumeratorDelegate); overload;
    destructor  Destroy; override;
    property Options: TOmniParallelLoopOptions read oplOptions write oplOptions;
  end; { TOmniParallelLoopBase }

  TOmniParallelLoop = class(TOmniParallelLoopBase, IOmniParallelLoop,
                                                   IOmniParallelAggregatorLoop,
                                                   IOmniParallelIntoLoop)
  public
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop; overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
    function  Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntoDelegate); overload;
    function  ForEach: IOmniParallelLoop;
    function  GetEnumerator: IOmniValueEnumerator;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
    function  NoWait: IOmniParallelLoop;
    function  NumTasks(taskCount: integer): IOmniParallelLoop;
    function  OnStop(stopCode: TProc): IOmniParallelLoop;
    function  PreserveOrder: IOmniParallelLoop;
  end; { TOmniParallelLoop }

  TOmniParallelLoop<T> = class(TOmniParallelLoopBase, IOmniParallelLoop<T>,
                                                      IOmniParallelAggregatorLoop<T>,
                                                      IOmniParallelIntoLoop<T>)
  strict private
    oplDelegateEnum: TOmniDelegateEnumerator<T>;
    oplEnumerator  : TEnumerator<T>;
  public
    constructor Create(const enumerator: TEnumeratorDelegate<T>); overload;
    constructor Create(const enumerator: TEnumerator<T>); overload;
    destructor  Destroy; override;
    function  Aggregate(defaultAggregateValue: T;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>; overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue; overload;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorIntoDelegate<T>); overload;
    function  ForEach: IOmniParallelLoop<T>;
    function  GetEnumerator: IOmniValueEnumerator; { TODO 1 -ogabr : of T? }
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop<T>; overload;
    function  NoWait: IOmniParallelLoop<T>;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>;
    function  PreserveOrder: IOmniParallelLoop<T>;
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

class function Parallel.ForEach(low, high: integer; step: integer): IOmniParallelLoop<integer>;
begin
  Result := TOmniParallelLoop<integer>.Create(CreateSourceProvider(low, high, step), true);
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

class function Parallel.ForEach(const source: IOmniBlockingCollection): IOmniParallelLoop;
begin
  Result := ForEach(source as IOmniValueEnumerable);
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
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enum: IOmniValueEnumerator):
  IOmniParallelLoop<T>;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := TOmniParallelLoop<T>.Create(CreateSourceProvider(enum), true);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>;
begin
  Result := Parallel.ForEach<T>(enumerable.GetEnumerator());
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enum);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>;
begin
  Result := Parallel.ForEach<T>(enumerable.GetEnumerator);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(CreateSourceProvider(enum), true );
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enumerable);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>;
begin
  Result := ForEach<T>(source as IOmniValueEnumerable);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(enumerator: TEnumeratorDelegate<T>):
  IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enumerator);
end; { Parallel.ForEach<T> }

class procedure Parallel.Join(const task1, task2: TOmniTaskDelegate);
begin
  Join([task1, task2]);
end; { Parallel.Join }

class procedure Parallel.Join(const tasks: array of TOmniTaskDelegate);
var
  countStopped: TOmniResourceCount;
  firstTask   : IOmniTaskControl;
  prevTask    : IOmniTaskControl;
  proc        : TOmniTaskDelegate;
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
  oplNumTasks := Environment.Process.Affinity.Count;
  oplSourceProvider := sourceProvider;
  oplManagedProvider := managedProvider;
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

constructor TOmniParallelLoopBase.Create(const enumerator: TEnumeratorDelegate);
begin
  oplDelegateEnum := TOmniDelegateEnumerator.Create(enumerator);
  Create(CreateSourceProvider(oplDelegateEnum), true);
end; { TOmniParallelLoopBase.Create }

destructor TOmniParallelLoopBase.Destroy;
begin
  if oplManagedProvider then
    FreeAndNil(oplSourceProvider);
  FreeAndNil(oplDelegateEnum);
  FreeAndNil(oplDataManager);
  {$IFDEF OTL_ERTTI}
  if oplEnumerable.AsObject <> nil then begin
    oplDestroy.Invoke(oplEnumerable, []);
    oplRttiContext.Free;
  end;
  {$ENDIF OTL_ERTTI}
  inherited;
end; { TOmniParallelLoopBase.Destroy }

procedure TOmniParallelLoopBase.DoOnStop;
begin
  if assigned(oplOnStop) then
    oplOnStop();
end; { TOmniParallelLoopBase.DoOnStop }

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorDelegate);
begin
  { TODO 5 -ogabr : Handle ploPreserveOrder }
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue: TOmniLocalQueue;
      value     : TOmniValue;
    begin
      localQueue := oplDataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          loopBody(value);
      finally FreeAndNil(localQueue); end;
      if oplCountStopped.Allocate = 0 then
        DoOnStop;
    end
  );
end; { TOmniParallelLoopBase.InternalExecute }

function TOmniParallelLoopBase.InternalExecuteAggregate(loopBody:
  TOmniIteratorAggregateDelegate): TOmniValue;
begin
  if ploNoWait in Options then
    raise Exception.Create('NoWait cannot be used with the Aggregate');

  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      aggregate : TOmniValue;
      localQueue: TOmniLocalQueue;
      value     : TOmniValue;
    begin
      aggregate := TOmniValue.Null;
      localQueue := oplDataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          oplAggregator(aggregate, loopBody(value));
      finally FreeAndNil(localQueue); end;
      if not assigned(task) then
        oplAggregate := aggregate
      else begin
        task.Lock.Acquire;
        try
          oplAggregator(oplAggregate, aggregate);
        finally task.Lock.Release; end;
      end;
      if oplCountStopped.Allocate = 0 then
        DoOnStop;
    end
  );

  Result := oplAggregate;
end; { TOmniParallelLoopBase.InternalExecuteAggregate }

procedure TOmniParallelLoopBase.InternalExecuteInto(loopBody: TOmniIteratorIntoDelegate);
begin
  Assert(assigned(oplIntoQueueIntf));
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue      : TOmniLocalQueue;
      outputBuffer_ref: TOmniOutputBuffer;
      position        : int64;
      result          : TOmniValue;
      value           : TOmniValue;
    begin
      oplDataManager.SetOutput(oplIntoQueueIntf);
      localQueue := oplDataManager.CreateLocalQueue;
      try
        outputBuffer_ref := oplDataManager.AllocateOutputBuffer;
        try
          localQueue.AssociateBuffer(outputBuffer_ref);
          result := TOmniValue.Null;
          { TODO 1 : make sure that one thread cannot overtake all others and start filling its output buffer }
          while (not Stopped) and localQueue.GetNext(position, value) do begin
            loopBody(value, result);
            if not result.IsEmpty then begin
              outputBuffer_ref.Submit(position, result);
              result := TOmniValue.Null;
            end;
          end;
        finally oplDataManager.ReleaseOutputBuffer(outputBuffer_ref); end;
      finally FreeAndNil(localQueue); end;
      if oplCountStopped.Allocate = 0 then begin
        oplDataManager.Flush;
        oplIntoQueueIntf.CompleteAdding;
        DoOnStop;
      end;
    end
  );
end; { TOmniParallelLoopBase.InternalExecuteInto }

procedure TOmniParallelLoopBase.InternalExecuteTask(task: TOmniTaskDelegate);
var
  dmOptions    : TOmniDataManagerOptions;
  iTask        : integer;
  lockAggregate: IOmniCriticalSection;
begin
  oplCountStopped := TOmniResourceCount.Create(oplNumTasks);
  dmOptions := [];
  if ploPreserveOrder in Options then
    Include(dmOptions, dmoPreserveOrder);
  oplDataManager := CreateDataManager(oplSourceProvider, oplNumTasks, dmOptions); // destructor will do the cleanup
  if ((oplNumTasks = 1) or (Environment.Thread.Affinity.Count = 1)) and (not (ploNoWait in Options)) then
    task(nil)
  else begin
    lockAggregate := CreateOmniCriticalSection;
    for iTask := 1 to oplNumTasks do
      CreateTask(task, 'Parallel.ForEach worker #' + IntToStr(iTask))
        .WithLock(lockAggregate)
        .Unobserved
        .Schedule;
    if not (ploNoWait in Options) then
      WaitForSingleObject(oplCountStopped.Handle, INFINITE);
  end;
end; { TOmniParallelLoopBase.InternalExecuteTask }

procedure TOmniParallelLoopBase.SetAggregator(defaultAggregateValue: TOmniValue;
  aggregator: TOmniAggregatorDelegate);
begin
  oplAggregator := aggregator;
  oplAggregate := defaultAggregateValue;
end; { TOmniParallelLoopBase.SetAggregator }

procedure TOmniParallelLoopBase.SetCancellationToken(const token: IOmniCancellationToken);
begin
  oplCancellationToken := token;
end; { TOmniParallelLoopBase.SetCancellationToken }

procedure TOmniParallelLoopBase.SetIntoQueue(const queue: IOmniBlockingCollection);
begin
  oplIntoQueueIntf := queue;
end; { TOmniParallelLoopBase.SetIntoQueue }

procedure TOmniParallelLoopBase.SetNumTasks(taskCount: integer);
begin
  Assert(taskCount > 0);
  oplNumTasks := taskCount;
end; { TOmniParallelLoopBase.SetNumTasks }

procedure TOmniParallelLoopBase.SetOnStop(stopDelegate: TProc);
begin
  oplOnStop := stopDelegate;
end; { TOmniParallelLoopBase.SetOnStop }

function TOmniParallelLoopBase.Stopped: boolean;
begin
  Result := (assigned(oplCancellationToken) and oplCancellationToken.IsSignalled);
end; { TOmniParallelLoopBase.Stopped }

{ TOmniParallelLoop }

function TOmniParallelLoop.Aggregate(defaultAggregateValue: TOmniValue;
  aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop;
begin
  SetAggregator(defaultAggregateValue, aggregator);
  Result := Self;
end; { TOmniParallelLoop.Aggregate }

function TOmniParallelLoop.CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
begin
  SetCancellationToken(token);
  Result := Self;
end; { TOmniParallelLoop.CancelWith }

function TOmniParallelLoop.Execute(loopBody: TOmniIteratorAggregateDelegate): TOmniValue;
begin
  Result := InternalExecuteAggregate(loopBody);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorDelegate);
begin
  InternalExecute(loopBody);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorIntoDelegate);
begin
  InternalExecuteInto(loopBody);
end; { TOmniParallelLoop.Execute }

function TOmniParallelLoop.ForEach: IOmniParallelLoop;
begin
  { TODO 1 -ogabr : implement }
  Result := Self;
end; { TOmniParallelLoop.ForEach }

function TOmniParallelLoop.GetEnumerator: IOmniValueEnumerator;
begin
  { TODO 1 -ogabr : implement }
  Result := nil;
end; { TOmniParallelLoop.GetEnumerator }

function TOmniParallelLoop.Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop;
begin
  SetIntoQueue(queue);
  Result := Self;
end; { TOmniParallelLoop.Into }

function TOmniParallelLoop.NoWait: IOmniParallelLoop;
begin
  Options := Options + [ploNoWait];
  Result := Self;
end; { TOmniParallelLoop.NoWait }

function TOmniParallelLoop.NumTasks(taskCount: integer): IOmniParallelLoop;
begin
  SetNumTasks(taskCount);
  Result := Self;
end; { TOmniParallelLoop.taskCount }

function TOmniParallelLoop.OnStop(stopCode: TProc): IOmniParallelLoop;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop.OnStop }

function TOmniParallelLoop.PreserveOrder: IOmniParallelLoop;
begin
  Options := Options + [ploPreserveOrder];
  Result := Self;
end; { TOmniParallelLoop.PreserveOrder }

{ TOmniParalleLoop<T> }

constructor TOmniParallelLoop<T>.Create(const enumerator: TEnumeratorDelegate<T>);
begin
  oplDelegateEnum := TOmniDelegateEnumerator<T>.Create(enumerator);
  Create(CreateSourceProvider(oplDelegateEnum), true);
end; { TOmniParallelLoop }

constructor TOmniParallelLoop<T>.Create(const enumerator: TEnumerator<T>);
begin
  oplEnumerator := enumerator;
  Create(
    function(var next: T): boolean
    begin
      Result := oplEnumerator.MoveNext;
      if Result then
        next := oplEnumerator.Current;
    end
  );
end; { TOmniParallelLoop<T>.Create }

destructor TOmniParallelLoop<T>.Destroy;
begin
  FreeAndNil(oplDelegateEnum);
  FreeAndNil(oplEnumerator);
  inherited;
end; { TOmniParallelLoop }

function TOmniParallelLoop<T>.Aggregate(defaultAggregateValue: T;
  aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>;
begin
  SetAggregator(TOmniValue.CastFrom<T>(defaultAggregateValue), aggregator);
  Result := Self;
end; { TOmniParallelLoop<T>.Aggregate }

function TOmniParallelLoop<T>.CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
begin
  SetCancellationToken(token);
  Result := Self;
end; { TOmniParallelLoop<T>.CancelWith }

function TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorAggregateDelegate<T>): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    function (const value: TOmniValue): TOmniValue
    begin
      Result := loopBody(value.CastAs<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorDelegate<T>);
begin
  InternalExecute(
    procedure (const value: TOmniValue)
    begin
      loopBody(value.CastAs<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorIntoDelegate<T>);
begin
  InternalExecuteInto(
    procedure (const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value.CastAs<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

function TOmniParallelLoop<T>.ForEach: IOmniParallelLoop<T>;
begin
  { TODO 1 -ogabr : implement }
  Result := Self;
end; { TOmniParallelLoop<T>.ForEach }

function TOmniParallelLoop<T>.GetEnumerator: IOmniValueEnumerator;
begin
  { TODO 1 -ogabr : implement }
  Result := nil;
end; { TOmniParallelLoop<T>.GetEnumerator }

function TOmniParallelLoop<T>.Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop<T>;
begin
  SetIntoQueue(queue);
  Result := Self;
end; { TOmniParallelLoop<T>.Into }

function TOmniParallelLoop<T>.NoWait: IOmniParallelLoop<T>;
begin
  Options := Options + [ploNoWait];
  Result := Self;
end; { TOmniParallelLoop<T>.NoWait }

function TOmniParallelLoop<T>.NumTasks(taskCount: integer): IOmniParallelLoop<T>;
begin
  SetNumTasks(taskCount);
  Result := Self;
end; { TOmniParallelLoop<T>.NumTasks }

function TOmniParallelLoop<T>.OnStop(stopCode: TProc): IOmniParallelLoop<T>;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop<T>.OnStop }

function TOmniParallelLoop<T>.PreserveOrder: IOmniParallelLoop<T>;
begin
  Options := Options + [ploPreserveOrder];
  Result := Self;
end; { TOmniParallelLoop<T>.PreserveOrder }

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

{ TOmniDelegateEnumerator<T> }

constructor TOmniDelegateEnumerator<T>.Create(delegate: TEnumeratorDelegate<T>);
begin
  odeDelegate := delegate;
end; { TOmniDelegateEnumerator }

function TOmniDelegateEnumerator<T>.GetCurrent: TOmniValue;
begin
  Result := TOmniValue.CastFrom<T>(odeValue);
end; { TOmniDelegateEnumerator }

function TOmniDelegateEnumerator<T>.MoveNext: boolean;
begin
  Result := odeDelegate(odeValue);
end; { TOmniDelegateEnumerator }

end.
