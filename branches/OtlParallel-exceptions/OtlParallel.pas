///<summary>High-level parallel execution management.
///    Part of the OmniThreadLibrary project. Requires Delphi 2009 or newer.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011 Primoz Gabrijelcic
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
///   Last modification : 2011-07-18
///   Version           : 1.13
///</para><para>
///   History:
///     1.13: 2011-07-18
///       - Added exception handling to Parallel.Join. Tasks' fatal exceptions are wrapped
///         in EJoinException and raised at the end of Parallel.Join method.
///       - Two version of Parallel.Async (the ones with explicit termination handlers)
///         were removed as this functionality can be achieved by using
///         Parallel.TaskConfig.OnTerminated.
///     1.12: 2011-07-17
///       - Added exception handling to IOmniFuture<T>. Tasks' fatal exception is raised
///         in .Value. New function .FatalException and .DetachException.
///     1.11: 2011-07-16
///       - GParallelPool and GPipelinePool are now initialized on the fly which allows
///         OtlParallel to be used inside a DLL.
///       - GParallelPool and GPipelinePool are now private and must be accessed with
///         global methods GlobalParallelPool and GlobalPipelinePool.
///     1.10a: 2011-06-25
///       - Bug fixed: Parallel.ForEach was never running on more than
///         Process.Affinity.Count tasks.
///     1.10: 2011-04-16
///       - Parallel.Join supports TaskConfig.
///       - Parallel.Future supports TaskConfig.
///       - Parallel.Pipeline supports TaskConfig.
///       - Parallel.ForEach supports TaskConfig.
///     1.09: 2011-04-06
///       - Implemented Parallel.ForkJoin.
///       - Implemented Parallel.Async.
///       - Implemented Parallel.TaskConfig.
///     1.08: 2011-03-09
///       - Faster IOmniFuture<T>.IsDone.
///     1.07a: 2011-02-15
///       - Compiles in Delphi 2009.
///     1.07: 2010-12-09
///       - Parallel.Join(TProc) executes one task in the current thread.
///       - Parallel.ForEach.NoWait runs on NumCores-1 tasks.
///       - Parallel.Pipeline throttling low watermark defaults to 1/4 of the high
///         watermark if pipeline runs on more tasks than there are available cores.
///     1.06: 2010-12-02
///       - Parallel.Pipeline implements Cancel method.
///       - Parallel.Pipeline stage delegate can accept additional parameter of type
///         IOmniTask. Stage can use it to check if the pipeline was cancelled.
///       - Implemented task state in ForEach (ForEach.Initialize.Finalize.Execute).
///     1.05c: 2010-11-25
///       - CompleteAdding is called only when all tasks for the stage have completed the
///         work.
///       - .NumTasks works correctly after .Stage().
///     1.05b: 2010-11-25
///       - Parallel.Pipeline uses its own thread pool with unlimited number of running
///         threads.
///     1.05a: 2010-11-22
///       - Two overloaded versions of Join added back. They were needed after all.
///       - Fixed bugs in Join implementation - thanks to Mason Wheeler for
///         the bug report.
///       - Parallel.Pipeline.Run returns output collection.
///       - Parallel.Pipeline.Throttle is fully implemented. Throttling level defaults to
///         10240 elements.
///     1.05: 2010-11-21
///       - OtlFutures functionality moved into this unit.
///       - Futures can be created by calling Parallel.Future<T>(action).
///       - GForEachPool renamed into GParallelPool and used for all Parallel
///         tasking.
///       - Two overloaded versions of Join removed.
///     1.04: 2010-11-20
///       - Small fix regarding setting GParallelPool.MaxExecuting.
///     1.04: 2010-07-22
///       - Introduced overloaded Execute methods with delegates that accept the task
///         interface parameter.
///       - Introduced OnTaskCreate hook and MonitorWith shorthand.
///     1.03: 2010-07-17
///       - ForEach tasks are scheduled in the specialized pool.
///     1.02: 2010-07-01
///       - Includes OTLOptions.inc.
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

unit OtlParallel;

{$I OtlOptions.inc}

interface

// TODO 3 -oPrimoz Gabrijelcic : Maybe we could use .Aggregate<T> where T is the aggregate type?
// TODO 3 -oPrimoz Gabrijelcic : Change .Aggregate to use .Into signature for loop body?
// TODO 1 -oPrimoz Gabrijelcic : How to combine Futures and NoWait version of Aggregate?
// TODO 5 -oPrimoz Gabrijelcic : Single-threaded access to a data source - how? (datasets etc)

// Notes for OTL 3
// - Parallel.ForEach should use task pool.
// - Task pool would dynamically schedule tasks over available cores.
// - Task pool would know how many different kinds of tasks are there (one per distinct
//   ForEach) and would balance load so that all different kinds of tasks would get executed.
// - ForEach would support .DegreeOfConcurrency (or something like that) which would
//   default to one meaning that one task can easily consume one core. Setting it to two
//   (it would be a real number) would mean that one task can only consume one half of a
//   core and that 2*<number of cores> is a good number of threads for this particular task.

uses
  SysUtils,
  {$IFDEF OTL_ERTTI}
  TypInfo,
  RTTI,
  {$ENDIF OTL_ERTTI}
  SyncObjs,
  Generics.Collections,
  GpLists,
  OtlCommon,
  OtlSync,
  OtlCollections,
  OtlTask,
  OtlTaskControl,
  OtlDataManager,
  OtlEventMonitor,
  OtlThreadPool;

const
  CDefaultPipelineThrottle = 10240;

type
  IOmniTaskConfig = interface
    procedure Apply(const task: IOmniTaskControl);
    function  CancelWith(const token: IOmniCancellationToken): IOmniTaskConfig;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskConfig;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskConfig; overload;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniMessageExec): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskConfig; overload;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskConfig;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskConfig; overload;
    function  WithLock(const lock: IOmniCriticalSection): IOmniTaskConfig; overload;
//    property Param: TOmniValueContainer read GetParam;
  end; { IOmniTaskConfig }

  IOmniParallelLoop = interface;
  IOmniParallelLoop<T> = interface;

  TOmniAggregatorDelegate = reference to procedure(var aggregate: TOmniValue; const value: TOmniValue);

  TOmniIteratorDelegate = reference to procedure(const value: TOmniValue);
  TOmniIteratorDelegate<T> = reference to procedure(const value: T);
  TOmniIteratorTaskDelegate = reference to procedure(const task: IOmniTask; const value: TOmniValue);
  TOmniIteratorTaskDelegate<T> = reference to procedure(const task: IOmniTask; const value: T);

  TOmniIteratorStateDelegate = reference to procedure(const value: TOmniValue; var taskState: TOmniValue);
  TOmniIteratorStateDelegate<T> = reference to procedure(const value: T; var taskState: TOmniValue);

  TOmniIteratorIntoDelegate = reference to procedure(const value: TOmniValue; var result: TOmniValue);
  TOmniIteratorIntoDelegate<T> = reference to procedure(const value: T; var result: TOmniValue);
  TOmniIteratorIntoTaskDelegate = reference to procedure(const task: IOmniTask; const value: TOmniValue; var result: TOmniValue);
  TOmniIteratorIntoTaskDelegate<T> = reference to procedure(const task: IOmniTask; const value: T; var result: TOmniValue);

  TOmniTaskCreateDelegate = reference to procedure(const task: IOmniTask);
  TOmniTaskControlCreateDelegate = reference to procedure(const task: IOmniTaskControl);

  TOmniTaskInitializerDelegate = reference to procedure(var taskState: TOmniValue);
  TOmniTaskFinalizerDelegate = reference to procedure(const taskState: TOmniValue);

  IOmniParallelAggregatorLoop = interface
    function  Execute(loopBody: TOmniIteratorIntoDelegate): TOmniValue;
  end; { IOmniParallelAggregatorLoop }

  IOmniParallelAggregatorLoop<T> = interface
    function  Execute(loopBody: TOmniIteratorIntoDelegate<T>): TOmniValue;
  end; { IOmniParallelAggregatorLoop<T> }

  IOmniParallelInitializedLoop = interface
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniParallelInitializedLoop;
    procedure Execute(loopBody: TOmniIteratorStateDelegate);
  end; { IOmniParallelInitializedLoop }

  IOmniParallelInitializedLoop<T> = interface
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniParallelInitializedLoop<T>;
    procedure Execute(loopBody: TOmniIteratorStateDelegate<T>);
  end; { IOmniParallelInitializedLoop }

  IOmniParallelIntoLoop = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate);
  end; { IOmniParallelIntoLoop }

  IOmniParallelIntoLoop<T> = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate<T>);
  end; { IOmniParallelIntoLoop<T> }

  IOmniParallelLoop = interface
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop;
    function  AggregateSum: IOmniParallelAggregatorLoop;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorTaskDelegate); overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniParallelInitializedLoop;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
    function  NoWait: IOmniParallelLoop;
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop;
    function  PreserveOrder: IOmniParallelLoop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop;
  end; { IOmniParallelLoop }

  IOmniParallelLoop<T> = interface
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>;
    function  AggregateSum: IOmniParallelAggregatorLoop<T>;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorTaskDelegate<T>); overload;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniParallelInitializedLoop<T>;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop<T>; overload;
    function  NoWait: IOmniParallelLoop<T>;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop<T>; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop<T>; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop<T>; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>;
    function  PreserveOrder: IOmniParallelLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop<T>;
  end; { IOmniParallelLoop<T> }

  TEnumeratorDelegate = reference to function(var next: TOmniValue): boolean;
  TEnumeratorDelegate<T> = reference to function(var next: T): boolean;

  TOmniFutureDelegate<T> = reference to function: T;
  TOmniFutureDelegateEx<T> = reference to function(const task: IOmniTask): T;

  IOmniFuture<T> = interface
    procedure Cancel;
    function  DetachException: Exception;
    function  FatalException: Exception;
    function  IsCancelled: boolean;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { IOmniFuture<T> }

  { TODO : maybe add Value(timeout_ms) }
  TOmniFuture<T> = class(TInterfacedObject, IOmniFuture<T>)
  strict private
    ofCancellable  : boolean;
    ofCancelled    : boolean;
    ofCompleted    : boolean;
    ofTaskException: Exception;
    ofResult       : T;
    ofTask         : IOmniTaskControl;
  strict protected
    procedure DestroyTask;
    procedure DetachExceptionFromTask;
    procedure Execute(action: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
  public
    constructor Create(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig = nil); // sadly, those two Creates cannot be overloaded as this crashes the compiler (internal error T888)
    constructor CreateEx(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig = nil);
    destructor  Destroy; override;
    procedure Cancel;
    function DetachException: Exception; inline;
    function  FatalException: Exception; inline;
    function  IsCancelled: boolean; inline;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { TOmniFuture<T> }

  EFutureError = class(Exception);
  EFutureCancelled = class(Exception);

  TPipelineStageDelegate = reference to procedure (const input, output:
    IOmniBlockingCollection);
  TPipelineStageDelegateEx = reference to procedure (const input, output:
    IOmniBlockingCollection; const task: IOmniTask);

  IOmniPipeline = interface
    procedure Cancel;
    function  Input(const queue: IOmniBlockingCollection): IOmniPipeline;
    function  NumTasks(numTasks: integer): IOmniPipeline;
    function  Run: IOmniBlockingCollection;
    function  Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Throttle(numEntries: integer; unblockAtCount: integer = 0): IOmniPipeline;
  end; { IOmniPipeline }

  TOmniForkJoinDelegate = reference to procedure;
  TOmniForkJoinDelegateEx = reference to procedure(const task: IOmniTask);

  TOmniForkJoinDelegate<T> = reference to function: T;
  TOmniForkJoinDelegateEx<T> = reference to function(const task: IOmniTask): T;

  IOmniCompute = interface ['{488441DC-D280-4F94-9B6A-296A19729DD5}']
    procedure Execute;
    function  IsDone: boolean;
    procedure Await;
  end; { IOmniCompute<T> }

  IOmniCompute<T> = interface ['{915A5BDB-ECAA-4928-B449-EFCB2311B28B}']
    procedure Execute;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { IOmniCompute<T> }

  TOmniCompute<T> = class(TInterfacedObject, IOmniCompute<T>)
  strict private
    ocAction  : TOmniForkJoinDelegate<T>;
    ocComputed: boolean;
    ocInput   : IOmniBlockingCollection;
    ocResult  : T;
  public
    constructor Create(action: TOmniForkJoinDelegate<T>; input: IOmniBlockingCollection);
    procedure Execute;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { TOmniCompute<T> }

  TOmniCompute = class(TInterfacedObject, IOmniCompute)
  strict private
    ocCompute: IOmniCompute<boolean>;
  public
    constructor Create(compute: IOmniCompute<boolean>);
    procedure Await;
    procedure Execute;
    function  IsDone: boolean;
  end; { TOmniCompute }

  IOmniForkJoin = interface
    function  Compute(action: TOmniForkJoinDelegate): IOmniCompute;
    function  NumTasks(numTasks: integer): IOmniForkJoin;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin;
  end; { IOmniForkJoin }

  IOmniForkJoin<T> = interface
    function  Compute(action: TOmniForkJoinDelegate<T>): IOmniCompute<T>;
    function  NumTasks(numTasks: integer): IOmniForkJoin<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin<T>;
  end; { IOmniForkJoin<T> }

  TOmniForkJoin<T> = class(TInterfacedObject, IOmniForkJoin<T>)
  strict private
    ofjNumTasks  : integer;
    ofjPoolInput : IOmniBlockingCollection;
    ofjTaskConfig: IOmniTaskConfig;
    ofjTaskPool  : IOmniPipeline;
  strict protected
    procedure Asy_ProcessComputations(const input, output: IOmniBlockingCollection);
    procedure StartWorkerTasks;
  public
    constructor Create;
    function  Compute(action: TOmniForkJoinDelegate<T>): IOmniCompute<T>;
    function  NumTasks(numTasks: integer): IOmniForkJoin<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin<T>;
  end; { TOmniForkJoin }

  TOmniForkJoin = class(TInterfacedObject, IOmniForkJoin)
  strict private
    ofjForkJoin: TOmniForkJoin<boolean>;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Compute(action: TOmniForkJoinDelegate): IOmniCompute;
    function  NumTasks(numTasks: integer): IOmniForkJoin;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin;
  end; { TOmniForkJoin }

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
    oplAggregate          : TOmniValue;
    oplAggregator         : TOmniAggregatorDelegate;
    oplCancellationToken  : IOmniCancellationToken;
    oplDataManager        : TOmniDataManager;
    oplDelegateEnum       : TOmniDelegateEnumerator;
    oplIntoQueueIntf      : IOmniBlockingCollection;
    oplManagedProvider    : boolean;
    oplNumTasks           : integer;
    oplNumTasksManual     : boolean;
    oplOnMessageList      : TGpIntegerObjectList;
    oplOnStop             : TProc;
    oplOnTaskControlCreate: TOmniTaskControlCreateDelegate;
    oplOnTaskCreate       : TOmniTaskCreateDelegate;
    oplOptions            : TOmniParallelLoopOptions;
    oplSourceProvider     : TOmniSourceProvider;
    oplTaskConfig         : IOmniTaskConfig;
    oplTaskFinalizer      : TOmniTaskFinalizerDelegate;
    oplTaskInitializer    : TOmniTaskInitializerDelegate;
  strict protected
    procedure DoOnStop;
    procedure InternalExecute(loopBody: TOmniIteratorDelegate); overload;
    procedure InternalExecute(loopBody: TOmniIteratorTaskDelegate); overload;
    procedure InternalExecute(loopBody: TOmniIteratorStateDelegate); overload;
    function  InternalExecuteAggregate(loopBody: TOmniIteratorIntoDelegate): TOmniValue; overload;
    function  InternalExecuteAggregate(loopBody: TOmniIteratorIntoTaskDelegate): TOmniValue; overload;
    procedure InternalExecuteInto(loopBody: TOmniIteratorIntoDelegate); overload;
    procedure InternalExecuteInto(loopBody: TOmniIteratorIntoTaskDelegate); overload;
    procedure InternalExecuteIntoOrdered(loopBody: TOmniIteratorIntoDelegate); overload;
    procedure InternalExecuteIntoOrdered(loopBody: TOmniIteratorIntoTaskDelegate); overload;
    procedure InternalExecuteTask(taskDelegate: TOmniTaskDelegate);
    procedure SetAggregator(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate);
    procedure SetAggregatorSum;
    procedure SetCancellationToken(const token: IOmniCancellationToken);
    procedure SetFinalizer(taskFinalizer: TOmniTaskFinalizerDelegate);
    procedure SetInitializer(taskInitializer: TOmniTaskInitializerDelegate);
    procedure SetIntoQueue(const queue: IOmniBlockingCollection); overload;
    procedure SetNumTasks(taskCount: integer);
    procedure SetOnMessage(eventDispatcher: TObject); overload;
    procedure SetOnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent); overload;
    procedure SetOnMessage(msgID: word; eventHandler: TOmniOnMessageFunction); overload;
    procedure SetOnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate); overload;
    procedure SetOnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate); overload;
    procedure SetOnStop(stopDelegate: TProc);
    procedure SetTaskConfig(const config: IOmniTaskConfig);
    function  Stopped: boolean; inline;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean); overload;
    constructor Create(const enumerator: TEnumeratorDelegate); overload;
    destructor  Destroy; override;
    property Options: TOmniParallelLoopOptions read oplOptions write oplOptions;
  end; { TOmniParallelLoopBase }

  TOmniParallelLoop = class(TOmniParallelLoopBase, IOmniParallelLoop,
                                                   IOmniParallelAggregatorLoop,
                                                   IOmniParallelInitializedLoop,
                                                   IOmniParallelIntoLoop)
  public
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop;
    function  AggregateSum: IOmniParallelAggregatorLoop;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
    function  ExecuteAggregate(loopBody: TOmniIteratorIntoDelegate): TOmniValue; overload;
    function  ExecuteAggregate(loopBody: TOmniIteratorIntoTaskDelegate): TOmniValue; overload;
    function  IOmniParallelAggregatorLoop.Execute = ExecuteAggregate;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorTaskDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntoDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntoTaskDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorStateDelegate); overload;
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate):
      IOmniParallelInitializedLoop;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate):
      IOmniParallelInitializedLoop;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
    function  NoWait: IOmniParallelLoop;
    function  NumTasks(taskCount: integer): IOmniParallelLoop;
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop;
    function  PreserveOrder: IOmniParallelLoop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop;
  end; { TOmniParallelLoop }

  TOmniParallelLoop<T> = class(TOmniParallelLoopBase, IOmniParallelLoop<T>,
                                                      IOmniParallelAggregatorLoop<T>,
                                                      IOmniParallelInitializedLoop<T>,
                                                      IOmniParallelIntoLoop<T>)
  strict private
    oplDelegateEnum: TOmniDelegateEnumerator<T>;
    oplEnumerator  : TEnumerator<T>;
  public
    constructor Create(const enumerator: TEnumeratorDelegate<T>); overload;
    constructor Create(const enumerator: TEnumerator<T>); overload;
    destructor  Destroy; override;
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>;
    function  AggregateSum: IOmniParallelAggregatorLoop<T>;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
    function  ExecuteAggregate(loopBody: TOmniIteratorIntoDelegate<T>): TOmniValue; overload;
    function  ExecuteAggregate(loopBody: TOmniIteratorIntoTaskDelegate<T>): TOmniValue; overload;
    function  IOmniParallelAggregatorLoop<T>.Execute = ExecuteAggregate;
    procedure Execute(loopBody: TOmniIteratorDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorTaskDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorIntoDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorIntoTaskDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorStateDelegate<T>); overload;
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate):
      IOmniParallelInitializedLoop<T>;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate):
      IOmniParallelInitializedLoop<T>;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop<T>; overload;
    function  NoWait: IOmniParallelLoop<T>;
    function  NumTasks(taskCount: integer): IOmniParallelLoop<T>;
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop<T>; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop<T>; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop<T>; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>;
    function  PreserveOrder: IOmniParallelLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop<T>;
  end; { TOmniParallelLoop<T> }

  EJoinException = class(Exception)
  strict private
    jeExceptions: TGpIntegerObjectList;
  public type
    TJoinInnerException = record
      FatalException: Exception;
      TaskNumber    : integer;
    end;
  strict protected
    function  GetInner(idxException: integer): TJoinInnerException;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;
    procedure Add(iTask: integer; taskException: Exception);
    function  Count: integer;
    property Inner[idxException: integer]: TJoinInnerException read GetInner; default;
  end; { EJoinException }

  {$REGION 'Documentation'}
  ///	<summary>Parallel class represents a base class for all high-level language
  ///	features in the OmniThreadLibrary. Most features are implemented as factories while
  ///	two (Async, Join) are implemented as a class procedures that does the real work.</summary>
  {$ENDREGION}
  Parallel = class
  public
  // ForEach
    ///	<summary>Creates parallel loop that iterates over IOmniValueEnumerable (for
    ///	example IOmniBlockingCollection).</summary>
    class function  ForEach(const enumerable: IOmniValueEnumerable): IOmniParallelLoop; overload;

    ///	<summary>Creates parallel loop that iterates over IOmniEnumerator (for example
    ///	IOmniBlockingCollection).</summary>
    class function  ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop; overload;

    ///	<summary>Creates parallel loop that iterates over IEnumerable.</summary>
    class function  ForEach(const enumerable: IEnumerable): IOmniParallelLoop; overload;
    class function  ForEach(const enum: IEnumerator): IOmniParallelLoop; overload;
    class function  ForEach(const source: IOmniBlockingCollection): IOmniParallelLoop; overload;
    class function  ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop; overload;
    class function  ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop; overload;
    class function  ForEach(low, high: integer; step: integer = 1): IOmniParallelLoop<integer>; overload;
    {$IFDEF OTL_ERTTI}
    class function  ForEach(const enumerable: TObject): IOmniParallelLoop; overload;
    {$ENDIF OTL_ERTTI}
    class function  ForEach<T>(const enumerable: IOmniValueEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IOmniValueEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>; overload;
    class function  ForEach<T>(enumerator: TEnumeratorDelegate<T>): IOmniParallelLoop<T>; overload;
    {$IFDEF OTL_ERTTI}
    class function  ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>; overload;
    {$ENDIF OTL_ERTTI}

  // Join
    class procedure Join(const task1, task2: TProc; taskConfig: IOmniTaskConfig = nil); overload;
    class procedure Join(const task1, task2: TOmniTaskDelegate; taskConfig: IOmniTaskConfig = nil); overload;
    class procedure Join(const tasks: array of TProc; taskConfig: IOmniTaskConfig = nil); overload;
    class procedure Join(const tasks: array of TOmniTaskDelegate; taskConfig: IOmniTaskConfig = nil); overload;

  // Future
    class function Future<T>(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;
    class function Future<T>(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;

  // Pipeline
    class function Pipeline: IOmniPipeline; overload;
    class function Pipeline(const stages: array of TPipelineStageDelegate;
      const input: IOmniBlockingCollection = nil): IOmniPipeline; overload;

  // Fork/Join
    class function ForkJoin: IOmniForkJoin; overload;
    class function ForkJoin<T>: IOmniForkJoin<T>; overload;

  // Async
    class procedure Async(task: TProc; taskConfig: IOmniTaskConfig = nil); overload;
    class procedure Async(task: TOmniTaskDelegate; taskConfig: IOmniTaskConfig = nil); overload;

  // task configuration
    class function TaskConfig: IOmniTaskConfig;
  end; { Parallel }

  {$REGION 'Documentation'}
  ///	<summary>A workaround used in TOmniForkJoin&lt;T&gt; to add work units into
  ///	blocking collection. Calling IOmniBlockinCollection.Add directly causes internal
  ///	compiler error.</summary>
  {$ENDREGION}
  procedure AddToBC(const queue: IOmniBlockingCollection; value: IInterface);

  {$REGION 'Documentation'}
  ///	<summary>Applies task configuration to a task. TaskConfig may be nil - in this case
  ///	nothing is done. Must be public or the code below wouldn't compile due to the
  ///	limitations of the compiler.</summary>
  {$ENDREGION}
  procedure ApplyConfig(const taskConfig: IOmniTaskConfig; const task: IOmniTaskControl);

  ///	<returns>Global pool used for almost all OtlParallel constructs with the excetion
  ///	of Parallel.Pipeline.</returns>
  function GlobalParallelPool: IOmniThreadPool;

  ///	<returns>Global pool used for Parallel.Pipeline tasks..</returns>
  function GlobalPipelinePool: IOmniThreadPool;

implementation

uses
  Windows,
  Classes,
  DSiWin32,
  GpStuff,
  OtlComm;

type
  IOmniPipelineStage = interface ['{C34393C7-E9EE-4CE7-895F-EECA553F4E54}']
    function  GetNumTasks: integer;
    function  GetTaskConfig: IOmniTaskConfig;
    function  GetThrottle: integer;
    function  GetThrottleLow: integer;
    function  GetThrottleLowSat: integer;
    procedure SetNumTasks(const value: integer);
    procedure SetThrottle(const value: integer);
    procedure SetThrottleLow(const value: integer);
    procedure SetThrottleLowSat(const value: integer);
  //
    procedure Execute(const inQueue, outQueue: IOmniBlockingCollection;
      const task: IOmniTask);
    property NumTasks: integer read GetNumTasks write SetNumTasks;
    property TaskConfig: IOmniTaskConfig read GetTaskConfig;
    property Throttle: integer read GetThrottle write SetThrottle;
    property ThrottleLow: integer read GetThrottleLow write SetThrottleLow;
    property ThrottleLowSat: integer read GetThrottleLowSat write SetThrottleLowSat;
  end; { IOmniPipelineStage }

  TOmniPipelineStage = class(TInterfacedObject, IOmniPipelineStage)
  strict private
    opsNumTasks      : integer;
    opsStage         : TPipelineStageDelegate;
    opsStageEx       : TPipelineStageDelegateEx;
    opsTaskConfig    : IOmniTaskConfig;
    opsThrottle      : integer;
    opsThrottleLow   : integer;
    opsThrottleLowSat: integer;
  protected
    function  GetNumTasks: integer;
    function  GetTaskConfig: IOmniTaskConfig;
    function  GetThrottle: integer;
    function  GetThrottleLow: integer;
    function  GetThrottleLowSat: integer;
    procedure SetNumTasks(const value: integer);
    procedure SetThrottle(const value: integer);
    procedure SetThrottleLow(const value: integer);
    procedure SetThrottleLowSat(const value: integer);
  public
    constructor Create(stage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig); overload;
    constructor Create(stage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig); overload;
    procedure Execute(const inQueue, outQueue: IOmniBlockingCollection;
      const task: IOmniTask);
    property NumTasks: integer read GetNumTasks write SetNumTasks;
    property TaskConfig: IOmniTaskConfig read GetTaskConfig;
    property Throttle: integer read GetThrottle write SetThrottle;
    property ThrottleLow: integer read GetThrottleLow write SetThrottleLow;
    property ThrottleLowSat: integer read GetThrottleLowSat write SetThrottleLowSat;
  end; { TOmniPipelineStage }

  TOmniPipeline = class(TInterfacedObject, IOmniPipeline)
  strict private
    opCancelWith    : IOmniCancellationToken;
    opCheckpoint    : integer;
    opInput         : IOmniBlockingCollection;
    opNumTasks      : integer;
    opOutQueues     : TInterfaceList;
    opStages        : TInterfaceList;
    opThrottle      : integer;
    opThrottleLow   : integer;
    opThrottleLowSat: integer;
  strict protected
    procedure AddSingleStage(const stage: IOmniPipelineStage);
    function GetStage(idxStage: integer): IOmniPipelineStage;
    property PipeStage[idxStage: integer]: IOmniPipelineStage read GetStage;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Cancel;
    function  Input(const queue: IOmniBlockingCollection): IOmniPipeline;
    { TODO 1 -ogabr : When running stages in parallel, additional work has to be done to ensure proper output order! }
    function  NumTasks(numTasks: integer): IOmniPipeline;
    function  Run: IOmniBlockingCollection;
    function  Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Throttle(numEntries: integer; unblockAtCount: integer = 0): IOmniPipeline;
  end; { TOmniPipeline }

  TOmniTaskConfig = class(TInterfacedObject, IOmniTaskConfig)
  strict private
    otcCancelWithToken         : IOmniCancellationToken;
    otcMonitorWithMonitor      : IOmniTaskControlMonitor;
    otcOnMessageEventDispatcher: TObject;
    otcOnMessageEventHandler   : TOmniTaskMessageEvent;
    otcOnMessageList           : TGpIntegerObjectList;
    otcOnTerminated            : TOmniTaskTerminatedEvent;
    otcOnTerminatedFunc        : TOmniOnTerminatedFunction;
    otcWithCounterCounter      : IOmniCounter;
    otcWithLockAutoDestroy     : boolean;
    otcWithLockOmniLock        : IOmniCriticalSection;
    otcWithLockSyncLock        : TSynchroObject;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Apply(const task: IOmniTaskControl);
    function  CancelWith(const token: IOmniCancellationToken): IOmniTaskConfig; inline;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskConfig; inline;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskConfig; overload; inline;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload; inline;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload; inline;
    function  OnMessage(msgID: word; eventHandler: TOmniMessageExec): IOmniTaskConfig; overload; inline;
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskConfig; overload; inline;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskConfig; overload;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskConfig; inline;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskConfig; overload; inline;
    function  WithLock(const lock: IOmniCriticalSection): IOmniTaskConfig; overload; inline;
  end; { TOmniTaskConfig }

var
  GParallelPool: IOmniThreadPool;
  GPipelinePool: IOmniThreadPool;

{ exports }

procedure AddToBC(const queue: IOmniBlockingCollection; value: IInterface);
begin
  queue.Add(value);
end; { AddToBC }

procedure ApplyConfig(const taskConfig: IOmniTaskConfig; const task: IOmniTaskControl);
begin
  if assigned(taskConfig) then
    taskConfig.Apply(task);
end; { ApplyConfig }

function GlobalParallelPool: IOmniThreadPool;
begin
  if not assigned(GParallelPool) then begin
    GParallelPool := CreateThreadPool('OtlParallel pool');
    GParallelPool.IdleWorkerThreadTimeout_sec := 60*1000; // 1 minute
  end;
  Result := GParallelPool;
end; { GlobalParallelPool }

function GlobalPipelinePool: IOmniThreadPool;
begin
  if not assigned(GPipelinePool) then begin
    GPipelinePool := CreateThreadPool('Parallel.Pipeline pool');
    GPipelinePool.MaxExecuting := -1;
  end;
  Result := GPipelinePool;
end; { GlobalPipelinePool }

{ EJoinException }

constructor EJoinException.Create;
begin
  inherited Create('');
  jeExceptions := TGpIntegerObjectList.Create(true);
end; { EJoinException.Create }

destructor EJoinException.Destroy;
begin
  FreeAndNil(jeExceptions);
  inherited;
end; { EJoinException.Destroy }

procedure EJoinException.Add(iTask: integer; taskException: Exception);
begin
  jeExceptions.AddObject(iTask, taskException);
end; { EJoinException.Add }

function EJoinException.Count: integer;
begin
  Result := jeExceptions.Count;
end; { EJoinException.Count }

function EJoinException.GetInner(idxException: integer): TJoinInnerException;
begin
  Result.FatalException := Exception(jeExceptions.Objects[idxException]);
  Result.TaskNumber := jeExceptions[idxException];
end; { EJoinException.GetInner }

{ Parallel }

class function Parallel.ForEach(const enumerable: IOmniValueEnumerable):
  IOmniParallelLoop;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class procedure Parallel.Async(task: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
begin
  Async(
    task,
    procedure begin end,
    taskConfig
  );
end; { Parallel.Async }

class procedure Parallel.Async(task: TProc; taskConfig: IOmniTaskConfig);
begin
  Async(
    procedure (const omniTask: IOmniTask)
    begin
      task;
    end,
    taskConfig
  );
end; { Parallel.Async }

class function Parallel.ForEach(low, high: integer; step: integer = 1):
  IOmniParallelLoop<integer>;
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

{$IFDEF OTL_ERTTI}
class function Parallel.ForEach(const enumerable: TObject): IOmniParallelLoop;
begin
  Result := TOmniParallelLoop.Create(enumerable);
end; { Parallel.ForEach }
{$ENDIF OTL_ERTTI}

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

{$IFDEF OTL_ERTTI}
class function Parallel.ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enumerable);
end; { Parallel.ForEach<T> }
{$ENDIF OTL_ERTTI}

class function Parallel.ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>;
begin
  Result := ForEach<T>(source as IOmniValueEnumerable);
end; { Parallel.ForEach<T> }

class function Parallel.ForEach<T>(enumerator: TEnumeratorDelegate<T>):
  IOmniParallelLoop<T>;
begin
  Result := TOmniParallelLoop<T>.Create(enumerator);
end; { Parallel.ForEach<T> }

class function Parallel.ForkJoin: IOmniForkJoin;
begin
  Result := TOmniForkJoin.Create;
end; { Parallel.ForkJoin }

class function Parallel.ForkJoin<T>: IOmniForkJoin<T>;
begin
  Result := TOmniForkJoin<T>.Create;
end; { Parallel.ForkJoin<T> }

class function Parallel.Future<T>(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig): IOmniFuture<T>;
begin
  Result := TOmniFuture<T>.Create(action, taskConfig);
end; { Parallel.Future<T> }

class function Parallel.Future<T>(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig): IOmniFuture<T>;
begin
  Result := TOmniFuture<T>.CreateEx(action, taskConfig);
end; { Parallel.Future<T> }

class procedure Parallel.Join(const task1, task2: TProc; taskConfig: IOmniTaskConfig);
begin
  Join([task1, task2], taskConfig);
end; { Parallel.Join }

class procedure Parallel.Join(const tasks: array of TProc; taskConfig: IOmniTaskConfig);
var
  countStopped : IOmniResourceCount;
  joinException: EJoinException;
  otlTasks     : array of IOmniTaskControl;

  function EnsureException: EJoinException;
  begin
    if not assigned(joinException) then
      joinException := EJoinException.Create;
    Result := joinException;
  end; { EnsureException }

  procedure Execute(iTask: integer);
  begin
    try
      tasks[iTask]();
    except
      on E: Exception do
        EnsureException.Add(iTask, AcquireExceptionObject);
    end;
  end; { Execute }

  procedure CreateJoinTask(iTask: integer);
  var
    proc   : TProc;
    intProc: integer absolute proc;
  begin
    proc := tasks[iTask];
    otlTasks[iTask] := CreateTask(
        procedure (const task: IOmniTask)
        begin
          try
            TOmniTaskDelegate(task.Param['Proc'].AsInteger)(task);
          finally // task may raise an exception
            countStopped.Allocate;
          end;
        end
      ).Unobserved
       .SetParameter('Proc', intProc);
    ApplyConfig(taskConfig, otlTasks[iTask]);
    otlTasks[iTask].Schedule(GlobalParallelPool);
  end; { CreateJoinTask }

var
  iProc         : integer;
  numInlineTasks: integer;

begin { Parallel.Join }
  Assert(SizeOf(integer) = SizeOf(TProc));
  joinException := nil;
  SetLength(otlTasks, Length(tasks));
  if ((Environment.Process.Affinity.Count = 1) or (Length(tasks) = 1)) and (not assigned(taskConfig)) then
  begin
    for iProc := Low(tasks) to High(tasks) do
      Execute(iProc);
  end
  else begin
    if assigned(taskConfig) then
      numInlineTasks := 0
    else
      numInlineTasks := 1;
    countStopped := TOmniResourceCount.Create(Length(tasks) - numInlineTasks);
    for iProc := Low(tasks)+numInlineTasks to High(tasks) do
      CreateJoinTask(iProc);
    if numInlineTasks = 1 then
      Execute(Low(tasks));
    WaitForSingleObject(countStopped.Handle, INFINITE);
    for iProc := Low(otlTasks) to High(otlTasks) do
      if assigned(otlTasks[iProc]) then begin
        otlTasks[iProc].WaitFor(INFINITE);
        if assigned(otlTasks[iProc].FatalException) then
          EnsureException.Add(iProc, otlTasks[iProc].DetachException);
      end;
  end;
  if assigned(joinException) then
    raise joinException;
end; { Parallel.Join }

class procedure Parallel.Join(const task1, task2: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
begin
  Join([task1, task2], taskConfig);
end; { Parallel.Join }

class procedure Parallel.Join(const tasks: array of TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
var
  joinException: EJoinException;

  function EnsureException: EJoinException;
  begin
    if not assigned(joinException) then
      joinException := EJoinException.Create;
    Result := joinException;
  end; { EnsureException }

var
  proc        : TOmniTaskDelegate;
  countStopped: IOmniResourceCount;
  firstTask   : IOmniTaskControl;
  intProc     : integer absolute proc;
  intTasks    : array of TOmniTaskDelegate;
  iProc       : integer;
  otlTasks    : array of IOmniTaskControl;
  prevTask    : IOmniTaskControl;

begin { EnsureException }
  Assert(SizeOf(integer) = SizeOf(TProc));
  joinException := nil;
  SetLength(otlTasks, Length(tasks));
  if (Environment.Process.Affinity.Count = 1) or (Length(tasks) = 1) then begin
    prevTask := nil;
    for iProc := Low(tasks) to High(tasks) do begin
      proc := tasks[iProc];
      otlTasks[iProc] := CreateTask(proc).Unobserved;
      ApplyConfig(taskConfig, otlTasks[iProc]);
      if assigned(prevTask) then
        prevTask.ChainTo(otlTasks[iProc]);
      prevTask := otlTasks[iProc];
      if not assigned(firstTask) then
        firstTask := otlTasks[iProc];
    end;
    if assigned(firstTask) then begin
      firstTask.Schedule;
      prevTask.WaitFor(INFINITE);
    end;
  end
  else begin
    countStopped := TOmniResourceCount.Create(Length(tasks));
    SetLength(intTasks, Length(tasks));
    for iProc := Low(tasks) to High(tasks) do begin
      proc := tasks[iProc];
      otlTasks[iProc] := CreateTask(
          procedure (const task: IOmniTask)
          begin
            try
              TOmniTaskDelegate(task.Param['Proc'].AsInteger)(task);
            finally
              countStopped.Allocate;
            end;
          end
        ).Unobserved
         .SetParameter('Proc', intProc);
      ApplyConfig(taskConfig, otlTasks[iProc]);
      otlTasks[iProc].Schedule(GlobalParallelPool);
    end;
    WaitForSingleObject(countStopped.Handle, INFINITE);
  end;
  for iProc := Low(otlTasks) to High(otlTasks) do
    if assigned(otlTasks[iProc]) then begin
      otlTasks[iProc].WaitFor(INFINITE);
      if assigned(otlTasks[iProc].FatalException) then
        EnsureException.Add(iProc, otlTasks[iProc].DetachException);
    end;
  if assigned(joinException) then
    raise joinException;
end; { Parallel.Join }

class function Parallel.Pipeline: IOmniPipeline;
begin
  Result := TOmniPipeline.Create;
end; { Parallel.Pipeline }

class function Parallel.Pipeline(const stages: array of TPipelineStageDelegate; const
  input: IOmniBlockingCollection): IOmniPipeline;
begin
  Result := Parallel.Pipeline
    .Input(input)
    .Stages(stages);
end; { Parallel.Pipeline }

class function Parallel.TaskConfig: IOmniTaskConfig;
begin
  Result := TOmniTaskConfig.Create;
end; { Parallel.TaskConfig }

{ TOmniParallelLoopBase }

constructor TOmniParallelLoopBase.Create(const sourceProvider: TOmniSourceProvider;
  managedProvider: boolean);
begin
  inherited Create;
  oplNumTasks := Environment.Process.Affinity.Count;
  oplSourceProvider := sourceProvider;
  oplManagedProvider := managedProvider;
  oplOnMessageList := TGpIntegerObjectList.Create;
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
  FreeAndNil(oplDataManager);
  FreeAndNil(oplOnMessageList);
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

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorTaskDelegate);
begin
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue: TOmniLocalQueue;
      value     : TOmniValue;
    begin
      localQueue := oplDataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          loopBody(task, value);
      finally FreeAndNil(localQueue); end;
    end
  );
end; { TOmniParallelLoopBase.InternalExecute }

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorDelegate);
begin
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue)
    begin
      loopBody(value);
    end
  );
end; { TOmniParallelLoopBase.InternalExecute }

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorStateDelegate);
begin
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue: TOmniLocalQueue;
      taskState : TOmniValue;
      value     : TOmniValue;
    begin
      oplTaskInitializer(taskState);
      localQueue := oplDataManager.CreateLocalQueue;
      try
        while (not Stopped) and localQueue.GetNext(value) do
          loopBody(value, taskState);
      finally FreeAndNil(localQueue); end;
      oplTaskFinalizer(taskState);
    end
  );
end; { TOmniParallelLoopBase.InternalExecute }

function TOmniParallelLoopBase.InternalExecuteAggregate(loopBody:
  TOmniIteratorIntoTaskDelegate): TOmniValue;
begin
  if ploNoWait in Options then
    raise Exception.Create('NoWait cannot be used with Aggregate');

  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      aggregate : TOmniValue;
      localQueue: TOmniLocalQueue;
      result    : TOmniValue;
      value     : TOmniValue;
    begin
      aggregate := TOmniValue.Null;
      localQueue := oplDataManager.CreateLocalQueue;
      try
        result.Clear;
        while (not Stopped) and localQueue.GetNext(value) do begin
          loopBody(task, value, result);
          if not result.IsEmpty then begin
            oplAggregator(aggregate, result);
            result.Clear;
          end;
        end;
      finally FreeAndNil(localQueue); end;
      if not assigned(task) then
        oplAggregate := aggregate
      else begin
        task.Lock.Acquire;
        try
          oplAggregator(oplAggregate, aggregate);
        finally task.Lock.Release; end;
      end;
    end
  );

  Result := oplAggregate;
end; { TOmniParallelLoopBase.InternalExecuteAggregate }

function TOmniParallelLoopBase.InternalExecuteAggregate(
  loopBody: TOmniIteratorIntoDelegate): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value, result);
    end
  );
end; { TOmniParallelLoopBase.InternalExecuteAggregate }

procedure TOmniParallelLoopBase.InternalExecuteInto(loopBody: TOmniIteratorIntoTaskDelegate);
begin
  Assert(assigned(oplIntoQueueIntf));
  if (ploPreserveOrder in Options) and (oplNumTasks > 1) then
    InternalExecuteIntoOrdered(loopBody)
  else // no order preservation; no output buffering required
    InternalExecuteTask(
      procedure (const task: IOmniTask)
      var
        localQueue: TOmniLocalQueue;
        result    : TOmniValue;
        value     : TOmniValue;
      begin
        localQueue := oplDataManager.CreateLocalQueue;
        try
          result.Clear;
          while (not Stopped) and localQueue.GetNext(value) do begin
            loopBody(task, value, result);
            if not result.IsEmpty then begin
              oplIntoQueueIntf.Add(result);
              result.Clear;
            end;
          end;
        finally FreeAndNil(localQueue); end;
      end
    );
end; { TOmniParallelLoopBase.InternalExecuteInto }

procedure TOmniParallelLoopBase.InternalExecuteInto(loopBody: TOmniIteratorIntoDelegate);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value, result);
    end
  );
end; { TOmniParallelLoopBase.InternalExecuteInto }

procedure TOmniParallelLoopBase.InternalExecuteIntoOrdered(
  loopBody: TOmniIteratorIntoTaskDelegate);
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
          while (not Stopped) and localQueue.GetNext(position, value) do begin
            loopBody(task, value, result);
            if not result.IsEmpty then begin
              outputBuffer_ref.Submit(position, result);
              result := TOmniValue.Null;
            end;
          end;
        finally oplDataManager.ReleaseOutputBuffer(outputBuffer_ref); end;
      finally FreeAndNil(localQueue); end;
    end
  );
end; { TOmniParallelLoopBase.InternalExecuteIntoOrdered }

procedure TOmniParallelLoopBase.InternalExecuteIntoOrdered(
  loopBody: TOmniIteratorIntoDelegate);
begin
  InternalExecuteIntoOrdered(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value, result);
    end
  );
end; { TOmniParallelLoopBase.InternalExecuteIntoOrdered }

procedure TOmniParallelLoopBase.InternalExecuteTask(taskDelegate: TOmniTaskDelegate);
var
  countStopped : IOmniResourceCount;
  dmOptions    : TOmniDataManagerOptions;
  iTask        : integer;
  kv           : TGpKeyValue;
  lockAggregate: IOmniCriticalSection;
  numTasks     : integer;
  task         : IOmniTaskControl;
begin
  dmOptions := [];
  numTasks := oplNumTasks;
  if ploPreserveOrder in Options then begin
    Include(dmOptions, dmoPreserveOrder);
    if (numTasks > 1) and (not oplNumTasksManual) then
      Dec(numTasks);
  end
  else if (ploNoWait in Options) and (numTasks > 1) and (not oplNumTasksManual) then
    Dec(numTasks);
  oplDataManager := CreateDataManager(oplSourceProvider, numTasks, dmOptions); // destructor will do the cleanup
  if ((numTasks = 1) or (Environment.Thread.Affinity.Count = 1)) and
     (not ((ploNoWait in Options) or assigned(oplOnTaskCreate) or assigned(oplOnTaskControlCreate)))
  then
    taskDelegate(nil)
  else begin
    countStopped := TOmniResourceCount.Create(numTasks + 1);
    lockAggregate := CreateOmniCriticalSection;
    { TODO 3 -oPrimoz : Still not optimal - should know how many Parallel.ForEach are currently executing! }
    if numTasks > GlobalParallelPool.MaxExecuting then
      GlobalParallelPool.MaxExecuting := numTasks;
    for iTask := 1 to numTasks do begin
      task := CreateTask(
        procedure (const task: IOmniTask)
        begin
          if assigned(oplOnTaskCreate) then
            oplOnTaskCreate(task);
          taskDelegate(task);
          if countStopped.Allocate = 1 then begin
            if ploNoWait in Options then begin
              if assigned(oplIntoQueueIntf) then
                oplIntoQueueIntf.CompleteAdding;
              DoOnStop;
            end;
            countStopped.Allocate;
          end;
        end,
        'Parallel.ForEach worker #' + IntToStr(iTask))
        .WithLock(lockAggregate)
        .Unobserved;
      ApplyConfig(oplTaskConfig, task);
      for kv in oplOnMessageList.WalkKV do
        task.OnMessage(kv.Key, TOmniMessageExec.Clone(TOmniMessageExec(kv.Value)));
      if assigned(oplOnTaskControlCreate) then
        oplOnTaskControlCreate(task);
      task.Schedule(GlobalParallelPool);
    end;
    if not (ploNoWait in Options) then begin
      WaitForSingleObject(countStopped.Handle, INFINITE);
      if assigned(oplIntoQueueIntf) then
        oplIntoQueueIntf.CompleteAdding;
      DoOnStop;
    end;
  end;
end; { TOmniParallelLoopBase.InternalExecuteTask }

procedure TOmniParallelLoopBase.SetAggregator(defaultAggregateValue: TOmniValue;
  aggregator: TOmniAggregatorDelegate);
begin
  oplAggregator := aggregator;
  oplAggregate := defaultAggregateValue;
end; { TOmniParallelLoopBase.SetAggregator }

procedure TOmniParallelLoopBase.SetAggregatorSum;
begin
  SetAggregator(0,
    procedure (var aggregate: TOmniValue; const value: TOmniValue)
    begin
      aggregate.AsInt64 := aggregate.AsInt64 + value.AsInt64;
    end
  );
end; { TOmniParallelLoopBase.SetAggregatorSum }

procedure TOmniParallelLoopBase.SetCancellationToken(const token: IOmniCancellationToken);
begin
  oplCancellationToken := token;
end; { TOmniParallelLoopBase.SetCancellationToken }

procedure TOmniParallelLoopBase.SetFinalizer(taskFinalizer: TOmniTaskFinalizerDelegate);
begin
  oplTaskFinalizer := taskFinalizer;
end; { TOmniParallelLoopBase.SetFinalizer }

procedure TOmniParallelLoopBase.SetInitializer(taskInitializer:
  TOmniTaskInitializerDelegate);
begin
  oplTaskInitializer := taskInitializer;
end; { TOmniParallelLoopBase.SetInitializer }

procedure TOmniParallelLoopBase.SetIntoQueue(const queue: IOmniBlockingCollection);
begin
  oplIntoQueueIntf := queue;
end; { TOmniParallelLoopBase.SetIntoQueue }

procedure TOmniParallelLoopBase.SetNumTasks(taskCount: integer);
begin
  Assert(taskCount > 0);
  oplNumTasks := taskCount;
  oplNumTasksManual := true;
end; { TOmniParallelLoopBase.SetNumTasks }

procedure TOmniParallelLoopBase.SetOnMessage(eventDispatcher: TObject);
begin
  oplOnMessageList.AddObject(COtlReservedMsgID, TOmniMessageExec.Create(eventDispatcher));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnMessage(msgID: word;
  eventHandler: TOmniTaskMessageEvent);
begin
  oplOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnMessage(msgID: word; eventHandler: TOmniOnMessageFunction);
begin
  oplOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnStop(stopDelegate: TProc);
begin
  oplOnStop := stopDelegate;
end; { TOmniParallelLoopBase.SetOnStop }

procedure TOmniParallelLoopBase.SetOnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate);
begin
  Assert(not assigned(oplOnTaskCreate));
  oplOnTaskCreate := taskCreateDelegate;
end; { TOmniParallelLoopBase.SetOnTaskCreate }

procedure TOmniParallelLoopBase.SetOnTaskCreate(
  taskCreateDelegate: TOmniTaskControlCreateDelegate);
begin
  Assert(not assigned(oplOnTaskControlCreate));
  oplOnTaskControlCreate := taskCreateDelegate;
end; { TOmniParallelLoopBase.SetOnTaskCreate }

procedure TOmniParallelLoopBase.SetTaskConfig(const config: IOmniTaskConfig);
begin
  oplTaskConfig := config;
end; { TOmniParallelLoopBase.SetTaskConfig }

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

function TOmniParallelLoop.AggregateSum: IOmniParallelAggregatorLoop;
begin
  SetAggregatorSum;
  Result := Self;
end; { TOmniParallelLoop.AggregateSum }

function TOmniParallelLoop.CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
begin
  SetCancellationToken(token);
  Result := Self;
end; { TOmniParallelLoop.CancelWith }

function TOmniParallelLoop.ExecuteAggregate(loopBody: TOmniIteratorIntoDelegate): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value, result);
    end
  );
end; { TOmniParallelLoop.ExecuteAggregate }

function TOmniParallelLoop.ExecuteAggregate(loopBody: TOmniIteratorIntoTaskDelegate): TOmniValue;
begin
  Result := InternalExecuteAggregate(loopBody);
end; { TOmniParallelLoop.ExecuteAggregate }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorDelegate);
begin
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue)
    begin
      loopBody(value);
    end
  );
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorTaskDelegate);
begin
  InternalExecute(loopBody);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorIntoDelegate);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value, result)
    end
  );
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorIntoTaskDelegate);
begin
  InternalExecuteInto(loopBody);
end; { TOmniParallelLoop.Execute }

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorStateDelegate);
begin
  InternalExecute(loopBody);
end; { TOmniParallelLoop.Execute }

function TOmniParallelLoop.Finalize(taskFinalizer: TOmniTaskFinalizerDelegate):
  IOmniParallelInitializedLoop;
begin
  SetFinalizer(taskFinalizer);
  Result := Self;
end; { TOmniParallelLoop.Finalize }

function TOmniParallelLoop.Initialize(taskInitializer: TOmniTaskInitializerDelegate):
  IOmniParallelInitializedLoop;
begin
  SetInitializer(taskInitializer);
  Result := Self;
end; { TOmniParallelLoop.Initialize }

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
  Assert(taskCount > 0);
  SetNumTasks(taskCount);
  Result := Self;
end; { TOmniParallelLoop.taskCount }

function TOmniParallelLoop.OnMessage(eventDispatcher: TObject): IOmniParallelLoop;
begin
  SetOnMessage(eventDispatcher);
  Result := Self;
end; { TOmniParallelLoop.OnMessage }

function TOmniParallelLoop.OnMessage(msgID: word;
  eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop;
begin
  SetOnMessage(msgID, eventHandler);
  Result := Self;
end; { TOmniParallelLoop.OnMessage }

function TOmniParallelLoop.OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop;
begin
  SetOnMessage(msgID, eventHandler);
  Result := Self;
end; { TOmniParallelLoop.OnMessage }

function TOmniParallelLoop.OnStop(stopCode: TProc): IOmniParallelLoop;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop.OnStop }

function TOmniParallelLoop.OnTaskCreate(
  taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop;
begin
  SetOnTaskCreate(taskCreateDelegate);
  Result := Self;
end; { TOmniParallelLoop.OnTaskCreate }

function TOmniParallelLoop.OnTaskCreate(
  taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop;
begin
  SetOnTaskCreate(taskCreateDelegate);
  Result := Self;
end; { TOmniParallelLoop.OnTaskCreate }

function TOmniParallelLoop.PreserveOrder: IOmniParallelLoop;
begin
  Options := Options + [ploPreserveOrder];
  Result := Self;
end; { TOmniParallelLoop.PreserveOrder }

function TOmniParallelLoop.TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop;
begin
  SetTaskConfig(config);
  Result := Self;
end; { TOmniParallelLoop.TaskConfig }

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

function TOmniParallelLoop<T>.Aggregate(defaultAggregateValue: TOmniValue;
  aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop<T>;
begin
  SetAggregator(defaultAggregateValue, aggregator);
  Result := Self;
end; { TOmniParallelLoop<T>.Aggregate }

function TOmniParallelLoop<T>.AggregateSum: IOmniParallelAggregatorLoop<T>;
begin
  SetAggregatorSum;
  Result := Self;
end; { TOmniParallelLoop<T>.AggregateSum }

function TOmniParallelLoop<T>.CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop<T>;
begin
  SetCancellationToken(token);
  Result := Self;
end; { TOmniParallelLoop<T>.CancelWith }

function TOmniParallelLoop<T>.ExecuteAggregate(loopBody: TOmniIteratorIntoDelegate<T>): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value.CastAs<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.ExecuteAggregate }

function TOmniParallelLoop<T>.ExecuteAggregate(
  loopBody: TOmniIteratorIntoTaskDelegate<T>): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(task, value.CastAs<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.ExecuteAggregate }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorDelegate<T>);
begin
  InternalExecute(
    procedure (const value: TOmniValue)
    begin
      loopBody(value.CastAs<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorTaskDelegate<T>);
begin
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue)
    begin
      loopBody(task, value.CastAs<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorIntoDelegate<T>);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value.CastAs<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorIntoTaskDelegate<T>);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(task, value.CastAs<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorStateDelegate<T>);
begin
  InternalExecute(
    procedure (const value: TOmniValue; var taskState: TOmniValue)
    begin
      loopBody(value.CastAs<T>, taskState);
    end
  );
end; { TOmniParallelLoop }

function TOmniParallelLoop<T>.Finalize(taskFinalizer: TOmniTaskFinalizerDelegate):
  IOmniParallelInitializedLoop<T>;
begin
  SetFinalizer(taskFinalizer);
  Result := Self;
end; { TOmniParallelLoop }

function TOmniParallelLoop<T>.Initialize(taskInitializer: TOmniTaskInitializerDelegate):
  IOmniParallelInitializedLoop<T>;
begin
  SetInitializer(taskInitializer);
  Result := Self;
end; { TOmniParallelLoop }

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

function TOmniParallelLoop<T>.OnMessage(eventDispatcher: TObject): IOmniParallelLoop<T>;
begin
  SetOnMessage(eventDispatcher);
  Result := Self;
end; { TOmniParallelLoop<T>.OnMessage }

function TOmniParallelLoop<T>.OnMessage(msgID: word;
  eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop<T>;
begin
  SetOnMessage(msgID, eventHandler);
  Result := Self;
end; { TOmniParallelLoop.OnMessage<T> }

function TOmniParallelLoop<T>.OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop<T>;
begin
  SetOnMessage(msgID, eventHandler);
  Result := Self;
end; { TOmniParallelLoop.OnMessage<T> }

function TOmniParallelLoop<T>.OnStop(stopCode: TProc): IOmniParallelLoop<T>;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop<T>.OnStop }

function TOmniParallelLoop<T>.OnTaskCreate(
  taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop<T>;
begin
  SetOnTaskCreate(taskCreateDelegate);
  Result := Self;
end; { TOmniParallelLoop<T>.OnTaskCreate }

function TOmniParallelLoop<T>.OnTaskCreate(
  taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop<T>;
begin
  SetOnTaskCreate(taskCreateDelegate);
  Result := Self;
end; { TOmniParallelLoop<T>.OnTaskCreate }

function TOmniParallelLoop<T>.PreserveOrder: IOmniParallelLoop<T>;
begin
  Options := Options + [ploPreserveOrder];
  Result := Self;
end; { TOmniParallelLoop<T>.PreserveOrder }

function TOmniParallelLoop<T>.TaskConfig(const config: IOmniTaskConfig):
  IOmniParallelLoop<T>;
begin
  SetTaskConfig(config);
  Result := Self;
end; { TOmniParallelLoop }

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

{ TOmniFuture<T> }

constructor TOmniFuture<T>.Create(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  ofCancellable := false;
  ofCompleted := false;
  Execute(
    procedure (const task: IOmniTask)
    begin
      try
        ofResult := action();
      finally // action may raise exception
        ofCompleted := true;
      end;
    end,
    taskConfig);
end; { TOmniFuture<T>.Create }

constructor TOmniFuture<T>.CreateEx(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  ofCancellable := true;
  ofCompleted := false;
  Execute(
    procedure (const task: IOmniTask)
    begin
      try
        ofResult := action(task);
      finally // action may raise exception
        ofCompleted := true;
      end;
    end,
    taskConfig);
end; { TOmniFuture<T>.CreateEx }

destructor TOmniFuture<T>.Destroy;
begin
  DestroyTask;
  FreeAndNil(ofTaskException);
  inherited;
end; { TOmniFuture<T>.Destroy }

procedure TOmniFuture<T>.Cancel;
begin
  if not ofCancellable then
    raise EFutureError.Create('Action cannot be cancelled');
  if not IsCancelled then begin
    ofCancelled := true;
    if assigned(ofTask) then
      ofTask.CancellationToken.Signal;
  end;
end; { TOmniFuture<T>.Cancel }

procedure TOmniFuture<T>.DestroyTask;
begin
  if assigned(ofTask) then begin
    ofTask.Terminate;
    ofTask := nil;
  end;
end; { TOmniFuture<T>.DestroyTask }

function TOmniFuture<T>.DetachException: Exception;
begin
  Result := FatalException; // this will in turn detach exception from task
  ofTaskException := nil;
end; { TOmniFuture }

procedure TOmniFuture<T>.DetachExceptionFromTask;
begin
  if IsDone and assigned(ofTask) and (not assigned(ofTaskException)) then begin
    ofTask.WaitFor(INFINITE); // task may not have terminated yet
    ofTaskException := ofTask.DetachException;
  end;
end; { TOmniFuture }

procedure TOmniFuture<T>.Execute(action: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
begin
  ofTask := CreateTask(action, 'TOmniFuture action');
  ApplyConfig(taskConfig, ofTask);
  ofTask.Schedule(GlobalParallelPool);
end; { TOmniFuture<T>.Execute }

function TOmniFuture<T>.FatalException: Exception;
begin
  DetachExceptionFromTask;
  Result := ofTaskException;
end; { TOmniFuture }

function TOmniFuture<T>.IsCancelled: boolean;
begin
  Result := ofCancelled;
end; { TOmniFuture<T>.IsCancelled }

function TOmniFuture<T>.IsDone: boolean;
begin
  Result := ofCompleted;
end; { TOmniFuture<T>.IsDone }

function TOmniFuture<T>.TryValue(timeout_ms: cardinal; var value: T): boolean;
var
  taskExcept: Exception;
begin
  Result := false;
  if ofCancelled then
    raise EFutureCancelled.Create('Action was cancelled');
  if assigned(ofTask) then begin
    if not ofTask.WaitFor(timeout_ms) then
      Exit;
    DetachExceptionFromTask;
    DestroyTask;
  end;
  if assigned(ofTaskException) then begin
    taskExcept := ofTaskException;
    ofTaskException := nil;
    raise taskExcept;
  end;
  value := ofResult;
  Result := true;
end; { TOmniFuture<T>.TryValue }

function TOmniFuture<T>.Value: T;
begin
  TryValue(INFINITE, Result);
end; { TOmniFuture<T>.Value }

{ TOmniPipelineStage }

constructor TOmniPipelineStage.Create(stage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  opsStage := stage;
  opsNumTasks := 1;
  opsTaskConfig := taskConfig;
end; { TOmniPipelineStage.Create }

constructor TOmniPipelineStage.Create(stage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  opsStageEx := stage;
  opsNumTasks := 1;
  opsTaskConfig := taskConfig;
end; { TOmniPipelineStage.Create }

procedure TOmniPipelineStage.Execute(const inQueue, outQueue: IOmniBlockingCollection;
  const task: IOmniTask);
begin
  // D2009 doesn't like TProc casts so we're casting to integer
  Assert(SizeOf(TProc) = SizeOf(integer));
  if PInteger(@opsStage)^ <> integer(nil) then begin
    Assert(PInteger(@opsStageEx)^ = integer(nil));
    opsStage(inQueue, outQueue);
  end
  else begin
    Assert(PInteger(@opsStageEx)^ <> integer(nil));
    opsStageEx(inQueue, outQueue, task);
  end;
end; { TOmniPipelineStage.Execute }

function TOmniPipelineStage.GetNumTasks: integer;
begin
  Result := opsNumTasks;
end; { TOmniPipelineStage.GetNumTasks }

function TOmniPipelineStage.GetTaskConfig: IOmniTaskConfig;
begin
  Result := opsTaskConfig;
end; { TOmniPipelineStage.GetTaskConfig }

function TOmniPipelineStage.GetThrottle: integer;
begin
  Result := opsThrottle;
end; { TOmniPipelineStage.GetThrottle }

function TOmniPipelineStage.GetThrottleLow: integer;
begin
  Result := opsThrottleLow;
end; { TOmniPipelineStage.GetThrottleLow }

function TOmniPipelineStage.GetThrottleLowSat: integer;
begin
  Result := opsThrottleLowSat;
end; { TOmniPipelineStage.GetThrottleLowSat }

procedure TOmniPipelineStage.SetNumTasks(const value: integer);
begin
  opsNumTasks := value;
end; { TOmniPipelineStage.SetNumTasks }

procedure TOmniPipelineStage.SetThrottle(const value: integer);
begin
  opsThrottle := value;
end; { TOmniPipelineStage.SetThrottle }

procedure TOmniPipelineStage.SetThrottleLow(const value: integer);
begin
  opsThrottleLow := value;
end; { TOmniPipelineStage.SetThrottleLow }

procedure TOmniPipelineStage.SetThrottleLowSat(const value: integer);
begin
  opsThrottleLowSat := value;
end; { TOmniPipelineStage.SetThrottleLowSat }

{ TOmniPipeline }

constructor TOmniPipeline.Create;
begin
  inherited Create;
  opStages := TInterfaceList.Create;
  opNumTasks := 1;
  opThrottle := CDefaultPipelineThrottle;
  opThrottleLow := Round(3/4 * opThrottle);
  opThrottleLowSat := Round(1/4 * opThrottle);
  opOutQueues := TInterfaceList.Create;
  opCancelWith := CreateOmniCancellationToken;
end; { TOmniPipeline.Create }

destructor TOmniPipeline.Destroy;
begin
  FreeAndNil(opOutQueues);
  FreeAndNil(opStages);
  inherited Destroy;
end; { TOmniPipeline.Destroy }

procedure TOmniPipeline.AddSingleStage(const stage: IOmniPipelineStage);
begin
  stage.NumTasks := opNumTasks;
  stage.Throttle := opThrottle;
  stage.ThrottleLow := opThrottleLow;
  stage.ThrottleLowSat := opThrottleLowSat;
  opStages.Add(stage);
end; { TOmniPipeline.AddSingleStage }

procedure TOmniPipeline.Cancel;
var
  outQueue: IInterface;
begin
  opCancelWith.Signal;
  for outQueue in opOutQueues do
    (outQueue as IOmniBlockingCollection).CompleteAdding;
end; { TOmniPipeline.Cancel }

function TOmniPipeline.GetStage(idxStage: integer): IOmniPipelineStage;
begin
  Result := (opStages[idxStage] as IOmniPipelineStage);
end; { TOmniPipeline.GetStage }

function TOmniPipeline.Input(const queue: IOmniBlockingCollection): IOmniPipeline;
begin
  opInput := queue;
  Result := Self;
end; { TOmniPipeline.Input }

function TOmniPipeline.NumTasks(numTasks: integer): IOmniPipeline;
var
  iStage: integer;
begin
  if opStages.Count = 0 then
    opNumTasks := numTasks
  else for iStage := opCheckpoint to opStages.Count - 1 do
    PipeStage[iStage].NumTasks := numTasks;
  Result := Self;
end; { TOmniPipeline.NumTasks }

function TOmniPipeline.Run: IOmniBlockingCollection;
var
  countStopped: IOmniResourceCount;
  inQueue     : IOmniBlockingCollection;
  iStage      : integer;
  iTask       : integer;
  outQueue    : IOmniBlockingCollection;
  stageName   : string;
  task        : IOmniTaskControl;
  totalTasks  : integer;
begin
  outQueue := opInput;
  totalTasks := 0;
  for iStage := 0 to opStages.Count - 1 do
    Inc(totalTasks, PipeStage[iStage].NumTasks);
  for iStage := 0 to opStages.Count - 1 do begin
    inQueue := outQueue;
    outQueue := TOmniBlockingCollection.Create;
    if totalTasks > Environment.Process.Affinity.Count then
      outQueue.SetThrottling(PipeStage[iStage].Throttle, PipeStage[iStage].ThrottleLowSat)
    else
      outQueue.SetThrottling(PipeStage[iStage].Throttle, PipeStage[iStage].ThrottleLow);
    opOutQueues.Add(outQueue);
    countStopped := TOmniResourceCount.Create(PipeStage[iStage].NumTasks);
    for iTask := 1 to PipeStage[iStage].NumTasks do begin
      stageName := Format('Pipeline stage #%d', [iStage]);
      if PipeStage[iStage].NumTasks > 1 then
        stageName := Format('%s worker %d', [stageName, iTask]);
      task := CreateTask(
          procedure (const task: IOmniTask)
          var
            inQueue    : IOmniBlockingCollection;
            opStage    : IOmniPipelineStage;
            outQueue   : IOmniBlockingCollection;
          begin
            inQueue := Task.Param['Input'].AsInterface as IOmniBlockingCollection;
            outQueue := Task.Param['Output'].AsInterface as IOmniBlockingCollection;
            opStage := Task.Param['Stage'].AsInterface as IOmniPipelineStage;
            opStage.Execute(inQueue, outQueue, Task);
            if (Task.Param['Stopped'].AsInterface as IOmniResourceCount).Allocate = 0 then
              outQueue.CompleteAdding;
          end,
          stageName
        )
        .Unobserved
        .CancelWith(opCancelWith)
        .SetParameter('Input', inQueue)
        .SetParameter('Stage', opStages[iStage])
        .SetParameter('Output', outQueue)
        .SetParameter('Stopped', countStopped)
        .SetParameter('Cancelled', opCancelWith);
      ApplyConfig((opStages[iStage] as IOmniPipelineStage).TaskConfig, task);
      task.Schedule(GlobalPipelinePool);
    end; //for iTask
  end; //for iStage
  Result := outQueue;
end; { TOmniPipeline.Run }

function TOmniPipeline.Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig): IOmniPipeline;
begin
  AddSingleStage(TOmniPipelineStage.Create(pipelineStage, taskConfig));
  opCheckpoint := opStages.Count - 1;
  Result := Self;
end; { TOmniPipeline.Stage }

function TOmniPipeline.Stage(pipelineStage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig): IOmniPipeline;
begin
  AddSingleStage(TOmniPipelineStage.Create(pipelineStage, taskConfig));
  opCheckpoint := opStages.Count - 1;
  Result := Self;
end; { TOmniPipeline.Stage }

function TOmniPipeline.Stages(const pipelineStages: array of TPipelineStageDelegate; taskConfig: IOmniTaskConfig):
  IOmniPipeline;
var
  oneStage: TPipelineStageDelegate;
begin
  Assert(Length(pipelineStages) > 0);
  opCheckpoint := opStages.Count;
  for oneStage in pipelineStages do
    AddSingleStage(TOmniPipelineStage.Create(oneStage, taskConfig));
  Result := Self;
end; { TOmniPipeline.Stages }

function TOmniPipeline.Stages(
  const pipelineStages: array of TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig): IOmniPipeline;
var
  oneStage: TPipelineStageDelegateEx;
begin
  Assert(Length(pipelineStages) > 0);
  opCheckpoint := opStages.Count;
  for oneStage in pipelineStages do
    AddSingleStage(TOmniPipelineStage.Create(oneStage, taskConfig));
  Result := Self;
end; { TOmniPipeline.Stages }

function TOmniPipeline.Throttle(numEntries: integer; unblockAtCount: integer): IOmniPipeline;
var
  iStage              : integer;
  throttleLow         : integer;
  throttleLowSaturated: integer;
begin
  Assert(unblockAtCount < numEntries);
  if unblockAtCount = 0 then begin
    throttleLow := Round(3/4 * numEntries);
    throttleLowSaturated := Round(1/4 * numEntries);
  end
  else begin
    throttleLow := unblockAtCount;
    throttleLowSaturated := unblockAtCount;
  end;
  if opStages.Count = 0 then begin
    opThrottle := numEntries;
    opThrottleLow := throttleLow;
    opThrottleLowSat := throttleLowSaturated;
  end
  else for iStage := opCheckpoint to opStages.Count - 1 do begin
    PipeStage[iStage].Throttle := numEntries;
    PipeStage[iStage].ThrottleLow := throttleLow;
    PipeStage[iStage].ThrottleLowSat := throttleLowSaturated;
  end;
  Result := Self;
end; { TOmniPipeline.Throttle }

{ TOmniCompute<T> }

constructor TOmniCompute<T>.Create(action: TOmniForkJoinDelegate<T>;
  input: IOmniBlockingCollection);
begin
  inherited Create;
  ocAction := action;
  ocInput := input;
end; { TOmniCompute<T>.Create }

procedure TOmniCompute<T>.Execute;
begin
  Assert(not ocComputed);
  ocResult := ocAction;
  ocComputed := true;
end; { TOmniCompute }

function TOmniCompute<T>.IsDone: boolean;
begin
  Result := ocComputed;
end; { TOmniCompute }

function TOmniCompute<T>.TryValue(timeout_ms: cardinal; var value: T): boolean;
var
  compute: TOmniValue;
begin
  Result := false;
  while not ocComputed do begin
    if ocInput.Take(compute) then
      (compute.AsInterface as IOmniCompute<T>).Execute
    else
      DSiYield;
  end;
  value := ocResult;
  Result := true;
end; { TOmniCompute<T>.TryValue }

function TOmniCompute<T>.Value: T;
begin
  TryValue(INFINITE, Result);
end; { TOmniCompute<T>.Value }

{ TOmniCompute }

constructor TOmniCompute.Create(compute: IOmniCompute<boolean>);
begin
  inherited Create;
  ocCompute := compute;
end; { TOmniCompute.Create }

procedure TOmniCompute.Await;
begin
  ocCompute.Value;
end; { TOmniCompute.Await }

procedure TOmniCompute.Execute;
begin
  ocCompute.Execute;
end; { TOmniCompute.Execute }

function TOmniCompute.IsDone: boolean;
begin
  Result := ocCompute.IsDone;
end; { TOmniCompute.IsDone }

{ TOmniForkJoin }

procedure TOmniForkJoin<T>.Asy_ProcessComputations(const input, output:
  IOmniBlockingCollection);
var
  computation: TOmniValue;
begin
  for computation in input do
    (computation.AsInterface as IOmniCompute<T>).Execute;
end; { TOmniForkJoin }

function TOmniForkJoin<T>.Compute(action: TOmniForkJoinDelegate<T>): IOmniCompute<T>;
var
  intf: IInterface;
begin
  StartWorkerTasks;
  Result := TOmniCompute<T>.Create(action, ofjPoolInput);
  AddToBC(ofjPoolInput, Result);
end; { TOmniForkJoin<T>.Compute }

constructor TOmniForkJoin<T>.Create;
begin
  inherited Create;
  ofjNumTasks := Environment.Process.Affinity.Count - 1;
end; { TOmniForkJoin<T>.Create }

function TOmniForkJoin<T>.NumTasks(numTasks: integer): IOmniForkJoin<T>;
begin
  ofjNumTasks := numTasks;
  Result := Self;
end; { TOmniForkJoin<T>.NumTasks }

procedure TOmniForkJoin<T>.StartWorkerTasks;
begin
  if not assigned(ofjTaskPool) then begin
    //Use pipeline with one parallelized stage as a simple task pool.
    ofjPoolInput := TOmniBlockingCollection.Create(ofjNumTasks+1);
    if ofjNumTasks > 0 then begin
      ofjTaskPool := Parallel.Pipeline
        .NumTasks(ofjNumTasks)
        .Input(ofjPoolInput)
        .Stage(Asy_ProcessComputations, ofjTaskConfig);
      ofjTaskPool.Run;
    end;
  end;
end; { TOmniForkJoin<T.StartWorkerTasks }

function TOmniForkJoin<T>.TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin<T>;
begin
  ofjTaskConfig := config;
  Result := Self;
end; { TOmniForkJoin }

{ TOmniForkJoin }

constructor TOmniForkJoin.Create;
begin
  inherited Create;
  ofjForkJoin := TOmniForkJoin<boolean>.Create;
end; { TOmniForkJoin.Create }

destructor TOmniForkJoin.Destroy;
begin
  FreeAndNil(ofjForkJoin);
  inherited;
end; { TOmniForkJoin.Destroy }

function TOmniForkJoin.Compute(action: TOmniForkJoinDelegate): IOmniCompute;
begin
  Result := TOmniCompute.Create(
    ofjForkJoin.Compute(
      function: boolean
      begin
        action;
        Result := true;
      end
    )
  );
end; { TOmniForkJoin.Compute }

function TOmniForkJoin.NumTasks(numTasks: integer): IOmniForkJoin;
begin
  ofjForkJoin.NumTasks(numTasks);
  Result := self;
end; { TOmniForkJoin.NumTasks }

function TOmniForkJoin.TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin;
begin
  ofjForkJoin.TaskConfig(config);
  Result := Self;
end; { TOmniForkJoin.TaskConfig }

{ TOmniTaskConfig }

constructor TOmniTaskConfig.Create;
begin
  inherited Create;
  otcOnMessageList := TGpIntegerObjectList.Create(true);
end; { TOmniTaskConfig.Create }

destructor TOmniTaskConfig.Destroy;
begin
  FreeAndNil(otcOnMessageList);
  inherited Destroy;
end; { TOmniTaskConfig.Destroy }

procedure TOmniTaskConfig.Apply(const task: IOmniTaskControl);
var
  kv: TGpKeyValue;
begin
  if assigned(otcCancelWithToken) then
    task.CancelWith(otcCancelWithToken);
  if assigned(otcMonitorWithMonitor) then
    task.MonitorWith(otcMonitorWithMonitor);
  if assigned(otcOnMessageEventDispatcher) then
    task.OnMessage(otcOnMessageEventDispatcher);
  if assigned(otcOnMessageEventHandler) then
    task.OnMessage(otcOnMessageEventHandler);
  for kv in otcOnMessageList.WalkKV do
    TOmniMessageExec(kv.Value).Apply(kv.Key, task);
  if assigned(otcOnTerminated) then
    task.OnTerminated(otcOnTerminated);
  if assigned(otcOnTerminatedFunc) then
    task.OnTerminated(otcOnTerminatedFunc);
  if assigned(otcWithCounterCounter) then
    task.WithCounter(otcWithCounterCounter);
  if assigned(otcWithLockOmniLock) then
    task.WithLock(otcWithLockOmniLock);
  if assigned(otcWithLockSyncLock) then
    task.WithLock(otcWithLockSyncLock, otcWithLockAutoDestroy);
end; { TOmniTaskConfig.Apply }

function TOmniTaskConfig.OnMessage(eventDispatcher: TObject): IOmniTaskConfig;
begin
  otcOnMessageEventDispatcher := eventDispatcher;
  Result := Self;
end; { TOmniTaskConfig.OnMessage }

function TOmniTaskConfig.CancelWith(const token: IOmniCancellationToken): IOmniTaskConfig;
begin
  otcCancelWithToken := token;
  Result := Self;
end; { TOmniTaskConfig.CancelWith }

function TOmniTaskConfig.MonitorWith(const monitor: IOmniTaskControlMonitor):
  IOmniTaskConfig;
begin
  otcMonitorWithMonitor := monitor;
  Result := Self;
end; { TOmniTaskConfig.MonitorWith }

function TOmniTaskConfig.OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig;
begin
  otcOnMessageEventHandler := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnMessage }

function TOmniTaskConfig.OnMessage(msgID: word; eventHandler: TOmniMessageExec):
  IOmniTaskConfig;
begin
  otcOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
  Result := Self;
end; { TOmniTaskConfig.OnMessage }

function TOmniTaskConfig.OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent):
  IOmniTaskConfig;
begin
  otcOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
  Result := Self;
end; { TOmniTaskConfig.OnMessage }

function TOmniTaskConfig.OnTerminated(eventHandler: TOmniTaskTerminatedEvent):
  IOmniTaskConfig;
begin
  otcOnTerminated := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnTerminated }

function TOmniTaskConfig.OnTerminated(eventHandler: TOmniOnTerminatedFunction):
  IOmniTaskConfig;
begin
  otcOnTerminatedFunc := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnTerminated }

function TOmniTaskConfig.WithCounter(const counter: IOmniCounter): IOmniTaskConfig;
begin
  otcWithCounterCounter := counter;
  Result := Self;
end; { TOmniTaskConfig.WithCounter }

function TOmniTaskConfig.WithLock(const lock: IOmniCriticalSection): IOmniTaskConfig;
begin
  otcWithLockOmniLock := lock;
  Result := Self;
end; { TOmniTaskConfig.WithLock }

function TOmniTaskConfig.WithLock(const lock: TSynchroObject; autoDestroyLock: boolean =
  true): IOmniTaskConfig;
begin
  otcWithLockSyncLock := lock;
  otcWithLockAutoDestroy := autoDestroyLock;
  Result := Self;
end; { TOmniTaskConfig.WithLock }

end.
