///<summary>High-level parallel execution management.
///    Part of the OmniThreadLibrary project. Requires Delphi 2009 or newer.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2017 Primoz Gabrijelcic
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
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://plus.google.com/communities/112307748950248514961
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : Sean B. Durkin
///   Creation date     : 2010-01-08
///   Last modification : 2017-07-05
///   Version           : 1.52a
///</para><para>
///   History:
///     1.52a: 2017-07-05
///       - IOmniParallelLoop<T>.OnStopInvoke and IOmniParallelMapper<T1, T2> is removed
///         for pre-XE7 compilers because of compiler bugs. This removes
///         Paralell.Map.OnStopInvoke and Paralell.ForEach.OnStopInvoke support for
///         pre-XE7 compilers.
///     1.52: 2017-07-04
///       - Added IOmniTaskConfig.NoThreadPool. This allows high-level abstractions to
///         bypass thread pool entirely and run in 'non-pooled' threads.
///     1.51: 2017-06-21
///       - Added OnStop overload that accepts 'reference to procedure (const task: IOmniTask)'
///         to Parallel.Join and Parallel.ParallelTask.
///       - Added OnStopInvoke to all abstractions that implement OnStop method.
///       - Fixed: Parallel.Future did not create task with the .Unobserved qualifier.
///     1.50: 2017-06-11
///       - Small tweaks in TOmniTimedTask implementation.
///     1.49b: 2017-04-06
///       - Compiles with Delphi 10.2 Tokyo.
///       - GParallelPool.IdleWorkerThreadTimeout_sec was incorrectly set to 60.000 seconds
///         instead of 60 seconds. [issue #93]
///     1.49a: 2017-02-03
///       - If a future's cancellation token is signalled before the future is even
///         created, the future worker is not started at all. [issue #85]
///     1.49: 2017-02-02
///       - Added property IOmniWorkItem.SkipCompletionHandler.
///         If it is set to True when work item is created or during its execution,
///         request handlers for that work item won't be called.
///         If it is set to True in the OnRequestDone_Asy handler, then only
///         OnRequestDone handler won't be called.
///     1.48: 2017-01-31
///       - Implemented IOmniBackgroundWorker.OnStop.
///     1.47: 2016-11-08
///       - Added function IOmniPipeline.NoThrottling which disables throttling on an
///         entire pipeline or one of its stages.
///     1.46: 2016-10-17
///       - Implemented Parallel.TimedTask.
///     1.45: 2016-04-21
///       - Parallel.For<T>(const arr: TArray<T>) is not available on 2009 and 2010
///         because generics support is not good enough in these two compilers.
///     1.44: 2016-01-14
///       - Implemented EJoinException.DetachInner.
///     1.43: 2015-12-16
///       - Implemented Parallel.For<T>(const arr: TArray<T>).
///     1.42: 2015-12-14
///       - Added DetachException, FatalException, and IsExceptional to IOmniParallelTask.
///     1.41: 2015-10-04
///       - Imported mobile support by [Sean].
///     1.40: 2015-09-04
///       - TOmniPipeline.Destroy calls TOmniPipeline.Cancel so a pipeline can be shut
///         down if user forgets to call Input.CompleteAdding.
///     1.39a: 2015-09-03
///       - IOmniPipeline.PipelineStage[].Input and .Output are now always available
///         immediately after the IOmniPipeline.Run.
///     1.39: 2015-02-17
///       - Corrected Parallel.For execution for negative steps.
///       - Implemented Parallel.For.CancelWith.
///     1.38: 2015-02-04
///       - NumTasks parameter can be negative. In that case, specified number of cores
///         will be reserved for other purposes and all other will be used for processing.
///         Example: If NumTasks(-2) is used when process has access to 8 cores,
///         6 of them (8 - 2) will be used to run the task.
///     1.37: 2015-01-30
///       - Implemented Parallel.Map.
///       - Task finalizers in Parallel.For were not called.
///       - Implemented Parallel.For.WaitFor.
///     1.36: 2014-09-27
///       - Implemented simple and fast Parallel.&For which supports only integer ranges.
///     1.35: 2014-07-03
///       - Added overloaded Execute methods to IOmniParallelInitializedLoop and
///         IOmniParallelInitializedLoop<T> so that IOmniTask parameter can be passed
///         to the executor.
///     1.34a: 2014-03-13
///       - Fixed race condition in IOmniPipeline termination code.
///     1.34: 2014-01-08
///       - Added SetPriority function to the IOmniTaskConfig.
///     1.33: 2013-10-14
///       - Different thread pool can be specified for all operations via the new
///         TaskConfig.ThreadPool function.
///       - Included stability fixes by [Tommaso Ercole].
///     1.32: 2013-10-13
///       - Removed optimization which caused ForEach to behave differently on
///         uniprocessor computers.
///     1.31b: 2013-07-02
///       - Simple pipline stage handles exceptions in the executor function.
///     1.31a: 2013-03-10
///       - ForEach destructor waits for all internal tasks to be stopped before the
///         object is destroyed.
///     1.31: 2013-02-21
///       - Implemented IOmniPipeline.PipelineStage[] property returning Input/Ouput
///         collections of a specific stage.
///     1.30: 2012-10-03
///       - Added Async/Await abstraction.
///     1.29: 2012-08-12
///       - IOmniBackgroundWorker extended with task initializer (Initialize) and
///         task finalizer (Finalize).
///       - IOmniWorkItem extended with property TaskState.
///       - Inlined bunch of TOmniWorkItem methods.
///     1.28: 2012-07-03
///       - Added OnStop overload to Parallel.Pipeline that accepts
///         'reference to procedure (const task: IOmniTask)'.
///     1.27: 2012-06-09
///       - Added OnStop overload to Parallel.ForEach that accepts
///         'reference to procedure (const task: IOmniTask)'.
///     1.26c: 2012-06-06
///       - ForEach finalizer is called if an exception occurs inside the ForEach task.
///       - Marked IOmniParallelLoop.OnMessage as deprecated.
///     1.26b: 2012-06-05
///       - Invalid 'joinState' was passed to the worker task in Parallel.Join if number
///         of tasks to be executed was larger than the number of worker threads.
///     1.26a: 2012-06-03
///       - Parallel.Join was broken if number of task to be executed was larger than
///         the number of worker threads.
///     1.26: 2012-03-31
///       - Task property added to the IOmniWorkItem interface.
///       - Fixed overloaded OnMessage declaration in the IOmniTaskConfig interface.
///     1.25: 2012-03-26
///       - Parallel.Pipeline implements OnStop.
///     1.24b: 2012-03-21
///       - IOmniJoinState.Task was not correctly set in TOmniParallelJoin.Execute.
///         Thanks to [Mayjest] for reproducible test case.
///     1.24a: 2012-02-23
///       - Exception handling in Async works correctly if Async has OnTerminated
///         configured.
///     1.24: 2012-02-20
///       - Async re-raises task exception in OnTerminated handler.
///     1.23a: 2011-12-09
///       - Removed unused global variable GPipelinePool.
///     1.23: 2011-11-25
///       - Implemented background worker abstraction, Parallel.BackgroundWorker.
///     1.22a: 2011-11-16
///       - Number of producers/consumers in TOmniForkJoin<T>.StartWorkerTasks was off
///         by 1. Thanks to [meishier] for tracking the bug down.
///     1.22: 2011-11-15
///       - Parallel.Join implementation fixed to not depend on thread pool specifics.
///         Parallel.Join.NumTasks works again.
///       - GUIDs removed again (GUIDs on generic interfaces don't work). Hard casting is
///         used whenever possible.
///     1.21: 2011-11-11
///       - All interfaces decorated with GUIDs.
///     1.20a: 2011-11-09
///       - [Anton Alisov] Fixed potential leak in Pipeline exception handling.
///     1.20: 2011-11-03
///       - Fixed two Parallel.Pipeline overloads to not override internal input
///         collection if 'input' parameter was not provided.
///       - Only one thread pool used internally.
///       - GlobalParallelPool no longer limits maximum number of concurrent threads.
///     1.19: 2011-11-01
///       - Implemented IOmniParallelTask.TaskConfig.
///       - Added IOmniParallelTask.Execute overload.
///       - Added IOmniParallelIntoLoop and IOmniParallelIntoLoop<T> Execute overload.
///     1.18: 2011-09-06
///       - Initial implementation of the Parallel.ParallelTask.
///       - Parallel.Join implements OnStop.
///     1.17: 2011-08-29
///       - *** Breaking change *** IOmniPipeline.Input renamed to IOmniPipeline.From.
///       - *** Breaking change *** IOmniPipeline.Run now returns Self instead of
///         IOmniBlockingCollection.
///       - Added properties Input, Output: IOmniBlockingCollection to the IOmniPipeline.
///         Input always points to valid blocking collection (either the built-in one or
///         to the collection provided in the From method) and can be used to send data
///         to the first stage. Output can be used to read data from the last stage.
///       - Exception in any stage is caught and stored as an exception object in the
///         TOmniValue wrapper, which is passed to the output collection so it can be
///         processed by the next stage. By default, the next stage automatically reraises
///         this exception (and so on until the exception is passed to the final
///         collection) until you decorate the stage with the .HandleExceptions method.
///         (You can also mark all stages to handle exceptions by calling HandleExceptions
///         before defining any stage.) If a stage is handling exceptions, it will receive
///         TOmniValue holding an exception on input (value.IsException will be true). In
///         this case, it should either reraise the exception or (eventually) release the
///         exception object (value.AsException.Free). Demo 48_OtlParallelExceptions shows
///         possible ways to handle exceptions in the IOmniPipeline.
///     1.16: 2011-08-27
///       - Added two more Parallel.Pipeline overloads.
///       - Parallel.Pipeline accepts simple stages - TPipelineSimpleStageDelegate -
///         where collection iteration is implemented internally.
///       - Implemented IOmniPipeline.WaitFor.
///       - Added support for parameterless OnTerminated version to the TaskConfig.
///     1.15: 2011-07-26
///       - *** Breaking change *** Parallel.Join reimplemented as IOmniParallelJoin
///         interface to add exception and cancellation support. User code must call
///         .Execute on the interface returned from the Parallel.Join to start the
///         execution.
///       - Parallel.Join(const task: TOmniTaskDelegate) is no longer supported. It was
///         replaced with the Parallel.Join(const task: IOmniJoinState).
///       - Parallel.Join no longer supports taskConfig parameter (replaced by the
///         IOmniParallelJoin.TaskConfig function).
///       - Number of simultaneously executed task in Parallel.Join may be set by calling
///         the new IOmniParallelJoin.NumTasks function.
///     1.14: 2011-07-21
///       - Parallel.Future implements WaitFor.
///     1.13: 2011-07-18
///       - Added exception handling to Parallel.Join. Tasks' fatal exceptions are wrapped
///         in EJoinException and raised at the end of Parallel.Join method.
///       - Two version of Parallel.Async (the ones with explicit termination handlers)
///         were removed as this functionality can be achieved by using
///         Parallel.TaskConfig.OnTerminated.
///     1.12: 2011-07-18
///       - Added exception handling to IOmniFuture<T>. Tasks' fatal exception is raised
///         in .Value. New function .FatalException and .DetachException.
///       - Parallel.Join with TProc parameters was leaking memory.
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

// TODO 1 -oPrimoz Gabrijelcic : Replace OnStop with TaskConfig.OnTerminate whenever appropriate?
// TODO 1 -oPrimoz Gabrijelcic : IOmniParallelLoop.Initialize should return 'normal' interface which should implement Finalize; no need for 'InitializedLoop' interface
// TODO 1 -oPrimoz Gabrijelcic : IOmniParallelLoop.Execute should return 'self'
// TODO 1 -oPrimoz Gabrijelcic : Remove IOmniParallelLoop.OnMessage
// TODO 1 -oPrimoz Gabrijelcic : IOmniFuture<T>.IsExceptional
// TODO 1 -oPrimoz Gabrijelcic : ??TryFatalException with timeout??

// TODO 3 -oPrimoz Gabrijelcic : Maybe we could use .Aggregate<T> where T is the aggregate type?
// TODO 3 -oPrimoz Gabrijelcic : Change .Aggregate to use .Into signature for loop body?
// TODO 1 -oPrimoz Gabrijelcic : How to combine Futures and NoWait version of Aggregate?
// TODO 5 -oPrimoz Gabrijelcic : Single-threaded access to a data source - how? (datasets etc)
// TODO 3 -oPrimoz Gabrijelcic : Parallel.MapReduce?

// Notes for OTL 3
// - Parallel.ForEach should use task pool.
// - Task pool would dynamically schedule tasks over available cores.
// - Task pool would know how many different kinds of tasks are there (one per distinct
//   ForEach) and would balance load so that all different kinds of tasks would get executed.
// - ForEach would support .DegreeOfConcurrency (or something like that) which would
//   default to one meaning that one task can easily consume one core. Setting it to two
//   (it would be a real, not integer) would mean that one task can only consume one half of a
//   core and that 2*<number of cores> is a good number of threads for this particular task.

uses
  {$IFDEF MSWINDOWS}
  Windows,
  Messages,
  {$ENDIF MSWINDOWS}
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
    function  NoThreadPool: IOmniTaskConfig;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskConfig; overload;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskConfig; overload;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskConfig;
    function  ThreadPool(const threadPool: IOmniThreadPool): IOmniTaskConfig;
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
  TOmniIteratorStateTaskDelegate = reference to procedure(const task: IOmniTask; const value: TOmniValue; var taskState: TOmniValue);
  TOmniIteratorStateTaskDelegate<T> = reference to procedure(const task: IOmniTask; const value: T; var taskState: TOmniValue);

  TOmniIteratorIntoDelegate = reference to procedure(const value: TOmniValue; var result: TOmniValue);
  TOmniIteratorIntoDelegate<T> = reference to procedure(const value: T; var result: TOmniValue);
  TOmniIteratorIntoTaskDelegate = reference to procedure(const task: IOmniTask; const value: TOmniValue; var result: TOmniValue);
  TOmniIteratorIntoTaskDelegate<T> = reference to procedure(const task: IOmniTask; const value: T; var result: TOmniValue);

  TOmniTaskCreateDelegate = TOmniTaskDelegate;
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
    procedure Execute(loopBody: TOmniIteratorStateDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorStateTaskDelegate); overload;
  end; { IOmniParallelInitializedLoop }

  IOmniParallelInitializedLoop<T> = interface
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniParallelInitializedLoop<T>;
    procedure Execute(loopBody: TOmniIteratorStateDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorStateTaskDelegate<T>); overload;
  end; { IOmniParallelInitializedLoop }

  IOmniParallelIntoLoop = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorIntoTaskDelegate); overload;
  end; { IOmniParallelIntoLoop }

  IOmniParallelIntoLoop<T> = interface
    procedure Execute(loopBody: TOmniIteratorIntoDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorIntoTaskDelegate<T>); overload;
  end; { IOmniParallelIntoLoop<T> }

  TOmniTaskStopDelegate = TOmniTaskDelegate;

  IOmniParallelLoop = interface
    function  Aggregate(defaultAggregateValue: TOmniValue;
      aggregator: TOmniAggregatorDelegate): IOmniParallelAggregatorLoop;
    function  AggregateSum: IOmniParallelAggregatorLoop;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelLoop;
    procedure Execute(loopBody: TOmniIteratorDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorTaskDelegate); overload;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniParallelInitializedLoop;
    function  Into(const queue: IOmniBlockingCollection): IOmniParallelIntoLoop; overload;
    function  NoWait: IOmniParallelLoop;
    function  NumTasks(taskCount : integer): IOmniParallelLoop;
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop; overload; deprecated 'use TaskConfig';
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop; overload; deprecated 'use TaskConfig';
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop; overload; deprecated 'use TaskConfig';
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelLoop;
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
    function  OnMessage(eventDispatcher: TObject): IOmniParallelLoop<T>; overload; deprecated 'use TaskConfig';
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniParallelLoop<T>; overload; deprecated 'use TaskConfig';
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniParallelLoop<T>; overload; deprecated 'use TaskConfig';
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnTaskCreate(taskCreateDelegate: TOmniTaskControlCreateDelegate): IOmniParallelLoop<T>; overload;
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop<T>; overload;
    {$IFDEF OTL_FixedGenericIncompletelyDefined}
    function  OnStopInvoke(stopCode: TProc): IOmniParallelLoop<T>;
    {$ENDIF OTL_FixedGenericIncompletelyDefined}
    function  PreserveOrder: IOmniParallelLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop<T>;
  end; { IOmniParallelLoop<T> }

  TOmniIteratorSimpleSimpleDelegate = reference to procedure(value: integer);
  TOmniIteratorSimpleDelegate = reference to procedure(taskIndex, value: integer);
  TOmniIteratorSimpleFullDelegate = reference to procedure(const task: IOmniTask; taskIndex, value: integer);
  TOmniSimpleTaskInitializerDelegate = reference to procedure(taskIndex, fromIndex, toIndex: integer);
  TOmniSimpleTaskInitializerTaskDelegate = reference to procedure(const task: IOmniTask; taskIndex, fromIndex, toIndex: integer);
  TOmniSimpleTaskFinalizerDelegate = reference to procedure(taskIndex, fromIndex, toIndex: integer);
  TOmniSimpleTaskFinalizerTaskDelegate = reference to procedure(const task: IOmniTask; taskIndex, fromIndex, toIndex: integer);

  IOmniParallelSimpleLoop = interface
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelSimpleLoop;
    function  NoWait: IOmniParallelSimpleLoop;
    function  NumTasks(taskCount : integer): IOmniParallelSimpleLoop;
    function  OnStop(stopCode: TProc): IOmniParallelSimpleLoop; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelSimpleLoop;
    procedure Execute(loopBody: TOmniIteratorSimpleSimpleDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleFullDelegate); overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop; overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop; overload;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { IOmniParallelSimpleLoop }

  {$IFDEF OTL_GoodGenerics}
  TOmniIteratorSimpleSimpleDelegate<T> = reference to procedure(var value: T);
  TOmniIteratorSimpleDelegate<T> = reference to procedure(taskIndex: integer; var value: T);
  TOmniIteratorSimpleFullDelegate<T> = reference to procedure(const task: IOmniTask; taskIndex: integer; var value: T);

  IOmniParallelSimpleLoop<T> = interface
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelSimpleLoop<T>;
    function  NoWait: IOmniParallelSimpleLoop<T>;
    function  NumTasks(taskCount : integer): IOmniParallelSimpleLoop<T>;
    function  OnStop(stopCode: TProc): IOmniParallelSimpleLoop<T>; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop<T>; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelSimpleLoop<T>;
    procedure Execute(loopBody: TOmniIteratorSimpleSimpleDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleFullDelegate<T>); overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop<T>; overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop<T>; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop<T>; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop<T>; overload;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { IOmniParallelSimpleLoop }
  {$ENDIF OTL_GoodGenerics}

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
    function  WaitFor(timeout_ms: cardinal): boolean;
   end; { IOmniFuture<T> }

  TOmniFuture<T> = class(TInterfacedObject, IOmniFuture<T>)
  strict private
    FCancellable  : boolean;
    FCancelled    : boolean;
    FCompleted    : boolean;
    FTaskException: Exception;
    FResult       : T;
    FTask         : IOmniTaskControl;
  strict protected
    procedure DestroyTask;
    procedure DetachExceptionFromTask;
    procedure Execute(action: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
  public
    constructor Create(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig = nil); // sadly, those two Creates cannot be overloaded as this crashes the compiler (internal error T888)
    constructor CreateEx(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig = nil);
    destructor  Destroy; override;
    procedure Cancel;
    function  DetachException: Exception; inline;
    function  FatalException: Exception; inline;
    function  IsCancelled: boolean; inline;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
    function  WaitFor(timeout_ms: cardinal): boolean;
  end; { TOmniFuture<T> }

  EFutureError = class(Exception);
  EFutureCancelled = class(Exception);

  IOmniPipelineStage = interface ['{DFDA7A07-6B28-4AA6-9218-59D3DF9C4B8E}']
    function  GetInput: IOmniBlockingCollection;
    function  GetOutput: IOmniBlockingCollection;
  //
    property Input: IOmniBlockingCollection read GetInput;
    property Output: IOmniBlockingCollection read GetOutput;
  end; { IOmniPipelineStage }

  TPipelineSimpleStageDelegate = reference to procedure (const input: TOmniValue;
    var output: TOmniValue);
  TPipelineStageDelegate = reference to procedure (const input, output:
    IOmniBlockingCollection);
  TPipelineStageDelegateEx = reference to procedure (const input, output:
    IOmniBlockingCollection; const task: IOmniTask);

  IOmniPipeline = interface
    function  GetInput: IOmniBlockingCollection;
    function  GetOutput: IOmniBlockingCollection;
    function  GetPipelineStage(idxStage: integer): IOmniPipelineStage;
  //
    procedure Cancel;
    function  From(const queue: IOmniBlockingCollection): IOmniPipeline;
    function  HandleExceptions: IOmniPipeline;
    function  NumTasks(numTasks: integer): IOmniPipeline;
    function  OnStop(stopCode: TProc): IOmniPipeline; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniPipeline; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniPipeline;
    function  NoThrottling: IOmniPipeline;
    function  Run: IOmniPipeline;
    function  Stage(pipelineStage: TPipelineSimpleStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineSimpleStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Throttle(numEntries: integer; unblockAtCount: integer = 0): IOmniPipeline;
    function  WaitFor(timeout_ms: cardinal): boolean;
    property Input: IOmniBlockingCollection read GetInput;
    property Output: IOmniBlockingCollection read GetOutput;
    property PipelineStage[idxStage: integer]: IOmniPipelineStage read GetPipelineStage;
  end; { IOmniPipeline }

  TOmniForkJoinDelegate = reference to procedure;
  TOmniForkJoinDelegateEx = TOmniTaskDelegate;

  TOmniForkJoinDelegate<T> = reference to function: T;
  TOmniForkJoinDelegateEx<T> = reference to function(const task: IOmniTask): T;

  IOmniCompute = interface
    procedure Execute;
    function  IsDone: boolean;
    procedure Await;
  end; { IOmniCompute<T> }

  IOmniCompute<T> = interface
    procedure Execute;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { IOmniCompute<T> }

  TOmniCompute<T> = class(TInterfacedObject, IOmniCompute<T>)
  strict private
    FAction  : TOmniForkJoinDelegate<T>;
    FComputed: boolean;
    FInput   : IOmniBlockingCollection;
    FResult  : T;
  public
    constructor Create(action: TOmniForkJoinDelegate<T>; input: IOmniBlockingCollection);
    procedure Execute;
    function  IsDone: boolean;
    function  TryValue(timeout_ms: cardinal; var value: T): boolean;
    function  Value: T;
  end; { TOmniCompute<T> }

  TOmniCompute = class(TInterfacedObject, IOmniCompute)
  strict private
    FCompute: IOmniCompute<boolean>;
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
    FNumTasks  : integer;
    FPoolInput : IOmniBlockingCollection;
    FTaskConfig: IOmniTaskConfig;
    FTaskPool  : IOmniPipeline;
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
    FForkJoin: TOmniForkJoin<boolean>;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Compute(action: TOmniForkJoinDelegate): IOmniCompute;
    function  NumTasks(numTasks: integer): IOmniForkJoin;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin;
  end; { TOmniForkJoin }

  TOmniDelegateEnumerator = class(TOmniValueEnumerator)
  strict private
    FDelegate: TEnumeratorDelegate;
    FValue   : TOmniValue;
  public
    constructor Create(delegate: TEnumeratorDelegate);
    function  GetCurrent: TOmniValue; override;
    function  MoveNext: boolean; override;
  end; { TOmniDelegateEnumerator }

  TOmniDelegateEnumerator<T> = class(TOmniValueEnumerator)
  strict private
    FDelegate: TEnumeratorDelegate<T>;
    FValue   : T;
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
    FDestroy    : TRttiMethod;
    FEnumerable : TValue;
    FGetCurrent : TRttiMethod;
    FMoveNext   : TRttiMethod;
    FRttiContext: TRttiContext;
  public
    constructor Create(enumerable: TObject); overload;
  {$ENDIF OTL_ERTTI}
  strict private
    FAggregate          : TOmniValue;
    FAggregator         : TOmniAggregatorDelegate;
    FCancellationToken  : IOmniCancellationToken;
    FCountStopped       : IOmniResourceCount;
    FDataManager        : TOmniDataManager;
    FDelegateEnum       : TOmniDelegateEnumerator;
    FIntoQueueIntf      : IOmniBlockingCollection;
    FManagedProvider    : boolean;
    FNumTasks           : integer;
    FNumTasksManual     : boolean;
    FOnMessageList      : TGpIntegerObjectList;
    FOnStop             : TOmniTaskStopDelegate;
    FOnTaskControlCreate: TOmniTaskControlCreateDelegate;
    FOnTaskCreate       : TOmniTaskCreateDelegate;
    FOptions            : TOmniParallelLoopOptions;
    FSourceProvider     : TOmniSourceProvider;
    FTaskConfig         : IOmniTaskConfig;
    FTaskFinalizer      : TOmniTaskFinalizerDelegate;
    FTaskInitializer    : TOmniTaskInitializerDelegate;
  strict protected
    procedure DoOnStop(const task: IOmniTask);
    procedure InternalExecute(loopBody: TOmniIteratorDelegate); overload;
    procedure InternalExecute(loopBody: TOmniIteratorTaskDelegate); overload;
    procedure InternalExecute(loopBody: TOmniIteratorStateDelegate); overload;
    procedure InternalExecute(loopBody: TOmniIteratorStateTaskDelegate); overload;
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
    procedure SetOnStop(stopDelegate: TOmniTaskStopDelegate);
    procedure SetTaskConfig(const config: IOmniTaskConfig);
    function  Stopped: boolean; inline;
  public
    constructor Create(const sourceProvider: TOmniSourceProvider; managedProvider: boolean); overload;
    constructor Create(const enumerator: TEnumeratorDelegate); overload;
    destructor  Destroy; override;
    property Options: TOmniParallelLoopOptions read FOptions write FOptions;
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
    procedure Execute(loopBody: TOmniIteratorStateTaskDelegate); overload;
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
    function  OnStop(stopCode: TProc): IOmniParallelLoop; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelLoop;
    function  PreserveOrder: IOmniParallelLoop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop;
  end; { TOmniParallelLoop }

  TOmniParallelLoop<T> = class(TOmniParallelLoopBase, IOmniParallelLoop<T>,
                                                      IOmniParallelAggregatorLoop<T>,
                                                      IOmniParallelInitializedLoop<T>,
                                                      IOmniParallelIntoLoop<T>)
  strict private
    FDelegateEnum: TOmniDelegateEnumerator<T>;
    FEnumerator  : TEnumerator<T>;
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
    procedure Execute(loopBody: TOmniIteratorStateTaskDelegate<T>); overload;
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
    function  OnStop(stopCode: TProc): IOmniParallelLoop<T>; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop<T>; overload;
  {$IFDEF OTL_FixedGenericIncompletelyDefined}
    function  OnStopInvoke(stopCode: TProc): IOmniParallelLoop<T>;
  {$ENDIF OTL_FixedGenericIncompletelyDefined}
    function  PreserveOrder: IOmniParallelLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelLoop<T>;
  end; { TOmniParallelLoop<T> }

  TOmniParallelSimpleLoop = class(TInterfacedObject, IOmniParallelSimpleLoop)
  strict private type
    TPartitionInfo = record
      LowBound : integer;
      HighBound: integer;
    end;
    TTaskDelegate = reference to procedure (const task: IOmniTask; taskIndex: integer);
  strict private
    FCancelWith         : IOmniCancellationToken;
    FCountStopped       : IOmniResourceCount;
    FFinalizerDelegate  : TOmniSimpleTaskFinalizerTaskDelegate;
    FFirst              : integer;
    FInitializerDelegate: TOmniSimpleTaskInitializerTaskDelegate;
    FLast               : integer;
    FNoWait             : boolean;
    FNumTasks           : integer;
    FNumTasksManual     : boolean;
    FOnMessageList      : TGpIntegerObjectList;
    FOnStop             : TOmniTaskStopDelegate;
    FPartition          : array of TPartitionInfo;
    FStep               : integer;
    FTaskConfig         : IOmniTaskConfig;
  strict protected
    function  CreateForTask(taskIndex: integer; const taskDelegate: TTaskDelegate): IOmniTaskControl;
    procedure CreatePartitions(var numTasks: integer);
    procedure InternalExecute(const taskDelegate: TTaskDelegate);
  public
    constructor Create(first, last: integer; step: integer = 1);
    destructor  Destroy; override;
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelSimpleLoop;
    function  NoWait: IOmniParallelSimpleLoop;
    function  NumTasks(taskCount : integer): IOmniParallelSimpleLoop;
    function  OnStop(stopCode: TProc): IOmniParallelSimpleLoop; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelSimpleLoop;
    procedure Execute(loopBody: TOmniIteratorSimpleSimpleDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleDelegate); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleFullDelegate); overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop; overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop; overload;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop; overload;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { IOmniParallelSimpleLoop }

  {$IFDEF OTL_GoodGenerics}
  TOmniParallelSimpleLoop<T> = class(TInterfacedObject, IOmniParallelSimpleLoop<T>)
  strict private
    FData    : TArray<T>;
    FIterator: IOmniParallelSimpleLoop;
  public
    constructor Create(const arr: TArray<T>);
    function  CancelWith(const token: IOmniCancellationToken): IOmniParallelSimpleLoop<T>; inline;
    function  NoWait: IOmniParallelSimpleLoop<T>; inline;
    function  NumTasks(taskCount : integer): IOmniParallelSimpleLoop<T>; inline;
    function  OnStop(stopCode: TProc): IOmniParallelSimpleLoop<T>; overload; inline;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop<T>; overload; inline;
    function  OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop<T>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelSimpleLoop<T>; inline;
    procedure Execute(loopBody: TOmniIteratorSimpleSimpleDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleDelegate<T>); overload;
    procedure Execute(loopBody: TOmniIteratorSimpleFullDelegate<T>); overload;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop<T>; overload; inline;
    function  Initialize(taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop<T>; overload; inline;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop<T>; overload; inline;
    function  Finalize(taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop<T>; overload; inline;
    function  WaitFor(maxWait_ms: cardinal): boolean; inline;
  end; { TOmniParallelSimpleLoop<T> }
  {$ENDIF OTL_GoodGenerics}

  EJoinException = class(Exception)
  strict private
    FExceptions: TGpIntegerObjectList;
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
    function  DetachInner(idxException: integer): Exception;
    property Inner[idxException: integer]: TJoinInnerException read GetInner; default;
  end; { EJoinException }

  IOmniJoinState = interface ['{8A198F20-24F3-44F8-A77B-CA93E83291D3}']
    function  GetTask: IOmniTask;
  //
    procedure Cancel;
    function  IsCancelled: boolean;
    function  IsExceptional: boolean;
    property Task: IOmniTask read GetTask;
  end; { IOmniJoinState }

  IOmniJoinStateEx = interface ['{BF2E87C9-99EC-4A52-9F37-7FC24111BD75}']
    function  GetTaskControl: IOmniTaskControl;
    procedure SetTaskControl(const aTask: IOmniTaskControl);
  //
    procedure SetTask(const aTask: IOmniTask);
    property TaskControl: IOmniTaskControl read GetTaskControl write SetTaskControl;
  end; { IOmniJoinStateEx }

  TOmniJoinState = class(TInterfacedObject, IOmniJoinState, IOmniJoinStateEx)
  strict private
    FGlobalCancelationFlag: IOmniCancellationToken;
    FGlobalExceptionFlag  : IOmniCancellationToken;
    FTask                 : IOmniTask;
    FTaskControl          : IOmniTaskControl;
  protected
    function  GetTask: IOmniTask;
    function  GetTaskControl: IOmniTaskControl;
    procedure SetTaskControl(const aTask: IOmniTaskControl);
  public
    constructor Create(const globalCancelationFlag, globalExceptionFlag:
      IOmniCancellationToken);
    procedure Cancel;
    function  IsCancelled: boolean;
    function  IsExceptional: boolean;
    procedure SetTask(const aTask: IOmniTask);
    property Task: IOmniTask read GetTask;
    property TaskControl: IOmniTaskControl read GetTaskControl write SetTaskControl;
  end; { TOmniJoinState }

  TOmniJoinDelegate = reference to procedure (const joinState: IOmniJoinState);

  IOmniParallelJoin = interface
    function  Cancel: IOmniParallelJoin;
    function  DetachException: Exception;
    function  Execute: IOmniParallelJoin;
    function  FatalException: Exception;
    function  IsCancelled: boolean;
    function  IsExceptional: boolean;
    function  NoWait: IOmniParallelJoin;
    function  NumTasks(numTasks: integer): IOmniParallelJoin;
    function  OnStop(const stopCode: TProc): IOmniParallelJoin; overload;
    function  OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelJoin; overload;
    function  OnStopInvoke(const stopCode: TProc): IOmniParallelJoin;
    function  Task(const task: TProc): IOmniParallelJoin; overload;
    function  Task(const task: TOmniJoinDelegate): IOmniParallelJoin; overload;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelJoin;
    function  WaitFor(timeout_ms: cardinal): boolean;
  end; { IOmniParallelJoin }

  TOmniParallelJoin = class(TInterfacedObject, IOmniParallelJoin)
  strict private
    FCountStopped          : IOmniResourceCount;
    FGlobalCancellationFlag: IOmniCancellationToken;
    FGlobalExceptionFlag   : IOmniCancellationToken;
    FInput                 : IOmniBlockingCollection;
    FJoinStates            : array of IOmniJoinState;
    FNoWait                : boolean;
    FNumTasks              : integer;
    FOnStop                : TOmniTaskStopDelegate;
    FTaskConfig            : IOmniTaskConfig;
    FTaskException         : Exception;
    FTaskExceptionLock     : TOmniCS;
    FTasks                 : TList<TOmniJoinDelegate>;
  strict protected
    procedure DoOnStop(const task: IOmniTask);
    function  InternalWaitFor(timeout_ms: cardinal): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Cancel: IOmniParallelJoin;
    function  DetachException: Exception;
    function  Execute: IOmniParallelJoin;
    function  FatalException: Exception;
    function  IsCancelled: boolean;
    function  IsExceptional: boolean;
    function  NoWait: IOmniParallelJoin;
    function  NumTasks(numTasks: integer): IOmniParallelJoin;
    function  OnStop(const stopCode: TProc): IOmniParallelJoin; overload;
    function  OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelJoin; overload;
    function  OnStopInvoke(const stopCode: TProc): IOmniParallelJoin;
    function  Task(const aTask: TOmniJoinDelegate): IOmniParallelJoin; overload;
    function  Task(const aTask: TProc): IOmniParallelJoin; overload;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelJoin;
    function  WaitFor(timeout_ms: cardinal): boolean;
  end; { TOmniParallelJoin }

  TOmniParallelTaskDelegate = TOmniTaskDelegate;

  IOmniParallelTask = interface
    function  DetachException: Exception;
    function  Execute(const aTask: TProc): IOmniParallelTask; overload;
    function  Execute(const aTask: TOmniParallelTaskDelegate): IOmniParallelTask; overload;
    function  FatalException: Exception;
    function  IsExceptional: boolean;
    function  NoWait: IOmniParallelTask;
    function  NumTasks(numTasks: integer): IOmniParallelTask;
    function  OnStop(const stopCode: TProc): IOmniParallelTask; overload;
    function  OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelTask; overload;
    function  OnStopInvoke(const stopCode: TProc): IOmniParallelTask;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelTask;
    function  WaitFor(timeout_ms: cardinal): boolean;
  end; { IOmniParallelTask }

  IOmniWorkItem = interface ['{3CE2762F-B7A3-4490-BF22-2109C042EAD1}']
    function  GetCancellationToken: IOmniCancellationToken;
    function  GetData: TOmniValue;
    function  GetResult: TOmniValue;
    function  GetSkipCompletionHandler: boolean;
    function  GetTask: IOmniTask;
    function  GetTaskState: TOmniValue;
    function  GetUniqueID: int64;
    procedure SetResult(const value: TOmniValue);
    procedure SetSkipCompletionHandler(const value: boolean);
  //
    function  DetachException: Exception;
    function  FatalException: Exception;
    function  IsExceptional: boolean;
    property CancellationToken: IOmniCancellationToken read GetCancellationToken;
    property Data: TOmniValue read GetData;
    property Result: TOmniValue read GetResult write SetResult;
    property SkipCompletionHandler: boolean read GetSkipCompletionHandler write
      SetSkipCompletionHandler;
    property Task: IOmniTask read GetTask;
    property TaskState: TOmniValue read GetTaskState;
    property UniqueID: int64 read GetUniqueID;
  end; { IOmniWorkItem }

  IOmniBackgroundWorker = interface;

  TOmniBackgroundWorkerDelegate = reference to procedure (const workItem: IOmniWorkItem);
  TOmniWorkItemDoneDelegate = reference to procedure (const Sender: IOmniBackgroundWorker;
    const workItem: IOmniWorkItem);

  IOmniWorkItemConfig = interface
    function  OnExecute(const aTask: TOmniBackgroundWorkerDelegate): IOmniWorkItemConfig;
    function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
    function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
  end; { IOmniWorkItemConfig }

  IOmniBackgroundWorker = interface
    function  CreateWorkItem(const data: TOmniValue): IOmniWorkItem;
    procedure CancelAll; overload;
    procedure CancelAll(upToUniqueID: int64); overload;
    function  Config: IOmniWorkItemConfig;
    function  Execute(const aTask: TOmniBackgroundWorkerDelegate = nil): IOmniBackgroundWorker;
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniBackgroundWorker;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniBackgroundWorker;
    function  NumTasks(numTasks: integer): IOmniBackgroundWorker;
    function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
    function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
    function  OnStop(stopCode: TProc): IOmniBackgroundWorker; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniBackgroundWorker; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniBackgroundWorker;
    procedure Schedule(const workItem: IOmniWorkItem; const workItemConfig: IOmniWorkItemConfig = nil); overload;
    function  StopOn(const token: IOmniCancellationToken): IOmniBackgroundWorker;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniBackgroundWorker;
    function  Terminate(maxWait_ms: cardinal): boolean;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { IOmniBackgroundWorker }

  IOmniTimedTask = interface
    function  GetActive: boolean;
    function  GetInterval: integer;
    procedure SetActive(const value: boolean);
    procedure SetInterval(const value: integer);
  //
    function  Every(interval_ms: integer): IOmniTimedTask;
    function  Execute(const aTask: TProc): IOmniTimedTask; overload;
    function  Execute(const aTask: TOmniTaskDelegate): IOmniTimedTask; overload;
    procedure ExecuteNow;
    procedure Start;
    procedure Stop;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniTimedTask;
    function  Terminate(maxWait_ms: cardinal): boolean;
    function  WaitFor(maxWait_ms: cardinal): boolean;
    property Active: boolean read GetActive write SetActive;
    property Interval: integer read GetInterval write SetInterval;
  end; { IOmniTimedTask }

  TMapProc<T1,T2> = reference to function(const source: T1; var target: T2): boolean;

  {$IFDEF OTL_HasArrayOfT}
  {$IFDEF OTL_GoodGenerics}
  IOmniParallelMapper<T1,T2> = interface
    function  Execute(mapper: TMapProc<T1,T2>): IOmniParallelMapper<T1,T2>;
    function  NoWait: IOmniParallelMapper<T1,T2>;
    function  NumTasks(numTasks: integer): IOmniParallelMapper<T1,T2>;
    function  OnStop(stopCode: TProc): IOmniParallelMapper<T1,T2>; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelMapper<T1,T2>; overload;
  {$IFDEF OTL_FixedGenericIncompletelyDefined}
    function  OnStopInvoke(stopCode: TProc): IOmniParallelMapper<T1,T2>;
  {$ENDIF OTL_FixedGenericIncompletelyDefined}
    function  Result: TArray<T2>;
    function  Source(const data: TArray<T1>; makeCopy: boolean = false): IOmniParallelMapper<T1,T2>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelMapper<T1,T2>;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { IOmniParallelMapper<T1,T2> }

  TOmniParallelMapper<T1,T2> = class(TInterfacedObject, IOmniParallelMapper<T1,T2>)
  strict private type
    TTargetBounds = record Low, High: integer; end;
  strict private
    FNoWait    : boolean;
    FNumTasks  : integer;
    FOnStop    : TOmniTaskStopDelegate;
    FSource    : TArray<T1>;
    FTarget    : TArray<T2>;
    FTargetData: TArray<TTargetBounds>;
    FTaskConfig: IOmniTaskConfig;
    FWorker    : IOmniParallelSimpleLoop;
  strict protected
    procedure CompressTarget;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Execute(mapper: TMapProc<T1,T2>): IOmniParallelMapper<T1,T2>;
    function  NoWait: IOmniParallelMapper<T1,T2>;
    function  NumTasks(numTasks: integer): IOmniParallelMapper<T1,T2>;
    function  OnStop(stopCode: TProc): IOmniParallelMapper<T1,T2>; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelMapper<T1,T2>; overload;
  {$IFDEF OTL_FixedGenericIncompletelyDefined}
    function  OnStopInvoke(stopCode: TProc): IOmniParallelMapper<T1,T2>;
  {$ENDIF OTL_FixedGenericIncompletelyDefined}
    function  Result: TArray<T2>;
    function  Source(const data: TArray<T1>; makeCopy: boolean = false): IOmniParallelMapper<T1,T2>;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelMapper<T1,T2>;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { TOmniParallelMapper<T1,T2> }
  {$ENDIF OTL_GoodGenerics}
  {$ENDIF OTL_HasArrayOfT}

  {$REGION 'Documentation'}
  ///	<summary>Parallel class represents a base class for all high-level language
  ///	features in the OmniThreadLibrary. Most features are implemented as factories while
  ///	Async is implemented as a class procedures that does the real work.</summary>
  {$ENDREGION}
  Parallel = class
  public
  // For
    /// <summary>Creates fast parallel loop without support for work stealing which
    /// only enumerates integer ranges.</summary>
    class function  &For(first, last: integer; step: integer = 1): IOmniParallelSimpleLoop; overload;
    {$IFDEF OTL_GoodGenerics}
    /// <summary>Creates fast parallel loop without support for work stealing which
    ///   only enumerates dynamic arrays.</summary>
    class function  &For<T>(const arr: TArray<T>): IOmniParallelSimpleLoop<T>; overload;
    {$ENDIF OTL_GoodGenerics}

  // ForEach
    ///	<summary>Creates parallel loop that iterates over an IOmniValueEnumerable.</summary>
    class function  ForEach(const enumerable: IOmniValueEnumerable): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates over an IOmniEnumerator.</summary>
    class function  ForEach(const enum: IOmniValueEnumerator): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates over an IEnumerable.</summary>
    class function  ForEach(const enumerable: IEnumerable): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates with an IEnumerator.</summary>
    class function  ForEach(const enum: IEnumerator): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates over an IOmniBlockingCollection.</summary>
    class function  ForEach(const source: IOmniBlockingCollection): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates over a TOmniSourceProvider.</summary>
    class function  ForEach(const sourceProvider: TOmniSourceProvider): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that uses a TEnumeratorDelegate to produce elements.</summary>
    class function  ForEach(enumerator: TEnumeratorDelegate): IOmniParallelLoop; overload;
    ///	<summary>Creates parallel loop that iterates over an integer range.</summary>
    class function  ForEach(first, last: integer; step: integer = 1): IOmniParallelLoop<integer>; overload;
    {$IFDEF OTL_ERTTI}
    ///	<summary>Creates parallel loop that iterates over a GetEnumerator implemented in the object.</summary>
    class function  ForEach(const enumerable: TObject): IOmniParallelLoop; overload;
    {$ENDIF OTL_ERTTI}
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IOmniValueEnumerable.</summary>
    class function  ForEach<T>(const enumerable: IOmniValueEnumerable): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IOmniEnumerator.</summary>
    class function  ForEach<T>(const enum: IOmniValueEnumerator): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IEnumerable.</summary>
    class function  ForEach<T>(const enumerable: IEnumerable): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IEnumerator.</summary>
    class function  ForEach<T>(const enum: IEnumerator): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IEnumerable&lt;T&gt;.</summary>
    class function  ForEach<T>(const enumerable: TEnumerable<T>): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IEnumerator&lt;T&gt;.</summary>
    class function  ForEach<T>(const enum: TEnumerator<T>): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from an IOmniBlockingCollection.</summary>
    class function  ForEach<T>(const source: IOmniBlockingCollection): IOmniParallelLoop<T>; overload;
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from a TEnumeratorDelegate.</summary>
    class function  ForEach<T>(enumerator: TEnumeratorDelegate<T>): IOmniParallelLoop<T>; overload;
    {$IFDEF OTL_ERTTI}
    ///	<summary>Creates parallel loop that iterates over elements of type T returned from a GetEnumerator implemented in the object.</summary>
    class function  ForEach<T>(const enumerable: TObject): IOmniParallelLoop<T>; overload;
    {$ENDIF OTL_ERTTI}

  // Join
    ///	<summary>Creates a Join interface.</summary>
    class function  Join: IOmniParallelJoin; overload;
    ///	<summary>Creates Join of two tasks.</summary>
    class function  Join(const task1, task2: TProc): IOmniParallelJoin; overload;
    ///	<summary>Creates Join of two tasks.</summary>
    class function  Join(const task1, task2: TOmniJoinDelegate): IOmniParallelJoin; overload;
    ///	<summary>Creates Join of any number of tasks.</summary>
    class function  Join(const tasks: array of TProc): IOmniParallelJoin; overload;
    ///	<summary>Creates Join of any number of tasks.</summary>
    class function  Join(const tasks: array of TOmniJoinDelegate): IOmniParallelJoin; overload;

  // Future
    ///	<summary>Creates a Future returning &lt;T&gt;.</summary>
    class function Future<T>(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;
    ///	<summary>Creates a Future returning &lt;T&gt;.</summary>
    class function Future<T>(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig = nil): IOmniFuture<T>; overload;

  // Pipeline
    ///	<summary>Creates a Pipeline interface.</summary>
    class function Pipeline: IOmniPipeline; overload;
    ///	<summary>Creates a Pipeline from given stage delegates.</summary>
    class function Pipeline(const stages: array of TPipelineStageDelegate;
      const input: IOmniBlockingCollection = nil): IOmniPipeline; overload;
    ///	<summary>Creates a Pipeline from given stage delegates.</summary>
    class function Pipeline(const stages: array of TPipelineStageDelegateEx;
      const input: IOmniBlockingCollection = nil): IOmniPipeline; overload;

  // Fork/Join
    ///	<summary>Creates a Fork/Join interface.</summary>
    class function ForkJoin: IOmniForkJoin; overload;
    ///	<summary>Creates a Fork/Join&lt;T&gt; interface.</summary>
    class function ForkJoin<T>: IOmniForkJoin<T>; overload;

  // Async
    ///	<summary>Creates an Async task.</summary>
    class procedure Async(task: TProc; taskConfig: IOmniTaskConfig = nil); overload;
    ///	<summary>Creates an Async task.</summary>
    class procedure Async(task: TOmniTaskDelegate; taskConfig: IOmniTaskConfig = nil);
      overload;

  // Parallel
    ///	<summary>Creates a Parallel task.</summary>
    class function ParallelTask: IOmniParallelTask;

  // BackgroundWorker
    ///	<summary>Creates a Background worker.</summary>
    class function BackgroundWorker: IOmniBackgroundWorker;

  // TimedTask
    ///	<summary>Creates a Timed task.</summary>
    class function TimedTask: IOmniTimedTask;

  // Map
    {$IFDEF OTL_HasArrayOfT}
    {$IFDEF OTL_GoodGenerics}
    ///	<summary>Creates a parallel Map mapping &lt;T1&gt; to &lt;T2&gt;.</summary>
    class function Map<T1,T2>: IOmniParallelMapper<T1,T2>; overload;
    ///	<summary>Maps and array of &lt;T1&gt; to an array of &lt;T2&gt;.</summary>
    class function Map<T1,T2>(const source: TArray<T1>;
      mapper: TMapProc<T1,T2>): TArray<T2>; overload;
    {$ENDIF OTL_GoodGenerics}
    {$ENDIF OTL_HasArrayOfT}

  // task configuration
    ///	<summary>Creates Task configuration block.</summary>
    class function TaskConfig: IOmniTaskConfig;

  // helpers
    ///	<summary>Applies task configuration to a task. TaskConfig may be nil - in this case
    ///	nothing is done.</summary>
    class procedure ApplyConfig(const taskConfig: IOmniTaskConfig; const task: IOmniTaskControl);
    /// <summary>Creates an anonymous method which calls CompleteAdding on a queue.</summary>
    class function CompleteQueue(const queue: IOmniBlockingCollection): TProc;
    /// <summary>Starts a worker task according to a specified configuration.</summary>
    class procedure Start(const taskControl: IOmniTaskControl; const taskConfig:
      IOmniTaskConfig);
  end; { Parallel }

  IOmniAwait = interface
    procedure Await(proc: TProc);
  end; { IOmniAwait }

  function Async(proc: TProc): IOmniAwait;

  {$REGION 'Documentation'}
  ///	<summary>A workaround used in TOmniForkJoin&lt;T&gt; to add work units into
  ///	blocking collection. Calling IOmniBlockinCollection.Add directly causes internal
  ///	compiler error.</summary>
  {$ENDREGION}
  procedure AddToBC(const queue: IOmniBlockingCollection; value: IInterface);

  ///	<returns>Global pool used for all OtlParallel constructs.</returns>
  function GlobalParallelPool: IOmniThreadPool;

implementation

uses
  {$IFDEF MSWINDOWS}
  DSiWin32,
  {$ENDIF MSWINDOWS}
  Classes,
  GpStuff,
  OtlComm,
  OtlContainerObserver;

type
{$IF CompilerVersion < 23} //pre-XE2
  NativeInt = integer;
{$IFEND}

  IOmniPipelineStageEx = interface ['{C34393C7-E9EE-4CE7-895F-EECA553F4E54}']
    function  GetHandleExceptions: boolean;
    function  GetNumTasks: integer;
    function  GetTaskConfig: IOmniTaskConfig;
    function  GetThrottle: integer;
    function  GetThrottleLow: integer;
    function  GetThrottleLowSat: integer;
    procedure SetHandleExceptions(const value: boolean);
    procedure SetNumTasks(const value: integer);
    procedure SetThrottle(const value: integer);
    procedure SetThrottleLow(const value: integer);
    procedure SetThrottleLowSat(const value: integer);
  //
    procedure Execute(const task: IOmniTask);
    procedure SetQueues(const inQueue, outQueue: IOmniBlockingCollection);
    property HandleExceptions: boolean read GetHandleExceptions write SetHandleExceptions;
    property NumTasks: integer read GetNumTasks write SetNumTasks;
    property TaskConfig: IOmniTaskConfig read GetTaskConfig;
    property Throttle: integer read GetThrottle write SetThrottle;
    property ThrottleLow: integer read GetThrottleLow write SetThrottleLow;
    property ThrottleLowSat: integer read GetThrottleLowSat write SetThrottleLowSat;
  end; { IOmniPipelineStageEx }

  TOmniPipelineStage = class(TInterfacedObject, IOmniPipelineStage, IOmniPipelineStageEx)
  strict private
    opsHandleExceptions: boolean;
    opsInput           : IOmniBlockingCollection;
    opsNumTasks        : integer;
    opsOutput          : IOmniBlockingCollection;
    opsSimpleStage     : TPipelineSimpleStageDelegate;
    opsStage           : TPipelineStageDelegate;
    opsStageEx         : TPipelineStageDelegateEx;
    opsTaskConfig      : IOmniTaskConfig;
    opsThrottle        : integer;
    opsThrottleLow     : integer;
    opsThrottleLowSat  : integer;
  strict protected
    procedure ExecuteSimpleStage(const task: IOmniTask; const stage:
      TPipelineSimpleStageDelegate; const input, output: IOmniBlockingCollection);
  protected
    function  GetHandleExceptions: boolean;
    function  GetInput: IOmniBlockingCollection;
    function  GetNumTasks: integer;
    function  GetOutput: IOmniBlockingCollection;
    function  GetTaskConfig: IOmniTaskConfig;
    function  GetThrottle: integer;
    function  GetThrottleLow: integer;
    function  GetThrottleLowSat: integer;
    procedure SetHandleExceptions(const value: boolean);
    procedure SetNumTasks(const value: integer);
    procedure SetThrottle(const value: integer);
    procedure SetThrottleLow(const value: integer);
    procedure SetThrottleLowSat(const value: integer);
  public
    constructor Create(stage: TPipelineSimpleStageDelegate; taskConfig: IOmniTaskConfig); overload;
    constructor Create(stage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig); overload;
    constructor Create(stage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig); overload;
  public // IOmniPipelineStage
    property Input: IOmniBlockingCollection read GetInput;
    property Output: IOmniBlockingCollection read GetOutput;
  public // IOmniPipelineStageEx
    procedure Execute(const task: IOmniTask);
    procedure SetQueues(const inQueue, outQueue: IOmniBlockingCollection);
    property HandleExceptions: boolean read GetHandleExceptions write SetHandleExceptions;
    property NumTasks: integer read GetNumTasks write SetNumTasks;
    property TaskConfig: IOmniTaskConfig read GetTaskConfig;
    property Throttle: integer read GetThrottle write SetThrottle;
    property ThrottleLow: integer read GetThrottleLow write SetThrottleLow;
    property ThrottleLowSat: integer read GetThrottleLowSat write SetThrottleLowSat;
  end; { TOmniPipelineStage }

  TOmniPipeline = class(TInterfacedObject, IOmniPipeline)
  strict private
    opCancelWith      : IOmniCancellationToken;
    opCheckpoint      : integer;
    opCountStopped    : IOmniResourceCount;
    opHandleExceptions: boolean;
    opInput           : IOmniBlockingCollection;
    opNumTasks        : integer;
    opOnStop          : TOmniTaskStopDelegate;
    opOutput          : IOmniBlockingCollection;
    opOutQueues       : TInterfaceList;
    opShutDownComplete: TOmniTransitionEvent;
    opStages          : TInterfaceList;
    opThrottle        : integer;
    opThrottleLow     : integer;
    opThrottleLowSat  : integer;
  strict protected
    procedure AddSingleStage(const stage: IOmniPipelineStageEx);
    procedure DoOnStop(const task: IOmniTask);
    function  GetStage(idxStage: integer): IOmniPipelineStageEx;
    property PipeStage[idxStage: integer]: IOmniPipelineStageEx read GetStage;
  protected
    function  GetInput: IOmniBlockingCollection;
    function  GetOutput: IOmniBlockingCollection;
    function  GetPipelineStage(idxStage: integer): IOmniPipelineStage;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Cancel;
    function  From(const queue: IOmniBlockingCollection): IOmniPipeline;
    function  HandleExceptions: IOmniPipeline;
    function  NoThrottling: IOmniPipeline;
    { TODO 1 -ogabr : When running stages in parallel, additional work has to be done to ensure proper output order! }
    function  NumTasks(numTasks: integer): IOmniPipeline;
    function  OnStop(stopCode: TProc): IOmniPipeline; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniPipeline; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniPipeline;
    function  Run: IOmniPipeline;
    function  Stage(pipelineStage: TPipelineSimpleStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stage(pipelineStage: TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineSimpleStageDelegate; taskConfig:
      IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegate; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Stages(const pipelineStages: array of TPipelineStageDelegateEx; taskConfig: IOmniTaskConfig = nil): IOmniPipeline; overload;
    function  Throttle(numEntries: integer; unblockAtCount: integer = 0): IOmniPipeline;
    function  WaitFor(timeout_ms: cardinal): boolean;
    property Input: IOmniBlockingCollection read GetInput;
    property Output: IOmniBlockingCollection read GetOutput;
    property PipelineStage[idxStage: integer]: IOmniPipelineStage read GetPipelineStage;
  end; { TOmniPipeline }

  TOmniParallelTask = class(TInterfacedObject, IOmniParallelTask)
  strict private
    optJoin    : IOmniParallelJoin;
    optNoWait  : boolean;
    optNumTasks: integer;
  public
    constructor Create;
    function  DetachException: Exception; inline;
    function  Execute(const aTask: TProc): IOmniParallelTask; overload;
    function  Execute(const aTask: TOmniParallelTaskDelegate): IOmniParallelTask; overload;
    function  FatalException: Exception; inline;
    function  IsExceptional: boolean; inline;
    function  NoWait: IOmniParallelTask;
    function  NumTasks(numTasks: integer): IOmniParallelTask;
    function  OnStop(const stopCode: TProc): IOmniParallelTask; overload;
    function  OnStop(const stopCode: TOmniTaskStopDelegate): IOmniParallelTask; overload;
    function  OnStopInvoke(const stopCode: TProc): IOmniParallelTask;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniParallelTask;
    function  WaitFor(timeout_ms: cardinal): boolean;
  end; { IOmniParallelTask }

  TOmniTaskConfigTerminated = record
    Event : TOmniTaskTerminatedEvent;
    Func  : TOmniOnTerminatedFunction;
    Simple: TOmniOnTerminatedFunctionSimple;
    procedure Call(const task: IOmniTaskControl);
    procedure Clear;
  end; { TOmniTaskConfigTerminated }

  IOmniTaskConfigInternal = interface ['{8678C3A4-7825-4E7C-8FEF-4DD6CD3D3E29}']
    procedure DetachTerminated(var terminated: TOmniTaskConfigTerminated);
    function  GetThreadPool: IOmniThreadPool;
    function  RunDirectly: boolean;
  end; { IOmniTaskConfigInternal }

  TOmniTaskConfig = class(TInterfacedObject, IOmniTaskConfig, IOmniTaskConfigInternal)
  strict private
    otcCancelWithToken         : IOmniCancellationToken;
    otcMonitorWithMonitor      : IOmniTaskControlMonitor;
    otcNoThreadPool            : boolean;
    otcOnMessageEventDispatcher: TObject;
    otcOnMessageEventHandler   : TOmniTaskMessageEvent;
    otcOnMessageList           : TGpIntegerObjectList;
    otcOnTerminated            : TOmniTaskConfigTerminated;
    otcPriority                : TOTLThreadPriority;
    otcThreadPool              : IOmniThreadPool;
    otcWithCounterCounter      : IOmniCounter;
    otcWithLockAutoDestroy     : boolean;
    otcWithLockOmniLock        : IOmniCriticalSection;
    otcWithLockSyncLock        : TSynchroObject;
  public
    constructor Create;
    destructor  Destroy; override;
  public //IOmniTaskConfig
    procedure Apply(const task: IOmniTaskControl);
    function  CancelWith(const token: IOmniCancellationToken): IOmniTaskConfig; inline;
    function  MonitorWith(const monitor: IOmniTaskControlMonitor): IOmniTaskConfig; inline;
    function  NoThreadPool: IOmniTaskConfig;
    function  OnMessage(eventDispatcher: TObject): IOmniTaskConfig; overload; inline;
    function  OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload; inline;
    function  OnMessage(msgID: word; eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig; overload; inline;
    function  OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction): IOmniTaskConfig; overload; inline;
    function  OnTerminated(eventHandler: TOmniTaskTerminatedEvent): IOmniTaskConfig; overload; inline;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunction): IOmniTaskConfig; overload;
    function  OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple): IOmniTaskConfig; overload;
    function  SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskConfig;
    function  ThreadPool(const threadPool: IOmniThreadPool): IOmniTaskConfig;
    function  WithCounter(const counter: IOmniCounter): IOmniTaskConfig; inline;
    function  WithLock(const lock: TSynchroObject; autoDestroyLock: boolean = true): IOmniTaskConfig; overload; inline;
    function  WithLock(const lock: IOmniCriticalSection): IOmniTaskConfig; overload; inline;
  public //IOmniTaskConfigInternal
    procedure DetachTerminated(var terminated: TOmniTaskConfigTerminated);
    function  GetThreadPool: IOmniThreadPool; inline;
    function  RunDirectly: boolean; inline;
  end; { TOmniTaskConfig }

  TOmniTimedTaskWorker = class(TOmniWorker)
  public const
    MsgSetTask    = 1;
    MsgExecuteNow = 2;
    MsgApplyTimer = 3;
  strict private
    FTask: TOmniTaskDelegate;
  strict protected
    procedure DoExecute;
  public
    procedure ApplyTimer(var msg: TOmniMessage); message MsgApplyTimer;
    procedure ExecuteNow(var msg: TOmniMessage); message MsgExecuteNow;
    procedure SetTask(var msg: TOmniMessage); message MsgSetTask;
  published
    procedure TaskInterval;
  end; { TOmniTimedTaskWorker }

  TOmniTimedTask = class(TInterfacedObject, IOmniTimedTask)
  strict private
    FActive  : boolean;
    FInterval: integer;
    FWorker  : IOmniTaskControl;
  protected
    procedure ApplyTimer;
    function  GetActive: boolean; inline;
    function  GetInterval: integer; inline;
    procedure SetActive(const value: boolean);
    procedure SetInterval(const value: integer);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Every(interval_ms: integer): IOmniTimedTask;
    function  Execute(const aTask: TProc): IOmniTimedTask; overload;
    function  Execute(const aTask: TOmniTaskDelegate): IOmniTimedTask; overload;
    procedure ExecuteNow; inline;
    procedure Start; inline;
    procedure Stop; inline;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniTimedTask; inline;
    function  Terminate(maxWait_ms: cardinal): boolean; inline;
    function  WaitFor(maxWait_ms: cardinal): boolean; inline;
    property Active: boolean read GetActive write SetActive;
    property Interval: integer read GetInterval write SetInterval;
  end; { TOmniTimedTask }

{$IFDEF MSWINDOWS}
const
  MSG_WORK_ITEM_DONE = WM_USER; // used only in internal window created inside TOmniBackgroundWorker
{$ENDIF MSWINDOWS}

type
  IOmniWorkItemConfigEx = interface ['{42CEC5CB-404F-4868-AE81-6A13AD7E3C6B}']
    function  GetOnExecute: TOmniBackgroundWorkerDelegate;
    function  GetOnRequestDone: TOmniWorkItemDoneDelegate;
    function  GetOnRequestDone_Asy: TOmniWorkItemDoneDelegate;
  end; { IOmniWorkItemConfigEx }

  IOmniWorkItemEx = interface ['{3B48D012-CF1C-4B47-A4A0-3072A9067A3E}']
    function  GetConfig: IOmniWorkItemConfig;
    procedure SetConfig(const value: IOmniWorkItemConfig);
    procedure SetTask(const task: IOmniTask; const taskState: TOmniValue);
  //
    property Config: IOmniWorkItemConfig read GetConfig write SetConfig;
  end; { IOmniWorkItemEx }

  TOmniWorkItem = class(TInterfacedObject, IOmniWorkItem, IOmniWorkItemEx)
  strict private
    FCancelAllUpToID_ref  : ^TOmniAlignedInt64;
    FCancellationToken    : IOmniCancellationToken;
    FConfig               : IOmniWorkItemConfig;
    FData                 : TOmniValue;
    FResult               : TOmniValue;
    FSkipCompletionHandler: boolean;
    FTask                 : IOmniTask;
    FTaskState            : TOmniValue;
    FUniqueID             : int64;
  strict protected
    procedure FreeException;
  protected //IOmniWorkItem
    function  GetCancellationToken: IOmniCancellationToken;
    function  GetData: TOmniValue; inline;
    function  GetResult: TOmniValue; inline;
    function  GetSkipCompletionHandler: boolean;
    function  GetTask: IOmniTask; inline;
    function  GetTaskState: TOmniValue; inline;
    function  GetUniqueID: int64; inline;
    procedure SetResult(const value: TOmniValue); inline;
    procedure SetSkipCompletionHandler(const value: boolean);
  protected //IOmniWorkItemEx
    function  GetConfig: IOmniWorkItemConfig; inline;
    procedure SetConfig(const value: IOmniWorkItemConfig); inline;
    procedure SetTask(const task: IOmniTask; const taskState: TOmniValue); inline;
  public
    constructor Create(const data: TOmniValue; uniqueID: int64; var cancelAllUpToID:
      TOmniAlignedInt64);
    destructor  Destroy; override;
  public //IOmniWorkItem
    function  DetachException: Exception;
    function  FatalException: Exception;
    function  IsExceptional: boolean; inline;
    property Data: TOmniValue read GetData;
    property Result: TOmniValue read GetResult write SetResult;
    property SkipCompletionHandler: boolean read GetSkipCompletionHandler write
      SetSkipCompletionHandler;
    property UniqueID: int64 read GetUniqueID;
  public //IOmniWorkItemEx
    property Config: IOmniWorkItemConfig read GetConfig write SetConfig;
  end; { TOmniWorkItem }

  TOmniWorkItemConfig = class(TInterfacedObject, IOmniWorkItemConfig, IOmniWorkItemConfigEx)
  strict private
    FOnExecute        : TOmniBackgroundWorkerDelegate;
    FOnRequestDone    : TOmniWorkItemDoneDelegate;
    FOnRequestDone_Asy: TOmniWorkItemDoneDelegate;
  public
    constructor Create(defaults: IOmniWorkItemConfig = nil);
  public //IOmniWorkItemConfig
    function  OnExecute(const aTask: TOmniBackgroundWorkerDelegate): IOmniWorkItemConfig;
    function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
    function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
  public //IOmniWorkItemConfigEx
    function  GetOnExecute: TOmniBackgroundWorkerDelegate;
    function  GetOnRequestDone: TOmniWorkItemDoneDelegate;
    function  GetOnRequestDone_Asy: TOmniWorkItemDoneDelegate;
  end; { TOmniWorkItemConfig }

  TOmniBackgroundWorker = class(TInterfacedObject, IOmniBackgroundWorker)
  strict private
    FCancelAllToID    : TOmniAlignedInt64;
    FDefaultConfig    : IOmniWorkItemConfig;
    FDefaultConfigEx  : IOmniWorkItemConfigEx;
    FNumTasks         : integer;
    {$IFDEF MSWINDOWS}
    FObserver         : TOmniContainerObserver;
    {$ENDIF MSWINDOWS}
    FOnStop           : TOmniTaskStopDelegate;
    FStopOn           : IOmniCancellationToken;
    FTaskConfig       : IOmniTaskConfig;
    FTaskFinalizer    : TOmniTaskFinalizerDelegate;
    FTaskInitializer  : TOmniTaskInitializerDelegate;
    FUniqueID         : IOmniCounter;
    {$IFDEF MSWINDOWS}
    FWindow           : THandle;
    {$ENDIF MSWINDOWS}
    FWorker           : IOmniPipeline;
  strict protected
    procedure BackgroundWorker(const input, output: IOmniBlockingCollection;
      const task: IOmniTask);
    {$IFDEF MSWINDOWS}
    procedure ObserverWndProc(var message: TMessage);
    {$ENDIF MSWINDOWS}
  public
    constructor Create;
    destructor  Destroy; override;
    procedure CancelAll; overload;
    procedure CancelAll(upToUniqueID: int64); overload;
    function  Config: IOmniWorkItemConfig;
    function  CreateWorkItem(const data: TOmniValue): IOmniWorkItem;
    function  Execute(const aTask: TOmniBackgroundWorkerDelegate = nil): IOmniBackgroundWorker;
    function  Finalize(taskFinalizer: TOmniTaskFinalizerDelegate): IOmniBackgroundWorker;
    function  Initialize(taskInitializer: TOmniTaskInitializerDelegate): IOmniBackgroundWorker;
    function  NumTasks(numTasks: integer): IOmniBackgroundWorker;
    function  OnRequestDone(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
    function  OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate): IOmniBackgroundWorker;
    function  OnStop(stopCode: TProc): IOmniBackgroundWorker; overload;
    function  OnStop(stopCode: TOmniTaskStopDelegate): IOmniBackgroundWorker; overload;
    function  OnStopInvoke(stopCode: TProc): IOmniBackgroundWorker;
    procedure Schedule(const workItem: IOmniWorkItem; const workItemConfig: IOmniWorkItemConfig = nil);
    function  StopOn(const token: IOmniCancellationToken): IOmniBackgroundWorker;
    function  TaskConfig(const config: IOmniTaskConfig): IOmniBackgroundWorker;
    function  Terminate(maxWait_ms: cardinal): boolean;
    function  WaitFor(maxWait_ms: cardinal): boolean;
  end; { TOmniBackgroundWorker}

  TOmniAwait = class(TInterfacedObject, IOmniAwait)
  strict private
    FAsync: TProc;
  public
    constructor Create(async: TProc);
    procedure Await(proc: TProc);
  end; { TOmniAwait }

var
  GParallelPool: IOmniThreadPool;

{ exports }

procedure AddToBC(const queue: IOmniBlockingCollection; value: IInterface);
begin
  queue.Add(value);
end; { AddToBC }

function Async(proc: TProc): IOmniAwait;
begin
  Result := TOmniAwait.Create(proc);
end; { Async }

function GlobalParallelPool: IOmniThreadPool;
begin
  if not assigned(GParallelPool) then begin
    GParallelPool := CreateThreadPool('OtlParallel pool');
    GParallelPool.IdleWorkerThreadTimeout_sec := 60; // 1 minute
    GParallelPool.MaxExecuting := -1;
    GParallelPool.MaxQueuedTime_sec := 0;
  end;
  Result := GParallelPool;
end; { GlobalParallelPool }

{ EJoinException }

constructor EJoinException.Create;
begin
  inherited Create('');
  FExceptions := TGpIntegerObjectList.Create(true);
end; { EJoinException.Create }

destructor EJoinException.Destroy;
begin
  FreeAndNil(FExceptions);
  inherited;
end; { EJoinException.Destroy }

procedure EJoinException.Add(iTask: integer; taskException: Exception);
begin
  FExceptions.AddObject(iTask, taskException);
  if Message = '' then
    Message := taskException.Message
  else
    Message := Message + '; '#13#10 + taskException.Message;
end; { EJoinException.Add }

function EJoinException.Count: integer;
begin
  Result := FExceptions.Count;
end; { EJoinException.Count }

function EJoinException.DetachInner(idxException: integer): Exception;
var
  i: integer;
begin
  Result := Exception(FExceptions.ExtractObject(idxException));
  Message := '';
  for i := 0 to Count - 1 do
    if Message = '' then
      Message := Inner[i].FatalException.Message
    else
      Message := Message + '; '#13#10 + Inner[i].FatalException.Message;
end; { EJoinException.DetachInner }

function EJoinException.GetInner(idxException: integer): TJoinInnerException;
begin
  Result.FatalException := Exception(FExceptions.Objects[idxException]);
  Result.TaskNumber := FExceptions[idxException];
end; { EJoinException.GetInner }

{ TOmniJoinState }

constructor TOmniJoinState.Create(const globalCancelationFlag, globalExceptionFlag:
  IOmniCancellationToken);
begin
  inherited Create;
  FGlobalCancelationFlag := globalCancelationFlag;
  FGlobalExceptionFlag := globalExceptionFlag;
end; { TOmniJoinState.Create }

procedure TOmniJoinState.Cancel;
begin
  FGlobalCancelationFlag.Signal;
end; { TOmniJoinState.Cancel }

function TOmniJoinState.GetTask: IOmniTask;
begin
  Result := FTask;
end; { TOmniJoinState.GetTask }

function TOmniJoinState.GetTaskControl: IOmniTaskControl;
begin
  Result := FTaskControl;
end; { TOmniJoinState.GetTaskControl }

function TOmniJoinState.IsCancelled: boolean;
begin
  Result := FGlobalCancelationFlag.IsSignalled;
end; { TOmniJoinState.IsCancelled }

function TOmniJoinState.IsExceptional: boolean;
begin
  Result := FGlobalExceptionFlag.IsSignalled;
end; { TOmniJoinState.IsExceptional }

procedure TOmniJoinState.SetTask(const aTask: IOmniTask);
begin
  FTask := aTask;
end; { TOmniJoinState.SetTask }

procedure TOmniJoinState.SetTaskControl(const aTask: IOmniTaskControl);
begin
  FTaskControl := aTask;
end; { TOmniJoinState.SetTaskControl }

{ TOmniParallelJoin }

constructor TOmniParallelJoin.Create;
begin
  inherited Create;
  FTasks := TList<TOmniJoinDelegate>.Create;
  FNumTasks := Environment.Process.Affinity.Count;
  FGlobalCancellationFlag := CreateOmniCancellationToken;
  FGlobalExceptionFlag := CreateOmniCancellationToken;
end; { TOmniParallelJoin.Create }

destructor TOmniParallelJoin.Destroy;
var
  iTask: integer;
begin
  for iTask := Low(FJoinStates) to High(FJoinStates) do begin
    (FJoinStates[iTask] as IOmniJoinStateEx).TaskControl.Terminate;
    (FJoinStates[iTask] as IOmniJoinStateEx).TaskControl := nil;
  end;
  FreeAndNil(FTasks);
  inherited Destroy;
end; { TOmniParallelJoin.Destroy }

function TOmniParallelJoin.Cancel: IOmniParallelJoin;
begin
  FGlobalCancellationFlag.Signal;
  Result := Self;
end; { TOmniParallelJoin.Cancel }

function TOmniParallelJoin.DetachException: Exception;
begin
  Result := FatalException; // this will in turn detach exception from task
  FTaskException := nil;
end; { TOmniParallelJoin.DetachException }

procedure TOmniParallelJoin.DoOnStop(const task: IOmniTask);
begin
  if assigned(FOnStop) then
    FOnStop(task);
end; { TOmniParallelJoin.DoOnStop }

function TOmniParallelJoin.Execute: IOmniParallelJoin;
var
  iProc      : integer;
  taskControl: IOmniTaskControl;
  taskCount  : integer;
begin
  taskCount := FNumTasks;
  if taskCount > FTasks.Count then
    taskCount := FTasks.Count;
  SetLength(FJoinStates, taskCount);
  FCountStopped := CreateResourceCount(taskCount + 1);
  FInput := TOmniBlockingCollection.Create;
  for iProc := 0 to taskCount - 1 do
    FJoinStates[iProc] := TOmniJoinState.Create(FGlobalCancellationFlag, FGlobalExceptionFlag);
  for iProc := 0 to FTasks.Count - 1 do
    FInput.Add(iProc);
  FInput.CompleteAdding;
  for iProc := 0 to taskCount - 1 do begin
    taskControl :=
      CreateTask(
        procedure (const task: IOmniTask)
        var
          joinStateEx: IOmniJoinStateEx;
          numWorker  : integer;
          procNum    : TOmniValue;
        begin
          try
            numWorker := Task.Param['NumWorker'].AsInteger;
            joinStateEx := FJoinStates[numWorker] as IOmniJoinStateEx;
            for procNum in FInput do begin
              joinStateEx.SetTask(task);
              try
                FTasks[procNum](FJoinStates[numWorker]);
              except
                FTaskExceptionLock.Acquire;
                try
                  if not assigned(FTaskException) then
                    FTaskException := EJoinException.Create;
                  EJoinException(FTaskException).Add(procNum, Exception(AcquireExceptionObject));
                finally FTaskExceptionLock.Release; end;
                FGlobalExceptionFlag.Signal;
              end;
            end; //for ovTask
          finally
            if FCountStopped.Allocate = 1 then begin
              if FNoWait then
                DoOnStop(task);
              FCountStopped.Allocate;
            end;
          end;
        end,
        Format('Join worker #%d', [iProc + 1])
      ).SetParameter('NumWorker', iProc);
    Parallel.ApplyConfig(FTaskConfig, taskControl);
    taskControl.Unobserved;
    (FJoinStates[iProc] as IOmniJoinStateEx).TaskControl := taskControl;
    Parallel.Start(taskControl, FTaskConfig);
  end;
  if not FNoWait then begin
    WaitFor(INFINITE);
    DoOnStop(nil);
    Result := nil;
  end
  else
    Result := Self;
end; { TOmniParallelJoin.Execute }

function TOmniParallelJoin.FatalException: Exception;
begin
  Result := FTaskException;
end; { TOmniParallelJoin.FatalException }

function TOmniParallelJoin.InternalWaitFor(timeout_ms: cardinal): boolean;
begin
  // Blocks until FCountStopped value is zero.
  {$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(FCountStopped.Handle, timeout_ms) = WAIT_OBJECT_0;
  {$ELSE}
  Result := FCountStopped.Synchro.WaitFor(timeout_ms) = wrSignaled;
  {$ENDIF}
end; { TOmniParallelJoin.InternalWaitFor }

function TOmniParallelJoin.IsCancelled: boolean;
begin
  Result := FGlobalCancellationFlag.IsSignalled;
end; { TOmniParallelJoin.IsCancelled }

function TOmniParallelJoin.IsExceptional: boolean;
begin
  Result := FGlobalExceptionFlag.IsSignalled;
end; { TOmniParallelJoin.IsExceptional }

function TOmniParallelJoin.NoWait: IOmniParallelJoin;
begin
  FNoWait := true;
  Result := Self;
end; { TOmniParallelJoin.NoWait }

function TOmniParallelJoin.NumTasks(numTasks: integer): IOmniParallelJoin;
begin
  Assert(numTasks <> 0);
  if numTasks > 0 then
    FNumTasks := numTasks
  else
    FNumTasks := Environment.Process.Affinity.Count + numTasks;
  Result := Self;
end; { TOmniParallelJoin.NumTasks }

function TOmniParallelJoin.OnStop(const stopCode: TProc): IOmniParallelJoin;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
end; { TOmniParallelJoin.OnStop }

function TOmniParallelJoin.OnStop(const stopCode: TOmniTaskStopDelegate):
  IOmniParallelJoin;
begin
   FOnStop := stopCode;
   Result := Self;
end; { TOmniParallelJoin.OnStop }

function TOmniParallelJoin.OnStopInvoke(const stopCode: TProc): IOmniParallelJoin;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelJoin.OnStopInvoke }

function TOmniParallelJoin.Task(const aTask: TOmniJoinDelegate): IOmniParallelJoin;
begin
  FTasks.Add(aTask);
  Result := Self;
end; { TOmniParallelJoin.Task }

function TOmniParallelJoin.Task(const aTask: TProc): IOmniParallelJoin;
begin
  Result := Task(
    procedure (const joinState: IOmniJoinState)
    begin
      aTask
    end);
end; { TOmniParallelJoin.Task }

function TOmniParallelJoin.TaskConfig(const config: IOmniTaskConfig): IOmniParallelJoin;
begin
  FTaskConfig := config;
  Result := Self;
end; { TOmniParallelJoin.TaskConfig }

function TOmniParallelJoin.WaitFor(timeout_ms: cardinal): boolean;
var
  taskExcept: Exception;
begin
  Result := InternalWaitFor(timeout_ms);
  if Result then begin
    if assigned(FTaskException) then begin
      taskExcept := FTaskException;
      FTaskException := nil;
      raise taskExcept;
    end;
  end;
end; { TOmniParallelJoin.WaitFor }

{ Parallel }

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

class procedure Parallel.Async(task: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
var
  omniTask  : IOmniTaskControl;
  terminated: TOmniTaskConfigTerminated;
begin
  if assigned(taskConfig) then
    (taskConfig as IOmniTaskConfigInternal).DetachTerminated(terminated);
  omniTask := CreateTask(task, 'Parallel.Async').Unobserved.OnTerminated(
    procedure (const task: IOmniTaskControl)
    var
      exc: Exception;
    begin
      terminated.Call(task);
      exc := task.DetachException;
      if assigned(exc) then
        raise exc;
    end
  );
  Parallel.ApplyConfig(taskConfig, omniTask);
  omniTask.Unobserved;
  Parallel.Start(omniTask, taskConfig);
end; { Parallel.Async }

class function Parallel.BackgroundWorker: IOmniBackgroundWorker;
begin
  Result := TOmniBackgroundWorker.Create;
end; { Parallel.BackgroundWorker }

class procedure Parallel.ApplyConfig(const taskConfig: IOmniTaskConfig; const task: IOmniTaskControl);
begin
  if assigned(taskConfig) then
    taskConfig.Apply(task);
end; { Parallel.ApplyConfig }

class function Parallel.CompleteQueue(const queue: IOmniBlockingCollection): TProc;
begin
  Result :=
    procedure
    begin
      queue.CompleteAdding;
    end;
end; { Parallel.CompleteQueue }

class function Parallel.&For(first, last: integer; step: integer = 1): IOmniParallelSimpleLoop;
begin
  Result := TOmniParallelSimpleLoop.Create(first, last, step);
end; { Parallel.&For }

{$IFDEF OTL_GoodGenerics}
class function Parallel.&For<T>(const arr: TArray<T>): IOmniParallelSimpleLoop<T>;
begin
  Result := TOmniParallelSimpleLoop<T>.Create(arr);
end; { Parallel }
{$ENDIF OTL_GoodGenerics}

class function Parallel.ForEach(const enumerable: IOmniValueEnumerable):
  IOmniParallelLoop;
begin
  // Assumes that enumerator's TryTake method is threadsafe!
  Result := Parallel.ForEach(enumerable.GetEnumerator);
end; { Parallel.ForEach }

class function Parallel.ForEach(first, last: integer; step: integer = 1):
  IOmniParallelLoop<integer>;
begin
  Result := TOmniParallelLoop<integer>.Create(CreateSourceProvider(first, last, step), true);
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

class function Parallel.Join(const task1, task2: TProc): IOmniParallelJoin;
begin
  Result := TOmniParallelJoin.Create.Task(task1).Task(task2);
end; { Parallel.Join }

class function Parallel.Join(const tasks: array of TProc): IOmniParallelJoin;
var
  aTask: TProc;
begin
  Result := TOmniParallelJoin.Create;
  for aTask in tasks do
    Result.Task(aTask);
end; { Parallel.Join }

class function Parallel.Join(const task1, task2: TOmniJoinDelegate): IOmniParallelJoin;
begin
  Result := TOmniParallelJoin.Create.Task(task1).Task(task2);
end; { Parallel.Join }

class function Parallel.Join(const tasks: array of TOmniJoinDelegate): IOmniParallelJoin;
var
  aTask: TOmniJoinDelegate;
begin
  Result := TOmniParallelJoin.Create;
  for aTask in tasks do
    Result.Task(aTask);
end; { Parallel.Join }

class function Parallel.Join: IOmniParallelJoin;
begin
  Result := TOmniParallelJoin.Create;
end; { Parallel.Join }

{$IFDEF OTL_HasArrayOfT}
{$IFDEF OTL_GoodGenerics}
class function Parallel.Map<T1, T2>(const source: TArray<T1>; mapper: TMapProc<T1,T2>):
  TArray<T2>;
var
  map: IOmniParallelMapper<T1,T2>;
begin
  map := Parallel.Map<T1,T2>.Source(source);
  map.Execute(mapper);
  map.WaitFor(INFINITE);
  Result := map.Result;
end; { Parallel.Map }

class function Parallel.Map<T1,T2>: IOmniParallelMapper<T1,T2>;
begin
  Result := TOmniParallelMapper<T1,T2>.Create;
end; { Parallel.Map }
{$ENDIF OTL_GoodGenerics}
{$ENDIF OTL_HasArrayOfT}

class function Parallel.ParallelTask: IOmniParallelTask;
begin
  Result := TOmniParallelTask.Create;
end; { Parallel.ParallelTask }

class function Parallel.Pipeline: IOmniPipeline;
begin
  Result := TOmniPipeline.Create;
end; { Parallel.Pipeline }

class function Parallel.Pipeline(const stages: array of TPipelineStageDelegate; const
  input: IOmniBlockingCollection): IOmniPipeline;
begin
  Result := Parallel.Pipeline;
  if assigned(input) then
    Result.From(input);
  Result.Stages(stages);
end; { Parallel.Pipeline }

class function Parallel.Pipeline(const stages: array of TPipelineStageDelegateEx; const
  input: IOmniBlockingCollection): IOmniPipeline;
begin
  Result := Parallel.Pipeline;
  if assigned(input) then
    Result.From(input);
  Result.Stages(stages);
end; { Parallel.Pipeline }

class procedure Parallel.Start(const taskControl: IOmniTaskControl; const taskConfig:
  IOmniTaskConfig);
var
  taskCfg: IOmniTaskConfigInternal;
begin
  if not assigned(taskConfig) then
    taskControl.Schedule(GlobalParallelPool)
  else begin
    taskCfg := (taskConfig as IOmniTaskConfigInternal);
    if taskCfg.RunDirectly then
      taskControl.Run
    else
      taskControl.Schedule(taskCfg.GetThreadPool);
  end;
end; { Parallel.Start }

class function Parallel.TaskConfig: IOmniTaskConfig;
begin
  Result := TOmniTaskConfig.Create;
end; { Parallel.TaskConfig }

class function Parallel.TimedTask: IOmniTimedTask;
begin
  Result := TOmniTimedTask.Create;
end; { Parallel.TimedTask }

{ TOmniParallelLoopBase }

constructor TOmniParallelLoopBase.Create(const sourceProvider: TOmniSourceProvider;
  managedProvider: boolean);
begin
  inherited Create;
  FNumTasks := Environment.Process.Affinity.Count;
  FSourceProvider := sourceProvider;
  FManagedProvider := managedProvider;
  FOnMessageList := TGpIntegerObjectList.Create;
end; { TOmniParallelLoopBase.Create }

constructor TOmniParallelLoopBase.Create(const enumerator: TEnumeratorDelegate);
begin
  FDelegateEnum := TOmniDelegateEnumerator.Create(enumerator);
  Create(CreateSourceProvider(FDelegateEnum), true);
end; { TOmniParallelLoopBase.Create }

{$IFDEF OTL_ERTTI}
constructor TOmniParallelLoopBase.Create(enumerable: TObject);
var
  rm: TRttiMethod;
  rt: TRttiType;
begin
  FRttiContext := TRttiContext.Create;
  rt := FRttiContext.GetType(enumerable.ClassType);
  Assert(assigned(rt));
  rm := rt.GetMethod('GetEnumerator');
  Assert(assigned(rm));
  Assert(assigned(rm.ReturnType) and (rm.ReturnType.TypeKind = tkClass));
  FEnumerable := rm.Invoke(enumerable, []);
  Assert(FEnumerable.AsObject <> nil);
  rt := FRttiContext.GetType(FEnumerable.TypeInfo);
  FMoveNext := rt.GetMethod('MoveNext');
  Assert(assigned(FMoveNext));
  Assert((FMoveNext.ReturnType.TypeKind = tkEnumeration) and SameText(FMoveNext.ReturnType.Name, 'Boolean'));
  FGetCurrent := rt.GetMethod('GetCurrent');
  Assert(assigned(FGetCurrent));
  FDestroy := rt.GetMethod('Destroy');
  Assert(assigned(FDestroy));
  Create(
    function (var next: TOmniValue): boolean begin
      Result := FMoveNext.Invoke(FEnumerable, []).AsBoolean;
      if Result then
        next := FGetCurrent.Invoke(FEnumerable, []);
    end
  );
end; { TOmniParallelLoopBase.Create }
{$ENDIF OTL_ERTTI}

destructor TOmniParallelLoopBase.Destroy;
begin
  if assigned(FCountStopped) then
    {$IFDEF MSWINDOWS}
    WaitForSingleObject(FCountStopped.Handle, INFINITE);
    {$ELSE}
    FCountStopped.Synchro.WaitFor(INFINITE);
    {$ENDIF ~MSWINDOWS}
  if FManagedProvider then
    FreeAndNil(FSourceProvider);
  FreeAndNil(FDelegateEnum);
  FreeAndNil(FDataManager);
  FreeAndNil(FOnMessageList);
  {$IFDEF OTL_ERTTI}
  if FEnumerable.AsObject <> nil then begin
    FDestroy.Invoke(FEnumerable, []);
    FRttiContext.Free;
  end;
  {$ENDIF OTL_ERTTI}
  inherited;
end; { TOmniParallelLoopBase.Destroy }

procedure TOmniParallelLoopBase.DoOnStop(const task: IOmniTask);
begin
  if assigned(FOnStop) then
    FOnStop(task);
end; { TOmniParallelLoopBase.DoOnStop }

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorTaskDelegate);
begin
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue: TOmniLocalQueue;
      value     : TOmniValue;
    begin
      localQueue := FDataManager.CreateLocalQueue;
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
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue; var taskState: TOmniValue)
    begin
      loopBody(value, taskState);
    end
  );
end; { TOmniParallelLoopBase.InternalExecute }

procedure TOmniParallelLoopBase.InternalExecute(loopBody: TOmniIteratorStateTaskDelegate);
begin
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue: TOmniLocalQueue;
      taskState : TOmniValue;
      value     : TOmniValue;
    begin
      FTaskInitializer(taskState);
      try
        localQueue := FDataManager.CreateLocalQueue;
        try
          while (not Stopped) and localQueue.GetNext(value) do
            loopBody(task, value, taskState);
        finally FreeAndNil(localQueue); end;
      finally FTaskFinalizer(taskState); end;
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
      localQueue := FDataManager.CreateLocalQueue;
      try
        result.Clear;
        while (not Stopped) and localQueue.GetNext(value) do begin
          loopBody(task, value, result);
          if not result.IsEmpty then begin
            FAggregator(aggregate, result);
            result.Clear;
          end;
        end;
      finally FreeAndNil(localQueue); end;
      if not assigned(task) then
        FAggregate := aggregate
      else begin
        task.Lock.Acquire;
        try
          FAggregator(FAggregate, aggregate);
        finally task.Lock.Release; end;
      end;
    end
  );

  Result := FAggregate;
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
  Assert(assigned(FIntoQueueIntf));
  if (ploPreserveOrder in Options) and (FNumTasks > 1) then
    InternalExecuteIntoOrdered(loopBody)
  else // no order preservation; no output buffering required
    InternalExecuteTask(
      procedure (const task: IOmniTask)
      var
        localQueue: TOmniLocalQueue;
        result    : TOmniValue;
        value     : TOmniValue;
      begin
        localQueue := FDataManager.CreateLocalQueue;
        try
          result.Clear;
          while (not Stopped) and localQueue.GetNext(value) do begin
            loopBody(task, value, result);
            if not result.IsEmpty then begin
              FIntoQueueIntf.Add(result);
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
  Assert(assigned(FIntoQueueIntf));
  InternalExecuteTask(
    procedure (const task: IOmniTask)
    var
      localQueue      : TOmniLocalQueue;
      outputBuffer_ref: TOmniOutputBuffer;
      position        : int64;
      result          : TOmniValue;
      value           : TOmniValue;
    begin
      FDataManager.SetOutput(FIntoQueueIntf);
      localQueue := FDataManager.CreateLocalQueue;
      try
        outputBuffer_ref := FDataManager.AllocateOutputBuffer;
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
        finally FDataManager.ReleaseOutputBuffer(outputBuffer_ref); end;
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
  dmOptions    : TOmniDataManagerOptions;
  iTask        : integer;
  kv           : TGpKeyValue;
  lockAggregate: IOmniCriticalSection;
  numTasks     : integer;
  task         : IOmniTaskControl;
begin
  dmOptions := [];
  numTasks := FNumTasks;
  if ploPreserveOrder in Options then begin
    Include(dmOptions, dmoPreserveOrder);
    if (numTasks > 1) and (not FNumTasksManual) then
      Dec(numTasks);
  end
  else if (ploNoWait in Options) and (numTasks > 1) and (not FNumTasksManual) then
    Dec(numTasks);
  FDataManager := CreateDataManager(FSourceProvider, numTasks, dmOptions); // destructor will do the cleanup
  FCountStopped := CreateResourceCount(numTasks + 1);
  lockAggregate := CreateOmniCriticalSection;
  for iTask := 1 to numTasks do begin
    task := CreateTask(
      procedure (const task: IOmniTask)
      begin
        if assigned(FOnTaskCreate) then
          FOnTaskCreate(task);
        taskDelegate(task);
        if FCountStopped.Allocate = 1 then begin
          if ploNoWait in Options then begin
            if assigned(FIntoQueueIntf) then
              FIntoQueueIntf.CompleteAdding;
            DoOnStop(task);
          end;
          FCountStopped.Allocate;
        end;
      end,
      'Parallel.ForEach worker #' + IntToStr(iTask))
      .WithLock(lockAggregate);
    Parallel.ApplyConfig(FTaskConfig, task);
    task.Unobserved;
    for kv in FOnMessageList.WalkKV do
      task.OnMessage(kv.Key, TOmniMessageExec.Clone(TOmniMessageExec(kv.Value)));
    if assigned(FOnTaskControlCreate) then
      FOnTaskControlCreate(task);
    Parallel.Start(task, FTaskConfig);
  end;
  if not (ploNoWait in Options) then begin
    {$IFDEF MSWINDOWS}
    WaitForSingleObject(FCountStopped.Handle, INFINITE);
    {$ELSE}
    FCountStopped.Synchro.WaitFor(INFINITE);
    {$ENDIF ~MSWINDOWS}
    if assigned(FIntoQueueIntf) then
      FIntoQueueIntf.CompleteAdding;
    DoOnStop(nil);
  end;
end; { TOmniParallelLoopBase.InternalExecuteTask }

procedure TOmniParallelLoopBase.SetAggregator(defaultAggregateValue: TOmniValue;
  aggregator: TOmniAggregatorDelegate);
begin
  FAggregator := aggregator;
  FAggregate := defaultAggregateValue;
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
  FCancellationToken := token;
end; { TOmniParallelLoopBase.SetCancellationToken }

procedure TOmniParallelLoopBase.SetFinalizer(taskFinalizer: TOmniTaskFinalizerDelegate);
begin
  FTaskFinalizer := taskFinalizer;
end; { TOmniParallelLoopBase.SetFinalizer }

procedure TOmniParallelLoopBase.SetInitializer(taskInitializer:
  TOmniTaskInitializerDelegate);
begin
  FTaskInitializer := taskInitializer;
end; { TOmniParallelLoopBase.SetInitializer }

procedure TOmniParallelLoopBase.SetIntoQueue(const queue: IOmniBlockingCollection);
begin
  FIntoQueueIntf := queue;
end; { TOmniParallelLoopBase.SetIntoQueue }

procedure TOmniParallelLoopBase.SetNumTasks(taskCount: integer);
begin
  Assert(taskCount <> 0);
  if taskCount > 0 then
    FNumTasks := taskCount
  else
    FNumTasks := Environment.Process.Affinity.Count + taskCount;
  FNumTasksManual := true;
end; { TOmniParallelLoopBase.SetNumTasks }

procedure TOmniParallelLoopBase.SetOnMessage(eventDispatcher: TObject);
begin
  FOnMessageList.AddObject(COtlReservedMsgID, TOmniMessageExec.Create(eventDispatcher));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnMessage(msgID: word;
  eventHandler: TOmniTaskMessageEvent);
begin
  FOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnMessage(msgID: word; eventHandler: TOmniOnMessageFunction);
begin
  FOnMessageList.AddObject(msgID, TOmniMessageExec.Create(eventHandler));
end; { TOmniParallelLoopBase.SetOnMessage }

procedure TOmniParallelLoopBase.SetOnStop(stopDelegate: TOmniTaskStopDelegate);
begin
  FOnStop := stopDelegate;
end; { TOmniParallelLoopBase.SetOnStop }

procedure TOmniParallelLoopBase.SetOnTaskCreate(taskCreateDelegate: TOmniTaskCreateDelegate);
begin
  Assert(not assigned(FOnTaskCreate));
  FOnTaskCreate := taskCreateDelegate;
end; { TOmniParallelLoopBase.SetOnTaskCreate }

procedure TOmniParallelLoopBase.SetOnTaskCreate(
  taskCreateDelegate: TOmniTaskControlCreateDelegate);
begin
  Assert(not assigned(FOnTaskControlCreate));
  FOnTaskControlCreate := taskCreateDelegate;
end; { TOmniParallelLoopBase.SetOnTaskCreate }

procedure TOmniParallelLoopBase.SetTaskConfig(const config: IOmniTaskConfig);
begin
  FTaskConfig := config;
end; { TOmniParallelLoopBase.SetTaskConfig }

function TOmniParallelLoopBase.Stopped: boolean;
begin
  Result := (assigned(FCancellationToken) and FCancellationToken.IsSignalled);
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

procedure TOmniParallelLoop.Execute(loopBody: TOmniIteratorStateTaskDelegate);
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
  SetOnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
  Result := Self;
end; { TOmniParallelLoop.OnStop }

function TOmniParallelLoop.OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop.OnStop }

function TOmniParallelLoop.OnStopInvoke(stopCode: TProc): IOmniParallelLoop;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelLoop.OnStopInvoke }

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
  FDelegateEnum := TOmniDelegateEnumerator<T>.Create(enumerator);
  Create(CreateSourceProvider(FDelegateEnum), true);
end; { TOmniParallelLoop }

constructor TOmniParallelLoop<T>.Create(const enumerator: TEnumerator<T>);
begin
  FEnumerator := enumerator;
  Create(
    function(var next: T): boolean
    begin
      Result := FEnumerator.MoveNext;
      if Result then
        next := FEnumerator.Current;
    end
  );
end; { TOmniParallelLoop<T>.Create }

destructor TOmniParallelLoop<T>.Destroy;
begin
  FreeAndNil(FDelegateEnum);
  FreeAndNil(FEnumerator);
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
      loopBody(value.CastTo<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.ExecuteAggregate }

function TOmniParallelLoop<T>.ExecuteAggregate(
  loopBody: TOmniIteratorIntoTaskDelegate<T>): TOmniValue;
begin
  Result := InternalExecuteAggregate(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(task, value.CastTo<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.ExecuteAggregate }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorDelegate<T>);
begin
  InternalExecute(
    procedure (const value: TOmniValue)
    begin
      loopBody(value.CastTo<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorTaskDelegate<T>);
begin
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue)
    begin
      loopBody(task, value.CastTo<T>);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorIntoDelegate<T>);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(value.CastTo<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorIntoTaskDelegate<T>);
begin
  InternalExecuteInto(
    procedure (const task: IOmniTask; const value: TOmniValue; var result: TOmniValue)
    begin
      loopBody(task, value.CastTo<T>, result);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorStateDelegate<T>);
begin
  InternalExecute(
    procedure (const value: TOmniValue; var taskState: TOmniValue)
    begin
      loopBody(value.CastTo<T>, taskState);
    end
  );
end; { TOmniParallelLoop }

procedure TOmniParallelLoop<T>.Execute(loopBody: TOmniIteratorStateTaskDelegate<T>);
begin
  InternalExecute(
    procedure (const task: IOmniTask; const value: TOmniValue; var taskState: TOmniValue)
    begin
      loopBody(task, value.CastTo<T>, taskState);
    end
  );
end; { TOmniParallelLoop<T>.Execute }

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
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end
  );
end; { TOmniParallelLoop<T>.OnStop }

function TOmniParallelLoop<T>.OnStop(stopCode: TOmniTaskStopDelegate): IOmniParallelLoop<T>;
begin
  SetOnStop(stopCode);
  Result := Self;
end; { TOmniParallelLoop }

{$IFDEF OTL_FixedGenericIncompletelyDefined}
function TOmniParallelLoop<T>.OnStopInvoke(stopCode: TProc): IOmniParallelLoop<T>;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelLoop<T>.OnStopInvoke }
{$ENDIF OTL_FixedGenericIncompletelyDefined}

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
  inherited Create;
  FDelegate := delegate;
end; { TOmniDelegateEnumerator.Create }

function TOmniDelegateEnumerator.GetCurrent: TOmniValue;
begin
  Result := FValue;
end; { TOmniDelegateEnumerator.GetCurrent }

function TOmniDelegateEnumerator.MoveNext: boolean;
begin
  Result := FDelegate(FValue);
end; { TOmniDelegateEnumerator.MoveNext }

{ TOmniDelegateEnumerator<T> }

constructor TOmniDelegateEnumerator<T>.Create(delegate: TEnumeratorDelegate<T>);
begin
  FDelegate := delegate;
end; { TOmniDelegateEnumerator }

function TOmniDelegateEnumerator<T>.GetCurrent: TOmniValue;
begin
  Result := TOmniValue.CastFrom<T>(FValue);
end; { TOmniDelegateEnumerator }

function TOmniDelegateEnumerator<T>.MoveNext: boolean;
begin
  Result := FDelegate(FValue);
end; { TOmniDelegateEnumerator }

{ TOmniParallelSimpleLoop }

constructor TOmniParallelSimpleLoop.Create(first, last, step: integer);
begin
  if step = 0 then
    raise Exception.Create('TOmniParallelSimpleLoop.Create: step must not be 0');
  FFirst := first;
  FLast := last;
  FStep := step;
  FNumTasks := Environment.Process.Affinity.Count;
  FOnMessageList := TGpIntegerObjectList.Create;
end; { TOmniParallelSimpleLoop.Create }

destructor TOmniParallelSimpleLoop.Destroy;
begin
  FreeAndNil(FOnMessageList);
  if assigned(FCountStopped) then
    {$IFDEF MSWINDOWS}
    WaitForSingleObject(FCountStopped.Handle, INFINITE);
    {$ELSE}
    FCountStopped.Synchro.WaitFor(INFINITE);
    {$ENDIF ~MSWINDOWS}
  inherited;
end; { TOmniParallelSimpleLoop.Destroy }

function TOmniParallelSimpleLoop.CancelWith(
  const token: IOmniCancellationToken): IOmniParallelSimpleLoop;
begin
  FCancelWith := token;
  Result := Self;
end; { TOmniParallelSimpleLoop.CancelWith }

function TOmniParallelSimpleLoop.CreateForTask(taskIndex: integer;
  const taskDelegate: TTaskDelegate): IOmniTaskControl;
begin
  Result := CreateTask(
    procedure (const task: IOmniTask)
    begin
      if assigned(FInitializerDelegate) then
        FInitializerDelegate(task, taskIndex, FPartition[taskIndex].LowBound, FPartition[taskIndex].HighBound);
      taskDelegate(task, taskIndex);
      if assigned(FFinalizerDelegate) then
        FFinalizerDelegate(task, taskIndex, FPartition[taskIndex].LowBound, FPartition[taskIndex].HighBound);
      if FCountStopped.Allocate = 1 then begin
        if FNoWait then
          if assigned(FOnStop) then
            FOnStop(task);
        FCountStopped.Allocate;
      end;
    end,
    'Parallel.For worker #' + IntToStr(taskIndex));
end; { TOmniParallelSimpleLoop.CreateForTask }

procedure TOmniParallelSimpleLoop.CreatePartitions(var numTasks: integer);
var
  first    : integer;
  i        : integer;
  numSteps : integer;
  thisSteps: integer;
begin
  //TOmniParallelMapper<T1,T2>.Execute assumes that partitions are created such that
  //FPartition[i].LowBound = FPartition[i-1].HighBound + 1
  SetLength(FPartition, numTasks);
  numSteps := (FLast - FFirst + FStep) div FStep;
  first := FFirst;
  i := numTasks - 1;
  while i >= 0 do begin
    thisSteps := numSteps div (i + 1);
    if thisSteps = 0 then
      Dec(numTasks)
    else begin
      FPartition[i].LowBound := first;
      FPartition[i].HighBound := first + (thisSteps - 1) * FStep;
      first := FPartition[i].HighBound + FStep;
      numSteps := numSteps - thisSteps;
    end;
    Dec(i);
  end;
end; { TOmniParallelSimpleLoop.CreatePartitions }

procedure TOmniParallelSimpleLoop.Execute(loopBody: TOmniIteratorSimpleSimpleDelegate);
begin
  InternalExecute(
    procedure (const task: IOmniTask; taskIndex: integer)
    var
      first: integer;
      last : integer;
      step : integer;
    begin
      first := FPartition[taskIndex].LowBound;
      last := FPartition[taskIndex].HighBound;
      step := FStep;
      if step > 0 then begin
        if assigned(FCancelWith) then
          while (first <= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(first);
            Inc(first, step);
          end
        else
          while first <= last do begin
            loopBody(first);
            Inc(first, step);
          end
      end
      else begin
        if assigned(FCancelWith) then
          while (first >= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(first);
            Inc(first, step);
          end
        else
          while first >= last do begin
            loopBody(first);
            Inc(first, step);
          end
      end;
    end);
end; { TOmniParallelSimpleLoop.Execute }

procedure TOmniParallelSimpleLoop.Execute(loopBody: TOmniIteratorSimpleDelegate);
begin
  InternalExecute(
    procedure (const task: IOmniTask; taskIndex: integer)
    var
      first: integer;
      last : integer;
      step : integer;
    begin
      first := FPartition[taskIndex].LowBound;
      last := FPartition[taskIndex].HighBound;
      step := FStep;
      if step > 0 then begin
        if assigned(FCancelWith) then
          while (first <= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(taskIndex, first);
            Inc(first, step);
          end
        else
          while first <= last do begin
            loopBody(taskIndex, first);
            Inc(first, step);
          end
      end
      else begin
        if assigned(FCancelWith) then
          while (first >= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(taskIndex, first);
            Inc(first, step);
          end
        else
          while first >= last do begin
            loopBody(taskIndex, first);
            Inc(first, step);
          end
      end;
    end);
end; { TOmniParallelSimpleLoop.Execute }

procedure TOmniParallelSimpleLoop.Execute(loopBody: TOmniIteratorSimpleFullDelegate);
begin
  InternalExecute(
    procedure (const task: IOmniTask; taskIndex: integer)
    var
      first: integer;
      last : integer;
      step : integer;
    begin
      first := FPartition[taskIndex].LowBound;
      last := FPartition[taskIndex].HighBound;
      step := FStep;
      if step > 0 then begin
        if assigned(FCancelWith) then
          while (first <= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(task, taskIndex, first);
            Inc(first, step);
          end
        else
          while first <= last do begin
            loopBody(task, taskIndex, first);
            Inc(first, step);
          end
      end
      else begin
        if assigned(FCancelWith) then
          while (first >= last) and (not FCancelWith.IsSignalled) do begin
            loopBody(task, taskIndex, first);
            Inc(first, step);
          end
        else
          while first >= last do begin
            loopBody(task, taskIndex, first);
            Inc(first, step);
          end
      end;
    end);
end; { TOmniParallelSimpleLoop.Execute }

function TOmniParallelSimpleLoop.Finalize(
  taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop;
begin
  FFinalizerDelegate := taskFinalizer;
  Result := Self;
end; { TOmniParallelSimpleLoop.Finalize }

function TOmniParallelSimpleLoop.Finalize(
  taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop;
begin
  Result := Finalize(
    procedure (const task: IOmniTask; taskIndex, fromIndex, toIndex: integer)
    begin
      taskFinalizer(taskIndex, fromIndex, toIndex);
    end);
end; { TOmniParallelSimpleLoop.Finalize }

function TOmniParallelSimpleLoop.Initialize(
  taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop;
begin
  Result := Initialize(
    procedure (const task: IOmniTask; taskIndex, fromIndex, toIndex: integer)
    begin
      taskInitializer(taskIndex, fromIndex, toIndex);
    end);
end; { TOmniParallelSimpleLoop.Initialize }

function TOmniParallelSimpleLoop.Initialize(
  taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop;
begin
  FInitializerDelegate := taskInitializer;
  Result := Self;
end; { TOmniParallelSimpleLoop.Initialize }

procedure TOmniParallelSimpleLoop.InternalExecute(const taskDelegate: TTaskDelegate);
var
  dmOptions    : TOmniDataManagerOptions;
  iTask        : integer;
  kv           : TGpKeyValue;
  lockAggregate: IOmniCriticalSection;
  task         : IOmniTaskControl;
  taskCount    : integer;
begin
  dmOptions := [];
  taskCount := FNumTasks;
  if FNoWait and (taskCount > 1) and (not FNumTasksManual) then
    Dec(taskCount);
  CreatePartitions(taskCount);
  FCountStopped := CreateResourceCount(taskCount + 1);
  lockAggregate := CreateOmniCriticalSection;
  for iTask := 0 to taskCount - 1 do begin
    task := CreateForTask(iTask, taskDelegate);
    Parallel.ApplyConfig(FTaskConfig, task);
    task.Unobserved;
    for kv in FOnMessageList.WalkKV do
      task.OnMessage(kv.Key, TOmniMessageExec.Clone(TOmniMessageExec(kv.Value)));
    Parallel.Start(task, FTaskConfig);
  end;
  if not FNoWait then begin
    if taskCount = 0 then
      FCountStopped.Allocate //all done
    else
      {$IFDEF MSWINDOWS}
      WaitForSingleObject(FCountStopped.Handle, INFINITE);
      {$ELSE}
      FCountStopped.Synchro.WaitFor(INFINITE);
      {$ENDIF ~MSWINDOWS}
    if assigned(FOnStop) then
      FOnStop(nil);
  end;
end; { TOmniParallelSimpleLoop.InternalExecute }

function TOmniParallelSimpleLoop.NoWait: IOmniParallelSimpleLoop;
begin
  FNoWait := true;
  Result := Self;
end; { TOmniParallelSimpleLoop.NoWait }

function TOmniParallelSimpleLoop.NumTasks(taskCount: integer): IOmniParallelSimpleLoop;
begin
  Assert(taskCount <> 0);
  if taskCount > 0 then
    FNumTasks := taskCount
  else
    FNumTasks := Environment.Process.Affinity.Count + taskCount;
  FNumTasksManual := true;
  Result := Self;
end; { TOmniParallelSimpleLoop.NumTasks }

function TOmniParallelSimpleLoop.OnStop(
  stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop;
begin
  FOnStop := stopCode;
  Result := Self;
end; { TOmniParallelSimpleLoop.OnStop }

function TOmniParallelSimpleLoop.OnStop(stopCode: TProc): IOmniParallelSimpleLoop;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
end; { TOmniParallelSimpleLoop.OnStop }

function TOmniParallelSimpleLoop.OnStopInvoke(stopCode: TProc): IOmniParallelSimpleLoop;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelSimpleLoop.OnStopInvoke }

function TOmniParallelSimpleLoop.TaskConfig(
  const config: IOmniTaskConfig): IOmniParallelSimpleLoop;
begin
  FTaskConfig := config;
  Result := Self;
end; { TOmniParallelSimpleLoop.TaskConfig }

function TOmniParallelSimpleLoop.WaitFor(maxWait_ms: cardinal): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(FCountStopped.Handle, maxWait_ms) = WAIT_OBJECT_0;
  {$ELSE}
  Result := FCountStopped.Synchro.WaitFor(maxWait_ms) = wrSignaled;
  {$ENDIF ~MSWINDOWS}
end; { TOmniParallelSimpleLoop.WaitFor }

{$IFDEF OTL_GoodGenerics}
{ TOmniParallelSimpleLoop<T> }

constructor TOmniParallelSimpleLoop<T>.Create(const arr: TArray<T>);
begin
  inherited Create;
  FData := arr;
  FIterator := Parallel.For(Low(arr), High(arr));
end; { TOmniParallelSimpleLoop<T>.Create }

function TOmniParallelSimpleLoop<T>.CancelWith(
  const token: IOmniCancellationToken): IOmniParallelSimpleLoop<T>;
begin
  FIterator.CancelWith(token);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.CancelWith; }

procedure TOmniParallelSimpleLoop<T>.Execute(
  loopBody: TOmniIteratorSimpleFullDelegate<T>);
begin
  FIterator.Execute(
    procedure (const task: IOmniTask; taskIndex, i: integer)
    begin
      loopBody(task, taskIndex, FData[i]);
    end);
end; { TOmniParallelSimpleLoop<T>.Execute }

procedure TOmniParallelSimpleLoop<T>.Execute(loopBody: TOmniIteratorSimpleDelegate<T>);
begin
  FIterator.Execute(
    procedure (taskIndex, i: integer)
    begin
      loopBody(taskIndex, FData[i]);
    end);
end; { TOmniParallelSimpleLoop<T>.Execute }

procedure TOmniParallelSimpleLoop<T>.Execute(
  loopBody: TOmniIteratorSimpleSimpleDelegate<T>);
begin
  FIterator.Execute(
    procedure (i: integer)
    begin
      loopBody(FData[i]);
    end);
end; { TOmniParallelSimpleLoop<T>.Execute }

function TOmniParallelSimpleLoop<T>.Finalize(
  taskFinalizer: TOmniSimpleTaskFinalizerDelegate): IOmniParallelSimpleLoop<T>;
begin
  FIterator.Finalize(taskFinalizer);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.Finalize }

function TOmniParallelSimpleLoop<T>.Finalize(
  taskFinalizer: TOmniSimpleTaskFinalizerTaskDelegate): IOmniParallelSimpleLoop<T>;
begin
  FIterator.Finalize(taskFinalizer);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.Finalize }

function TOmniParallelSimpleLoop<T>.Initialize(
  taskInitializer: TOmniSimpleTaskInitializerTaskDelegate): IOmniParallelSimpleLoop<T>;
begin
  FIterator.Initialize(taskInitializer);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.Initialize }

function TOmniParallelSimpleLoop<T>.Initialize(
  taskInitializer: TOmniSimpleTaskInitializerDelegate): IOmniParallelSimpleLoop<T>;
begin
  FIterator.Initialize(taskInitializer);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.Initialize }

function TOmniParallelSimpleLoop<T>.NoWait: IOmniParallelSimpleLoop<T>;
begin
  FIterator.NoWait;
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.NoWait }

function TOmniParallelSimpleLoop<T>.NumTasks(
  taskCount: integer): IOmniParallelSimpleLoop<T>;
begin
  FIterator.NumTasks(taskCount);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.NumTasks }

function TOmniParallelSimpleLoop<T>.OnStop(
  stopCode: TOmniTaskStopDelegate): IOmniParallelSimpleLoop<T>;
begin
  FIterator.OnStop(stopCode);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.OnStop }

function TOmniParallelSimpleLoop<T>.OnStop(stopCode: TProc): IOmniParallelSimpleLoop<T>;
begin
  FIterator.OnStop(stopCode);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.OnStop }

function TOmniParallelSimpleLoop<T>.OnStopInvoke(stopCode: TProc):
  IOmniParallelSimpleLoop<T>;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelSimpleLoop }

function TOmniParallelSimpleLoop<T>.TaskConfig(
  const config: IOmniTaskConfig): IOmniParallelSimpleLoop<T>;
begin
  FIterator.TaskConfig(config);
  Result := Self;
end; { TOmniParallelSimpleLoop<T>.TaskConfig }

function TOmniParallelSimpleLoop<T>.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := FIterator.WaitFor(maxWait_ms);
end; { TOmniParallelSimpleLoop<T>.WaitFor }
{$ENDIF OTL_GoodGenerics}

{ TOmniFuture<T> }

constructor TOmniFuture<T>.Create(action: TOmniFutureDelegate<T>; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  FCancellable := false;
  FCompleted := false;
  Execute(
    procedure (const task: IOmniTask)
    begin
      try
        FResult := action();
      finally // action may raise exception
        FCompleted := true;
      end;
    end,
    taskConfig);
end; { TOmniFuture<T>.Create }

constructor TOmniFuture<T>.CreateEx(action: TOmniFutureDelegateEx<T>; taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  FCancellable := true;
  FCompleted := false;
  Execute(
    procedure (const task: IOmniTask)
    begin
      try
        FResult := action(task);
      finally // action may raise exception
        FCompleted := true;
      end;
    end,
    taskConfig);
end; { TOmniFuture<T>.CreateEx }

destructor TOmniFuture<T>.Destroy;
begin
  DestroyTask;
  FreeAndNil(FTaskException);
  inherited;
end; { TOmniFuture<T>.Destroy }

procedure TOmniFuture<T>.Cancel;
begin
  if not FCancellable then
    raise EFutureError.Create('Action cannot be cancelled');
  if not IsCancelled then begin
    FCancelled := true;
    if assigned(FTask) then
      FTask.CancellationToken.Signal;
  end;
end; { TOmniFuture<T>.Cancel }

procedure TOmniFuture<T>.DestroyTask;
begin
  if assigned(FTask) then begin
    FTask.Terminate;
    FTask := nil;
  end;
end; { TOmniFuture<T>.DestroyTask }

function TOmniFuture<T>.DetachException: Exception;
begin
  Result := FatalException; // this will in turn detach exception from task
  FTaskException := nil;
end; { TOmniFuture }

procedure TOmniFuture<T>.DetachExceptionFromTask;
begin
  if IsDone and assigned(FTask) and (not assigned(FTaskException)) then begin
    FTask.WaitFor(INFINITE); // task may not have terminated yet
    FTaskException := FTask.DetachException;
  end;
end; { TOmniFuture }

procedure TOmniFuture<T>.Execute(action: TOmniTaskDelegate; taskConfig: IOmniTaskConfig);
begin
  FTask := CreateTask(action, 'TOmniFuture action');
  Parallel.ApplyConfig(taskConfig, FTask);
  FTask.Unobserved;
  if assigned(FTask.CancellationToken) and FTask.CancellationToken.IsSignalled then begin
    FCancelled := true;
    FreeAndNil(FTask);
  end
  else
    Parallel.Start(FTask, taskConfig);
end; { TOmniFuture<T>.Execute }

function TOmniFuture<T>.FatalException: Exception;
begin
  DetachExceptionFromTask;
  Result := FTaskException;
end; { TOmniFuture }

function TOmniFuture<T>.IsCancelled: boolean;
begin
  Result := FCancelled;
end; { TOmniFuture<T>.IsCancelled }

function TOmniFuture<T>.IsDone: boolean;
begin
  Result := FCompleted;
end; { TOmniFuture<T>.IsDone }

function TOmniFuture<T>.TryValue(timeout_ms: cardinal; var value: T): boolean;
var
  taskExcept: Exception;
begin
  Result := false;
  if FCancelled then
    raise EFutureCancelled.Create('Action was cancelled');
  if assigned(FTask) then begin
    if not FTask.WaitFor(timeout_ms) then
      Exit;
    DetachExceptionFromTask;
    DestroyTask;
  end;
  if assigned(FTaskException) then begin
    taskExcept := FTaskException;
    FTaskException := nil;
    raise taskExcept;
  end;
  value := FResult;
  Result := true;
end; { TOmniFuture<T>.TryValue }

function TOmniFuture<T>.Value: T;
begin
  TryValue(INFINITE, Result);
end; { TOmniFuture<T>.Value }

function TOmniFuture<T>.WaitFor(timeout_ms: cardinal): boolean;
begin
  if assigned(FTask) then
    Result := FTask.WaitFor(timeout_ms)
  else
    Result := true;
end; { TOmniFuture }

{ TOmniPipelineStage }

constructor TOmniPipelineStage.Create(stage: TPipelineSimpleStageDelegate;
  taskConfig: IOmniTaskConfig);
begin
  inherited Create;
  opsSimpleStage := stage;
  opsNumTasks := 1;
  opsTaskConfig := taskConfig;
end; { TOmniPipelineStage.Create }

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

procedure TOmniPipelineStage.Execute(const task: IOmniTask);
begin
  // D2009 doesn't like TProc casts so we're casting to NativeInt
  Assert(SizeOf(TProc) = SizeOf(NativeInt));
  if PInteger(@opsSimpleStage)^ <> NativeInt(nil) then
    ExecuteSimpleStage(task, opsSimpleStage, opsInput, opsOutput)
  else if PInteger(@opsStage)^ <> NativeInt(nil) then begin
    Assert(PInteger(@opsStageEx)^ = NativeInt(nil));
    opsStage(opsInput, opsOutput);
  end
  else begin
    Assert(PInteger(@opsStageEx)^ <> NativeInt(nil));
    opsStageEx(opsInput, opsOutput, task);
  end;
end; { TOmniPipelineStage.Execute }

procedure TOmniPipelineStage.ExecuteSimpleStage(const task: IOmniTask; const stage:
  TPipelineSimpleStageDelegate; const input, output: IOmniBlockingCollection);
var
  exc     : Exception;
  inValue : TOmniValue;
  outValue: TOmniValue;
begin
  outValue.Clear;
  for inValue in input do begin
    if task.CancellationToken.IsSignalled then
      Exit;
    try
      stage(inValue, outValue);
    except
      exc := Exception(AcquireExceptionObject);
      if not output.TryAdd(exc) then begin
        // output collection is completed - pipeline is shutting down
        exc.Free;
        break; //for inValue
      end;
      outValue.Clear;
    end;
    if not outValue.IsEmpty then begin
      if not output.TryAdd(outValue) then
        // output collection is completed - pipeline is shutting down
        break; //for inValue
      outValue.Clear;
    end;
  end;
end; { TOmniPipelineStage.ExecuteSimpleStage }

function TOmniPipelineStage.GetHandleExceptions: boolean;
begin
  Result := opsHandleExceptions;
end; { TOmniPipelineStage.GetHandleExceptions }

function TOmniPipelineStage.GetInput: IOmniBlockingCollection;
begin
  Result := opsInput;
end; { TOmniPipelineStage.GetInput }

function TOmniPipelineStage.GetNumTasks: integer;
begin
  Result := opsNumTasks;
end; { TOmniPipelineStage.GetNumTasks }

function TOmniPipelineStage.GetOutput: IOmniBlockingCollection;
begin
  Result := opsOutput;
end; { TOmniPipelineStage.GetOutput }

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

procedure TOmniPipelineStage.SetHandleExceptions(const value: boolean);
begin
  opsHandleExceptions := value;
end; { TOmniPipelineStage.SetHandleExceptions }

procedure TOmniPipelineStage.SetNumTasks(const value: integer);
begin
  Assert(value <> 0);
  if value > 0 then
    opsNumTasks := value
  else
    opsNumTasks := Environment.Process.Affinity.Count + value;
end; { TOmniPipelineStage.SetNumTasks }

procedure TOmniPipelineStage.SetQueues(const inQueue, outQueue: IOmniBlockingCollection);
begin
  opsInput := inQueue;
  opsOutput := outQueue;
end; { TOmniPipelineStage.SetQueues }

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
  opInput := TOmniBlockingCollection.Create;
  opOutput := TOmniBlockingCollection.Create;
  {$IFDEF MSWINDOWS}
  opShutDownComplete := CreateEvent(nil, true, false, nil);
  {$ELSE}
  opShutDownComplete := CreateOmniEvent(true, false);
  {$ENDIF ~MSWINDOWS}
end; { TOmniPipeline.Create }

destructor TOmniPipeline.Destroy;
begin
  Cancel;
  FreeAndNil(opOutQueues);
  FreeAndNil(opStages);
  {$IFDEF MSWINDOWS}
  DSiCloseHandleAndNull(opShutDownComplete);
  {$ELSE}
  opShutdownComplete := nil;
  {$ENDIF ~MSWINDOWS}
  inherited Destroy;
end; { TOmniPipeline.Destroy }

procedure TOmniPipeline.AddSingleStage(const stage: IOmniPipelineStageEx);
begin
  stage.HandleExceptions := opHandleExceptions;
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
  if assigned(opCancelWith) then
    opCancelWith.Signal;
  if assigned(opInput) then
    opInput.CompleteAdding;
  for outQueue in opOutQueues do
    (outQueue as IOmniBlockingCollection).CompleteAdding;
end; { TOmniPipeline.Cancel }

procedure TOmniPipeline.DoOnStop(const task: IOmniTask);
begin
  if assigned(opOnStop) then
    opOnStop(task);
end; { TOmniPipeline.DoOnStop }

function TOmniPipeline.From(const queue: IOmniBlockingCollection): IOmniPipeline;
begin
  opInput := queue;
  Result := Self;
end; { TOmniPipeline.From }

function TOmniPipeline.GetInput: IOmniBlockingCollection;
begin
  Result := opInput;
end; { TOmniPipeline.GetInput }

function TOmniPipeline.GetStage(idxStage: integer): IOmniPipelineStageEx;
begin
  Result := (opStages[idxStage] as IOmniPipelineStageEx);
end; { TOmniPipeline.GetStage }

function TOmniPipeline.GetOutput: IOmniBlockingCollection;
begin
  Result := opOutput;
end; { TOmniPipeline.GetOutput }

function TOmniPipeline.GetPipelineStage(idxStage: integer): IOmniPipelineStage;
begin
  Result := PipeStage[idxStage] as IOmniPipelineStage;
end; { TOmniPipeline.GetPipelineStage }

function TOmniPipeline.HandleExceptions: IOmniPipeline;
var
  iStage: integer;
begin
  if opStages.Count = 0 then
    opHandleExceptions := true
  else for iStage := opCheckpoint to opStages.Count - 1 do
    PipeStage[iStage].HandleExceptions := true;
  Result := Self;
end; { TOmniPipeline.HandleExceptions }

function TOmniPipeline.NoThrottling: IOmniPipeline;
begin
  Result := Throttle(0, -1);
end; { TOmniPipeline.NoThrottling }

function TOmniPipeline.NumTasks(numTasks: integer): IOmniPipeline;
var
  iStage: integer;
begin
  Assert(numTasks <> 0);
  if numTasks < 0 then
    numTasks := Environment.Process.Affinity.Count + numTasks;

  if opStages.Count = 0 then
    opNumTasks := numTasks
  else for iStage := opCheckpoint to opStages.Count - 1 do
    PipeStage[iStage].NumTasks := numTasks;
  Result := Self;
end; { TOmniPipeline.NumTasks }

function TOmniPipeline.OnStop(stopCode: TProc): IOmniPipeline;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
end; { TOmniPipeline.OnStop }

function TOmniPipeline.OnStop(stopCode: TOmniTaskStopDelegate): IOmniPipeline;
begin
  opOnStop := stopCode;
  Result := Self;
end; { TOmniPipeline.OnStop }

function TOmniPipeline.OnStopInvoke(stopCode: TProc): IOmniPipeline;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniPipeline.OnStopInvoke }

function TOmniPipeline.Run: IOmniPipeline;
var
  countStopped: IOmniResourceCount;
  exc         : Exception;
  inQueue     : IOmniBlockingCollection;
  iStage      : integer;
  iTask       : integer;
  outQueue    : IOmniBlockingCollection;
  stageName   : string;
  task        : IOmniTaskControl;
  totalTasks  : integer;
begin
  Assert(not assigned(opCountStopped), 'TOmniPipeline.Run: Pipeline is already running');
  totalTasks := 0;
  for iStage := 0 to opStages.Count - 1 do
    Inc(totalTasks, PipeStage[iStage].NumTasks);
  opCountStopped := CreateResourceCount(totalTasks);
  outQueue := opInput;
  for iStage := 0 to opStages.Count - 1 do begin
    inQueue := outQueue;
    if iStage < (opStages.Count - 1) then
      outQueue := TOmniBlockingCollection.Create
    else
      outQueue := opOutput;
    if PipeStage[iStage].Throttle > 0 then
      if totalTasks > Environment.Process.Affinity.Count then
        outQueue.SetThrottling(PipeStage[iStage].Throttle, PipeStage[iStage].ThrottleLowSat)
      else
        outQueue.SetThrottling(PipeStage[iStage].Throttle, PipeStage[iStage].ThrottleLow);
    inQueue.ReraiseExceptions(not PipeStage[iStage].HandleExceptions);
    opOutQueues.Add(outQueue);
    countStopped := CreateResourceCount(PipeStage[iStage].NumTasks);
    for iTask := 1 to PipeStage[iStage].NumTasks do begin
      stageName := Format('Pipeline stage #%d', [iStage+1]);
      if PipeStage[iStage].NumTasks > 1 then
        stageName := Format('%s worker %d', [stageName, iTask]);
      PipeStage[iStage].SetQueues(inQueue, outQueue);
      task := CreateTask(
          procedure (const task: IOmniTask)
          var
            opStage: IOmniPipelineStageEx;
          begin
            try
              try
                opStage := Task.Param['Stage'].AsInterface as IOmniPipelineStageEx;
                try
                  opStage.Execute(Task);
                except
                  exc := Exception(AcquireExceptionObject);
                  if not outQueue.TryAdd(exc) then
                    Exc.Free;
                end;
              finally
                if (Task.Param['Stopped'].AsInterface as IOmniResourceCount).Allocate = 0 then
                  (opStage as IOmniPipelineStage).Output.CompleteAdding;
              end;
            finally
              if (Task.Param['TotalStopped'].AsInterface as IOmniResourceCount).Allocate = 0 then
              begin
                DoOnStop(task);
                {$IFDEF MSWINDOWS}
                SetEvent(Task.Param['ShutDownComplete']);
                {$ELSE}
                (Task.Param['ShutDownComplete'].AsInterface as IOmniEvent).SetEvent;
                {$ENDIF MSWINDOWS}
              end;
            end;
          end,
          stageName
        )
        .CancelWith(opCancelWith)
        .SetParameter('Stage', opStages[iStage])
        .SetParameter('Stopped', countStopped)
        .SetParameter('TotalStopped', opCountStopped)
        .SetParameter('Cancelled', opCancelWith)
        .SetParameter('ShutDownComplete', opShutDownComplete);
      Parallel.ApplyConfig((opStages[iStage] as IOmniPipelineStageEx).TaskConfig, task);
      task.Unobserved;
      Parallel.Start(task, (opStages[iStage] as IOmniPipelineStageEx).TaskConfig);
    end; //for iTask
  end; //for iStage
  opOutput.ReraiseExceptions(not opHandleExceptions);
  Result := Self;
end; { TOmniPipeline.Run }

function TOmniPipeline.Stage(pipelineStage: TPipelineSimpleStageDelegate;
  taskConfig: IOmniTaskConfig): IOmniPipeline;
begin
  AddSingleStage(TOmniPipelineStage.Create(pipelineStage, taskConfig));
  opCheckpoint := opStages.Count - 1;
  Result := Self;
end; { TOmniPipeline.Stage }

function TOmniPipeline.Stage(pipelineStage: TPipelineStageDelegate; taskConfig: IOmniTaskConfig): IOmniPipeline;
begin
  AddSingleStage(TOmniPipelineStage.Create(pipelineStage, taskConfig));
  opCheckpoint := opStages.Count - 1;
  Result := Self;
end; { TOmniPipeline.Stage }

function TOmniPipeline.Stage(pipelineStage: TPipelineStageDelegateEx;
  taskConfig: IOmniTaskConfig): IOmniPipeline;
begin
  AddSingleStage(TOmniPipelineStage.Create(pipelineStage, taskConfig));
  opCheckpoint := opStages.Count - 1;
  Result := Self;
end; { TOmniPipeline.Stage }

function TOmniPipeline.Stages(const pipelineStages: array of TPipelineSimpleStageDelegate;
  taskConfig: IOmniTaskConfig): IOmniPipeline;
var
  oneStage: TPipelineSimpleStageDelegate;
begin
  Assert(Length(pipelineStages) > 0);
  opCheckpoint := opStages.Count;
  for oneStage in pipelineStages do
    AddSingleStage(TOmniPipelineStage.Create(oneStage, taskConfig));
  Result := Self;
end; { TOmniPipeline.Stages }

function TOmniPipeline.Stages(const pipelineStages: array of TPipelineStageDelegate;
  taskConfig: IOmniTaskConfig): IOmniPipeline;
var
  oneStage: TPipelineStageDelegate;
begin
  Assert(Length(pipelineStages) > 0);
  opCheckpoint := opStages.Count;
  for oneStage in pipelineStages do
    AddSingleStage(TOmniPipelineStage.Create(oneStage, taskConfig));
  Result := Self;
end; { TOmniPipeline.Stages }

function TOmniPipeline.Stages(const pipelineStages: array of TPipelineStageDelegateEx;
  taskConfig: IOmniTaskConfig): IOmniPipeline;
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

function TOmniPipeline.WaitFor(timeout_ms: cardinal): boolean;
begin
  Assert(assigned(opCountStopped));
  {$IFDEF MSWINDOWS}
  Assert(opShutDownComplete <> 0);
  Result := (WaitForSingleObject(opShutDownComplete, timeout_ms) = WAIT_OBJECT_0);
  {$ELSE}
  Assert(opShutDownComplete <> nil);
  Result := opShutDownComplete.WaitFor(timeout_ms) = wrSignaled;
  {$ENDIF ~MSWINDOWS}
end; { TOmniPipeline.WaitFor }

{ TOmniCompute<T> }

constructor TOmniCompute<T>.Create(action: TOmniForkJoinDelegate<T>;
  input: IOmniBlockingCollection);
begin
  inherited Create;
  FAction := action;
  FInput := input;
end; { TOmniCompute<T>.Create }

procedure TOmniCompute<T>.Execute;
begin
  Assert(not FComputed);
  FResult := FAction;
  FComputed := true;
end; { TOmniCompute }

function TOmniCompute<T>.IsDone: boolean;
begin
  Result := FComputed;
end; { TOmniCompute }

function TOmniCompute<T>.TryValue(timeout_ms: cardinal; var value: T): boolean;
var
  compute: TOmniValue;
begin
  Result := false;
  while not FComputed do begin
    if FInput.Take(compute) then
      IOmniCompute<T>(compute.AsInterface).Execute
    else
      {$IFDEF MSWINDOWS}DSiYield;{$ELSE}TThread.Yield;{$ENDIF};
  end;
  value := FResult;
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
  FCompute := compute;
end; { TOmniCompute.Create }

procedure TOmniCompute.Await;
begin
  FCompute.Value;
end; { TOmniCompute.Await }

procedure TOmniCompute.Execute;
begin
  FCompute.Execute;
end; { TOmniCompute.Execute }

function TOmniCompute.IsDone: boolean;
begin
  Result := FCompute.IsDone;
end; { TOmniCompute.IsDone }

{ TOmniForkJoin }

procedure TOmniForkJoin<T>.Asy_ProcessComputations(const input, output:
  IOmniBlockingCollection);
var
  computation: TOmniValue;
begin
  for computation in input do
    IOmniCompute<T>(computation.AsInterface).Execute;
end; { TOmniForkJoin }

function TOmniForkJoin<T>.Compute(action: TOmniForkJoinDelegate<T>): IOmniCompute<T>;
var
  intf: IInterface;
begin
  StartWorkerTasks;
  Result := TOmniCompute<T>.Create(action, FPoolInput);
  AddToBC(FPoolInput, Result);
end; { TOmniForkJoin<T>.Compute }

constructor TOmniForkJoin<T>.Create;
begin
  inherited Create;
  FNumTasks := Environment.Process.Affinity.Count - 1;
end; { TOmniForkJoin<T>.Create }

function TOmniForkJoin<T>.NumTasks(numTasks: integer): IOmniForkJoin<T>;
begin
  Assert(numTasks <> 0);
  if numTasks > 0 then
    FNumTasks := numTasks
  else
    FNumTasks := Environment.Process.Affinity.Count + numTasks;
  Result := Self;
end; { TOmniForkJoin<T>.NumTasks }

procedure TOmniForkJoin<T>.StartWorkerTasks;
begin
  if not assigned(FTaskPool) then begin
    //Use pipeline with one parallelized stage as a simple task pool.
    FPoolInput := TOmniBlockingCollection.Create(FNumTasks);
    if FNumTasks > 0 then begin
      FTaskPool := Parallel.Pipeline
        .NumTasks(FNumTasks)
        .From(FPoolInput)
        .Stage(Asy_ProcessComputations, FTaskConfig);
      FTaskPool.Run;
    end;
  end;
end; { TOmniForkJoin<T.StartWorkerTasks }

function TOmniForkJoin<T>.TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin<T>;
begin
  FTaskConfig := config;
  Result := Self;
end; { TOmniForkJoin }

{ TOmniForkJoin }

constructor TOmniForkJoin.Create;
begin
  inherited Create;
  FForkJoin := TOmniForkJoin<boolean>.Create;
end; { TOmniForkJoin.Create }

destructor TOmniForkJoin.Destroy;
begin
  FreeAndNil(FForkJoin);
  inherited;
end; { TOmniForkJoin.Destroy }

function TOmniForkJoin.Compute(action: TOmniForkJoinDelegate): IOmniCompute;
begin
  Result := TOmniCompute.Create(
    FForkJoin.Compute(
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
  Assert(numTasks <> 0);
  if numTasks < 0 then
    numTasks := Environment.Process.Affinity.Count + numTasks;
  FForkJoin.NumTasks(numTasks);
  Result := self;
end; { TOmniForkJoin.NumTasks }

function TOmniForkJoin.TaskConfig(const config: IOmniTaskConfig): IOmniForkJoin;
begin
  FForkJoin.TaskConfig(config);
  Result := Self;
end; { TOmniForkJoin.TaskConfig }

{ TOmniParallelTask }

constructor TOmniParallelTask.Create;
begin
  inherited;
  optNumTasks := - Environment.Process.Affinity.Count;
  optJoin := Parallel.Join.NumTasks(-optNumTasks);
end; { TOmniParallelTask.Create }

function TOmniParallelTask.Execute(const aTask: TProc): IOmniParallelTask;
begin
  Result := Execute(
    procedure (const task: IOmniTask)
    begin
      aTask;
    end);
end; { TOmniParallelTask.Execute }

function TOmniParallelTask.DetachException: Exception;
begin
  Result := optJoin.DetachException;
end; { TOmniParallelTask.DetachException }

function TOmniParallelTask.Execute(const aTask: TOmniParallelTaskDelegate):
  IOmniParallelTask;
var
  iTask: integer;
begin
  if optNumTasks > 0 then
    optJoin.NumTasks(optNumTasks)
  else
    optNumTasks := - optNumTasks;
  for iTask := 1 to optNumTasks do
    optJoin.Task(
      procedure (const joinState: IOmniJoinState)
      begin
        aTask(joinState.Task);
      end);
  optJoin.Execute;
  if not optNoWait then
    WaitFor(INFINITE);
  Result := Self;
end;

function TOmniParallelTask.FatalException: Exception;
begin
  Result := optJoin.FatalException;
end; { TOmniParallelTask.FatalException }

function TOmniParallelTask.IsExceptional: boolean;
begin
  Result := optJoin.IsExceptional;
end; { TOmniParallelTask.IsExceptional }

function TOmniParallelTask.NoWait: IOmniParallelTask;
begin
  optJoin.NoWait;
  if optNumTasks < -1 then begin
    Inc(optNumTasks);
    optJoin.NumTasks(-optNumTasks);
  end;
  optNoWait := true;
  Result := Self;
end; { TOmniParallelTask.NoWait }

function TOmniParallelTask.NumTasks(numTasks: integer): IOmniParallelTask;
begin
  Assert(numTasks <> 0);
  if numTasks < 0 then
    numTasks := Environment.Process.Affinity.Count + numTasks;
  optJoin.NumTasks(numTasks);
  optNumTasks := numTasks;
  Result := Self;
end; { TOmniParallelTask.NumTasks }

function TOmniParallelTask.OnStop(const stopCode: TProc): IOmniParallelTask;
begin
  optJoin.OnStop(stopCode);
  Result := Self;
end; { TOmniParallelTask.OnStop }

function TOmniParallelTask.OnStop(const stopCode: TOmniTaskStopDelegate):
  IOmniParallelTask;
begin
  optJoin.OnStop(stopCode);
  Result := Self;
end; { TOmniParallelTask.OnStop }

function TOmniParallelTask.OnStopInvoke(const stopCode: TProc): IOmniParallelTask;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelTask.OnStopInvoke }

function TOmniParallelTask.TaskConfig(const config: IOmniTaskConfig): IOmniParallelTask;
begin
  optJoin.TaskConfig(config);
  Result := Self;
end; { TOmniParallelTask.TaskConfig }

function TOmniParallelTask.WaitFor(timeout_ms: cardinal): boolean;
begin
  Result := optJoin.WaitFor(timeout_ms);
end; { TOmniParallelTask.WaitFor }

{ TOmniWorkItem }

constructor TOmniWorkItem.Create(const data: TOmniValue; uniqueID: int64; var
  cancelAllUpToID: TOmniAlignedInt64);
begin
  inherited Create;
  FData := data;
  FUniqueID := uniqueID;
  FCancelAllUpToID_ref := @cancelAllUpToID;
end; { TOmniWorkItem.Create }

destructor TOmniWorkItem.Destroy;
begin
  FreeException;
  inherited;
end; { TOmniWorkItem.Destroy }

function TOmniWorkItem.DetachException: Exception;
begin
  Result := FatalException;
  FResult := nil;
end; { TOmniWorkItem.DetachException }

function TOmniWorkItem.FatalException: Exception;
begin
  Result := nil;
  if IsExceptional then
    Result := FResult.AsException;
end; { TOmniWorkItem.FatalException }

procedure TOmniWorkItem.FreeException;
begin
  if IsExceptional then begin
    FResult.AsException.Free;
    FResult := nil;
  end;
end; { TOmniWorkItem.FreeException }

function TOmniWorkItem.GetCancellationToken: IOmniCancellationToken;
var
  cancelUpToID: int64;
begin
  Result := Atomic<IOmniCancellationToken>.Initialize(FCancellationToken, CreateOmniCancellationToken);
  cancelUpToID := FCancelAllUpToID_ref^.Value;
  if (cancelUpToID > 0) and (UniqueID <= cancelUpToID) then
    Result.Signal;
end; { TOmniWorkItem.GetCancellationToken }

function TOmniWorkItem.GetConfig: IOmniWorkItemConfig;
begin
  Result := FConfig;
end; { TOmniWorkItem.GetConfig }

function TOmniWorkItem.GetData: TOmniValue;
begin
  Result := FData;
end; { TOmniWorkItem.GetData }

function TOmniWorkItem.GetResult: TOmniValue;
begin
  Result := FResult;
  if IsExceptional then
    raise DetachException;
end; { TOmniWorkItem.GetResult }

function TOmniWorkItem.GetSkipCompletionHandler: boolean;
begin
  Result := FSkipCompletionHandler;
end; { TOmniWorkItem.GetSkipCompletionHandler }

function TOmniWorkItem.GetTask: IOmniTask;
begin
  Result := FTask;
end; { TOmniWorkItem.GetTask }

function TOmniWorkItem.GetTaskState: TOmniValue;
begin
  Result := FTaskState;
end; { TOmniWorkItem.GetTaskState }

function TOmniWorkItem.GetUniqueID: int64;
begin
  Result := FUniqueID;
end; { TOmniWorkItem.GetUniqueID }

function TOmniWorkItem.IsExceptional: boolean;
begin
  Result := FResult.IsException;
end; { TOmniWorkItem.IsExceptional }

procedure TOmniWorkItem.SetConfig(const value: IOmniWorkItemConfig);
begin
  FConfig := value;
end; { TOmniWorkItem.SetConfig }

procedure TOmniWorkItem.SetResult(const value: TOmniValue);
begin
  FResult := value;
end; { TOmniWorkItem.SetResult }

procedure TOmniWorkItem.SetSkipCompletionHandler(const value: boolean);
begin
  FSkipCompletionHandler := value;
end; { TOmniWorkItem.SetSkipCompletionHandler }

procedure TOmniWorkItem.SetTask(const task: IOmniTask; const taskState: TOmniValue);
begin
  FTask := task;
  FTaskState := taskState;
end; { TOmniWorkItem.SetTask }

{ TOmniWorkItemConfig }

constructor TOmniWorkItemConfig.Create(defaults: IOmniWorkItemConfig = nil);
var
  defaultsEx: IOmniWorkItemConfigEx;
begin
  inherited Create;
  if assigned(defaults) then begin
    defaultsEx := defaults as IOmniWorkItemConfigEx;
    FOnExecute := defaultsEx.GetOnExecute;
    FOnRequestDone := defaultsEx.GetOnRequestDone;
    FOnRequestDone_Asy := defaultsEx.GetOnRequestDone_Asy;
  end;
end; { TOmniWorkItemConfig.Create }

function TOmniWorkItemConfig.GetOnExecute: TOmniBackgroundWorkerDelegate;
begin
  Result := FOnExecute;
end; { TOmniWorkItemConfig.GetOnExecute }

function TOmniWorkItemConfig.GetOnRequestDone: TOmniWorkItemDoneDelegate;
begin
  Result := FOnRequestDone;
end; { TOmniWorkItemConfig.GetOnRequestDone }

function TOmniWorkItemConfig.GetOnRequestDone_Asy: TOmniWorkItemDoneDelegate;
begin
  Result := FOnRequestDone_Asy;
end; { TOmniWorkItemConfig.GetOnRequestDone_Asy }

function TOmniWorkItemConfig.OnExecute(
  const aTask: TOmniBackgroundWorkerDelegate): IOmniWorkItemConfig;
begin
  FOnExecute := aTask;
  Result := Self;
end; { TOmniWorkItemConfig.OnExecute }

function TOmniWorkItemConfig.OnRequestDone(
  const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
begin
  FOnRequestDone := aTask;
  Result := Self;
end; { TOmniWorkItemConfig.OnRequestDone }

function TOmniWorkItemConfig.OnRequestDone_Asy(
  const aTask: TOmniWorkItemDoneDelegate): IOmniWorkItemConfig;
begin
  FOnRequestDone_Asy := aTask;
  Result := Self;
end; { TOmniWorkItemConfig.OnRequestDone_Asy }

{ TOmniBackgroundWorker }

constructor TOmniBackgroundWorker.Create;
begin
  inherited Create;
  FDefaultConfig := TOmniWorkItemConfig.Create;
  FDefaultConfigEx := (FDefaultConfig as IOmniWorkItemConfigEx);
  FUniqueID := CreateCounter;
  FNumTasks := 1;
end; { TOmniBackgroundWorker.Create }

function TOmniBackgroundWorker.CreateWorkItem(const data: TOmniValue): IOmniWorkItem;
begin
  Result := TOmniWorkItem.Create(data, FUniqueID.Increment, FCancelAllToID);
end; { TOmniBackgroundWorker.CreateWorkItem }

destructor TOmniBackgroundWorker.Destroy;
begin
  Terminate(INFINITE);
  inherited;
end; { TOmniBackgroundWorker.Destroy }

procedure TOmniBackgroundWorker.BackgroundWorker(
  const input, output: IOmniBlockingCollection; const task: IOmniTask);
var
  configEx  : IOmniWorkItemConfigEx;
  ovWorkItem: TOmniValue;
  taskState : TOmniValue;
  workItem  : IOmniWorkItem;
  workItemEx: IOmniWorkItemEx;
begin
  if assigned(FTaskInitializer) then
    FTaskInitializer(taskState);
  try
    for ovWorkItem in input do begin
      workItem := ovWorkItem.AsInterface as IOmniWorkItem;
      workItemEx := workItem as IOmniWorkItemEx;
      workItemEx.SetTask(task, taskState);
      configEx := workItemEx.Config as IOmniWorkItemConfigEx;
      if not workItem.CancellationToken.IsSignalled then begin
        try
          configEx.GetOnExecute()(workItem);
        except
          workItem.Result := Exception(AcquireExceptionObject);
        end;
      end;
      if (not workItem.SkipCompletionHandler) and assigned(configEx.GetOnRequestDone_Asy()) then
        configEx.GetOnRequestDone_Asy()(Self, workItem);
      if (not workItem.SkipCompletionHandler) and assigned(configEx.GetOnRequestDone()) then
        output.TryAdd(workItem);
    end;
  finally
    if assigned(FTaskFinalizer) then
      FTaskFinalizer(taskState);
  end;
end; { TOmniBackgroundWorker.BackgroundWorker }

procedure TOmniBackgroundWorker.CancelAll;
begin
  CancelAll(FUniqueID.Value);
end; { TOmniBackgroundWorker.CancelAll }

procedure TOmniBackgroundWorker.CancelAll(upToUniqueID: int64);
begin
  FCancelAllToID.Value := upToUniqueID;
end; { TOmniBackgroundWorker.CancelAll }

function TOmniBackgroundWorker.Config: IOmniWorkItemConfig;
begin
  Result := TOmniWorkItemConfig.Create(FDefaultConfig);
end; { TOmniBackgroundWorker.Config }

function TOmniBackgroundWorker.Execute(const aTask: TOmniBackgroundWorkerDelegate):
  IOmniBackgroundWorker;
begin
  Assert(FNumTasks > 0);
  FDefaultConfig.OnExecute(aTask);

  FWorker := Parallel.Pipeline
               .NumTasks(FNumTasks)
               .Stage(BackgroundWorker, FTaskConfig)
               .OnStop(
                 procedure (const task: IOmniTask)
                 begin
                   if assigned(FOnStop) then
                     FOnStop(task);
                 end);

  {$IFDEF MSWINDOWS}
  FWindow := DSiAllocateHWnd(ObserverWndProc);
  FObserver := CreateContainerWindowsMessageObserver(FWindow, MSG_WORK_ITEM_DONE, 0, 0);
  FWorker.Output.ContainerSubject.Attach(FObserver, coiNotifyOnAllInserts);
  {$ENDIF MSWINDOWS}
  FWorker.Run;
  Result := Self;
end; { TOmniBackgroundWorker.Execute }

function TOmniBackgroundWorker.Finalize(taskFinalizer: TOmniTaskFinalizerDelegate):
  IOmniBackgroundWorker;
begin
  FTaskFinalizer := taskFinalizer;
  Result := Self;
end; { TOmniBackgroundWorker.Finalize }

function TOmniBackgroundWorker.Initialize(taskInitializer: TOmniTaskInitializerDelegate):
  IOmniBackgroundWorker;
begin
  FTaskInitializer := taskInitializer;
  Result := Self;
end; { TOmniBackgroundWorker.Initialize }

function TOmniBackgroundWorker.NumTasks(numTasks: integer): IOmniBackgroundWorker;
begin
  Assert(numTasks <> 0);
  if numTasks > 0 then
    FNumTasks := numTasks
  else
    FNumTasks := Environment.Process.Affinity.Count + numTasks;
  Result := Self;
end; { TOmniBackgroundWorker.NumTasks }

{$IFDEF MSWINDOWS}
procedure TOmniBackgroundWorker.ObserverWndProc(var message: TMessage);
var
  ovWorkItem: TOmniValue;
  workItem  : IOmniWorkItem;
begin
  if message.Msg = MSG_WORK_ITEM_DONE then begin
    while FWorker.Output.TryTake(ovWorkItem) do begin
      workItem := ovWorkItem.AsInterface as IOmniWorkItem;
      ((workItem as IOmniWorkItemEx).Config as IOmniWorkItemConfigEx).GetOnRequestDone()(Self, workItem);
    end;
    message.Result := Ord(true);
  end;
end; { TOmniBackgroundWorker.ObserverWndProc }
{$ENDIF MSWINDOWS}

function TOmniBackgroundWorker.OnRequestDone(const aTask: TOmniWorkItemDoneDelegate):
  IOmniBackgroundWorker;
begin
  FDefaultConfig.OnRequestDone(aTask);
  Result := Self;
end; { TOmniBackgroundWorker.OnRequestDone }

function TOmniBackgroundWorker.OnRequestDone_Asy(const aTask: TOmniWorkItemDoneDelegate):
  IOmniBackgroundWorker;
begin
  FDefaultConfig.OnRequestDone_Asy(aTask);
  Result := Self;
end; { TOmniBackgroundWorker.OnRequestDone_Asy }

function TOmniBackgroundWorker.OnStop(stopCode: TProc): IOmniBackgroundWorker;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
end; { TOmniBackgroundWorker.OnStop }

function TOmniBackgroundWorker.OnStop(stopCode: TOmniTaskStopDelegate): IOmniBackgroundWorker;
begin
  FOnStop := stopCode;
  Result := Self;
end; { TOmniBackgroundWorker.OnStop }

function TOmniBackgroundWorker.OnStopInvoke(stopCode: TProc): IOmniBackgroundWorker;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniBackgroundWorker.OnStopInvoke }

procedure TOmniBackgroundWorker.Schedule(const workItem: IOmniWorkItem;
  const workItemConfig: IOmniWorkItemConfig);
begin
  if assigned(workItemConfig) then
    (workItem as IOmniWorkItemEx).Config := workItemConfig
  else
    (workItem as IOmniWorkItemEx).Config := FDefaultConfig;
  FWorker.Input.Add(workItem);
end; { TOmniBackgroundWorker.Schedule }

function TOmniBackgroundWorker.StopOn(const token: IOmniCancellationToken):
  IOmniBackgroundWorker;
begin
  FStopOn := token;
  Result := Self;
end; { TOmniBackgroundWorker.StopOn }

function TOmniBackgroundWorker.TaskConfig(const config: IOmniTaskConfig):
  IOmniBackgroundWorker;
begin
  FTaskConfig := config;
  Result := Self;
end; { TOmniBackgroundWorker.TaskConfig }

function TOmniBackgroundWorker.Terminate(maxWait_ms: cardinal): boolean;
begin
  Result := WaitFor(maxWait_ms);
  {$IFDEF MSWINDOWS}
  if Result then begin
    if assigned(FObserver) then begin
      FWorker.Output.ContainerSubject.Detach(FObserver, coiNotifyOnAllInserts);
      FreeAndNil(FObserver);
    end;
    DSiDeallocateHWnd(FWindow);
  end;
  {$ENDIF MSWINDOWS}
end; { TOmniBackgroundWorker.Terminate }

function TOmniBackgroundWorker.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := true;
  if assigned(FWorker) then begin
    FWorker.Input.CompleteAdding;
    Result := FWorker.WaitFor(maxWait_ms);
  end;
end; { TOmniBackgroundWorker.WaitFor }

{ TOmniTaskConfig }

constructor TOmniTaskConfig.Create;
begin
  inherited Create;
  otcOnMessageList := TGpIntegerObjectList.Create(true);
  otcPriority := TOTLThreadPriority.tpNormal;
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
  if assigned(otcOnTerminated.Event) then
    task.OnTerminated(otcOnTerminated.Event);
  if assigned(otcOnTerminated.Func) then
    task.OnTerminated(otcOnTerminated.Func);
  if assigned(otcOnTerminated.Simple) then
    task.OnTerminated(otcOnTerminated.Simple);
  if assigned(otcWithCounterCounter) then
    task.WithCounter(otcWithCounterCounter);
  if assigned(otcWithLockOmniLock) then
    task.WithLock(otcWithLockOmniLock);
  if assigned(otcWithLockSyncLock) then
    task.WithLock(otcWithLockSyncLock, otcWithLockAutoDestroy);
  task.SetPriority(otcPriority);
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

procedure TOmniTaskConfig.DetachTerminated(var terminated: TOmniTaskConfigTerminated);
begin
  terminated := otcOnTerminated;
  otcOnTerminated.Clear;
end; { TOmniTaskConfig.GetTerminated }

function TOmniTaskConfig.GetThreadPool: IOmniThreadPool;
begin
  Result := otcThreadPool;
end; { TOmniTaskConfig.GetThreadPool }

function TOmniTaskConfig.MonitorWith(const monitor: IOmniTaskControlMonitor):
  IOmniTaskConfig;
begin
  otcMonitorWithMonitor := monitor;
  Result := Self;
end; { TOmniTaskConfig.MonitorWith }

function TOmniTaskConfig.NoThreadPool: IOmniTaskConfig;
begin
  otcNoThreadPool := true;
  otcThreadPool := nil;
  Result := Self;
end; { TOmniTaskConfig.NoThreadPool }

function TOmniTaskConfig.OnMessage(eventHandler: TOmniTaskMessageEvent): IOmniTaskConfig;
begin
  otcOnMessageEventHandler := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnMessage }

function TOmniTaskConfig.OnMessage(msgID: word; eventHandler: TOmniOnMessageFunction):
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
  otcOnTerminated.Event := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnTerminated }

function TOmniTaskConfig.OnTerminated(eventHandler: TOmniOnTerminatedFunction):
  IOmniTaskConfig;
begin
  otcOnTerminated.Func := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnTerminated }

function TOmniTaskConfig.OnTerminated(eventHandler: TOmniOnTerminatedFunctionSimple):
  IOmniTaskConfig;
begin
  otcOnTerminated.Simple := eventHandler;
  Result := Self;
end; { TOmniTaskConfig.OnTerminated }

function TOmniTaskConfig.RunDirectly: boolean;
begin
  Result := otcNoThreadPool;
end; { TOmniTaskConfig.RunDirectly }

function TOmniTaskConfig.SetPriority(threadPriority: TOTLThreadPriority): IOmniTaskConfig;
begin
  otcPriority := threadPriority;
  Result := Self;
end; { TOmniTaskConfig.SetPriority }

function TOmniTaskConfig.ThreadPool(const threadPool: IOmniThreadPool): IOmniTaskConfig;
begin
  otcThreadPool := threadPool;
  otcNoThreadPool := false;
  Result := Self;
end; { TOmniTaskConfig.ThreadPool }

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

{ TOmniTaskConfigTerminated }

procedure TOmniTaskConfigTerminated.Call(const task: IOmniTaskControl);
begin
  if assigned(Event) then
    Event(task);
  if assigned(Func) then
    Func(task);
  if assigned(Simple) then
    Simple;
end; { TOmniTaskConfigTerminated.Call }

procedure TOmniTaskConfigTerminated.Clear;
begin
  Event := nil;
  Func := nil;
  Simple := nil;
end; { TOmniTaskConfigTerminated.Clear }

{ TOmniAwait }

constructor TOmniAwait.Create(async: TProc);
begin
  inherited Create;
  FAsync := async;
end; { TOmniAwait.Create }

procedure TOmniAwait.Await(proc: TProc);
begin
  Parallel.Async(FAsync, Parallel.TaskConfig.OnTerminated(
  procedure begin
    proc;
  end));
end; { TOmniAwait.Await }

{$IFDEF OTL_HasArrayOfT}
{$IFDEF OTL_GoodGenerics}

{ TOmniParallelMapper<T1,T2> }

constructor TOmniParallelMapper<T1,T2>.Create;
begin
  inherited Create;
  FNumTasks := Environment.Process.Affinity.Count;
end; { TOmniParallelMapper<T1,T2> }

destructor TOmniParallelMapper<T1,T2>.Destroy;
begin
  WaitFor(INFINITE);
  inherited;
end; { TOmniParallelMapper<T1,T2> }

procedure TOmniParallelMapper<T1, T2>.CompressTarget;
var
  firstEmpty: integer;
  i         : integer;
  j         : integer;
begin
  firstEmpty := FTargetData[High(FTargetData)].High + 1;
  for i := High(FTargetData)-1 downto Low(FTargetData) do begin
    if FTargetData[i].Low = firstEmpty then
      firstEmpty := FTargetData[i].High + 1
    else begin
      for j := FTargetData[i].Low to FTargetData[i].High do begin
        FTarget[firstEmpty] := FTarget[j];
        Inc(firstEmpty);
      end;
    end;
  end;
  SetLength(FTarget, firstEmpty);
end; { TOmniParallelMapper }

function TOmniParallelMapper<T1,T2>.Execute(
  mapper: TMapProc<T1,T2>): IOmniParallelMapper<T1,T2>;
var
  dest: T2;
  el  : T1;
  i   : integer;
begin
  SetLength(FTarget, Length(FSource));
  SetLength(FTargetData, FNumTasks);
  // Parallel.For may reduce number of tasks so make sure unused tasks will be correctly initialized
  for i := Low(FTargetData) to High(FTargetData) do
    FTargetData[i].High := Low(FSource) - 1;

  FWorker := Parallel.For(Low(FSource), High(FSource))
    .NumTasks(FNumTasks)
    .Initialize(
      procedure(taskIndex, fromIndex, toIndex: integer)
      begin
        FTargetData[taskIndex].Low := fromIndex;
        FTargetData[taskIndex].High := FTargetData[taskIndex].Low - 1;
      end)
    .OnStop(
      procedure (const task: IOmniTask)
      begin
        CompressTarget;
        if assigned(FOnStop) then
          FOnStop(task);
      end);

  if assigned(FTaskConfig) then
    FWorker.TaskConfig(FTaskConfig);
  if FNoWait then
    FWorker.NoWait;

  FWorker.Execute(
    procedure (taskIndex, value: integer)
    var
      data: T2;
      high: integer;
    begin
      if mapper(FSource[value], data) then begin
        high := FTargetData[taskIndex].High + 1;
        FTargetData[taskIndex].High := high;
        FTarget[high] := data;
      end;
    end);

  Result := Self;
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1, T2>.NoWait: IOmniParallelMapper<T1,T2>;
begin
  FNoWait := true;
  Result := Self;
end; { TOmniParallelMapper }

function TOmniParallelMapper<T1,T2>.NumTasks(numTasks: integer): IOmniParallelMapper<T1,T2>;
begin
  Assert(numTasks <> 0);
  if numTasks > 0 then
    FNumTasks := numTasks
  else
    FNumTasks := Environment.Process.Affinity.Count + numTasks;
  Result := Self;
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1,T2>.OnStop(stopCode: TProc): IOmniParallelMapper<T1,T2>;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      stopCode();
    end);
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1,T2>.OnStop(stopCode: TOmniTaskStopDelegate):
  IOmniParallelMapper<T1,T2>;
begin
  FOnStop := stopCode;
  Result := Self;
end; { TOmniParallelMapper<T1,T2 }

{$IFDEF OTL_FixedGenericIncompletelyDefined}
function TOmniParallelMapper<T1, T2>.OnStopInvoke(stopCode: TProc):
  IOmniParallelMapper<T1,T2>;
begin
  Result := OnStop(
    procedure (const task: IOmniTask)
    begin
      task.Invoke(
        procedure
        begin
          stopCode();
        end);
    end);
end; { TOmniParallelMapper }
{$ENDIF OTL_FixedGenericIncompletelyDefined}

function TOmniParallelMapper<T1,T2>.Result: TArray<T2>;
begin
  Result := FTarget;
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1, T2>.Source(const data: TArray<T1>;
  makeCopy: boolean): IOmniParallelMapper<T1,T2>;
var
  i: integer;
begin
  if not makeCopy then
    FSource := data
  else begin
    SetLength(FSource, Length(data));
    for i := Low(data) to High(data) do
      FSource[i] := data[i];
  end;
  Result := Self;
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1,T2>.TaskConfig(
  const config: IOmniTaskConfig): IOmniParallelMapper<T1,T2>;
begin
  FTaskConfig := config;
  Result := Self;
end; { TOmniParallelMapper<T1,T2> }

function TOmniParallelMapper<T1,T2>.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := true;
  if assigned(FWorker) then
    Result := FWorker.WaitFor(maxWait_ms);
end; { TOmniParallelMapper<T1,T2> }

{$ENDIF OTL_GoodGenerics}
{$ENDIF OTL_HasArrayOfT}

{ TOmniTimedTaskWorker }

procedure TOmniTimedTaskWorker.ApplyTimer(var msg: TOmniMessage);
var
  interval: integer;
begin
  interval := msg.MsgData;
  if interval > 0 then
    Task.SetTimer(1, Interval, @TOmniTimedTaskWorker.TaskInterval)
  else
    Task.ClearTimer(1);
end; { TOmniTimedTaskWorker.ApplyTimer }

procedure TOmniTimedTaskWorker.DoExecute;
begin
  if assigned(FTask) then
    FTask(Task);
end; { TOmniTimedTaskWorker.DoExecute }

procedure TOmniTimedTaskWorker.ExecuteNow(var msg: TOmniMessage);
begin
  DoExecute;
end; { TOmniTimedTaskWorker.ExecuteNow }

procedure TOmniTimedTaskWorker.SetTask(var msg: TOmniMessage);
begin
  FTask := msg.MsgData.Unwrap<TOmniTaskDelegate>();
end; { TOmniTimedTaskWorker.SetTask }

procedure TOmniTimedTaskWorker.TaskInterval;
begin
  DoExecute;
end; { TOmniTimedTaskWorker.TaskInterval }

{ TOmniTimedTask }

constructor TOmniTimedTask.Create;
begin
  inherited;
  FWorker := CreateTask(TOmniTimedTaskWorker.Create(), 'Timed task').Unobserved.Run;
end; { TOmniTimedTask.Create }

destructor TOmniTimedTask.Destroy;
begin
  Terminate(INFINITE);
  FWorker := nil;
  inherited;
end; { TOmniTimedTask.Destroy }

procedure TOmniTimedTask.ApplyTimer;
begin
  FWorker.Comm.Send(TOmniTimedTaskWorker.MsgApplyTimer, IFF(FActive and (Interval > 0), Interval, 0));
end; { TOmniTimedTask.ApplyTimer }

function TOmniTimedTask.Every(interval_ms: integer): IOmniTimedTask;
begin
  Interval := interval_ms;
  Result := Self;
end; { TOmniTimedTask.Every }

function TOmniTimedTask.Execute(const aTask: TProc): IOmniTimedTask;
begin
  Result := Execute(procedure (const task: IOmniTask) begin aTask(); end);
end; { TOmniTimedTask.Execute }

function TOmniTimedTask.Execute(const aTask: TOmniTaskDelegate): IOmniTimedTask;
begin
  FWorker.Comm.Send(TOmniTimedTaskWorker.MsgSetTask, TOmniValue.Wrap<TOmniTaskDelegate>(aTask));
  FActive := true;
  ApplyTimer;
  Result := Self;
end; { TOmniTimedTask.Execute }

procedure TOmniTimedTask.ExecuteNow;
begin
  ApplyTimer;
  FWorker.Comm.Send(TOmniTimedTaskWorker.MsgExecuteNow);
end; { TOmniTimedTask.ExecuteNow }

function TOmniTimedTask.GetActive: boolean;
begin
  Result := FActive;
end; { TOmniTimedTask.GetActive }

function TOmniTimedTask.GetInterval: integer;
begin
  Result := FInterval;
end; { TOmniTimedTask.GetInterval }

procedure TOmniTimedTask.SetActive(const value: boolean);
begin
  if value = FActive then
    Exit;
  FActive := value;
  ApplyTimer;
end; { TOmniTimedTask.SetActive }

procedure TOmniTimedTask.SetInterval(const value: integer);
begin
  if value <= 0 then
    raise Exception.CreateFmt('TOmniTimedTask.SetInterval: Invalid interval %d', [value]);
  FInterval := value;
  ApplyTimer;
end; { TOmniTimedTask.SetInterval }

procedure TOmniTimedTask.Start;
begin
  Active := true;
end; { TOmniTimedTask.Start }

procedure TOmniTimedTask.Stop;
begin
  Active := false;
end; { TOmniTimedTask.Stop }

function TOmniTimedTask.TaskConfig(const config: IOmniTaskConfig): IOmniTimedTask;
begin
  config.Apply(FWorker);
  Result := Self;
end; { TOmniTimedTask.TaskConfig }

function TOmniTimedTask.Terminate(maxWait_ms: cardinal): boolean;
begin
  Result := FWorker.Terminate(maxWait_ms);
end; { TOmniTimedTask.Terminate }

function TOmniTimedTask.WaitFor(maxWait_ms: cardinal): boolean;
begin
  Result := FWorker.WaitFor(maxWait_ms);
end; { TOmniTimedTask.WaitFor }

initialization
finalization
  GParallelPool := nil;
end.

