# Changes to make in the book

Differences between "Parallel Programming with OmniThreadLibrary" and the current code.
Remove an entry when the book is updated.

## 3.10: threads are not killed anymore

A task that does not stop in time is detached: the thread is not killed, the task continues to run
and the thread is destroyed when the task exits. Reason: `TerminateThread` can stop a thread while
it owns the memory manager (or other RTL) lock and block the whole program. OTL-NG also has no good
way to kill a thread on POSIX systems.

Chapter "Low-level multithreading":

* Stopping a task (`Terminate`): "after which it will kill the task" - the task is detached.
* Thread pool, `Cancel`: "the thread that is running the task will be killed by calling the
  TerminateThread Windows function" - the thread is detached.
* Thread pool, `Cancel`: "If a task is not yet executing, it will simply be removed from the input
  queue" was not true before 3.10 (the task was executed anyway). The text is correct now.
* Thread pool, `CancelAll`: the book says only "Cancels all tasks, waiting and running". Add that it
  returns while a task that ignores cancellation is still running.
* Thread pool, `WaitOnTerminate_sec`: "it will kill the thread with the TerminateThread call" - the
  thread is detached. A detached task is not counted in `MaxExecuting`/`CountExecuting`.
* `OnPoolThreadKilled`, `OnPoolWorkItemCompleted`: the thread is not killed but detached.

Chapter "High-level multithreading":

* `Join.Terminate`, `ParallelTask.Terminate`, `TimedTask.Terminate`: tasks that do not stop in time
  are detached. A detached Join worker does not start other Join tasks.

Add a warning: don't destroy objects that are used by a task until that task exits. OTL objects (task
controller, parameters, Comm, Join) stay alive, objects owned by the program do not.
