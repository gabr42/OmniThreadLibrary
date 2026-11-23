# Changes to make in the book

Differences between "Parallel Programming with OmniThreadLibrary" and the current code.
Remove an entry when the book is updated.

## 3.10: thread pool, CancelAll and Cancel (chapter "Low-level multithreading", Thread pool)

* `WaitOnTerminate_sec`: the book says that after a `Cancel` or `CancelAll` the thread is killed
  with `TerminateThread` after this time. This is correct only for `Cancel`. `CancelAll` detaches
  the thread: it is not killed, `CancelAll` returns while the task is still running, the task is
  not counted in `MaxExecuting`/`CountExecuting` anymore, and the thread is destroyed when the
  task exits.
* `CancelAll`: the book says only "Cancels all tasks, waiting and running". Add the above and a
  warning not to destroy objects used by tasks that ignore cancellation.
* Explain why the thread is not killed: `TerminateThread` can stop a thread while it owns the
  memory manager (or other RTL) lock and block the whole program. OTL-NG also has no good way to
  kill a thread on POSIX systems. OTL-NG detaches the thread in `Cancel`, too.
* `OnPoolThreadKilled`: the event is also called for a detached thread.
* `Cancel`: "If a task is not yet executing, it will simply be removed from the input queue" was
  not true before 3.10 (the task was executed anyway). The text is correct now.
