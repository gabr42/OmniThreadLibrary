unit OtlPlatform.SynchroPrimitives;

{
  Synchronisation Primitives are classes, interfaces and solutions for
    basic low-level thread synchronisation mechanisms, such as Event, Semaphore,
    CountDown, etc.

  There are 3 levels of synchronisation primitives:

    1. The basic level. These are non-reference counted classes and records.
        They are very simple and straight forward. The user has responsibility
        for life-cycle management. The basic level is ideal when the synchro
        is used exclusively within a class, and object pooling is not
        an important consideration.

    2. The interface level. These are reference counted interface solutions.
        They have very simple interface pointer types. When the last reference
        is released, they return to a pool, and become available for re-use,
        thus minimising creation events, which can be expensive for kernel-mode
        synchros. The interface level is ideal for shared access. Solutions to
        the interface level build on the basic level.

    3. The modular level. These are interfaced synchros which can be easily
        combined together in a composite design pattern. For example you may
        have an event and a semaphore, and you require to wait until either one
        is signaled. When the synchros are at the modular level, this is
        a trivial exercise. Solutions to the modular level build on the
        interface level.

  Synchronisation, under the hood, uses one of two modes:

    1. Kernel-mode locking. This is a heavy-weight mechanism that uses a call
        to the operating system kernel, and quiet likely results in a process
        switch. Process switching is expensive.

    2. Bus-mode locking. This is a light-weight mechanism that locks the bus
        while needed. If the bus lock is being used by other processor,
        this processor will spin until it has the lock. Long waits (>10ms)
        are expensive in terms of wasted CPU utilisation.

  This library provides 9 low-level synchonisation mechanisms. The first 8
   mechanisms are provided at each of the 3 levels. The Composite synchro
   is only available at the modular level.

    1. Manual event. An event models a boolean value. An event is either "Set"
        or not set. The event can be set by calling method SetEvent(), and
        cleared by calling ResetEvent(). A thread can block until the event is set.
        With a manual event, unblocking on a set event does not change the event state.

    2. Auto-reset event. An Auto-reset event is the same as a manual event,
        except that when a thread unblocks on a succesful WaitFor(), the event
        is automatically reset (cleared). The reset and the unblock occurs atomically.

    3. Semaphore. A semaphore models a cardinal number. Signaling the semaphore
        bumps the number up by +1. Behaviour is undefined when the number
        would exceed MaxCardinal. WaitFor() blocks until the number is positive,
        and then decrements the number. The Unblocking and the decrementing
        occurs atomically.

    4. CountDown. A CountDown models a cardinal number. Signaling the CountDown
        decrements the counter. Signaling when zero raises an exception.
        A thread can WaitFor() the count to go to zero. CountDowns can also
        be reset back to thier initial value.

    5. CountUp. The same as a CountDown, but it counts up. Signaling increments
        the counter. Signaling when at the threshold raises an exception.

    6. Critical section. Critical sections provide Enter() and Leave()
        methods and record the entered thread, and entry count.
        Enter() blocks until no other thread has entered. Enter() and Leave()
        calls must be balanced. Critical sections use kernel locking.

    7. SpinLock. SpinLocks are like critical sections but use bus locking
        instead. Unlike the Embarcadero spin lock, the spin locks in this
        library are re-enterent. SpinLock at the basic level is a record.

    8. Functional event. Functional events are like manual or auto-reset
        events, except that instead of an explicitly stored state, the
        signaled state is computed by a user supplied function.
        In other words, the call can wait on any condition which can be
        computed quickly and without block.

    9. Composite synchro. Composite synchros combine an array of one or
        more dependent synchros. Composite synchros cant be signaled but
        a calling thread may wait on some condition which is a function
        of the dependent synchros. In the case of the Win32/ Win64 platforms,
        waiting on ALL or ANY of the synchros to be signaled, can leverage
        underlying o/s support for efficient operation.

}
interface

implementation

end.
