
Ada Promises
============

The promise pattern is now ubiquous in programming languages.
The Wikipedia page on promises lists a lot of implementations.
But there are none for Ada yet !

This repository is a starting point to evaluate how we could
implement such promises in Ada.

To compile:

    gprbuild -p -Pdefault.gpr

To run:

    ./obj/test_promises

Start looking at the code from the test case (`test_promises.adb`)
and then look at the specs.

The are lots of things missing at this point:

   - a promise does not save its value, so adding a callback later
     will not allow us to execute it immediately. The value must be
     saved, and to save on memory allocations we could reuse the
     approaches from the ada-traits-containers repository at
     https://github.com/AdaCore/ada-traits-containers

   - a promise cannot be marked as failed

   - chaining promises works, but the syntax is not the best.
     See comments in `test_promises.adb`.

   - a promise really makes sense when doing some asynchronous
     programming, which in Ada means a task in general. It would
     be nice to have a generic wrapper around subprograms, which
     given a subprogram returning a type T wraps it in a task,
     but immediately returns a promise of T. This promise is
     resolved from the task when the subprogram finishes executing.
     This part might require compiler support to make it fully
     usable.
