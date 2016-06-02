
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

API
---

The goal is to support the usual semantics for promises, for instance
that of the javascript/A+ at https://promisesaplus.com

   - A promise is only resolved once.
     At that point, it executes all its callbacks and gives them the
     resulting values. Those callbacks will not be executes again,
     so that they can be freed.
     But it is possible to add a callback to an already resolved
     promise. The callback is then executed immediately. This means that
     a promise needs to store its value.

Requirements
------------

The following is a list of the needs that a promises library should
fullfill to be useful:

   - Chaining promises
     A promise is resolved, executes callback, which itself returns
     a promise, and so on. This is the natural way to execute promises,
     and the framework should support this as easily as possible.

     Unfortunately, Ada does not support lambda/anonymous functions yet,
     so the callbacks have to be implemented as separate code. That makes
     the use of promises slightly less convenient than in language with
     such constructs.

   - Type safe
     Hey, this is Ada. So we want the compiler to do as much static type
     checking as much possible, and what it can't do should be done at
     runtime.

Ada improvements that would be useful
-------------------------------------

This code does not require any change in the language, and works with
Ada 2012 compilers.
However, there are a number of improvements to the language that would
make it more convenient to use this package:

   - lambda or anonymous functions
     Rather than have to create objects for the callbacks, it would often
     be useful to use anonymous expression functions declared directly
     in the call to When_Done.

   - Dot notation with generic class wide operations
     It would be nice to extend the cases in which the dot notation can be
     used when calling subprograms. For instance, it would be nice, after
     instantiating a `Chains` package, to use "P.When_Done (A).When_Done (B)"
     with explicit function calls rather than the trick of overloading
     operators as we have done.
     This is minor at this point.

   - Implicit instantiations
     Even though it looks like it would be nice if we did not have to
     instantiate `Chains` and `Promises` explictly, such implicit
     instantiations like C++ has make the code much harder to following
     in practice. So no real benefit here !

Todo
----

Various improvement ideas for this library:

   - Look at reactive programming, observables and filter on them
     Link these observables to containers or tasks

   - Avoid memory alloc when there is a single callback set for a
     promise. This is the general case and should be optimized.

   - Should we have a wait() operation on a promise to wait until it is
     resolved ? This would only work when the routines are implemented
     with tasks, not with asynchronous I/O for instance.

   - Can we get rid of `Unrestricted_Access` ?
     These are used so that one can write "new A" directly in the call
     to `When_Done`, but this unsafe.
     Also, we should have a way of preventing the reuse of the same access
     for multiple callbacks (`Chains` already does this via its
     `Is_Registered` function, but this is not the case for `Promises`)

     One way to limit this is by taking subprogram access instead of
     tagged types. The latter are still useful when additional data needs
     to be passed though (simulates lamda functions for instance).

   - Add more use cases
     Show how to use tasks along with promises.
        (Should actually make promises task safe first)
     Show how to use promises in a gtk+ context
