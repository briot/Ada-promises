
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

   - Should we have an unsubscribe() operation ?
     Not so useful on promises, but more useful on observables

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
