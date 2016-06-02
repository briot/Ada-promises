--   A promise (also none sometimes as a future or deferred) is a
--   a synchronization mechanism between asynchronous routines.
--
--   Such routines could be implemented as tasks, but also be handled
--   via the system's asynchronous I/O primitives, or an event loop in a
--   GUI program for instance.
--
--   A promise is a value that such a routine can return immediately to the
--   caller, before it even starts its processing or the actual value is
--   available. The caller can then subscribe to the promise, so that when
--   the value becomes actually known, it gets notified and a callback is
--   executed.
--
--   Here is a simple example:
--
--       package Str_Promises is new Promises (String);
--
--       type Process_Page is new Str_Promises.Callback with null record;
--       overriding procedure Resolved
--          (Self : in out Process_Page; Page : String)
--       is
--       begin
--          Put_Line ("Page contents is known: " & Page);
--       end Resolved;
--
--       P : Str_Promises.Promise;
--
--       P := Fetch_URL_Asynchronously ("http://...");
--       P.When_Done (new Process_Page);
--
--   Where Fetch_URL_Asynchronously could run in a task, connect to a
--   web server and query a document.
--
--   But promises are more interesting when they are chained, i.e. the
--   action executed when the first promise is resolved will itself
--   return a promise. Here is an example:
--
--       package Str_Promises is new Promises (String);
--       package Int_Promises is new Promises (Integer);
--       package Str_To_Int is new Chains (Str_Promises, Int_Promises);
--       use Str_To_Int, Int_Promises;
--
--       type Count_Elements is new Str_Promises.Callback with null record;
--       overriding procedure Resolved
--          (Self : in out Process_Page; Page : String);
--       --  For instance, count the number of elements in the XML
--
--       type Report_Count is new Int_Promises.Callback with null record;
--       overriding procedure Resolved
--          (Self : in out Report_Count; Count : Integer);
--       --  For instance display the number of elements in a GUI
--
--       P : Str_Promises.Promise;
--
--       Start
--          (Fetch_URL_Asynchronously ("http://...")
--           and new Count_Elements
--           and> new Report_Count);
--
--   The code above returns immediately, even though the URL will be fetched
--   in the background (which could take a few seconds), then parsed to count
--   the number of elements (which could be done in a separate task and take
--   a few milliseconds), and finally this count will be displayed in a GUI.
--
--   The advantage of this code is that it is composed of small, independent
--   building blocks that are executed when data becomes available. The caller
--   does not have to take care of the synchronization, since the promises
--   handle that.
--
--   Behavior
--   ========
--
--   There are some standard behaviors associated with promises, which this
--   package tries to conform with:
--
--   * A promise is in one of three states:
--     - Pending: the promise has no associated value yet. Some subprogram is
--                still running to fetch that value.
--     - Resolved: the routine has successfully finished running, and given
--                an actual value to the promise.
--     - Failed: the routine failed, and no value will ever be provided to the
--                routine.
--
--   * Any number of callbacks can be set on a routine. They will all be
--     executed once when the promise is resolved or failed. They are never
--     executed afterwards.
--
--   * A promise can be resolved at any time. Whenever it is resolved, all
--     callbacks currently set on the promise are executed and then
--     disconnected. It is an error to resolve a promise more than once.
--
--   * A callback can be added to a promise at any time. If the promise has
--     already been resolved, the callback is executed immediately with the
--     value set on that promise.
--
--  Chaining and callbacks
--  ======================
--
--  Promises can be chained, so that the callback for the first promise will
--  itself return a promise, whose callback might in turn return a promise,
--  and so on.
--
--  The syntax to chain promises is:
--
--       Start (P and new A and new B and new C;
--
--  Let's take the following chain:
--
--      P and new A     --  A is the callback on P
--        and new B     --  B is the callback on the promise returned by A
--        and new C;    --  C is the callback on the promise returned by C
--
--  The following callbacks might occur (where *.P is the promise output
--  by the routine, *.V is the value of that promise, and *.R is the reason
--  for the failure of that promise, which defaults to the reason received
--  from the previous release unless overridden):
--
--       promises                                    calls
--  If P is resolved:           A.Resolved (P.V)
--     If A.P is resolved:         B.Resolved (A.V)
--        if B.P is resolved:        C.Resolved (B.V)
--        else B.P failed:           C.Failed (B.R)
--     else A.P failed:            B.Failed (A.R), C.Failed (B.R)
--  else P failed:              A.Failed (P.R), B.Failed (A.R), C.Failed (B.R)
--
--
--  Q: What if I want multiple callbacks on the same promise ?
--  A: You need to use intermediate variables, as in:
--       Q := P and new A;
--       Start (Q and new B);
--       Start (Q and new C);
--     Where both B and C are callbacks on A's return promise (and not
--     chained together).
--
--     A more convenient syntax exists, as in the following example:
--
--         P and (new A & new B)        --  A.P is passed on to the next step
--           and (new C & new D)        --  C.P is passed on to the next step
--           and new E;
--
--         If P is resolved:            A.Resolved (P.V), B.Resolved (P.V)
--            if A.P is resolved:         C.Resolved (A.V), D.Resolved (A.V)
--               if C.P is resolved:        E.Resolved (C.V)
--               else C.P failed:           E.Failed (C.R)
--            else A.P failed:            C.Failed (A.R), D.Failed (A.R),
--                                          E.Failed (C.R)
--         else P failed:               A.Failed (P.R), B.Failed (P.R),
--                                      C.Failed (A.R), D.Failed (A.R),
--                                      E.Failed (C.R)
--
--      Note that there is no guaranteed order in which the callbacks are
--      executed, so for instance it is possible that C.Resolved and
--      E.Resolved are called before B.Resolved.
--
--  Q: What if I want different resolve and failure callbacks ?
--  A: A callback is an object with both a Resolved and a Failed primitive
--     operations. So you could set two different callbacks on the same
--     promise (as we did above in the first question)

with GNATCOLL.Refcount;

package Promises is

   type Promise_State is (Pending, Resolved, Failed);
   --  The various states that a promise can have

   type Promise_Chain is tagged private;
   procedure Start (Self : Promise_Chain) with Inline => True;
   --  A dummy type used when chaining promises with the "and"
   --  operator. See below for an example of code.
   --
   --  Do not mark this procedure as "is null", since otherwise GNAT
   --  does not even call the last "and" in the chain.

   ----------
   -- Impl --
   ----------
   --  This package is for implementation details

   package Impl is

      type Abstract_Promise_Data
         (State : Promise_State) is abstract tagged null record;
      type Abstract_Promise_Data_Access
         is access all Abstract_Promise_Data'Class;
      procedure Free (Self : in out Abstract_Promise_Data) is null;
      procedure Dispatch_Free (Self : in out Abstract_Promise_Data'Class);

      type Abstract_Promise is interface;

      package Promise_Pointers is new GNATCOLL.Refcount.Shared_Pointers
         (Element_Type           => Abstract_Promise_Data'Class,
          Release                => Dispatch_Free,
          Atomic_Counters        => True,   --  thread-safe
          Potentially_Controlled => True);  --  a vector is controlled
      type Root_Promise is
         new Promise_Pointers.Ref and Abstract_Promise with null record;

      function Is_Created (Self : Root_Promise'Class) return Boolean
        with Inline;
      --  Whether the promise has been created

      function Get_State (Self : Root_Promise'Class) return Promise_State
        with
          Inline,
          Pre => Self.Is_Created;
      --  Return the state of the promise

      ---------------
      -- Callbacks --
      ---------------

      type Promise_Callback is interface;
      type Promise_Callback_Access is access all Promise_Callback'Class;

      procedure Failed
         (Self : in out Promise_Callback; Reason : String) is null;
      --  Called when a promise has failed and will never be resolved.

      procedure Free (Self : in out Promise_Callback) is null;
      --  Free the memory associated with Callback

   end Impl;

   --------------
   -- Promises --
   --------------

   generic
      type T (<>) is private;
   package Promises is

      type Promise is new Impl.Abstract_Promise with private;
      --  A promise is a smart pointer: it is a wrapper around shared
      --  data that is freed when no more reference to the promise
      --  exists.

      subtype Result_Type is T;

      ---------------
      -- Callbacks --
      ---------------

      type Callback is interface and Impl.Promise_Callback;
      type Callback_Access is access all Callback'Class;

      procedure Resolved (Self : in out Callback; R : Result_Type) is null;
      --  Executed when a promise is resolved. It provides the real value
      --  associated with the promise.

      --------------
      -- Promises --
      --------------

      function Create return Promise
        with
          Post => Create'Result.Is_Created
             and Create'Result.Get_State = Pending;
      --  Create a new promise, with no associated value.

      procedure Resolve (Self : in out Promise; R : T)
        with
          Pre => Self.Is_Created and Self.Get_State = Pending,
          Post => Self.Get_State = Resolved;
      --  Give a result to the promise.
      --  The callbacks' Resolved method is executed.
      --  This can only be called once on a promise.

      procedure Fail (Self : in out Promise; Reason : String)
        with
          Pre => Self.Is_Created and Self.Get_State = Pending,
          Post => Self.Get_State = Failed;
      --  Mark the promise has failed. It will never be resolved.
      --  The callbacks' Failed method is executed.

      procedure When_Done
        (Self : Promise;
         Cb   : not null access Callback'Class)
        with Pre => Self.Is_Created;
      function "and"
        (Self  : Promise;
         Cb    : not null access Callback'Class)
        return Promise_Chain
        with Pre => Self.Is_Created;
      --  Will call Cb when Self is resolved or failed (or immediately if Self
      --  has already been resolved or failed).
      --  Any number of callbacks can be set on each promise.
      --  If you want to chain promises (i.e. your callback itself returns
      --  a promise), take a look at the Chains package below.
      --
      --  Cb must be allocated specifically for this call, and will be
      --  freed as needed. You must not reuse the same pointer for multiple
      --  calls to When_Done.
      --  ??? This is unsafe
      --
      --  Self is modified, but does not need to be "in out" since a promise
      --  is a pointer. This means that When_Done can be directly called on
      --  the result of a function call, for instance.

      type Callback_List (<>) is private;

      function "&"
        (Cb    : not null access Callback'Class;
         Cb2   : not null access Callback'Class)
        return Callback_List;
      function "&"
        (List  : Callback_List;
         Cb2   : not null access Callback'Class)
        return Callback_List;

      function "and"
        (Self : Promise; Cb : Callback_List)
        return Promise_Chain;

      function Is_Created
         (Self : Promise'Class) return Boolean with Inline_Always;
      function Get_State
         (Self : Promise'Class) return Promise_State with Inline_Always;

   private
      type Promise is new Impl.Root_Promise with null record;

      type Callback_List is
         array (Natural range <>) of not null access Callback'Class;

      function Is_Created (Self : Promise'Class) return Boolean
        is (Impl.Is_Created (Self));
      function Get_State (Self : Promise'Class) return Promise_State
        is (Impl.Get_State (Self));
   end Promises;

   ------------
   -- Chains --
   ------------

   generic
      with package Input_Promises is new Promises (<>);
      with package Output_Promises is new Promises (<>);
   package Chains is

      type Callback is abstract new Input_Promises.Callback
         with private;
      procedure Resolved
        (Self   : in out Callback;
         Input  : Input_Promises.Result_Type;
         Output : in out Output_Promises.Promise)
        is abstract
        with Post'Class => Output.Get_State /= Pending;
      --  This is the procedure that needs overriding, not the one inherited
      --  from Input_Promises. When chaining, a callback returns another
      --  promise, to which the user can attach further callbacks, and so on.
      --
      --  Failures in a promise are by default propagated to the output
      --  promise, unless you override the Failed primitive operation of
      --  Self.

      type Callback_List (<>) is private;

      function Is_Registered
        (Self : not null access Callback'Class) return Boolean
        with Inline;
      function Is_Registered (Self : Callback_List) return Boolean
        with Inline;
      --  Whether the callback has already been set on a promise. It is
      --  invalid to use the same callback on multiple promises (or even
      --  multiple times on the same promise).

      function "and"
        (Input : Input_Promises.Promise;
         Cb    : not null access Callback'Class)
        return Output_Promises.Promise
        with
          Pre  => not Is_Registered (Cb) and Input.Is_Created,
          Post => Is_Registered (Cb)
             and "and"'Result.Is_Created;
      --  Chains two properties.
      --  When Input is resolved, Cb is executed and will in turn resolve
      --  the output promise
      --  These functions return immediately a promise that will be resolved
      --  later.

      function "&"
        (Cb    : not null access Callback'Class;
         Cb2   : not null access Input_Promises.Callback'Class)
        return Callback_List
        with
           Pre => not Is_Registered (Cb);
           --  ??? Results in GNAT bug box
           --  Post => "and"'Result = Cb
           --     and not Is_Registered ("and"'Result);
      function "&"
        (List  : Callback_List;
         Cb2   : not null access Input_Promises.Callback'Class)
        return Callback_List;
      --  Used to set multiple callbacks on the same promise, as in:
      --      P & (new A and new B) & new C
      --  Only Cb is expected to output a promise, which will be
      --  forwarded to the next step (C in this example). Cb2 only
      --  gets notified via its Resolved and Failed primitives.

      function "and"
        (Input : Input_Promises.Promise;
         Cb    : Callback_List)
        return Output_Promises.Promise
        with
          Pre  => not Is_Registered (Cb) and Input.Is_Created,
          Post => Is_Registered (Cb)
             and "and"'Result.Is_Created;
      --  Chaining multiple callbacks on the same promise

   private
      type Callback is abstract new Input_Promises.Callback with record
         Promise : aliased Output_Promises.Promise;
      end record;
      overriding procedure Resolved
         (Self : in out Callback; P : Input_Promises.Result_Type);
      overriding procedure Failed (Self : in out Callback; Reason : String);

      type Callback_Array is array (Natural range <>)
         of not null access Input_Promises.Callback'Class;
      type Callback_List (N : Natural) is record
         Cb  : not null access Callback'Class;
         Cb2 : Callback_Array (1 .. N);
      end record;
   end Chains;

private
   type Promise_Chain is tagged null record;
end Promises;
