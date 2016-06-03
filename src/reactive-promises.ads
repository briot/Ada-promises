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
--       overriding procedure On_Next
--          (Self : in out Process_Page; Page : String)
--       is
--       begin
--          Put_Line ("Page contents is known: " & Page);
--       end On_Next;
--
--       P : Str_Promises.Promise;
--
--       P := Fetch_URL_Asynchronously ("http://...");
--       P.Subscribe (new Process_Page);
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
--       overriding procedure On_Next
--          (Self : in out Process_Page; Page : String);
--       --  For instance, count the number of elements in the XML
--
--       type Report_Count is new Int_Promises.Callback with null record;
--       overriding procedure On_Next
--          (Self : in out Report_Count; Count : Integer);
--       --  For instance display the number of elements in a GUI
--
--       P : Str_Promises.Promise;
--
--       Subscribe
--          (Fetch_URL_Asynchronously ("http://...")
--           and new Count_Elements
--           and new Report_Count);
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
--                an actual value to the promise (via a call to Set_Value)
--     - Failed: the routine failed, and no value will ever be provided to the
--                routine. A reason for the failure is provided via a call
--                to Set_Error)
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
--  Tasks
--  =====
--
--  Promises are task safe. They can be used from multiple threads (and, as
--  always, a single call to Set_Value or Set_Error can be done), subscribed
--  to from multiple threads,...
--  However, the value itself is under your control. Although the promise
--  will only execute one callback at a time, to which is passes the value,
--  you should ensure that the value is not used from another thread in
--  parallel, or provide appropriate locking.
--  Using simple types like Integers or Strings should be safe.
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
--       Subscribe (P and new A and new B and new C;
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
--  If P is resolved:           A.On_Next (P.V)
--     If A.P is resolved:         B.On_Next (A.V)
--        if B.P is resolved:        C.On_Next (B.V)
--        else B.P failed:           C.On_Error (B.R)
--     else A.P failed:            B.On_Error (A.R), C.On_Error (B.R)
--  else P failed:              A.On_Error (P.R), B.On_Error (A.R),
--                                 C.On_Error (B.R)
--
--  Q: What if I want multiple callbacks on the same promise ?
--  A: You need to use intermediate variables, as in:
--       Q := P and new A;
--       Subscribe (Q and new B);
--       Subscribe (Q and new C);
--     Where both B and C are callbacks on A's return promise (and not
--     chained together).
--
--     A more convenient syntax exists, as in the following example:
--
--         P and (new A & new B)        --  A.P is passed on to the next step
--           and (new C & new D)        --  C.P is passed on to the next step
--           and new E;
--
--         If P is resolved:            A.On_Next (P.V), B.On_Next (P.V)
--            if A.P is resolved:         C.On_Next (A.V), D.On_Next (A.V)
--               if C.P is resolved:        E.On_Next (C.V)
--               else C.P failed:           E.On_Error (C.R)
--            else A.P failed:            C.On_Error (A.R), D.On_Error (A.R),
--                                          E.On_Error (C.R)
--         else P failed:               A.On_Error (P.R), B.On_Error (P.R),
--                                      C.On_Error (A.R), D.On_Error (A.R),
--                                      E.On_Error (C.R)
--
--      Note that there is no guaranteed order in which the callbacks are
--      executed, so for instance it is possible that C.On_Next and
--      E.On_Next are called before B.On_Next.
--
--  Q: What if I want different success and failure callbacks ?
--  A: A callback is an object with both a On_Next and a On_Error primitive
--     operations. So you could set two different callbacks on the same
--     promise (as we did above in the first question)

with GNATCOLL.Atomic;
with GNATCOLL.Refcount;

package Reactive.Promises is

   use type GNATCOLL.Atomic.Atomic_Counter;

   subtype Base_Promise_State is GNATCOLL.Atomic.Atomic_Counter;
   Pending     : constant Base_Promise_State := 0;
   Resolved    : constant Base_Promise_State := 1;
   Failed      : constant Base_Promise_State := 2;
   Resolving   : constant Base_Promise_State := 3;
   Failing     : constant Base_Promise_State := 4;
   Subscribing : constant Base_Promise_State := 5;
   subtype Promise_State is Base_Promise_State range Pending .. Subscribing;
   --  The various states that a promise can have.
   --  We use atomic operations when possible to manipulate it, to make
   --  promises task safe.

   ----------
   -- Impl --
   ----------
   --  This section is for implementation details

   type IPromise_Data is interface;
   procedure Free (Self : in out IPromise_Data) is null;
   procedure Dispatch_Free (Self : in out IPromise_Data'Class);

   type IAbstract_Promise is interface;

   package Promise_Pointers is new GNATCOLL.Refcount.Shared_Pointers
      (Element_Type           => IPromise_Data'Class,
       Release                => Dispatch_Free,
       Atomic_Counters        => True,   --  thread-safe
       Potentially_Controlled => True);  --  a vector is controlled
   type Root_Promise is
      new Promise_Pointers.Ref and IAbstract_Promise with null record;

   --------------
   -- Promises --
   --------------

   generic
      type T (<>) is private;
   package Promises is
      subtype Result_Type is T;

      package Observers is new Typed_Observers (T);
      package Observables is new Typed_Observables (Observers);

      type Promise is new IAbstract_Promise and Observables.IObservable
         with private;
      --  A promise is a smart pointer: it is a wrapper around shared
      --  data that is freed when no more reference to the promise
      --  exists.

      subtype Callback is Observers.IObserver;

      function Create return Promise
        with
          Post => Create'Result.Is_Created
             and Create'Result.Get_State = Pending;
      --  Create a new promise, with no associated value.

      procedure Set_Value (Self : in out Promise; R : T)
        with
          Pre => Self.Is_Created
             and Self.Get_State /= Resolved
             and Self.Get_State /= Failed,
          Post => Self.Get_State = Resolved;
      --  Give a result to the promise.
      --  The callbacks' On_Next methods are executed.
      --  This can only be called once on a promise.

      procedure Set_Error (Self : in out Promise; Reason : String)
        with
          Pre => Self.Is_Created
             and Self.Get_State /= Resolved
             and Self.Get_State /= Failed,
          Post => Self.Get_State = Failed;
      --  Mark the promise has failed. It will never be resolved.
      --  The callbacks' On_Error method are executed.

      overriding function Subscribe
        (Self : Promise;
         Cb   : not null access Callback'Class)
        return Subscription
        with Pre => Self.Is_Created;

      function "and"
        (Self  : Promise;
         Cb    : not null access Observers.IObserver'Class)
        return Subscription
        is (Observables."and" (Self, Cb))
        with Pre => Self.Is_Created;
      function "and"
        (Self : Promise; Cb : Observers.List) return Subscription
        is (Observables."and" (Self, Cb))
        with Pre => Self.Is_Created;

      subtype List is Observers.List;
      function "&"
        (Cb    : not null access Callback'Class;
         Cb2   : not null access Callback'Class) return List
        renames Observers."&";
      function "&"
        (Self  : List;
         Cb2   : not null access Callback'Class) return List
        renames Observers."&";

      function Is_Created
         (Self : Promise'Class) return Boolean with Inline_Always;
      --  Whether the promise has been created

      function Get_State
        (Self : Promise'Class) return Promise_State with Inline_Always;
      --  Used for pre and post conditions

   private
      type Promise is new Root_Promise and Observables.IObservable
         with null record;

      function Is_Created (Self : Promise'Class) return Boolean
        is (not Self.Is_Null);
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
      procedure On_Next
        (Self   : in out Callback;
         Input  : Input_Promises.Result_Type;
         Output : in out Output_Promises.Promise)
        is abstract
        with
           Post'Class =>
              Output.Get_State = Resolved
              or Output.Get_State = Failed;
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
      --  gets notified via its On_Next and On_Error primitives.

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
      overriding procedure On_Next
         (Self : in out Callback; P : Input_Promises.Result_Type);
      overriding procedure On_Error (Self : in out Callback; Reason : String);

      type Callback_Array is array (Natural range <>)
         of not null access Input_Promises.Callback'Class;
      type Callback_List (N : Natural) is record
         Cb  : not null access Callback'Class;
         Cb2 : Callback_Array (1 .. N);
      end record;
   end Chains;

private
   type Promise_Chain is tagged null record;
end Reactive.Promises;
