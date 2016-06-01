with Ada.Containers.Vectors;
with GNATCOLL.Refcount;
with GNAT.Strings;

package Promises is

   type Promise_Chain is tagged private;
   procedure Ignore (Self : Promise_Chain) is null;
   --  A dummy type used when chaining promises with the "and"
   --  operator. See below for an example of code.

   generic
      type T (<>) is private;
   package Promises is
      type Promise is tagged private;
      --  A promise is a smart pointer: it is a wrapper around shared
      --  data that is freed when no more reference to the promise
      --  exists.

      subtype Result_Type is T;

      ---------------
      -- Callbacks --
      ---------------

      type Callback is interface;
      type Callback_Access is access all Callback'Class;

      procedure Resolved (Self : in out Callback; R : Result_Type) is null;
      --  Executed when a promise is resolved. It provides the real value
      --  associated with the promise.

      procedure Failed (Self : in out Callback; Reason : String) is null;
      --  Called when a promise has failed and will never be resolved.

      procedure Free (Self : in out Callback) is null;
      --  Free the memory associated with Callback

      --------------
      -- Promises --
      --------------

      function Create return Promise
        with
          Post => Create'Result.Is_Created
             and Create'Result.Get_State = Pending;
      --  Create a new promise, with no associated value.

      function Is_Created (Self : Promise) return Boolean with Inline;
      --  Whether the promise has been created

      type Promise_State is (Pending, Resolved, Failed);
      function Get_State (Self : Promise) return Promise_State with Inline;
      --  Return the state of the promise

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

      procedure When_Done
        (Self     : Promise;
         Resolved : access procedure (R : T) := null;
         Failed   : access procedure (Reason : String) := null);
      --  A variant of When_Done that manipulate access to subprograms.

      ------------------------
      -- Chainging promises --
      ------------------------
      --  The following is a helper to write chains of promises in a more
      --  user-friendly fashion. Rather than using When_Done (or the
      --  version provides in the Chains package below), which requires
      --  a bit of an inversion in the order, as in:
      --
      --       Float_To_Str.When_Done
      --          (Int_To_Float.When_Done (P, new ...),
      --           new ...)
      --          .When_Done (...)
      --
      --  we can use the simpler:
      --
      --       use Float_To_Str, Int_To_Float;
      --       (P and new ... and new ...).Ignore;
      --
      --  with the exact order in which the callbacks will be executed.

      function "and"
         (Self  : Promise;
          Cb    : not null access Callback'Class)
         return Promise_Chain;
      --  Same as When_Done, easier to chain

   private
      package Cb_Vectors is new Ada.Containers.Vectors
         (Positive, Callback_Access);

      type T_Access is access all T;

      type Promise_Data (State : Promise_State := Pending) is record
         case State is
            when Pending =>
               Callbacks : Cb_Vectors.Vector;
               --  Need a vector here, but should try to limit memory allocs.
               --  A bounded vector might be more efficient, and sufficient in
               --  practice.

            when Resolved =>
               Value     : T_Access;
               --  ??? Using the ada-traits-containers approach, we could avoid
               --  some memory allocation here.

            when Failed =>
               Reason    : GNAT.Strings.String_Access;
         end case;
      end record;
      procedure Free (Self : in out Promise_Data);

      package Promise_Pointers is new GNATCOLL.Refcount.Shared_Pointers
         (Element_Type           => Promise_Data,
          Release                => Free,
          Atomic_Counters        => True,   --  thread-safe
          Potentially_Controlled => True);  --  a vector is controlled
      type Promise is new Promise_Pointers.Ref with null record;
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
        is abstract;
      --  This is the procedure that needs overriding, not the one inherited
      --  from Input_Promises. When chaining, a callback returns another
      --  promise, to which the user can attach further callbacks, and so on.
      --
      --  Failures in a promise are by default propagated to the output
      --  promise, unless you override the Failed primitive operation of
      --  Self.

      function When_Done
         (Self : Input_Promises.Promise;
          Cb   : not null access Callback'Class)
         return Output_Promises.Promise;
      --  Returns a new promise, which will be resolved by Cb eventually.

      function "and"
         (Self  : Input_Promises.Promise;
          Cb    : not null access Callback'Class)
         return Output_Promises.Promise;
      --  ??? Tentative syntax to improve When_Done syntax

   private
      type Callback is abstract new Input_Promises.Callback with record
         Promise : aliased Output_Promises.Promise;
      end record;
      overriding procedure Resolved
         (Self : in out Callback; P : Input_Promises.Result_Type);
      overriding procedure Failed (Self : in out Callback; Reason : String);
   end Chains;

private
   type Promise_Chain is tagged null record;
end Promises;
