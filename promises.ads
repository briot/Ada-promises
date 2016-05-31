--   Ideally, we want to chain promises:
--       Get_Promise_T
--       .When_Done (Do_Something)   --  receives a T argument
--       .When_Done (Do_Else)        --  receives result of Do_Something
--       .Ignore;                    --  don't care about the last
--
--       function Get_Promise_T return Promise<T>;
--       procedure Do_Something (P : T; O : in out Promise<T2>);
--       procedure Do_Else (P : T2; O : in out Promise<*>);
--
--  But when we instantiate Promise<T>, for instance, we should not have
--  to know that it will be chained with a function that manipulates T2
--  (although when we instantiate Do_Something we know both T and T2).
--
--  Multiple callbacks can be set on a promise. Each callback can return
--  a different type.
--
--      procedure Do_Something2 (P : T; O : in out Promise<T3>);
--      P : Promise<T> := Get_Promise_T;
--      P.When_Done (Do_Something);
--      P.When_Done (Do_Something2);
--
--  A promise is only resolved once, so as soon as the callbacks have
--  executed, they can be freed (if necessary). However, it is possible to
--  set a callback on an already resolved promise later on (and the callback
--  is called immediately, so the promise needs to store the callback.

with Ada.Containers.Vectors;

package Promises is

   generic
      type T (<>) is private;
   package Promises is
      type Promise is tagged private;
      subtype Result_Type is T;
   
      procedure Resolve (Self : in out Promise; R : T);
      --  Give a result to the promise, and execute all registered
      --  callbacks.
   
      type Callback is interface;
      type Callback_Access is access all Callback'Class;
      procedure Resolved (Self : in out Callback; R : T) is abstract;
      --  Executed when a promise is resolved. It provides the real value
      --  associated with the promise.
   
      procedure When_Done
         (Self : in out Promise;
          Cb   : not null access Callback'Class);
      --  Will call Cb when Self is resolved.
      --  Any number of callbacks can be set on each promise.
      --  If you want to chain promises (i.e. your callback itself returns
      --  a promise), take a look at the Chains package below.
   
   private
      package Cb_Vectors is new Ada.Containers.Vectors
         (Positive, Callback_Access);
   
      type Promise is tagged record
         Callbacks : Cb_Vectors.Vector;
         --  Need a vector here, but should try to limit memory allocs.
         --  A bounded vector might be more efficient, and sufficient in
         --  practice.
      end record;
   end Promises;

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
   
      function When_Done
         (Self : in out Input_Promises.Promise;
          Cb   : not null access Callback'Class)
         return access Output_Promises.Promise;
      --  Returns a new promise, which will be resolved by Cb eventually.
   
   private
      type Callback is abstract new Input_Promises.Callback with record
         Promise : aliased Output_Promises.Promise;
      end record;
      overriding procedure Resolved
         (Self : in out Callback; P : Input_Promises.Result_Type);
   end Chains;

end Promises;
