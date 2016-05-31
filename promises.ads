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

   ---------------
   -- Callbacks --
   ---------------

   generic
      type T (<>) is private;
   package Callbacks is
      type Callback is interface;
      type Callback_Access is access all Callback'Class;
      procedure Resolved (Self : in out Callback; R : T) is abstract;
   end Callbacks;

   --------------
   -- Promises --
   --------------
   
   generic
      type T (<>) is private;
   package Promises is
      type Promise is tagged private;
   
      procedure Resolve (Self : in out Promise; R : T);
      --  Give a result to the promise, and execute all registered
      --  callbacks.
   
      package T_Callbacks is new Callbacks (T);
      subtype Callback is T_Callbacks.Callback;

      procedure When_Done
         (Self : in out Promise;
          Cb   : not null access T_Callbacks.Callback'Class);
   
   private
      use type T_Callbacks.Callback_Access;
      package Cb_Vectors is new Ada.Containers.Vectors
         (Positive, T_Callbacks.Callback_Access);

      type Promise is tagged record
         Callbacks : Cb_Vectors.Vector;
         --  Need a vector here, but should try to limit memory allocs
      end record;
   end Promises;

   ------------
   -- Chains --
   ------------
   
   generic
      type T (<>) is private;
      type T2 (<>) is private;
      with package T_Promises is new Promises (T);
      with package T2_Promises is new Promises (T2);
   package Chains is
      type Callback is abstract new T_Promises.T_Callbacks.Callback
         with private;
      procedure Resolved
        (Self : in out Callback; P : T; Output : in out T2_Promises.Promise)
        is abstract;
      --  This is the procedure that needs overriding, not the one inherited
      --  from T_Callbacks

      --  must resolve Output eventually
   
      function Chain
         (Self : in out T_Promises.Promise;
          Cb   : not null access Callback'Class)
         return access T2_Promises.Promise;
      --  Returns a new promise, which will also be passed to Cb, to
      --  be resolved later

   private
      type Callback is abstract new T_Promises.T_Callbacks.Callback with
      record
         Promise : aliased T2_Promises.Promise;
      end record;
      overriding procedure Resolved (Self : in out Callback; P : T);
   end Chains;

end Promises;
