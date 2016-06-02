with Ada.Containers.Vectors;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;       use GNAT.Strings;

package body Promises is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Impl.Promise_Callback'Class, Impl.Promise_Callback_Access);

   procedure Free (Cb : in out Impl.Promise_Callback_Access);
   --  Free the memory associated with Cb

   package Cb_Vectors is new Ada.Containers.Vectors
      (Positive, Impl.Promise_Callback_Access, Impl."=");

   ----------
   -- Impl --
   ----------

   package body Impl is

      -------------------
      -- Dispatch_Free --
      -------------------

      procedure Dispatch_Free (Self : in out Abstract_Promise_Data'Class) is
      begin
         Free (Self);
      end Dispatch_Free;

      ----------------
      -- Is_Created --
      ----------------

      function Is_Created (Self : Root_Promise'Class) return Boolean is
      begin
         return not Self.Is_Null;
      end Is_Created;

      ---------------
      -- Get_State --
      ---------------

      function Get_State (Self : Root_Promise'Class) return Promise_State is
      begin
         return Self.Get.State;
      end Get_State;

   end Impl;

   use type Impl.Promise_Callback_Access;

   ----------
   -- Free --
   ----------

   procedure Free (Cb : in out Impl.Promise_Callback_Access) is
   begin
      if Cb /= null then
         Impl.Free (Cb.all);
         Unchecked_Free (Cb);
      end if;
   end Free;

   -----------
   -- Start --
   -----------

   procedure Start (Self : Promise_Chain) is
   begin
      null;
   end Start;

   --------------
   -- Promises --
   --------------

   package body Promises is

      type T_Access is access all T;

      type Promise_Data (State : Promise_State)
        is new Impl.Abstract_Promise_Data (State)
      with record
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

      type Promise_Data_Access is access all Promise_Data'Class;

      overriding procedure Free (Self : in out Promise_Data);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (T, T_Access);

      function Get_Data
        (Self : Promise'Class) return not null access Promise_Data'Class
        is (Promise_Data_Access (Impl.Promise_Pointers.Unchecked_Get (Self)))
        with Inline_Always;

      ------------
      -- Create --
      ------------

      function Create return Promise is
      begin
         return P : Promise do
            P.Set
               (Data => Promise_Data'
                  (Callbacks => Cb_Vectors.Empty_Vector,
                   State     => Pending));
         end return;
      end Create;

      ----------
      -- Free --
      ----------

      overriding procedure Free (Self : in out Promise_Data) is
      begin
         --  ??? Should we raise an exception if the promise was neither
         --  resolved nor failed
         case Self.State is
            when Pending =>
               null;
            when Resolved =>
               Unchecked_Free (Self.Value);
            when Failed =>
               Free (Self.Reason);
         end case;
      end Free;

      -------------
      -- Resolve --
      -------------

      procedure Resolve (Self : in out Promise; R : T) is
      begin
         --  ??? Checking the state is superfluous since this is in the
         --  preconditions.
         --  ??? This is not thread safe, we should use an atomic operation,
         --  like GNATCOLL.Atomics.Sync_Bool_Compare_And_Swap
         if Self.Get_Data.State = Pending then
            for Cb of Self.Get_Data.Callbacks loop
               Callback_Access (Cb).Resolved (R);
               Free (Impl.Promise_Callback_Access (Cb));
            end loop;

            Self.Set
               (Data => Promise_Data'(State => Resolved, Value => new T'(R)));
         end if;
      end Resolve;

      -------------
      -- Fail --
      -------------

      procedure Fail (Self : in out Promise; Reason : String) is
      begin
         --  ??? Checking the state is superfluous since this is in the
         --  preconditions.
         --  ??? This is not thread safe, we should use an atomic operation,
         --  like GNATCOLL.Atomics.Sync_Bool_Compare_And_Swap
         if Self.Get_Data.State = Pending then
            for Cb of Self.Get_Data.Callbacks loop
               Cb.Failed (Reason);
               Free (Impl.Promise_Callback_Access (Cb));
            end loop;

            Self.Set
               (Data => Promise_Data'
                  (State => Failed, Reason => new String'(Reason)));
         end if;
      end Fail;

      ---------------
      -- When_Done --
      ---------------

      procedure When_Done
        (Self : Promise; Cb : not null access Callback'Class)
      is
         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to When_Done.
         C : Callback_Access := Cb.all'Unrestricted_Access;
      begin
         case Self.Get_Data.State is
            when Pending =>
               Self.Get_Data.Callbacks.Append
                  (Impl.Promise_Callback_Access (C));

            when Resolved =>
               C.Resolved (Self.Get_Data.Value.all);
               Free (Impl.Promise_Callback_Access (C));

            when Failed =>
               C.Failed (Self.Get_Data.Reason.all);
               Free (Impl.Promise_Callback_Access (C));
         end case;
      end When_Done;

      -----------
      -- "and" --
      -----------

      function "and"
         (Self : Promise; Cb : Callback_List)
         return Promise_Chain is
      begin
         for C of Cb loop
            Self.When_Done (C);
         end loop;
         return Promise_Chain'(null record);
      end "and";

      -----------
      -- "and" --
      -----------

      function "and"
         (Self  : Promise;
          Cb    : not null access Callback'Class)
         return Promise_Chain is
      begin
         Self.When_Done (Cb);
         return Promise_Chain'(null record);
      end "and";

      ---------
      -- "&" --
      ---------

      function "&"
        (Cb    : not null access Callback'Class;
         Cb2   : not null access Callback'Class)
        return Callback_List is
      begin
         return (Cb.all'Unrestricted_Access,
                 Cb2.all'Unrestricted_Access);
      end "&";

      ---------
      -- "&" --
      ---------

      function "&"
        (List  : Callback_List;
         Cb2   : not null access Callback'Class)
        return Callback_List is
      begin
         return List & (1 => Cb2.all'Unrestricted_Access);
      end "&";

   end Promises;

   ------------
   -- Chains --
   ------------

   package body Chains is

      -----------
      -- "and" --
      -----------

      function "and"
         (Input : Input_Promises.Promise;
          Cb    : not null access Callback'Class)
         return Output_Promises.Promise is
      begin
         Cb.Promise := Output_Promises.Create;
         Input_Promises.When_Done (Input, Cb.all'Unrestricted_Access);
         return Cb.Promise;
      end "and";

      -----------
      -- "and" --
      -----------

      function "and"
        (Input : Input_Promises.Promise;
         Cb    : Callback_List)
        return Output_Promises.Promise
      is
         P : constant Output_Promises.Promise := Input and Cb.Cb;
      begin
         for C of Cb.Cb2 loop
            Input_Promises.When_Done (Input, C);
         end loop;
         return P;
      end "and";

      -------------------
      -- Is_Registered --
      -------------------

      function Is_Registered
         (Self : not null access Callback'Class) return Boolean is
      begin
         return Self.Promise.Is_Created;
      end Is_Registered;

      -------------------
      -- Is_Registered --
      -------------------

      function Is_Registered
         (Self : Callback_List) return Boolean is
      begin
         return Self.Cb.Promise.Is_Created;
      end Is_Registered;

      --------------
      -- Resolved --
      --------------

      overriding procedure Resolved
         (Self : in out Callback; P : Input_Promises.Result_Type) is
      begin
         Resolved (Callback'Class (Self), P, Self.Promise);
      exception
         when E : others =>
            Self.Promise.Fail (Exception_Message (E));
      end Resolved;

      ------------
      -- Failed --
      ------------

      overriding procedure Failed
         (Self : in out Callback; Reason : String) is
      begin
         --  Propagate the failure
         Self.Promise.Fail (Reason);
      end Failed;

      -----------
      -- "&" --
      -----------

      function "&"
         (Cb   : not null access Callback'Class;
          Cb2  : not null access Input_Promises.Callback'Class)
         return Callback_List is
      begin
         return Callback_List'
            (N   => 1,
             Cb  => Cb.all'Unrestricted_Access,
             Cb2 => (1 => Cb2.all'Unrestricted_Access));
      end "&";

      -----------
      -- "&" --
      -----------

      function "&"
         (List : Callback_List;
          Cb2  : not null access Input_Promises.Callback'Class)
         return Callback_List is
      begin
         return Callback_List'
            (N   => List.N + 1,
             Cb  => List.Cb,
             Cb2 => List.Cb2 & (1 => Cb2.all'Unrestricted_Access));
      end "&";

   end Chains;

end Promises;
