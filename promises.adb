with Ada.Unchecked_Deallocation;
with GNAT.Strings;   use GNAT.Strings;

package body Promises is

   --------------
   -- Promises --
   --------------
   
   package body Promises is

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Callback'Class, Callback_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (T, T_Access);
   
      ------------
      -- Create --
      ------------

      function Create return Promise is
      begin
         return P : Promise do
            P.Set
               (Data =>
                  (Callbacks => Cb_Vectors.Empty_Vector,
                   State     => Pending));
         end return;
      end Create;

      ----------------
      -- Is_Created --
      ----------------

      function Is_Created (Self : Promise) return Boolean is
      begin
         return not Self.Is_Null;
      end Is_Created;

      ---------------
      -- Get_State --
      ---------------

      function Get_State (Self : Promise) return Promise_State is
      begin
         return Self.Get.State;
      end Get_State;

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Promise_Data) is
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
         if Self.Get.State = Pending then
            for Cb of Self.Get.Callbacks loop
               Cb.Resolved (R);
               Cb.Free;
               Unchecked_Free (Cb);
            end loop;

            Self.Set
               (Data => (State => Resolved, Value => new T'(R)));
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
         if Self.Get.State = Pending then
            for Cb of Self.Get.Callbacks loop
               Cb.Failed (Reason);
               Cb.Free;
               Unchecked_Free (Cb);
            end loop;

            Self.Set
               (Data => (State => Failed, Reason => new String'(Reason)));
         end if;
      end Fail;

      ---------------
      -- When_Done --
      ---------------

      procedure When_Done
        (Self : Promise; Cb   : not null access Callback'Class)
      is
         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to When_Done.
         C : Callback_Access := Cb.all'Unrestricted_Access;
      begin
         case Self.Get.State is
            when Pending =>
               Self.Get.Callbacks.Append (C);

            when Resolved =>
               C.Resolved (Self.Get.Value.all);
               C.Free;
               Unchecked_Free (C);

            when Failed =>
               C.Failed (Self.Get.Reason.all);
               C.Free;
               Unchecked_Free (C);
         end case;
      end When_Done;

   end Promises;

   ------------
   -- Chains --
   ------------
   
   package body Chains is
   
      ---------------
      -- When_Done --
      ---------------

      function When_Done
         (Self : Input_Promises.Promise;
          Cb   : not null access Callback'Class)
         return Output_Promises.Promise is
      begin
         Cb.Promise := Output_Promises.Create;
         Input_Promises.When_Done (Self, Cb.all'Unrestricted_Access);
         return Cb.Promise;
      end When_Done;

      --------------
      -- Resolved --
      --------------

      overriding procedure Resolved
         (Self : in out Callback; P : Input_Promises.Result_Type) is
      begin
         Resolved (Callback'Class (Self), P, Self.Promise);
      end Resolved;

   end Chains;

end Promises;
