with Ada.Containers.Vectors;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.Atomic;    use GNATCOLL.Atomic;

package body Reactive.Promises is

   package Cb_Vectors is new Ada.Containers.Vectors
      (Positive, IObserver_Access);

   -------------------
   -- Dispatch_Free --
   -------------------

   procedure Dispatch_Free (Self : in out IPromise_Data'Class) is
   begin
      Free (Self);
   end Dispatch_Free;

   --------------
   -- Promises --
   --------------

   package body Promises is

      type T_Access is access all T;

      type Promise_Data is new IPromise_Data with record
         State     : aliased Base_Promise_State := Pending;

         Callbacks : Cb_Vectors.Vector;
         --  Need a vector here, but should try to limit memory allocs.
         --  A bounded vector might be more efficient, and sufficient in
         --  practice.

         Value     : T_Access;
         --  ??? Using the ada-traits-containers approach, we could avoid
         --  some memory allocation here.

         Reason    : GNAT.Strings.String_Access;
      end record;
      type Promise_Data_Access is access all Promise_Data'Class;

      overriding procedure Free (Self : in out Promise_Data);

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (T, T_Access);

      ---------------
      -- Get_State --
      ---------------

      function Get_State (Self : Promise'Class) return Promise_State is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Promise_Pointers.Unchecked_Get (Self));
      begin
         return Promise_State (D.State);
      end Get_State;

      ------------
      -- Create --
      ------------

      function Create return Promise is
      begin
         return P : Promise do
            P.Set
               (Data => Promise_Data'
                  (Callbacks => Cb_Vectors.Empty_Vector,
                   State     => Pending,
                   Value     => null,
                   Reason    => null));
         end return;
      end Create;

      ----------
      -- Free --
      ----------

      overriding procedure Free (Self : in out Promise_Data) is
      begin
         Unchecked_Free (Self.Value);
         Free (Self.Reason);
      end Free;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Self : in out Promise; R : T) is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Promise_Pointers.Unchecked_Get (Self));
         Old : Promise_State;
      begin
         loop
            Old := Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Resolving));
            case Old is
               when Resolved | Failed =>
                  --  Promise has already been completed, this is an error
                  return;

               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Pending =>
                  --  OK, we can change the state
                  for Cb of D.Callbacks loop
                     Observers.IObserver_Access (Cb).On_Next (R);
                     Free (IFreeable_Access (Cb));
                  end loop;

                  D.Callbacks.Clear;   --  No longer needed, release them
                  D.Value := new T'(R);
                  D.State := Resolved;  --  Fully resolved now
                  exit;
            end case;
         end loop;
      end Set_Value;

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error (Self : in out Promise; Reason : String) is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Promise_Pointers.Unchecked_Get (Self));
         Old : Promise_State;
      begin
         loop
            Old := Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Failing));
            case Old is
               when Resolved | Failed =>
                  --  Promise has already been completed, this is an error
                  return;

               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Pending =>
                  --  OK, we can change the state
                  for Cb of D.Callbacks loop
                     Observers.IObserver_Access (Cb).On_Error (Reason);
                     Free (IFreeable_Access (Cb));
                  end loop;

                  D.Callbacks.Clear;   --  No longer needed, release them
                  D.Reason := new String'(Reason);
                  D.State := Failed;  --  Fully failed now
                  exit;
            end case;
         end loop;
      end Set_Error;

      ---------------
      -- Subscribe --
      ---------------

      overriding function Subscribe
        (Self : Promise; Cb : not null access Callback'Class)
        return Subscription
      is
         D : constant not null access Promise_Data'Class :=
            Promise_Data_Access (Promise_Pointers.Unchecked_Get (Self));

         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to Subscribe.
         C : Observers.IObserver_Access := Cb.all'Unrestricted_Access;
         Old : Promise_State;
      begin
         loop
            Old := Promise_State
               (Sync_Val_Compare_And_Swap_Counter
                  (Ptr    => D.State'Access,
                   Oldval => Pending,
                   Newval => Subscribing));
            case Old is
               when Resolving | Failing | Subscribing =>
                  --  Try again
                  null;

               when Resolved =>
                  --  We don't need to change D, so we leave the state to
                  --  Pending
                  C.On_Next (D.Value.all);
                  Free (IFreeable_Access (C));
                  exit;

               when Failed =>
                  C.On_Error (D.Reason.all);
                  Free (IFreeable_Access (C));
                  exit;

               when Pending =>
                  D.Callbacks.Append (IObserver_Access (C));
                  D.State := Pending;
                  exit;
            end case;
         end loop;

         return Empty_Subscription;
      end Subscribe;

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
         return Output_Promises.Promise
      is
         Dummy : Subscription;
      begin
         Cb.Promise := Output_Promises.Create;
         Dummy := Input_Promises.Subscribe (Input, Cb.all'Unrestricted_Access);
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
         Dummy : Subscription;
      begin
         for C of Cb.Cb2 loop
            Dummy := Input_Promises.Subscribe (Input, C);
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

      -------------
      -- On_Next --
      -------------

      overriding procedure On_Next
         (Self : in out Callback; P : Input_Promises.Result_Type) is
      begin
         On_Next (Callback'Class (Self), P, Self.Promise);
      exception
         when E : others =>
            Self.Promise.Set_Error (Exception_Message (E));
      end On_Next;

      --------------
      -- On_Error --
      --------------

      overriding procedure On_Error
         (Self : in out Callback; Reason : String) is
      begin
         --  Propagate the failure
         Self.Promise.Set_Error (Reason);
      end On_Error;

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

end Reactive.Promises;
