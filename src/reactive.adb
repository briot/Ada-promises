with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;   use Ada.Text_IO;
with System;        use System;

package body Reactive is

   package Observer_Vectors is new Ada.Containers.Vectors
      (Positive, IObserver_Access);
   package Subscription_Vectors is new Ada.Containers.Vectors
      (Positive, Subscription);

   ------------------------
   -- Subscription_Group --
   ------------------------

   type Subscription_Group is new ISubscription_Data with record
      List : Subscription_Vectors.Vector;
   end record;
   type Subscription_Group_Access is access all Subscription_Group'Class;
   overriding procedure Run (Self : not null access Subscription_Group);
   overriding procedure Cancel (Self : not null access Subscription_Group);
   --  A subscription that forwards its events to multiple other subscriptions

   -------------------------------
   -- Observable_Data_With_List --
   -------------------------------

   type Observable_Data_With_List is new Observable_Data with record
      Observers : Observer_Vectors.Vector;
   end record;
   type Observable_Data_With_List_Access is
      access all Observable_Data_With_List'Class;
   --  The base type for all observables that store a list of observers.
   --  It is defined outside of the Typed_Observables_With_Observer_List
   --  generic package so that its code doesn't need to be duplicated for
   --  each instance of the package.

   function Subscribe
      (Self : not null access Observable_Data_With_List'Class;
       Obs  : not null access IObserver'Class)
      return Subscription;
   --  Internal implementation of Subscribe for observables with lists of
   --  observers.

   procedure Call_On_Error
      (Self : not null access Observable_Data_With_List'Class;
       Reason : String);
   procedure Call_On_Complete
      (Self : not null access Observable_Data_With_List'Class);
   --  Forward events to all observers

   --------------------------
   -- Subscription_In_List --
   --------------------------

   type Subscription_In_List is new ISubscription_Data with record
      Data   : access Observable_Data_With_List'Class;  --  UNSAFE ???
      --  the subscription might outlive the observable that contains Data,
      --  and we would be referencing freed data. Can we use a weak
      --  pointer here ?
         
      Obs    : System.Address;
      Cancelled : Boolean := False;
   end record;
   type Subscription_In_List_Access is access all Subscription_In_List'Class;
   overriding procedure Run (Self : not null access Subscription_In_List);
   overriding procedure Cancel (Self : not null access Subscription_In_List);
   --  A subscription that describes a specific observer in a list of
   --  observers, so that we can disconnect it later.
   --  The observer is purposefully represented as a System.Address, since
   --  it might be a pointer to data that has already been deallocated. Only
   --  the actual pointer is relevant so that we can find it in the list
   --  easily (it is possible, though very unlikely, that the original
   --  observer has been deallocated already, and a new observer allocated
   --  at the same address and subscribed to the same observable).

   function Create_Subscription_In_List
      (Self : not null access Observable_Data_With_List'Class;
       Obs  : not null access IObserver'Class) return Subscription;
   --  Return a new subscription

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : not null access Subscription_In_List) is
   begin
   end Run;

   ------------
   -- Cancel --
   ------------

   overriding procedure Cancel (Self : not null access Subscription_In_List) is
      use Observer_Vectors;
      C : Observer_Vectors.Cursor := Self.Data.Observers.First;
   begin
      Put_Line ("Cancel Subscription in list");
      while Has_Element (C) loop
         if Element (C).all'Address = Self.Obs then
            Self.Data.Observers.Delete (C);
            exit;
         end if;
         Next (C);
      end loop;

      Self.Obs  := System.Null_Address;
      Self.Data := null;
      Self.Cancelled := True;
   end Cancel;

   ---------------------------------
   -- Create_Subscription_In_List --
   ---------------------------------

   function Create_Subscription_In_List
      (Self : not null access Observable_Data_With_List'Class;
       Obs  : not null access IObserver'Class) return Subscription is
   begin
      return S : Subscription do
         S.Set (Subscription_In_List'
            (Data      => Self,
             Obs       => Obs.all'Address,
             Cancelled => False));
      end return;
   end Create_Subscription_In_List;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe
      (Self : not null access Observable_Data_With_List'Class;
       Obs  : not null access IObserver'Class)
      return Subscription
   is
   begin
      Self.Observers.Append (IObserver_Access (Obs));
      return Create_Subscription_In_List (Self, Obs);
   end Subscribe;

   -------------------
   -- Call_On_Error --
   -------------------

   procedure Call_On_Error
      (Self : not null access Observable_Data_With_List'Class;
       Reason : String) is
   begin
      for Obs of Self.Observers loop
         Obs.On_Error (Reason);
      end loop;
   end Call_On_Error;

   ----------------------
   -- Call_On_Complete --
   ----------------------

   procedure Call_On_Complete
      (Self : not null access Observable_Data_With_List'Class) is
   begin
      for Obs of Self.Observers loop
         Obs.On_Complete;
      end loop;
   end Call_On_Complete;

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : not null access Subscription_Group) is
   begin
      for S of Self.List loop
         S.Run;
      end loop;
   end Run;

   ------------
   -- Cancel --
   ------------

   overriding procedure Cancel (Self : not null access Subscription_Group) is
   begin
      for S of Self.List loop
         S.Cancel;
      end loop;
   end Cancel;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out IFreeable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (IFreeable'Class, IFreeable_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   -------------------
   -- Dispatch_Free --
   -------------------

   procedure Dispatch_Free (Self : in out Observable_Data'Class) is
   begin
      Free (IFreeable'Class (Self));
   end Dispatch_Free;

   -------------------
   -- Dispatch_Free --
   -------------------

   procedure Dispatch_Free (Self : in out ISubscription_Data'Class) is
   begin
      Free (IFreeable'Class (Self));
   end Dispatch_Free;

   ---------
   -- Run --
   ---------

   procedure Run (Self : Subscription'Class) is
   begin
      if not Self.Is_Null then
         ISubscription_Data'Class (Self.Get.Element.all).Run;
      end if;
   end Run;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Self : Subscription'Class) is
   begin
      if not Self.Is_Null then
         ISubscription_Data'Class (Self.Get.Element.all).Cancel;
      end if;
   end Cancel;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
      (Self : in out Subscription; Data : ISubscription_Data'Class) is
   begin
      Subscription_Pointers.Set (Self, Data);
   end Set_Data;

   ---------------------
   -- Typed_Observers --
   ---------------------

   package body Typed_Observers is

      ---------
      -- "&" --
      ---------

      function "&"
        (Cb    : not null access IObserver'Class;
         Cb2   : not null access IObserver'Class)
        return List is
      begin
         return (Cb.all'Unrestricted_Access,
                 Cb2.all'Unrestricted_Access);
      end "&";

      ---------
      -- "&" --
      ---------

      function "&"
        (Self  : List;
         Cb2   : not null access IObserver'Class)
        return List is
      begin
         return Self & (1 => Cb2.all'Unrestricted_Access);
      end "&";

   end Typed_Observers;

   -----------------------
   -- Typed_Observables --
   -----------------------

   package body Typed_Observables is

      -----------
      -- "and" --
      -----------

      function "and"
         (Self : IObservable'Class;
          Obs  : not null access Observers.IObserver'Class)
         return Subscription is
      begin
         Put_Line ("Typed_Observables.""and""");
         return Self.Subscribe (Obs);
      end "and";

      -----------
      -- "and" --
      -----------

      function "and"
         (Self : IObservable'Class; Obs  : Observers.List)
         return Subscription
      is
         G : Subscription_Group_Access;
      begin
         Put_Line ("Typed_Observables.""and"" (list)");
         return S : Subscription do
            S.Set (Subscription_Group'(others => <>));
            G := Subscription_Group_Access (S.Unchecked_Get);
            for Ob of Obs loop
               G.List.Append (Self.Subscribe (Ob));
            end loop;
         end return;
      end "and";

   end Typed_Observables;

   ------------------------------------------
   -- Typed_Observables_With_Observer_List --
   ------------------------------------------

   package body Typed_Observables_With_Observer_List is
      subtype Data is Observable_Data_With_List;
      subtype Data_Access is Observable_Data_With_List_Access;

      ------------
      -- Create --
      ------------

      procedure Create (Self : out Observable'Class) is
      begin
         Self.Data.Set
            (Data => Observable_Data_With_List'
                (Observers => Observer_Vectors.Empty_Vector));
      end Create;

      ----------------
      -- Is_Created --
      ----------------

      function Is_Created (Self : Observable'Class) return Boolean is
      begin
         return not Self.Data.Is_Null;
      end Is_Created;

      ---------------
      -- Subscribe --
      ---------------

      overriding function Subscribe
         (Self   : Observable;
          Obs    : not null access Observables.IObserver'Class)
         return Subscription is
      begin
         return Subscribe
            (Data_Access (Observable_Pointers.Unchecked_Get (Self.Data)), Obs);
      end Subscribe;

      ------------------
      -- Call_On_Next --
      ------------------

      procedure Call_On_Next
         (Self : Observable'Class; T : Observables.Result_Type)
      is
         D : constant Data_Access :=
            Data_Access (Observable_Pointers.Unchecked_Get (Self.Data));
      begin
         for Obs of D.Observers loop
            Observables.IObserver'Class (Obs.all).On_Next (T);
         end loop;
      end Call_On_Next;

      -------------------
      -- Call_On_Error --
      -------------------

      procedure Call_On_Error (Self : Observable'Class; Reason : String) is
      begin
         Call_On_Error
            (Data_Access (Observable_Pointers.Unchecked_Get (Self.Data)),
             Reason);
      end Call_On_Error;

      ----------------------
      -- Call_On_Complete --
      ----------------------

      procedure Call_On_Complete (Self : Observable'Class) is
      begin
         Call_On_Complete
            (Data_Access (Observable_Pointers.Unchecked_Get (Self.Data)));
      end Call_On_Complete;

   end Typed_Observables_With_Observer_List;

   -------------
   -- Filters --
   -------------

   package body Filters is

      type Typed_Data is new Observable_Data_With_List with record
         Filter    : IFilter_Access;
         Upstream  : Subscription;   --  subscription to input observable
      end record;
      type Typed_Data_Access is access all Typed_Data'Class;

      type Filter_Wrapper is
         new Output.IObservable
         and Input.Observers.IObserver
      with record
         Data       : Observable_Pointers.Ref;
      end record;
      overriding procedure On_Next
         (Self   : in out Filter_Wrapper;
          From   : Input.Result_Type);
      overriding procedure On_Error
         (Self   : in out Filter_Wrapper;
          Reason : String);
      overriding procedure On_Complete (Self : in out Filter_Wrapper);
      overriding function Subscribe
         (Self   : Filter_Wrapper;
          Obs    : not null access Output.IObserver'Class)
         return Subscription;

      -------------
      -- On_Next --
      -------------

      overriding procedure On_Next
         (Self   : in out Filter_Wrapper;
          From   : Input.Result_Type)
      is
         D : constant not null access Typed_Data'Class :=
            Typed_Data_Access
               (Observable_Pointers.Unchecked_Get (Self.Data));
      begin
         Put_Line ("MANU Filter_Wrapper.On_Next");
         if D.Filter /= null then
            for Obs of D.Observers loop
               D.Filter.On_Next
                  (From, Output.IObserver'Class (Obs.all)'Access);
            end loop;
         end if;
      end On_Next;

      --------------
      -- On_Error --
      --------------

      overriding procedure On_Error
         (Self   : in out Filter_Wrapper;
          Reason : String)
      is
         D : constant not null access Typed_Data'Class :=
            Typed_Data_Access
               (Observable_Pointers.Unchecked_Get (Self.Data));
      begin
         if D.Filter /= null then
            for Obs of D.Observers loop
               D.Filter.On_Error
                  (Reason, Output.IObserver'Class (Obs.all)'Access);
            end loop;
            D.Upstream.Run;  --  Stop running upstream if necessary
         end if;
      end On_Error;

      -----------------
      -- On_Complete --
      -----------------

      overriding procedure On_Complete (Self : in out Filter_Wrapper) is
         D : constant not null access Typed_Data'Class :=
            Typed_Data_Access
               (Observable_Pointers.Unchecked_Get (Self.Data));
      begin
         if D.Filter /= null then
            for Obs of D.Observers loop
               D.Filter.On_Complete (Output.IObserver'Class (Obs.all)'Access);
            end loop;
            D.Observers.Clear;
            Free (IFreeable_Access (D.Filter));

            D.Upstream.Run;  --  Stop running upstream if necessary
         end if;
      end On_Complete;

      ---------------
      -- Subscribe --
      ---------------

      overriding function Subscribe
         (Self   : Filter_Wrapper;
          Obs    : not null access Output.IObserver'Class)
         return Subscription is
      begin
         Put_Line ("MANU Filter_Wrapper.Subscribe");
         return Subscribe
            (Typed_Data_Access (Observable_Pointers.Unchecked_Get (Self.Data)),
             Obs);
      end Subscribe;

      ---------
      -- Run --
      ---------

      procedure Run (Self : Subscription) is
      begin
         --  Propagate upstream

         Self.Wrapper.Get.Upstream.Run;
      end Run;

      -----------
      -- "and" --
      -----------

      function "and"
         (From   : Input.IObservable'Class;
          Filter : not null access IFilter'Class)
         return Output.IObservable'Class
      is
         --  W is freed automatically by From, when it disconnects. However,
         --  the pointed data is refcounted, so will keep existing if there
         --  are references to it.
         W : constant access Filter_Wrapper := new Filter_Wrapper;
      begin
         Put_Line ("Filters.""and""");
         W.Data.Set
            (Data => Typed_Data'
                (Observers => Observer_Vectors.Empty_Vector,
                 Upstream  => <>,
                 Filter    => Filter));    --  Take ownership
         Typed_Data_Access
            (Observable_Pointers.Unchecked_Get (W.Data)).Upstream :=
               From.Subscribe (W);
         return W.all;
      end "and";

   end Filters;

end Reactive;
