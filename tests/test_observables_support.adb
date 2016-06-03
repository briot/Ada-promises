with Ada.Text_IO; use Ada.Text_IO;

package body Test_Observables_Support is

   ----------------
   -- Impl_Range --
   ----------------

   type Impl_Range is new Int_Observables.IObservable with record
      First, Last : Integer;
   end record;
   overriding procedure Subscribe
      (Self : Impl_Range;
       Obs  : not null access Int_Observables.IObserver'Class);

   ---------------
   -- Impl_Take --
   ---------------

   type Impl_Take is new Int_Int_Filters.IFilter with record
      Remaining : Natural;
   end record;
   overriding procedure On_Next
      (Self : in out Impl_Take;
       From : Integer;
       To    : not null access Int_Observables.IObserver'Class);
   overriding procedure On_Error
      (Self   : in out Impl_Take;
       Reason : String;
       To     : not null access Int_Observables.IObserver'Class);
   overriding procedure On_Complete
      (Self   : in out Impl_Take;
       To     : not null access Int_Observables.IObserver'Class);

   ----------------------
   -- Observable_Range --
   ----------------------

   function Observable_Range
      (First, Last : Integer) return Int_Observables.IObservable'Class is
   begin
      --  A hot observable would start running here (like connecting to a
      --  gtk+ event callback, or a background timer for instance).
      --  A cold observable, on the other hand, will start running only
      --  when Subscribe is called, and just for that specific observer.
      return Impl_Range'(First => First, Last => Last);
   end Observable_Range;

   type Range_Subscription is new ISubscription with record
      First, Last : Integer;
      Cancelled : Boolean := False;
   end record;
   overriding procedure Run (Self : Range_Subscription);

   overriding procedure Run (Self : Range_Subscription) is
      F : Integer;
   begin
      F := Self.First;
      while not Self.Cancelled and then F < Self.Last loop
         Put_Line ("Range.Subscribe, On_Next" & F'Img);
         Obs.On_Next (F);
         F := Integer'Succ (F);
      end loop;

      if not Self.Cancelled then
         Obs.On_Complete;
      end if;

      --  ??? Should automatically unsubscribe and break the chain
   end Run;

   ---------------
   -- Subscribe --
   ---------------

   overriding function Subscribe
      (Self : Impl_Range;
       Obs  : not null access Int_Observables.IObserver'Class)
      return ISubscription'Class
   is
      F : Integer;
   begin
      --  A hot observable would add the observer to a list and do nothing
      --  else here. Whenever an external event happens, propagate it to all
      --  registered observers.
      --  A cold observable needs to run the events for this specific
      --  observer. However, it needs to return a subscription object first, so
      --  that the observer can decide to cancel the emitting of events.

      return new Range_Subscription'
         (First => Self.First, Last => Self.Last, Cancelled => False);
      --  F := Self.First;
      --  while F < Self.Last loop
      --     Put_Line ("Range.Subscribe, On_Next" & F'Img);
      --     Obs.On_Next (F);
      --     F := Integer'Succ (F);
      --  end loop;
      --  Obs.On_Complete;
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
      (Self : in out Impl_Take;
       From : Integer;
       To    : not null access Int_Observables.IObserver'Class) is
   begin
      Put_Line
         ("Take.On_Next" & From'Img & " remaining=" & Self.Remaining'Img);
      if Self.Remaining > 0 then
         To.On_Next (From);
         Self.Remaining := Self.Remaining - 1;

         --  ??? If we reach 0, we could emit On_Complete immediately,
         --  and clear memory
      end if;
   end On_Next;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
      (Self   : in out Impl_Take;
       Reason : String;
       To     : not null access Int_Observables.IObserver'Class)
   is
      pragma Unreferenced (Self);
   begin
      To.On_Error (Reason);
   end On_Error;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete
      (Self   : in out Impl_Take;
       To     : not null access Int_Observables.IObserver'Class)
   is
      pragma Unreferenced (Self);
   begin
      --  ??? Unless already emitted by On_Next
      To.On_Complete;
   end On_Complete;

   ----------
   -- Take --
   ----------

   function Take
      (Count : Natural) return not null access Int_Int_Filters.IFilter'Class is
   begin
      return new Impl_Take'(Remaining => Count);
   end Take;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
      (Self : in out Display_Int; Value : Integer)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_Int.On_Next ->" & Value'Img);
   end On_Next;

end Test_Observables_Support;
