with Reactive;    use Reactive;

package Test_Observables_Support is

   package Int_Observers is new Typed_Observers (Integer);
   package Int_Observables is new Typed_Observables (Int_Observers);
   package Int_Int_Filters is new Filters (Int_Observables, Int_Observables);

   -----------
   -- Range --
   -----------

   function Observable_Range
      (First, Last : Integer) return Int_Observables.IObservable'Class;
   --  Create a new observable that always sends the sequence First..Last
   --  to all observables that subscribe to it, no matter how late they
   --  subscribe.

   ----------
   -- Take --
   ----------

   function Take
      (Count : Natural) return not null access Int_Int_Filters.IFilter'Class;
   --  Forward the first Count elements from the input stream to the output
   --  stream. Other elements are discarded.
   --  On_Error and n_Complete are propagated as is.

   -----------------
   -- Display_Int --
   -----------------

   type Display_Int is new Int_Observers.IObserver with null record;
   overriding procedure On_Next (Self : in out Display_Int; Value : Integer);
   --  Display an integer on the console

end Test_Observables_Support;
