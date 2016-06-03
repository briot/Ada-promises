--  Various packages to support reactive programming.
--  See http://reactivex.io
--
--  These packages provide a better approach to the classical
--  Observer design pattern.
--  They also encompass the notion of promises that is used to
--  synchronize between asynchronous tasks.

with GNATCOLL.Refcount;

package Reactive is

   ---------------
   -- IFreeable --
   ---------------

   type IFreeable is interface;
   type IFreeable_Access is access all IFreeable'Class;
   --  a general interface for objects that have an explicit Free
   --  primitive operation.

   procedure Free (Self : in out IFreeable) is null;
   --  Free internal data of Self

   procedure Free (Self : in out IFreeable_Access);
   --  Free self, via its primitive operation, and then free the pointer

   ---------------
   -- Observers --
   ---------------

   type IObserver is interface and IFreeable;
   type IObserver_Access is access all IObserver'Class;
   --  An observer is an object that monitors events on an observable.
   --  It has a number of primitive operations, one of which (On_Next) is
   --  typed and thus is defined in subclasses.

   procedure On_Error (Self : in out IObserver; Reason : String) is null;
   --  Called when an observable has ended up with an error.
   --  The observable will no longer send data after this.

   procedure On_Complete (Self : in out IObserver) is null;
   --  Called when an observable has finished successfully and will no longer
   --  be sending data.

   ---------------------
   -- Typed observers --
   ---------------------

   generic
      type T (<>) is private;
   package Typed_Observers is
      subtype Result_Type is T;

      type IObserver is interface and Reactive.IObserver;
      type IObserver_Access is access all IObserver'Class;

      procedure On_Next (Self : in out IObserver; R : Result_Type) is null;
      --  Executed when an observable emits an event.
      --  It receives the value of that event.

      type List is
         array (Natural range <>) of not null access IObserver'Class;
      --  Multiple observers, all subscribed to the same observable (or
      --  will be subscribed to the same observable).

      function "&"
        (Cb    : not null access IObserver'Class;
         Cb2   : not null access IObserver'Class) return List;
      function "&"
        (Self  : List;
         Cb2   : not null access IObserver'Class) return List;
      --  Create a list of observers that will all be subscribed to the same
      --  observable.
      --  All observers must be newly allocated, and cannot be stored in
      --  multiple lists or subscribed to multiple observables.

   end Typed_Observers;

   -----------------
   -- Observables --
   -----------------

   type IObservable is interface and IFreeable;

   ------------------
   -- Subscription --
   ------------------

   type Subscription is tagged private;
   --  This type represents a link between an observable and an observer (or
   --  multiple observers, as the case may be).
   --  It is used to start running a chaing of observers and filters, or to
   --  cancel a running chain.
   --  It is a reference counted type, which manages its own lifetime and
   --  memory allocation.

   procedure Run (Self : Subscription'Class);
   --  Request that Self start emitting values.
   --  This will in general be called once all filters and observers have been
   --  put in place, so that events are not lost.
   --  Some observables, like one reporting mouse events for instance, always
   --  emit events, even before Run is called.

   procedure Cancel (Self : Subscription'Class);
   --  Cancel a subscription (i.e. unsubscribe and possibly ask the observable
   --  to stop emitting events).
   --  When this is called, On_Complete is never called on the observer.

   Empty_Subscription : constant Subscription;
   --  A value that can be returned by any Subscribe function, when the
   --  observable runs automatically as soon as it is created or a
   --  subscription is made on it, and it is not cancellable.

   ------------------
   -- Subscription --
   ------------------

   type ISubscription_Data is interface and IFreeable;

   type Subscription_Data_Ref (Element : access ISubscription_Data'Class)
      is limited null record
      with Implicit_Dereference => Element;

   procedure Run (Self : not null access ISubscription_Data) is abstract;
   procedure Cancel (Self : not null access ISubscription_Data) is null;
   --  See the matching subprograms on the Subscription itself.

   procedure Set_Data
      (Self : in out Subscription; Data : ISubscription_Data'Class)
      with Inline;
   --  Stores a copy of Data in the subscription.
   --  This copy will be freed as needed when Self is no longer needed

   -----------------------
   -- Typed observables --
   -----------------------

   generic
      with package Observers is new Typed_Observers (<>);
   package Typed_Observables is
      subtype Result_Type is Observers.Result_Type;
      subtype IObserver is Observers.IObserver;
      subtype IObserver_Access is Observers.IObserver_Access;

      type IObservable is interface and Reactive.IObservable;
      type IObservable_Access is access all IObservable'Class;

      function Subscribe
        (Self : IObservable;
         Obs  : not null access Observers.IObserver'Class)
        return Subscription
        is abstract;
      --  Make sure that all events from Self result in executing one of
      --  the primitive operations of Obs.
      --  Obs must be allocated specifically for this call, and will be
      --  freed as needed. You must not reuse the same pointer for multiple
      --  calls to Subscribe, even on other observables.
      --  ??? This is unsafe since we cannot check that.
      --
      --  Any number of observers can be set on Self.
      --
      --  Conceptually, Self is modified. However, we did not use "in out"
      --  here assuming that Self is implemented as a smart pointer, since
      --  in general it has a long lifespan. This allows calling Subscribe
      --  directly on the result of a function call, and the use of the
      --  "and" operator to chain them.

      function "and"
         (Self : IObservable'Class;
          Obs  : not null access Observers.IObserver'Class)
         return Subscription;
      function "and"
         (Self : IObservable'Class; Obs  : Observers.List)
         return Subscription;
      --  An operator that does the same thing as Subscribe, but is more
      --  convenient to use when chaining observables and filters.
      --  When adding multiple observers, the returned subscription let you
      --  unsubscribe all observers at once.

   end Typed_Observables;

   ----------------------------------------
   -- Observables with list of observers --
   ----------------------------------------

   type Observable_Data is abstract new IFreeable with null record;
   procedure Dispatch_Free (Self : in out Observable_Data'Class);
   package Observable_Pointers is new GNATCOLL.Refcount.Shared_Pointers
      (Element_Type           => Observable_Data'Class,
       Release                => Dispatch_Free,
       Atomic_Counters        => True,   --  thread-safe
       Potentially_Controlled => True);  --  a vector is controlled
   --  Implementation details (to share the instance of shared pointers among
   --  all observables)

   generic
      with package Observables is new Typed_Observables (<>);
   package Typed_Observables_With_Observer_List is
      type Observable is new Observables.IObservable with private;
      --  A concrete implementation of observable, which stores all its
      --  subscribed observers in a list and forward the events to all of
      --  them as needed.
      --  Not all observables need to store such a list, since some can just
      --  immediately send all events to them when they subscribe. But for
      --  real asynchronous observables, they need to store such a list.
      --
      --  These observables are reference counted and thread safe

      procedure Create (Self : out Observable'Class)
         with Post => Self.Is_Created;
      --  Create a new empty observable

      function Is_Created (Self : Observable'Class) return Boolean
         with Inline_Always;
      --  Whether Self was created with a call to Create

      overriding function Subscribe
         (Self   : Observable;
          Obs    : not null access Observables.IObserver'Class)
         return Subscription
         with Pre => Self.Is_Created;

      procedure Call_On_Next
         (Self : Observable'Class; T : Observables.Result_Type)
         with Pre => Self.Is_Created;
      procedure Call_On_Error
         (Self : Observable'Class; Reason : String)
         with Pre => Self.Is_Created;
      procedure Call_On_Complete
         (Self : Observable'Class)
         with Pre => Self.Is_Created;
      --  Calls all registered observers' primitive operations.
      --  This is thread safe.

   private
      type Observable is new Observables.IObservable with record
         Data : Observable_Pointers.Ref;
      end record;

   end Typed_Observables_With_Observer_List;

   -------------
   -- Filters --
   -------------
   --  Filter play the role of both an observer on an input observable, and of
   --  observables for further filters or observers down the line.
   --  As such, they are implemented as observers, but with a slightly
   --  different profile for their On_Next operation.

   generic
      with package Input is new Typed_Observables (<>);
      with package Output is new Typed_Observables (<>);
   package Filters is

      type IFilter is interface and IFreeable;
      type IFilter_Access is access all IFilter'Class;

      procedure On_Next
         (Self   : in out IFilter;
          From   : Input.Result_Type;
          To     : not null access Output.IObserver'Class) is null;
      procedure On_Error
         (Self   : in out IFilter;
          Reason : String;
          To     : not null access Output.IObserver'Class) is null;
      procedure On_Complete
         (Self   : in out IFilter;
          To     : not null access Output.IObserver'Class) is null;
      --  Called upon receiving an even on From.

      function "and"
         (From   : Input.IObservable'Class;
          Filter : not null access IFilter'Class)
         return Output.IObservable'Class;
      --  Chain the filter between two observables.

   end Filters;

private
   procedure Dispatch_Free (Self : in out ISubscription_Data'Class);
   package Subscription_Pointers is new GNATCOLL.Refcount.Shared_Pointers
      (Element_Type           => ISubscription_Data'Class,
       Release                => Dispatch_Free,
       Atomic_Counters        => True,   --  thread-safe
       Potentially_Controlled => True);

   type Subscription is new Subscription_Pointers.Ref with null record;
   Empty_Subscription : constant Subscription :=
      (Subscription_Pointers.Null_Ref with null record);

end Reactive;
