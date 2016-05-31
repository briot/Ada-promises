package body Promises is

   --------------
   -- Promises --
   --------------
   
   package body Promises is
   
      procedure Resolve (Self : in out Promise; R : T) is
      begin
         for Cb of Self.Callbacks loop
            Cb.Resolved (R);
         end loop;
         Self.Callbacks.Clear;  --  Should also free memory
      end Resolve;

      procedure When_Done
         (Self : in out Promise;
          Cb   : not null access T_Callbacks.Callback'Class) is
      begin
         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to When_Done.
         Self.Callbacks.Append (Cb.all'Unrestricted_Access);
      end When_Done;

   end Promises;

   ------------
   -- Chains --
   ------------
   
   package body Chains is
   
      function Chain
         (Self : in out T_Promises.Promise;
          Cb   : not null access Callback'Class)
         return access T2_Promises.Promise
      is
      begin
         T_Promises.When_Done (Self, Cb.all'Unrestricted_Access);
         return Cb.Promise'Unrestricted_Access;  --  will be resolved later
      end Chain;

      overriding procedure Resolved (Self : in out Callback; P : T) is
      begin
         Resolved (Callback'Class (Self), P, Self.Promise);
      end Resolved;

   end Chains;

end Promises;
