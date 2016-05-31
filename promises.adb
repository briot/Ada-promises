package body Promises is

   --------------
   -- Promises --
   --------------
   
   package body Promises is
   
      -------------
      -- Resolve --
      -------------

      procedure Resolve (Self : in out Promise; R : T) is
      begin
         for Cb of Self.Callbacks loop
            Cb.Resolved (R);
         end loop;
         Self.Callbacks.Clear;  --  Should also free memory
      end Resolve;

      ---------------
      -- When_Done --
      ---------------

      procedure When_Done
         (Self : in out Promise;
          Cb   : not null access Callback'Class) is
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
   
      ---------------
      -- When_Done --
      ---------------

      function When_Done
         (Self : in out Input_Promises.Promise;
          Cb   : not null access Callback'Class)
         return access Output_Promises.Promise is
      begin
         Input_Promises.When_Done (Self, Cb.all'Unrestricted_Access);
         return Cb.Promise'Unrestricted_Access;  --  will be resolved later
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
