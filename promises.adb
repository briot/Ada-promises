package body Promises is

   --------------
   -- Promises --
   --------------
   
   package body Promises is
   
      ------------
      -- Create --
      ------------

      function Create return Promise is
      begin
         return P : Promise do
            P.Set (Data => (Callbacks => Cb_Vectors.Empty_Vector));
         end return;
      end Create;

      ----------------
      -- Is_Created --
      ----------------

      function Is_Created (Self : Promise) return Boolean is
      begin
         return not Self.Is_Null;
      end Is_Created;
  
      -------------
      -- Resolve --
      -------------

      procedure Resolve (Self : in out Promise; R : T) is
      begin
         for Cb of Self.Get.Callbacks loop
            Cb.Resolved (R);
         end loop;
         Self.Get.Callbacks.Clear;  --  Should also free memory
      end Resolve;

      ---------------
      -- When_Done --
      ---------------

      procedure When_Done
         (Self : Promise;
          Cb   : not null access Callback'Class) is
      begin
         --  ??? Unrestricted_Access is temporary, so that user can
         --  use "new Cb" directly in the call to When_Done.
         Self.Get.Callbacks.Append (Cb.all'Unrestricted_Access);
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
         return access Output_Promises.Promise is
      begin
         Cb.Promise := Output_Promises.Create;
         Input_Promises.When_Done (Self, Cb.all'Unrestricted_Access);
         return Cb.Promise'Unrestricted_Access;
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
