with Ada.Unchecked_Deallocation;

package body Reactive is

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Freeable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (IFreeable'Class, Freeable_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

end Reactive;
