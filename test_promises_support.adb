with GNAT.IO;   use GNAT.IO;
with Promises;

package body Test_Promises_Support is

   procedure Resolved
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise) is
   begin
      Put_Line ("Convert_Int.Resolved  input=" & P'Img);
      Output.Resolve (Float (P));
   end Resolved;

   procedure Resolved
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise) is
   begin
      Put_Line ("Convert_Float.Resolved  input=" & P'Img);
      Output.Resolve ("value was" & P'Img);
   end Resolved;

   procedure Resolved
      (Self   : in out Display_String;
       P      : String) is
   begin
      Put_Line ("Display_String.Resolved  input=" & P);
   end Resolved;

end Test_Promises_Support;
