with GNAT.IO;   use GNAT.IO;
with Promises;

package body Test_Promises_Support is

   procedure Resolved
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Str_Promises.Promise)
   is
   begin
      Put_Line ("Convert_Int.Resolved  input=" & P'Img);
      Output.Resolve ("some string" & P'Img);
   end Resolved;

   procedure Resolved
      (Self   : in out On_String;
       P      : String)
   is
   begin
      Put_Line ("On_String.Resolved  input=" & P);
   end Resolved;

end Test_Promises_Support;
