with GNAT.IO;   use GNAT.IO;

package body Test_Promises_Support is

   overriding procedure Resolved
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Int.Resolved  input=" & P'Img);
      Output.Resolve (Float (P));
   end Resolved;

   overriding procedure Resolved
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Float.Resolved  input=" & P'Img);
      Output.Resolve ("value was" & P'Img);
   end Resolved;

   overriding procedure Resolved
      (Self   : in out Display_String;
       P      : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.Resolved  input=" & P);
   end Resolved;

   overriding procedure Failed
      (Self   : in out Display_String;
       Reason : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.Failed because " & Reason);
   end Failed;

end Test_Promises_Support;
