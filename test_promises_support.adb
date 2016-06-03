with GNAT.IO;   use GNAT.IO;

package body Test_Promises_Support is

   overriding procedure On_Next
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Int.On_Next  input=" & P'Img);
      Output.Set_Value (Float (P));
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Convert_Float.On_Next  input=" & P'Img);
      Output.Set_Value ("value was" & P'Img);
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Display_Int;
       P      : Integer)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_Int.On_Next  input=" & P'Img);
   end On_Next;

   overriding procedure On_Next
      (Self   : in out Display_String;
       P      : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.On_Next  input=" & P);
   end On_Next;

   overriding procedure On_Error
      (Self   : in out Display_String;
       Reason : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Display_String.Failed because " & Reason);
   end On_Error;

   overriding procedure On_Next
      (Self   : in out Fail_On_Float;
       P      : Float;
       Output : in out Str_Promises.Promise)
   is
      pragma Unreferenced (Self, P);
   begin
      Put_Line ("Fail_On_Float: mark output as failed");
      Output.Set_Error ("explicit");
   end On_Next;

end Test_Promises_Support;
