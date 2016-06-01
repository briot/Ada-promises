with Promises; use Promises;

package Test_Promises_Support is
   package Int_Promises is new Promises.Promises (Integer);
   package Float_Promises is new Promises.Promises (Float);
   package Str_Promises is new Promises.Promises (String);

   package Int_To_Float is new Chains (Int_Promises, Float_Promises);
   package Float_To_Str is new Chains (Float_Promises, Str_Promises);

   type Convert_Int is new Int_To_Float.Callback with null record;
   procedure Resolved
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise);

   type Convert_Float is new Float_To_Str.Callback with null record;
   procedure Resolved
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise);

   type Display_String is new Str_Promises.Callback with null record;
   procedure Resolved
      (Self   : in out Display_String;
       P      : String);

end Test_Promises_Support;
