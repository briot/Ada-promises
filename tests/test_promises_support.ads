with Reactive.Promises; use Reactive.Promises;

package Test_Promises_Support is
   package Int_Promises is new Promises (Integer);
   package Float_Promises is new Promises (Float);
   package Str_Promises is new Promises (String);

   package Int_To_Float is new Chains (Int_Promises, Float_Promises);
   package Float_To_Str is new Chains (Float_Promises, Str_Promises);

   type Convert_Int is new Int_To_Float.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Float_Promises.Promise);

   type Convert_Float is new Float_To_Str.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Convert_Float;
       P      : Float;
       Output : in out Str_Promises.Promise);

   type Display_String is new Str_Promises.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Display_String;
       P      : String);
   overriding procedure On_Error
      (Self   : in out Display_String;
       Reason : String);

   type Display_Int is new Int_Promises.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Display_Int;
       P      : Integer);

   type Fail_On_Float is new Float_To_Str.Callback with null record;
   overriding procedure On_Next
      (Self   : in out Fail_On_Float;
       P      : Float;
       Output : in out Str_Promises.Promise);
   --  Always fails Output

end Test_Promises_Support;
