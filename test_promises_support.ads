with Promises;

package Test_Promises_Support is
   package Int_Promises is new Promises.Promises (Integer);
   package Str_Promises is new Promises.Promises (String);
   package Int_To_Str is new Promises.Chains 
      (Integer, String, Int_Promises, Str_Promises);

   type Convert_Int is new Int_To_Str.Callback with null record;
   procedure Resolved
      (Self   : in out Convert_Int;
       P      : Integer;
       Output : in out Str_Promises.Promise);

   type On_String is new Str_Promises.Callback with null record;
   procedure Resolved
      (Self   : in out On_String;
       P      : String);

end Test_Promises_Support;
