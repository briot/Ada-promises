with GNAT.IO;   use GNAT.IO;
with Promises;  use Promises;
with Test_Promises_Support;   use Test_Promises_Support;

procedure Test_Promises is
   use Int_To_Float, Float_To_Str, Str_Promises;

   function Get_Promise return Int_Promises.Promise;
   --  Dummy function

   function Get_Promise return Int_Promises.Promise is
      P : constant Int_Promises.Promise := Int_Promises.Create;
   begin
      --  ??? Could resolve in a task for instance
      return P;
   end Get_Promise;

   P : Int_Promises.Promise;

begin
   Put_Line ("=== Create chain");
   P := Get_Promise;
   Subscribe (P and (new Convert_Int & new Display_Int)
                and new Convert_Float
                and (new Display_String & new Display_String));

   Put_Line ("Resolving...");
   P.Set_Value (2);

   Put_Line ("=== Create chain, will Fail");
   P := Get_Promise;
   Subscribe (P and new Convert_Int
                and new Convert_Float
                and new Display_String);
   Put_Line ("Failing...");
   P.Set_Error ("Explicit failure");

   Put_Line ("=== Create chain, will Fail in middle");
   P := Get_Promise;
   Subscribe (P and new Convert_Int
                and new Fail_On_Float
                and (new Display_String & new Display_String));
   P.Set_Value (3);

end Test_Promises;
