with GNAT.IO;   use GNAT.IO;
with Promises;
with Test_Promises_Support;   use Test_Promises_Support;

procedure Test_Promises is
   function Get_Promise return Int_Promises.Promise is
      P : Int_Promises.Promise := Int_Promises.Create;
   begin
      --  ??? Could resolve in a task for instance
      return P;
   end Get_Promise;

   P : Int_Promises.Promise;

begin
   Put_Line ("Setting up the promise chain");
   P := Get_Promise;
   Int_To_Str
      .When_Done (P, new Convert_Int)
      .When_Done (new On_String);

   Put_Line ("Done setting up the promise chain");

   P.Resolve (2);
   Put_Line ("Done resolving P");

end Test_Promises;
