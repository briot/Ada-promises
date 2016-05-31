with GNAT.IO;   use GNAT.IO;
with Promises;
with Test_Promises_Support;   use Test_Promises_Support;

procedure Test_Promises is
   function Get_Promise return Int_Promises.Promise is
      P : Int_Promises.Promise;
   begin
      --  ??? Should resolve in a task for instance
      return P;
   end Get_Promise;

begin
   Put_Line ("Setting up the promise chain");

   --  ??? Unfortunately, we can't use the dot notation:
   --     When_Done and Chain need an "in out" param, so not direct
   --        result of Get_Promise
   --     Chain is not a primitive operation of promise
   --
   --  Get_Promise
   --     .Chain (new Convert_Int)
   --     .When_Done (new On_String);
   --
   --  ??? Also we are using Unrestricted_Access below, to be able
   --  to write "new ..." directly in the list of parameters.
   --
   --  ??? A promise should be a refcounted type, so that we do not
   --  make copies of it, in particular of its list of callbacks, when
   --  we return it from Chain or Get_Promise. Otherwise we are breaking
   --  the list of callbacks at some point.

   declare
      --  ??? This makes a copy, so won't have the same list of callbacks
      P : Int_Promises.Promise;
      C : access Str_Promises.Promise;
   begin
      P := Get_Promise;
      C := Int_To_Str.Chain (P, new Convert_Int);
      C.When_Done (new On_String);

      Put_Line ("Done setting up the promise chain");

      P.Resolve (2);
      Put_Line ("Done resolving P");
   end;

end Test_Promises;
