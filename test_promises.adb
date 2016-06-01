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
   Put_Line ("Setting up the promise chain");
   P := Get_Promise;

   --  Current syntax is not so good: although the various
   --  callbacks are listed in the order they will be executed, we
   --  still need to mention packages like Float_To_Str before
   --  earlier packages in the chain, like Int_To_Float.

   Float_To_Str.When_Done
      (Int_To_Float.When_Done (P, new Convert_Int),
       new Convert_Float)
      .When_Done (new Display_String);

   --  Idea 1
   --  ------
   --  If we use use-clauses, this helps a bit, although it hides some
   --  of the details
   --
   --      use Int_To_Float, Float_To_Str;
   --      When_Done
   --         (When_Done (P, new Convert_Int),
   --          new Convert_Float)
   --         .When_Done (new Display_String);
   --
   --  If we do not take advantage of the dot notation for the last one,
   --  things are more symmetrical:
   --
   --      use Int_To_Float, Float_To_Str, Str_Promises;
   --      When_Done
   --         (When_Done
   --            (When_Done (P, new Convert_Int),
   --             new Convert_Float),
   --         new Display_String);

   --  Idea 2
   --  ------
   --  Ideally, we want something similar to the following.
   --  This is doable in C++ because a template still defines a method
   --  that can be called with '.' (although not virtual of course). In
   --  Ada, this is not the case, so we can't use the dot notation
   --
   --     P.When_Done (new Convert_Int)
   --      .When_Done (new Convert_Float)
   --      .When_Done (new Display_String);

   --  Idea 3
   --  ------
   --  With casts, we could have a general When_Done primitive op that
   --  returns a Base_Promise'Class (an interface), which we cast to
   --  the appropriate type. But this requires run time checks, and if
   --  we chain more than 3 promises, we'll end up with the same problem
   --  where Float_To_Str appears before Int_To_Float.
   --
   --     Float_Promises.Promise (P.When_Done (new Convert_Int))
   --        .When_Done (new Convert_Float)
   --        .When_Done (new On_String);

   --  Idea 4
   --  ------
   --  Can we redefine some operators ("and" for instance) to get a nice
   --  syntax:

   Ignore (P and new Convert_Int
             and new Convert_Float
             and new Display_String);

   Put_Line ("Done setting up the promise chain");

   P.Resolve (2);
   Put_Line ("Done resolving P");

end Test_Promises;
