with Reactive;                  use Reactive;
with Test_Observables_Support;  use Test_Observables_Support;

procedure Test_Observables is

   use Int_Observables, Int_Int_Filters;

begin
   Run
      (Observable_Range (1, 10)
       and Take (2)
       and new Display_Int);
end Test_Observables;
