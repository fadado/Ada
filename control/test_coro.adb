------------------------------------------------------------------------------
--  Examples using Coroutines
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO; use Ada.Text_IO;

with Control.CoRoutines;
use Control;

with Gotcha;

procedure test_coro
is
begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Hello world example
   ---------------------------------------------------------------------------

   Test_1:
   declare
      package R is new CoRoutines (Context_Type => VOID);
      use R;

      procedure subgen(routine: not null COROUTINE_ACCESS) is
      begin
         Put("He");
         routine.Yield;
         Put("llo");
         raise Stop_Iteration;
      end subgen;

      procedure hello_world(routine: not null COROUTINE_ACCESS) is
      begin
         Put("Test 1-");
         routine.Yield;
         begin subgen(routine);
         exception
            when Stop_Iteration => null;
            when others => raise;
         end;
         Put(", world");
         routine.Yield;
         Put_Line("!");
      end;

      hello : COROUTINE_TYPE (hello_world'Access, NULL);

   begin
      while hello.Call loop null; end loop;
      pragma Assert(not hello.Call);
   end Test_1;

   New_Line;

exception
   when X : others =>
      Put_Line("Top-Level exception");
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_coro;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
