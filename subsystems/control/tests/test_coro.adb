------------------------------------------------------------------------------
--  Examples using Coroutines
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Text_IO;
use  Ada.Text_IO;

with Control.CoRoutines.Semi;

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
      use Control;

      package Semi is new CoRoutines.Semi (Context_Type => VOID);

      procedure stackfull
        (routine : in out CoRoutines.COROUTINE_INTERFACE'Class) is
      begin
         Put("He");
         routine.Yield;
         Put("llo");
      end stackfull;

      procedure hello_world
        (routine : in out CoRoutines.COROUTINE_INTERFACE'Class;
         context : access VOID) is
      begin
         Put("Test 1-");
         routine.Yield;
         stackfull(routine);
         Put(", world");
         routine.Yield;
         Put_Line("!");
      end;

      dispatcher : DISPATCHER_TYPE;
      hello      : Semi.COROUTINE_TYPE (hello_world'Access, NULL);

   begin
      loop
         hello.Resume(dispatcher);
      end loop;
   exception
      when Stop_Iteration => null; -- ignore
      when others => hello.Close; raise;
   end Test_1;

   New_Line;

exception
   when X : others =>
      Put_Line("Top-Level exception");
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_coro;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
