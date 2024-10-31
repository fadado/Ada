------------------------------------------------------------------------------
--  Hello world examples using routines
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

pragma Restrictions (
   No_Abort_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Select_Statements
);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Control.Routines;
use Control;

with Gotcha;

procedure test_hello_2 is

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Test 1 - Routines example
   ---------------------------------------------------------------------------

   Test_1:
   declare
      pragma Warnings (Off, "unreachable code");

      use Co_Op;

      package R is new Routines (Context_Type => NONE);

      procedure hello_world(self: R.ROUTINE_ACCESS) is
      begin
         Put("Test 1-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
         self.Yield; -- closed here
      end;

      hello : R.ROUTINE_TYPE (hello_world'Access, NULL);

   begin
      loop hello.Resume; end loop;
      hello.Close;
   exception
      when Stop_Iterator => null;
   end Test_1;

   ---------------------------------------------------------------------------
   --  Test 2 - Routines example
   ---------------------------------------------------------------------------

   Test_2:
   declare
      use Co_Op;

      package R is new Routines (Context_Type => NONE);

      procedure hello_world(self: R.ROUTINE_ACCESS) is
      begin
         Put("Test 2-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
      end;

      package routine is new R.Wrap (hello_world'Access);

   begin
      loop routine.Call; end loop;
   exception
      when Stop_Iterator => null;
   end Test_2;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello_2;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
