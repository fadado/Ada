------------------------------------------------------------------------------
--  Fibonacci infinite generator
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

with Control.Generators;
use Control;

with Gotcha;

procedure test_fibonacci is

begin
   Gotcha.Set_Handlers;

   Test_1:
   declare
      use Co_Op;

      package G is new Generators (
         Context_Type => NONE,
         Datum_Type   => POSITIVE
      );

      procedure fibonacci(self: G.GENERATOR_ACCESS) is
         m : POSITIVE := 1;
         n : POSITIVE := 1;
         t : POSITIVE;
      begin
         self.Yield(n);
         loop
            self.Yield(n);
            t := n; n := m+n; m := t;
         end loop;
      end;

   begin
      declare
         max : aliased INTEGER := 20;
         fib : G.GENERATOR_TYPE (fibonacci'Access, NULL);
         k : POSITIVE;
      begin
         for i in 1..max loop
            fib.Next(k);
            Put_Line(k'Image);
         end loop;
         fib.Close;
      exception
         when Stop_Iterator => null;
      end;
   end Test_1;

   Test_2:
   declare
      use Co_Op;

      package G is new Generators (
         Context_Type => INTEGER,
         Datum_Type   => POSITIVE
      );

      procedure fibonacci(self: G.GENERATOR_ACCESS) is
         max : INTEGER renames self.context.all;
         m : POSITIVE := 1;
         n : POSITIVE := 1;
         t : POSITIVE;
      begin
         self.Yield(n);
         for i in 2..max loop
            self.Yield(n);
            t := n; n := m+n; m := t;
         end loop;
      end;

   begin
      declare
         max : aliased INTEGER := 20;
         package fib is new G.Wrap (fibonacci'Access, max'Unchecked_Access);
         k : POSITIVE;
      begin
         loop
            fib.Call(k);
            Put_Line(k'Image);
         end loop;
      exception
         when Stop_Iterator => null;
      end;
   end Test_2;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_fibonacci;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
