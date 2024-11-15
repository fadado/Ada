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

   ---------------------------------------------------------------------------
   --  Test 1
   ---------------------------------------------------------------------------

   Test_1:
   declare
      use Co_Op;

      package fibonacci_types is new Generators (
         Context_Type => NONE,
         Element_Type => POSITIVE
      );
      use fibonacci_types;

      procedure fibonacci(self: not null GENERATOR_ACCESS) is
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
         max : aliased INTEGER := 5;
         fib : GENERATOR_TYPE (fibonacci'Access, NULL);
         k : POSITIVE;
      begin
         for i in 1..max loop
            fib.Resume(k);
            Put_Line(k'Image);
         end loop;
         New_Line;
         fib.Close;
      exception
         when Stop_Iterator => null;
      end;
   end Test_1;

   ---------------------------------------------------------------------------
   --  Test 2
   ---------------------------------------------------------------------------

   Test_2:
   declare
      use Co_Op;

      package fibonacci_types is new Generators (
         Context_Type => INTEGER,
         Element_Type => POSITIVE
      );
      use fibonacci_types;

      procedure fibonacci(self: not null GENERATOR_ACCESS) is
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
         max : aliased INTEGER := 5;
         package fib is new Wrap (fibonacci'Access, max'Unchecked_Access);
         k : POSITIVE;
      begin
         loop
            fib.Call(k);
            Put_Line(k'Image);
         end loop;
      exception
         when Stop_Iterator => New_Line;
      end;
   end Test_2;

   ---------------------------------------------------------------------------
   --  Test 3
   ---------------------------------------------------------------------------

   Test_3:
   declare
      use Co_Op;

      package fibonacci_types is new Generators (
         Context_Type => INTEGER,
         Element_Type => POSITIVE
      );
      use fibonacci_types;

      procedure fibonacci(self: not null GENERATOR_ACCESS) is
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
         max : aliased INTEGER := 5;
         fib : GENERATOR_TYPE (fibonacci'Access, max'Unchecked_Access);
         ptr : CURSOR_TYPE; --  := First(fib) raises Constraint_Error
         k   : POSITIVE;
      begin
         ptr := First(fib);
         loop
            exit when not Has_Element(ptr);
            k := Element(ptr);
            Put_Line(k'Image);
            Next(ptr);
         end loop;
         New_Line;
      end;
   end Test_3;

   ---------------------------------------------------------------------------
   --  Test 4
   ---------------------------------------------------------------------------

   Test_4:
   declare
      use Co_Op;

      package fibonacci_types is new Generators (
         Context_Type => INTEGER,
         Element_Type => POSITIVE
      );
      use fibonacci_types;

      procedure fibonacci(self: not null GENERATOR_ACCESS) is
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
         max : aliased INTEGER := 5;
         fib : GENERATOR_TYPE (fibonacci'Access, max'Unchecked_Access);
         procedure show(k: POSITIVE) is
         begin
            Put_Line(k'Image);
         end show;
      begin
         Iterate(fib, show'Access);
         New_Line;
      end;
   end Test_4;

   ---------------------------------------------------------------------------
   --  Test 5
   ---------------------------------------------------------------------------

   Test_5:
   declare
      use Co_Op;

      package fibonacci_types is new Generators (
         Context_Type => INTEGER,
         Element_Type => POSITIVE
      );
      use fibonacci_types;

      procedure fibonacci(self: not null GENERATOR_ACCESS) is
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
         max : aliased INTEGER := 10;
         fib : GENERATOR_TYPE (fibonacci'Access, max'Unchecked_Access);
      begin
         for cursor in fib.Iterate loop
            Put_Line(Element(cursor)'Image);
         end loop;
         New_Line;
      end;
   end Test_5;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_fibonacci;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
