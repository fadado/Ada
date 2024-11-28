------------------------------------------------------------------------------
--  Fibonacci infinite generator
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Control.Generators;
use Control;

with Gotcha;

procedure test_fibonacci is
   
   Limit : constant := 10;

   package fibonacci_types is new Generators (
      Context_Type => INTEGER,
      Element_Type => POSITIVE
   );

   use fibonacci_types;

   procedure infinite(self: not null GENERATOR_ACCESS) is
      m, n : POSITIVE := 1;
      t : POSITIVE;
   begin
      self.Yield(n);
      loop
         self.Yield(n);
         t := n; n := m+n; m := t;
      end loop;
   end infinite;

   procedure finite(self: not null GENERATOR_ACCESS) is
      max : INTEGER renames self.context.all;
      m, n : POSITIVE := 1;
      t : POSITIVE;
   begin
      self.Yield(n);
      for i in 2..max loop
         self.Yield(n);
         t := n; n := m+n; m := t;
      end loop;
   end finite;

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Test 1
   ---------------------------------------------------------------------------

   Test_1:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (infinite'Access, NULL);
      begin
         for i in 1..max loop
            Put(fib.Resume'Image);
         end loop;
         New_Line;
         fib.Close;
      end;
   end Test_1;

   ---------------------------------------------------------------------------
   --  Test 2
   ---------------------------------------------------------------------------

   Test_2:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (finite'Access, max'Unchecked_Access);
      begin
         loop
            Put(fib.Resume'Image);
         end loop;
      exception
         when Stop_Iteration => New_Line;
      end;
   end Test_2;

   ---------------------------------------------------------------------------
   --  Test 3
   ---------------------------------------------------------------------------

   Test_3:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (finite'Access, max'Unchecked_Access);
         ptr : CURSOR_TYPE; --  := First(fib) raises Constraint_Error
      begin
         ptr := First(fib);
         loop
            exit when not Has_Element(ptr);
            Put(Element(ptr)'Image);
            ptr := Next(ptr);
         end loop;
         New_Line;
      end;
   end Test_3;

   ---------------------------------------------------------------------------
   --  Test 4
   ---------------------------------------------------------------------------

   Test_4:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (finite'Access, max'Unchecked_Access);
         procedure show(k: POSITIVE) is
         begin
            Put(k'Image);
         end show;
      begin
         For_Each(fib, show'Access);
         New_Line;
      end;
   end Test_4;

   ---------------------------------------------------------------------------
   --  Test 5
   ---------------------------------------------------------------------------

   Test_5:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (finite'Access, max'Unchecked_Access);
      begin
         for p in fib.Iterate loop
            Put(Element(p)'Image);
         end loop;
         New_Line;
      end;
   end Test_5;

   ---------------------------------------------------------------------------
   --  Test 6
   ---------------------------------------------------------------------------

   Test_6:
   declare
   begin
      declare
         max : aliased INTEGER := Limit;
         fib : GENERATOR_TYPE (finite'Access, max'Unchecked_Access);
      begin
         for k of fib loop
            Put(k'Image);
         end loop;
         New_Line;
      end;
   end Test_6;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_fibonacci;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
