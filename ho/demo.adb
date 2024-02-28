-- demo.adb

with Ada.Text_IO; use Ada.Text_IO;

with High_Order;
use  High_Order;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Bounded_Doubly_Linked_Lists;

procedure demo is
   Error : exception;

   ---------------------------------------------------------------------
   procedure test_stack is
   ---------------------------------------------------------------------
      generic
         with package LIFO is new Signatures.LIFO
           (Element_Type => CHARACTER, others => <>);
      procedure test;

      procedure test is
         package Character_Stack is new Functors.Stack
           (Signature => LIFO);
         stack: Character_Stack.T;
      begin
          stack.Push('A'); 
          if stack.Pop /= 'A' then raise Error; end if;
          if not stack.Is_Empty then raise Error; end if;
      end test;

   begin
      declare
         package Vector_Structure is new Ada.Containers.Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
         use Vector_Structure;
         package Vector_Signature is new Signatures.LIFO
           (Structure    => VECTOR,
            Element_Type => CHARACTER);
         procedure run is new test(Vector_Signature);
      begin
          run;
      end;

      declare
         package List_Structure is new Ada.Containers.Doubly_Linked_Lists
           (Element_Type => CHARACTER);
         use List_Structure;
         package List_Signature is new Signatures.LIFO
           (Structure    => LIST,
            Element_Type => CHARACTER);
         procedure run is new test(List_Signature);
      begin
          run;
      end;
   end test_stack;

   ---------------------------------------------------------------------
   procedure test_swap is
   ---------------------------------------------------------------------
      procedure Swap is new G_Swap (INTEGER);
      i, j : INTEGER;
   begin
      i := 1;
      j := 2;
      Swap(i, j);
      if i /= 2 or j /= 1 then raise Error; end if;
   end test_swap;

   ---------------------------------------------------------------------
   procedure test_compose is
   ---------------------------------------------------------------------
      function square(n: INTEGER) return INTEGER is (n*n);
      function half(n: INTEGER)   return INTEGER is (n/2);
      function s_h is new G_Compose (INTEGER, half, square);
   begin
      if s_h(5) /= 12 then raise Error; end if;
   end test_compose;
------------------------------------------------------------------------
-- main
------------------------------------------------------------------------
begin
   test_stack;
   test_swap;
   test_compose;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
