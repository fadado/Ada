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
   procedure test_stack_1 is
      package Character_Stack is new
         G_Stack_1 (ELEMENT_TYPE => CHARACTER);
   ---------------------------------------------------------------------
   begin
      declare
         stack: Character_Stack.T;
      begin
        stack.Push('A'); 
        if stack.Pop /= 'A' then raise Error; end if;
        if not stack.Void then raise Error; end if;
      end;
   end test_stack_1;

   ---------------------------------------------------------------------
   procedure test_stack_2 is
   ---------------------------------------------------------------------
      package Structure is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      package Character_Stack is new G_Stack_2 (Structure);
   begin
      declare
         stack: Character_Stack.T;
      begin
        stack.Push('A'); 
        if stack.Pop /= 'A' then raise Error; end if;
        if not stack.Void then raise Error; end if;
      end;
   end test_stack_2;

   ---------------------------------------------------------------------
   procedure test_stack_3 is
   ---------------------------------------------------------------------
      package Structure is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Structure;
      package Signature is new LIFO_Signature(T => VECTOR, Element_Type => CHARACTER);
      package Character_Stack is new G_Stack_3 (Signature);
   begin
      declare
--         stack: Character_Stack.T;
      begin
--        stack.Push('A'); 
--        if stack.Pop /= 'A' then raise Error; end if;
--        if not stack.Void then raise Error; end if;
        null;
      end;
   end test_stack_3;

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
   test_stack_1;
   test_stack_2;
   test_swap;
   test_compose;

   declare
      package E is new Equality_Concept (INTEGER);
      package O is new Ordering_Concept (E);
   begin
      null;
   end;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
