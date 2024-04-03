-- tester.adb

pragma Suppress (Tampering_Check);
-- Faster containers

with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;
--  Vector structures

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
--  List structures

with Kiss.Signatures.Stack;
--  Signature with subprograms required to implement stacks.

with Kiss.Functors.Stack;
--  Makes a new structure from a signature.

with Kiss.Interfaces.Stack;
-- To bless with interface;

with Tests.Stack;
--  Generic stack test

procedure Tester is
begin

   Bless_Test:
   declare
      package IStackC is
         new Kiss.Interfaces.Stack
           (Element_Type => CHARACTER);
      -- Interface to stack of characters.
      
      procedure run_test(the_stack: in out IStackC.I'Class)
      is
      begin
         the_stack.Push('Z'); 
         the_stack.Push('A'); 
         if the_stack.Pop /= 'A' then raise Tests.Error; end if;
         if the_stack.Pop /= 'Z' then raise Tests.Error; end if;
         if not the_stack.Is_Empty then raise Tests.Error; end if;
      end run_test;
      -- Procedure declared BEFORE the concrete type is declared.

      package Container is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Container;
      package Stack_Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);
      package Character_Stack is new Kiss.Functors.Stack
         (Stack_Signature);
      -- Declare new stack structure.

      type T is new Character_Stack.T
         and IStackC.I with null record;
      -- Bless T as an stack interface.

      the_stack: T;
      -- Concrete character stack.
   begin
      run_test(the_stack);
   end Bless_Test;

   Full_Cascade_Test:
   declare
      package Container is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Container;
      package Stack_Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);
      package Character_Stack is new Kiss.Functors.Stack
         (Stack_Signature);
      the_stack: Character_Stack.T;
   begin
      the_stack.Push('Z'); 
      the_stack.Push('A'); 
      if the_stack.Pop /= 'A' then raise Tests.Error; end if;
      if the_stack.Pop /= 'Z' then raise Tests.Error; end if;
      if not the_stack.Is_Empty then raise Tests.Error; end if;
   end Full_Cascade_Test;

   Unbounded_Vector_Test:
   declare
      package Structure is
         new Ada.Containers.Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
      use Structure;
      package Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);
      procedure run_test is
         new Tests.Stack (Signature);
   begin
      run_test;
   end Unbounded_Vector_Test;

   Unbounded_List_Test:
   declare
      package Structure is
         new Ada.Containers.Doubly_Linked_Lists
           (Element_Type => CHARACTER);
      use Structure;
      package Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => LIST,
            Element_Type => CHARACTER);
      procedure run_test is
         new Tests.Stack (Signature);
   begin
      run_test;
   end Unbounded_List_Test;

   Bounded_Vector_Test:
   declare
      package Structure is
         new Ada.Containers.Bounded_Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
      use Structure;
      subtype VECTOR is Structure.VECTOR(11);
      package Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);
      procedure run_test is
         new Tests.Stack (Signature);
   begin
      run_test;
   end Bounded_Vector_Test;
end Tester;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
