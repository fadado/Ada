-- tester.adb

with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;
--  Vector structures

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
--  List structures

with Tests.Stack;
--  Generic stack test

with Kiss.Signatures.Stack;
--  Signature with subprograms required to implement stacks.

with Kiss.Functors.Stack;
--  Makes a new structure from a signature.

procedure Tester is
begin

   -- Complete cascade
   declare
      package Structure is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Structure;
      package Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);
      package LIFO is new Kiss.Functors.Stack
         (Signature);
      the_stack: LIFO.T;
   begin
      the_stack.Push('Z'); 
      the_stack.Push('A'); 
      if the_stack.Pop /= 'A' then raise Tests.Error; end if;
      if the_stack.Pop /= 'Z' then raise Tests.Error; end if;
      if not the_stack.Is_Empty then raise Tests.Error; end if;
   end;

   -- Implement an stack on unbounded vectors
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
   end;

   -- Implement an stack on unbounded lists
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
   end;

   -- Implement an stack on bounded vectors
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
   end;
end Tester;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
