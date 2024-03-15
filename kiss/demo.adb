-- demo.adb

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Kiss.Signatures.Stack;
with Kiss.Functors.Stack;
use  Kiss;

procedure demo is
   Error : exception;

   generic
      with package Signature is new Signatures.Stack
         (Element_Type => CHARACTER, others => <>);
   procedure stack_test;

   procedure stack_test is
      package Character_Stack is new Functors.Stack
         (Signature => Signature);
      the_stack: Character_Stack.T;
   begin
         the_stack.Push('Z'); 
         the_stack.Push('A'); 
         if the_stack.Pop /= 'A' then raise Error; end if;
         if the_stack.Pop /= 'Z' then raise Error; end if;
         if not the_stack.Is_Empty then raise Error; end if;
   end stack_test;

   procedure run_stack_tests is
      package AC renames Ada.Containers;
   begin
      declare
         package Structure is new AC.Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
         use Structure;
         package Signature is new Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);
         procedure run is new stack_test (Signature);
      begin
          run;
      end;

      declare
         package Structure is new AC.Doubly_Linked_Lists
           (Element_Type => CHARACTER);
         use Structure;
         package Signature is new Signatures.Stack
           (Data_Type    => LIST,
            Element_Type => CHARACTER);
         procedure run is new stack_test (Signature);
      begin
          run;
      end;

      declare
         package Structure is new AC.Bounded_Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
         use Structure;
         subtype VECTOR is Structure.VECTOR(11);
         package Signature is new Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);
         procedure run is new stack_test (Signature);
      begin
          run;
      end;
   end run_stack_tests;

begin -- main
   run_stack_tests;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
