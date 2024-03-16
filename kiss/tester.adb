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

procedure Tester is
begin

   -- Implement stack on unbounded vectors
   declare
      package Structure is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Structure;
      package Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);
      procedure run is new Tests.Stack (Signature);
   begin
      run;
   end;

   -- Implement stack on unbounded lists
   declare
      package Structure is new Ada.Containers.Doubly_Linked_Lists
         (Element_Type => CHARACTER);
      use Structure;
      package Signature is new Kiss.Signatures.Stack
         (Data_Type    => LIST,
          Element_Type => CHARACTER);
      procedure run is new Tests.Stack (Signature);
   begin
      run;
   end;

   -- Implement stack on bounded vectors
   declare
      package Structure is new Ada.Containers.Bounded_Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Structure;
      subtype VECTOR is Structure.VECTOR(11);
      package Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);
      procedure run is new Tests.Stack (Signature);
   begin
      run;
   end;
end Tester;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
