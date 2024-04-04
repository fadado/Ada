-- tester.adb

pragma Suppress (Tampering_Check);
--  Faster containers

with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;
--  Vector structures

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
--  List structures

with Kiss.Signatures.Stack;
--  Signature with subprograms required to implement stacks

with Kiss.Functors.Stack;
--  Makes a new structure from a signature

with Kiss.Interfaces.Stack;
--  Interfaces to bless types

with Tests.Stack_S;
--  Signature based stack test

with Tests.Stack_I;
--  Interface based stack test

procedure Tester is
begin

   ---------------------------------------------------------------------
   -- Inlined algorithm test
   ---------------------------------------------------------------------

   Inline_Test:
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
      if not the_stack.Void then raise Tests.Error; end if;
   end Inline_Test;

   ---------------------------------------------------------------------
   -- Interface based test
   ---------------------------------------------------------------------

   Bless_Test:
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

      subtype IStack is Tests.Stack_I.IStackC.I;

      type T is new Character_Stack.T
         and IStack with null record;
      --  Bless T with an stack interface.

      the_stack: T;
   begin
      Tests.Stack_I.run_test(the_stack);
   end Bless_Test;

   ---------------------------------------------------------------------
   -- Signature based Stack tests
   ---------------------------------------------------------------------

   Unbounded_Vector_Test:
   declare
      package Container is
         new Ada.Containers.Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
      use Container;

      package Stack_Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);

      procedure run_test is
         new Tests.Stack_S (Stack_Signature);
   begin
      run_test;
   end Unbounded_Vector_Test;

   Unbounded_List_Test:
   declare
      package Container is
         new Ada.Containers.Doubly_Linked_Lists
           (Element_Type => CHARACTER);
      use Container;

      package Stack_Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => LIST,
            Element_Type => CHARACTER);

      procedure run_test is
         new Tests.Stack_S (Stack_Signature);
   begin
      run_test;
   end Unbounded_List_Test;

   Bounded_Vector_Test:
   declare
      package Container is
         new Ada.Containers.Bounded_Vectors
           (Index_Type   => POSITIVE, 
            Element_Type => CHARACTER);
      use Container;

      subtype VECTOR is Container.VECTOR(11);

      package Stack_Signature is
         new Kiss.Signatures.Stack
           (Data_Type    => VECTOR,
            Element_Type => CHARACTER);

      procedure run_test is
         new Tests.Stack_S (Stack_Signature);
   begin
      run_test;
   end Bounded_Vector_Test;

   ---------------------------------------------------------------------
   -- Signature based Queue tests
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Signature based Deque tests
   ---------------------------------------------------------------------

end Tester;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
