------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------

with Ada.Containers.Vectors;

package High_Order is
   ---------------------------------------------------------------------
   -- Generic subprograms
   ---------------------------------------------------------------------

   generic -- procedure Swap
      type DEFINITE is private; -- definite
   procedure G_Swap (x, y: in out DEFINITE)
      with Inline;

   generic -- function Compose
      type A (<>) is limited private;
      with function F(x: A) return A;
      with function G(x: A) return A;
   function G_Compose (x: A) return A
      with Inline;

   generic -- function Compose_3
      type A (<>) is limited private;
      type B (<>) is limited private;
      type C (<>) is limited private;
      with function F(x: B) return C;
      with function G(x: A) return B;
   function G_Compose_3 (x: A) return C
      with Inline;

   ---------------------------------------------------------------------
   -- Concepts
   ---------------------------------------------------------------------

   generic -- Equality Concept
      type T;
      with function "="(L, R: T) return BOOLEAN is <>;
      -- with function "/=" provided automatically
   package Equality_Concept is end;

   generic -- Ordering Concept
      with package Eq is new Equality_Concept (<>);
      use Eq;
      with function "<"(L, R: T) return BOOLEAN is <>;
   package Ordering_Concept is end;

   ---------------------------------------------------------------------
   -- Data structures
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   generic -- Generic Stack 1
      type ELEMENT_TYPE is private;
   package G_Stack_1 is
      type T is tagged private;
      procedure Push(self: in out T; x: in ELEMENT_TYPE) with Inline;
      function  Pop(self: in out T) return ELEMENT_TYPE with Inline;
      function  Void(self: in T) return BOOLEAN with Inline;
   private
      use Ada.Containers;
      package Structure is new
         Vectors (Index_Type => POSITIVE, 
                  Element_Type => ELEMENT_TYPE);
      type T is new Structure.VECTOR with null record;
   end G_Stack_1;

   ---------------------------------------------------------------------
   generic -- Generic Stack 2
      with package Structure is new Ada.Containers.Vectors(<>);
   package G_Stack_2 is
      type T is tagged private;
      subtype ELEMENT_TYPE is Structure.Element_Type;
      procedure Push(self: in out T; x: in ELEMENT_TYPE) with Inline;
      function  Pop(self: in out T) return ELEMENT_TYPE with Inline;
      function  Void(self: in T) return BOOLEAN with Inline;
   private
      type T is new Structure.VECTOR with null record;
   end G_Stack_2;

   ---------------------------------------------------------------------
   subtype Count_Type is Ada.Containers.Count_Type;

   generic -- Stack Signature 
      type T is tagged;
      type ELEMENT_TYPE is private;
      with procedure Append(self: in out T; New_Item: ELEMENT_TYPE) is <>;
      with function  Last_Element(self: in T) return ELEMENT_TYPE is <>;
      with procedure Delete_Last(self: in out T; Count: Count_Type:=1) is <>;
      with function  Is_Empty(self: in T) return BOOLEAN is <>;
   package LIFO_Signature is private end;

   generic -- Generic Stack 3
      with package Signature is new LIFO_Signature (<>);
   package G_Stack_3 is
      procedure Push(self: in out Signature.T; x: in Signature.ELEMENT_TYPE) with Inline;
      function  Pop(self: in out Signature.T) return Signature.ELEMENT_TYPE with Inline;
      function  Void(self: in Signature.T) return BOOLEAN with Inline;
   end G_Stack_3;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
