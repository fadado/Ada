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
   -- Data structures
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   package Signatures is
      subtype Count_Type is Ada.Containers.Count_Type;

      generic
         type STRUCTURE is tagged private;
         type ELEMENT_TYPE is private;
         with procedure Append(Container: in out STRUCTURE; New_Item: ELEMENT_TYPE) is <>;
         with function  Last_Element(Container: in STRUCTURE) return ELEMENT_TYPE is <>;
         with procedure Delete_Last(Container: in out STRUCTURE; Count: Count_Type:=1) is <>;
         with function  Is_Empty(Container: in STRUCTURE) return BOOLEAN is <>;
      package Stack is private end;
   end Signatures;

   ---------------------------------------------------------------------
   package Functors is
      generic
         with package Signature is new Signatures.Stack (<>);
      package Stack is
         type T is tagged private;
         subtype ELEMENT_TYPE is Signature.ELEMENT_TYPE;

         procedure Push(self: in out T; x: in ELEMENT_TYPE) with Inline;
         function  Pop(self: in out T) return ELEMENT_TYPE with Inline;
         function  Peek(self: in T) return ELEMENT_TYPE with Inline;
         function  Is_Empty(self: in T) return BOOLEAN with Inline;

      private
         subtype PARENT is Signature.STRUCTURE;
         type T is new PARENT with null record;
      end Stack;
   end Functors;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
