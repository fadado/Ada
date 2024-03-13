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
         type Data_Type is tagged private;
         type Element_Type is private;
         with procedure Append(Container: in out Data_Type; New_Item: Element_Type) is <>;
         with function  Last_Element(Container: in Data_Type) return Element_Type is <>;
         with procedure Delete_Last(Container: in out Data_Type; Count: Count_Type:=1) is <>;
         with function  Is_Empty(Container: in Data_Type) return BOOLEAN is <>;
      package Stack is private end;
   end Signatures;

   ---------------------------------------------------------------------
   package Functors is
      generic
         with package Signature is new Signatures.Stack (<>);
      package Stack is
         type T is tagged private;
         subtype Element_Type is Signature.Element_Type;

         procedure Push(Container: in out T; x: in Element_Type) with Inline;
         function  Pop(Container: in out T) return Element_Type with Inline;
         function  Peek(Container: in T) return Element_Type with Inline;
         function  Is_Empty(Container: in T) return BOOLEAN with Inline;

      private
         subtype Data is Signature.Data_Type;
         type T is new Data with null record;
      end Stack;
   end Functors;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
