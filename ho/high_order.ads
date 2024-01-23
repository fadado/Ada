------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package High_Order is
   pragma Pure(High_Order);

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
      with package Eq is
         new Equality_Concept (<>);
      use Eq;
      with function "<"(L, R: T) return BOOLEAN is <>;
   package Ordering_Concept is end;

   ---------------------------------------------------------------------
   -- Data structures
   ---------------------------------------------------------------------

   generic
      type INDEX    is (<>);
      type ELEMENT  is private;
      type SEQUENCE is array (INDEX range <>) of ELEMENT;
   package G_Array is end;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
