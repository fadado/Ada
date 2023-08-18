------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package body High_Order is

   -- type A is private;
   procedure Swap (X, Y: in out A) is
      Z : A;
   begin
      Z := X;
      X := Y;
      Y := Z;
   end Swap;

   -- type A is private;
   -- type B is private;
   -- type C is private;
   -- with function F(x: B) return C;
   -- with function G(x: A) return B;
   function Compose (x: A) return C is
   begin
      return F(G(x));
   end Compose;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
