------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package body High_Order is

   procedure Swap (X, Y: in out A) is
      Z : A;
   begin
      Z := X;
      X := Y;
      Y := Z;
   end Swap;
   pragma Inline(Swap);

   function Compose (x: A) return C is
   begin
      return F(G(x));
   end Compose;
   pragma Inline(Compose);

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
