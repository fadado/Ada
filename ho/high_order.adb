------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package body High_Order is

   procedure G_Swap(x, y: in out DEFINITE) is
      z : constant DEFINITE := x;
   begin
      x := y;
      y := z;
   end G_Swap;

   function G_Compose(x: A) return A is
   begin
      return F(G(x));
   end G_Compose;

   function G_Compose_3(x: A) return C is
   begin
      return F(G(x));
   end G_Compose_3;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
