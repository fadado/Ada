------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package High_Order is
   pragma Pure(High_Order);

   generic
      type INDEX    is (<>);
      type ELEMENT  is private;
      type SEQUENCE is array (INDEX range <>) of ELEMENT;
   package G_Sequential is private end;

   generic
      type MUTABLE is private;
   procedure G_Swap (X, Y: in out MUTABLE);

   generic
      type A (<>) is limited private;
      with function F(x: A) return A;
      with function G(x: A) return A;
   function G_Compose (x: A) return A;

   generic
      type A (<>) is limited private;
      type B (<>) is limited private;
      type C (<>) is limited private;
      with function F(x: B) return C;
      with function G(x: A) return B;
   function G_Compose_3 (x: A) return C;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
