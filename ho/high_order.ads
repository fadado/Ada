------------------------------------------------------------------------
-- High Order Imperative Programming
------------------------------------------------------------------------
package High_Order is
   pragma Pure(High_Order);

   generic
      type INDEX   is (<>);
      type ELEMENT is private;
      type TUPLE   is array (INDEX range <>) of ELEMENT;
   package Tuple_Signature is private end;

   generic
      type A is private;
   procedure Swap (X, Y: in out A);

   generic
      type A (<>) is limited private;
      type B (<>) is limited private;
      type C (<>) is limited private;
      with function F(x: B) return C;
      with function G(x: A) return B;
   function Compose (x: A) return C;

end High_Order;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
