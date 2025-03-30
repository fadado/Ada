package body Generics is

 --generic
 --   type T(<>) is private;
   procedure Swap
     (x, y : in out T)
   is
      z : constant T := x;
   begin
      x := y;
      y := z;
   end Swap;

 --generic
 --   type A(<>) is limited private;
 --   type B(<>) is limited private;
 --   type C(<>) is limited private;
 --   with function F(x : in A) return B;
 --   with function G(x : in B) return C;
   function Compose
     (x : in A) return C
   is
   begin
      return G(F(x));
   end Compose;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
