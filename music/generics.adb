package body Generics is

 --generic
 --   type T(<>) is private;
   function Identity
     (x : in T) return T
   is
   begin
      return x;
   end Identity;

 --generic
 --   type T(<>) is private;
   procedure Swapper
     (x, y : in out T)
   is
      z : constant T := x;
   begin
      x := y;
      y := z;
   end Swapper;

 --generic
 --   type A(<>) is limited private;
 --   type B(<>) is limited private;
 --   type C(<>) is limited private;
 --   with function F(x: in A) return B;
 --   with function G(x: in B) return C;
   function Compose
     (x : in A) return C
   is
   begin
      return G(F(x));
   end Compose;

 --generic
 --   type A(<>) is limited private;
 --   type B(<>) is limited private;
 --   type C(<>) is limited private;
 --   with function F(x: in A; y: in B) return C;
 --   x : A;
   function Partial
     (y : in B) return C
   is
   begin
      return F(x, y);
   end Partial;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
