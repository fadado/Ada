package body Generics is

 --generic
 --   type T(<>) is private;
   function Identity
     (a : in T) return T
   is
   begin
      return a;
   end Identity;

 --generic
 --   type T(<>) is private;
   procedure Swapper
     (a, b : in out T)
   is
      z : constant T := a;
   begin
      a := b;
      b := z;
   end Swapper;

 --generic
 --   type α(<>) is limited private;
 --   type β(<>) is limited private;
 --   type γ(<>) is limited private;
 --   with function F(a: in α) return β;
 --   with function G(a: in β) return γ;
   function Compose
     (a : in α) return γ
   is
   begin
      return G(F(a));
   end Compose;

 --generic
 --   type α(<>) is limited private;
 --   type β(<>) is limited private;
 --   type γ(<>) is limited private;
 --   with function F(a: in α; b: in β) return γ;
 --   a : α;
   function Partial
     (b : in β) return γ
   is
   begin
      return F(a, b);
   end Partial;

end Generics;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
