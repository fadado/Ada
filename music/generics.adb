package body Generics is

   procedure Swap(x, y: in out T)
   is
      z : T := x;
   begin
      x := y;
      y := z;
   end Swap;

   function Compose(x: A) return C is
   begin
      return G(F(x));
   end Compose;

   package body Tuples is
      -- Sort
   end Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
