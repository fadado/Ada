pragma Assertion_Policy(Check); -- Check / Ignore

package Generics is
   pragma Pure(Generics);

   Not_Found       : exception;
   Not_Implemented : exception;

   generic
      type T(<>) is private;
   function Identity
     (x : in T) return T
   with Inline;

   generic
      type T(<>) is private;
   procedure Swapper
     (x, y : in out T)
   with Inline;

   generic
      type A(<>) is limited private;
      type B(<>) is limited private;
      type C(<>) is limited private;
      with function F(x: in A) return B;
      with function G(x: in B) return C;
   function Compose
     (x : in A) return C
   with Inline;

   generic
      type A(<>) is limited private;
      type B(<>) is limited private;
      type C(<>) is limited private;
      with function F(x: in A; y: in B) return C;
      x : A;
   function Partial
     (y : in B) return C
   with Inline;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
