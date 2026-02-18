pragma Assertion_Policy(Check); -- Check / Ignore

pragma Optimize(Time);

package Generics is
   pragma Pure(Generics);

   Not_Found       : exception;
   Not_Implemented : exception;

   generic
      type T(<>) is private;
   function Identity
     (a : in T) return T
   with Inline;

   generic
      type T(<>) is private;
   procedure Swapper
     (a, b : in out T)
   with Inline;

   generic
      type α(<>) is limited private;
      type β(<>) is limited private;
      type γ(<>) is limited private;
      with function F(a: in α) return β;
      with function G(a: in β) return γ;
   function Compose
     (a : in α) return γ
   with Inline;

   generic
      type α(<>) is limited private;
      type β(<>) is limited private;
      type γ(<>) is limited private;
      with function F(a: in α; b: in β) return γ;
      a : α;
   function Partial
     (b : in β) return γ
   with Inline;

end Generics;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
