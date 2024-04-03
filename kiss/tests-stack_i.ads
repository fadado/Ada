------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Interfaces.Stack;

package Tests.Stack_I is

   package IStackC is
      new Kiss.Interfaces.Stack
         (Element_Type => CHARACTER);
   --  Interface to stack of characters.
   
   procedure run_test(the_stack: in out IStackC.I'Class);
   --  Procedure declared BEFORE the concrete type is declared.

end Tests.Stack_I;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
