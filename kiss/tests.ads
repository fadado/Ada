------------------------------------------------------------------------
-- 
------------------------------------------------------------------------

with Kiss.Interfaces.Stack;

package Tests is

   --pragma Pure (Tests);

   Error : exception;

   package PStackC is new Kiss.Interfaces.Stack
      (Element_Type => CHARACTER);
   subtype IStackC is PStackC.T;
   -- Interface to stack of characters

end Tests;

-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
