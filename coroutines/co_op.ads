------------------------------------------------------------------------------
--  Cooperative concurrency top unit
------------------------------------------------------------------------------

package Co_Op is

   type NONE is null record;
   --  Helper for void contexts

   Stop_Routine : exception;
   --  Raised after detaching a control

end Co_Op;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
