------------------------------------------------------------------------
package Terminal is 
------------------------------------------------------------------------
   pragma Pure(Terminal);

   NUL : constant CHARACTER := CHARACTER'Val(0);
   DEL : constant CHARACTER := CHARACTER'Val(127);
   subtype CODE is CHARACTER range NUL .. DEL;

end Terminal;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
