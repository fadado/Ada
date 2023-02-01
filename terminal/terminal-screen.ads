------------------------------------------------------------------------
package Terminal.Screen is
------------------------------------------------------------------------
   procedure Print with Inline;
   procedure Print(Item: CHARACTER) with Inline;
   procedure Print(Item: STRING) with Inline;
   procedure Print(Item: INTEGER; Width: INTEGER := 1) with Inline;
end Terminal.Screen;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
