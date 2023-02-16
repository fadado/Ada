------------------------------------------------------------------------
package Terminal.Device is
------------------------------------------------------------------------
   procedure Write with Inline;
   procedure Write(Item: CHARACTER) with Inline;
   procedure Write(Item: STRING) with Inline;
   procedure Write(Item: INTEGER; Width: POSITIVE := 1) with Inline;
end Terminal.Device;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
