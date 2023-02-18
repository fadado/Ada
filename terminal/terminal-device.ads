------------------------------------------------------------------------
package Terminal.Device is
------------------------------------------------------------------------
   procedure Put with Inline;
   procedure Put(Item: CHARACTER) with Inline;
   procedure Put(Item: STRING) with Inline;
   procedure Put(Item: INTEGER; Width: POSITIVE := 1) with Inline;
end Terminal.Device;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
