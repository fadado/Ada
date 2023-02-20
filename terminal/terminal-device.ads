------------------------------------------------------------------------
package Terminal.Device is
------------------------------------------------------------------------
   procedure Emit with Inline;
   procedure Emit(Item: CHARACTER) with Inline;
   procedure Emit(Item: STRING) with Inline;
   procedure Emit(Item: INTEGER; Width: POSITIVE := 1) with Inline;
   procedure Print(Item: STRING) with Inline;
   procedure Print(Item: CHARACTER) with Inline;
end Terminal.Device;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
