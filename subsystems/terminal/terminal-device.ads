------------------------------------------------------------------------
package Terminal.Device is
------------------------------------------------------------------------

   ---------------------------------------------------------------------
   -- Controls
   ---------------------------------------------------------------------
   procedure Emit(Item: CODE) with Inline;
   procedure Emit(Item: STRING) with Inline;
   procedure Emit(Item: INTEGER; Width: POSITIVE := 1) with Inline;

   ---------------------------------------------------------------------
   -- Text
   ---------------------------------------------------------------------
   procedure Print with Inline;
   procedure Print(Item: STRING) with Inline;
   procedure Print(Item: CHARACTER) with Inline;

end Terminal.Device;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
