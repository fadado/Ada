with Ada.Text_IO;
------------------------------------------------------------------------
package body Terminal is
------------------------------------------------------------------------
   procedure Emit(Code: CHARACTER) is
   begin
      Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, Code);
   end;
   procedure Emit(Code: STRING) is
   begin
      Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, Code);
   end;
end Terminal;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
