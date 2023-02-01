with Ada.Text_IO;
------------------------------------------------------------------------
package body Terminal.Screen is
------------------------------------------------------------------------
   package Integer_IO is new Ada.Text_IO.Integer_IO (Num => INTEGER);

   -- print to screen
   procedure Print is
   begin
      Ada.Text_IO.New_Line;
   end;
   procedure Print(Item: CHARACTER) is
   begin
      Ada.Text_IO.Put(Item);
   end;
   procedure Print(Item: STRING) is
   begin
      Ada.Text_IO.Put(Item);
   end;
   procedure Print(Item: INTEGER; Width: INTEGER := 1) is
   begin
      Integer_IO.Put(Item);
   end;
end Terminal.Screen;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
