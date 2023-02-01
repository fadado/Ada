with Ada.Text_IO; use Ada;
------------------------------------------------------------------------
package body Terminal.Screen is
------------------------------------------------------------------------
   package Integer_IO is new Text_IO.Integer_IO (Num => INTEGER);

   -- print to screen
   procedure Print is
   begin
      Text_IO.New_Line;
   end;
   procedure Print(Item: CHARACTER) is
   begin
      Text_IO.Put(Item);
   end;
   procedure Print(Item: STRING) is
   begin
      Text_IO.Put(Text_IO.Standard_Error, Item);
   end;
   procedure Print(Item: INTEGER; Width: INTEGER := 1) is
   begin
      Integer_IO.Put(Text_IO.Standard_Error, Item);
   end;
end Terminal.Screen;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
