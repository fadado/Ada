with Ada.Text_IO; use Ada;
with Terminal.Control; use Terminal.Control;
------------------------------------------------------------------------
package body Terminal.Device is
------------------------------------------------------------------------
   package Integer_IO is new Text_IO.Integer_IO (Num => INTEGER);

   Output : Text_IO.File_Type renames Text_IO.Standard_Error;

   procedure Write is
   begin
      Text_IO.Put(Output, C0.CR & C0.LF);
   end;

   procedure Write(Item: CHARACTER) is
   begin
      Text_IO.Put(Output, Item);
   end;

   procedure Write(Item: STRING) is
   begin
      Text_IO.Put(Output, Item);
   end;

   procedure Write(Item: INTEGER; Width: POSITIVE := 1) is
   begin
      Integer_IO.Put(Output, Item);
   end;

   -- Receive...
   Input : Text_IO.File_Type renames Text_IO.Standard_Input;

end Terminal.Device;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
