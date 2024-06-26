with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO; use Ada;
with Terminal.Control; use Terminal.Control;

------------------------------------------------------------------------
package body Terminal.Device is
------------------------------------------------------------------------
   package UTF renames Ada.Strings.UTF_Encoding.Strings;
   package Integer_IO is new Text_IO.Integer_IO (Num => INTEGER);

   Output : Text_IO.File_Type renames Text_IO.Standard_Error;

   ---------------------------------------------------------------------
   -- Send control sequences
   ---------------------------------------------------------------------
   procedure Emit(Item: CODE) is
   begin
      Text_IO.Put(Output, Item);
   end;

   procedure Emit(Item: STRING) is
   begin
      Text_IO.Put(Output, Item);
   end;

   procedure Emit(Item: INTEGER; Width: POSITIVE:=1) is
   begin
      Integer_IO.Put(Output, Item);
   end;

   ---------------------------------------------------------------------
   -- Print Latin1 input as UTF-8
   ---------------------------------------------------------------------
   procedure Print is
   begin
      Text_IO.Put(Output, Format.new_line);
   end;

   procedure Print(Item: STRING) is
   begin
      Text_IO.Put(Output, UTF.Encode(Item));
   end;

   procedure Print(Item: CHARACTER) is
   begin
      Text_IO.Put(Output, UTF.Encode((1 => Item)));
   end;

   ---------------------------------------------------------------------
   -- Receive...
   ---------------------------------------------------------------------
   Input : Text_IO.File_Type renames Text_IO.Standard_Input;

end Terminal.Device;
-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
