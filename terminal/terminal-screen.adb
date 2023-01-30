with Ada.Text_IO;
with Terminal.ANSI;
------------------------------------------------------------------------
package body Terminal.Screen is
------------------------------------------------------------------------
   package ANSI renames terminal.ANSI;
   package Integer_IO is new Ada.Text_IO.Integer_IO (Num => INTEGER);

   procedure Print with Inline is
   begin
      Ada.Text_IO.New_Line;
   end;
   procedure Print(Item: CHARACTER) with Inline is
   begin
      Ada.Text_IO.Put(Item);
   end;
   procedure Print(Item: STRING) with Inline is
   begin
      Ada.Text_IO.Put(Item);
   end;
   procedure Print(Item: INTEGER; Width: INTEGER := 1) with Inline is
   begin
      Integer_IO.Put(Item);
   end;

   procedure Reset is
   begin
      Print(ANSI.reset);
   end Reset;

   procedure Beep is
   begin
      Print(ANSI.bell);
   end Beep;

   procedure Clear_Screen is
   begin
      Print(ANSI.clear_screen);
   end Clear_Screen;

   procedure Move_to(line: Height; column: Width) is
   begin                                                
      Print(ANSI.cursor_position(line, column));
   end Move_to;  

begin
   null;
end Terminal.Screen;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
