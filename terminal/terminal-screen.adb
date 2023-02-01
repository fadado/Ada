with Ada.Text_IO;
with Terminal.ANSI;
------------------------------------------------------------------------
package body Terminal.Screen is
------------------------------------------------------------------------
   package ANSI renames terminal.ANSI;
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

   --
   procedure Reset is
   begin
      Print(ANSI.reset);
   end Reset;

   procedure Beep is
   begin
      Print(ANSI.bell);
   end Beep;

   procedure Clear is
      use ANSI;
   begin
      Print(erase_display(Mode => Full) & cursor_move);
   end Clear;

   procedure Move(line: Height; column: Width) is
   begin                                                
      Print(ANSI.cursor_move(line, column));
   end Move;  

   -- SGR
   procedure Display(s0: STRING) is
   begin
      Print(ANSI.attributes(s0));
   end Display;

   procedure Display(s0, s1: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1));
   end Display;

   procedure Display(s0, s1, s2: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2));
   end Display;

   procedure Display(s0, s1, s2, s3: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3));
   end Display;

   procedure Display(s0, s1, s2, s3, s4: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4));
   end Display;

   procedure Display(s0, s1, s2, s3, s4, s5: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5));
   end Display;

   procedure Display(s0, s1, s2, s3, s4, s5, s6: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6));
   end Display;

   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7));
   end Display;

   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7, s8: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7&';'&s8));
   end Display;

   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9: STRING) is
   begin
      Print(ANSI.attributes(s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7&';'&s8&';'&s9));
   end Display;
end Terminal.Screen;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
