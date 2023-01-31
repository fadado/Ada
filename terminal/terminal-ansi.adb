with Ada.Characters.Latin_1;
------------------------------------------------------------------------
package body terminal.ANSI is
------------------------------------------------------------------------
   BEL : CHARACTER renames Ada.Characters.Latin_1.BEL;
   HT  : CHARACTER renames Ada.Characters.Latin_1.HT;
   ESC : CHARACTER renames Ada.Characters.Latin_1.ESC;
   CSI : constant STRING := ESC & '[';

   -- C0
   function bell return CHARACTER is
   begin
      return BEL;
   end bell;

   function tab return CHARACTER is
   begin
      return HT;
   end tab;

   -- CSI cursor
   function cursor_up(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'A';
   end cursor_up;

   function cursor_down(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'B';
   end cursor_down;

   function cursor_right(Columns: POSITIVE := 1) return STRING is
      C : STRING renames Columns'Image;
   begin
      return CSI & C(2..C'Last) & 'C';
   end cursor_right;

   function cursor_left(Columns: POSITIVE := 1) return STRING is
      C : STRING renames Columns'Image;
   begin
      return CSI & C(2..C'Last) & 'D';
   end cursor_left;

   function cursor_down_1st(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'E';
   end cursor_down_1st;

   function cursor_up_1st(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'F';
   end cursor_up_1st;

   function cursor_column(Column: POSITIVE := 1) return STRING is
      C : STRING renames Column'Image;
   begin
      return CSI & C(2..C'Last) & 'G';
   end cursor_column;

   function cursor_position(Line, Column: POSITIVE := 1) return STRING is
      L : STRING renames line'Image;
      C : STRING renames column'Image;
   begin
      return CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
   end cursor_position;

   -- CSI ...
   
   --
   function clear_screen return STRING is
   begin
      return CSI & "1;1H" & CSI & "2J"; -- goto 1-1 and erase
   end clear_screen;

   -- Fs
   function reset return STRING is
   begin
      return ESC & 'c';
   end reset;
end terminal.ANSI;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
