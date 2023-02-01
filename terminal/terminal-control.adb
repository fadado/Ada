with Ada.Characters.Latin_1;
------------------------------------------------------------------------
package body Terminal.Control is
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

   function cursor_move(Line, Column: POSITIVE := 1) return STRING is
      L : STRING renames Line'Image;
      C : STRING renames Column'Image;
   begin
      return CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
   end cursor_move;

   function cursor_save return STRING is
   begin
      return CSI & 's';
   end cursor_save;

   function cursor_restore return STRING is
   begin
      return CSI & 'r';
   end cursor_restore;

   function cursor_hide return STRING is
   begin
      return CSI & "?25l";
   end cursor_hide;

   function cursor_show return STRING is
   begin
      return CSI & "?25h";
   end cursor_show;

   -- CSI eraser
   function erase_display(Mode: Display_Eraser_Mode := Full) return STRING is
      M : STRING renames Display_Eraser_Mode'Pos(Mode)'Image;
   begin
      return CSI & M(2..M'Last) & 'J';
   end erase_display;

   function erase_line(Mode: Line_Eraser_Mode := Full) return STRING is
      M : STRING renames Line_Eraser_Mode'Pos(Mode)'Image;
   begin
      return CSI & M(2..M'Last) & 'K';
   end erase_line;
   
   -- CSI scroll
   function scroll_up(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'S';
   end scroll_up;

   function scroll_down(Lines: POSITIVE := 1) return STRING is
      L : STRING renames Lines'Image;
   begin
      return CSI & L(2..L'Last) & 'T';
   end scroll_down;

   -- SGR attributes
   function foreground(Color: SGR.COLOR) return STRING is
      C : STRING renames SGR.COLOR'Pos(Color)'Image;
   begin
      return '3' & C(2..C'Last);
   end foreground;
   
   function background(Color: SGR.COLOR) return STRING is
      C : STRING renames SGR.COLOR'Pos(Color)'Image;
   begin
      return '4' & C(2..C'Last);
   end background;
   
   function attributes(s0: STRING) return STRING is
   begin
      return CSI & s0 & 'm';
   end attributes;

   function attributes(s0, s1: STRING) return STRING is
   begin
      return CSI & s0&';'&s1 & 'm';
   end attributes;

   function attributes(s0, s1, s2: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4, s5: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4, s5, s6: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4, s5, s6, s7: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4, s5, s6, s7, s8: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7&';'&s8 & 'm';
   end attributes;

   function attributes(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9: STRING) return STRING is
   begin
      return CSI & s0&';'&s1&';'&s2&';'&s3&';'&s4&';'&s5&';'&s6&';'&s7&';'&s8&';'&s9 & 'm';
   end attributes;
   -- Fs
   function reset_device return STRING is
   begin
      return ESC & 'c';
   end reset_device;
end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
