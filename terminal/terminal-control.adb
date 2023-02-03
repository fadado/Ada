------------------------------------------------------------------------
package body Terminal.Control is
------------------------------------------------------------------------
   ESC : constant CHARACTER := CHARACTER'Val(27);
   CSI : constant STRING := ESC & '[';

   -- CSI cursor
   package body Cursor is
      function home return STRING is
      begin
         return CSI & 'H';
      end home;

      function up(Lines: POSITIVE := 1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'A';
      end up;

      function down(Lines: POSITIVE := 1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'B';
      end down;

      function forward(Columns: POSITIVE := 1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return CSI & C(2..C'Last) & 'C';
      end forward;

      function backward(Columns: POSITIVE := 1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return CSI & C(2..C'Last) & 'D';
      end backward;

      function down_1st(Lines: POSITIVE := 1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'E';
      end down_1st;

      function up_1st(Lines: POSITIVE := 1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'F';
      end up_1st;

      function column(Column: POSITIVE := 1) return STRING is
         C : STRING renames Column'Image;
      begin
         return CSI & C(2..C'Last) & 'G';
      end column;

      function move(Line, Column: POSITIVE := 1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
      end move;

      function save return STRING is
      begin
         return CSI & 's';
         --return ESC & '7';
      end save;

      function restore return STRING is
      begin
         return CSI & 'r';
         --return ESC & '8';
      end restore;

      function hide return STRING is
      begin
         return CSI & "?25l";
      end hide;

      function show return STRING is
      begin
         return CSI & "?25h";
      end show;
   end Cursor;

   -- SGR attributes
   package body SGR is
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
      
      function attributes(p0: STRING) return STRING is
      begin
         return CSI & p0 & 'm';
      end attributes;
      function attributes(p0, p1: STRING) return STRING is
      begin
         return CSI & p0&';'&p1 & 'm';
      end attributes;
      function attributes(p0, p1, p2: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4, p5: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8 & 'm';
      end attributes;
      function attributes(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8&';'&p9 & 'm';
      end attributes;
   end SGR;

   -- CSI erase
   function display_erase(Mode: Display_Eraser_Mode := Display) return STRING is
      M : STRING renames Display_Eraser_Mode'Pos(Mode)'Image;
   begin
      return CSI & M(2..M'Last) & 'J';
   end display_erase;

   function display_erase_line(Mode: Line_Eraser_Mode := Line) return STRING is
      M : STRING renames Line_Eraser_Mode'Pos(Mode)'Image;
   begin
      return CSI & M(2..M'Last) & 'K';
   end display_erase_line;
   
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

   -- Other
   function reset_device return STRING is
   begin
      return ESC & 'c';
   end reset_device;

   function E_test return STRING is
   begin
      return ESC & "#8";
   end E_test;
end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
