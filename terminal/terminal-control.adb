------------------------------------------------------------------------
package body Terminal.Control is
------------------------------------------------------------------------
   -- Introducers
   ESC : constant CHARACTER := CHARACTER'Val(27);
   CSI : constant STRING    := ESC & '[';

   ---------------------------------------------------------------------
   package body Cursor is
   ---------------------------------------------------------------------
      -- CUU
      function up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'A';
      end up;

      -- CUD
      function down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'B';
      end down;

      -- CUF
      function forward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return CSI & C(2..C'Last) & 'C';
      end forward;

      -- CUB
      function backward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return CSI & C(2..C'Last) & 'D';
      end backward;

      -- CNL
      function next_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'E';
      end next_line;

      -- CPL
      function preceding_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'F';
      end preceding_line;

      -- CHA
      function horizontal_absolute(Column: POSITIVE:=1) return STRING is
         C : STRING renames Column'Image;
      begin
         return CSI & C(2..C'Last) & 'G';
      end horizontal_absolute;

      -- CUP
      function position(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
      end position;

      function home return STRING is
      begin
         return CSI & 'H';
      end home;

      -- SCP
      function save return STRING is
      begin
         return CSI & 's';
      end save;

      -- RCP
      function restore return STRING is
      begin
         return CSI & 'r';
      end restore;

      -- VT220
      function hide return STRING is
      begin
         return CSI & "?25l";
      end hide;

      function show return STRING is
      begin
         return CSI & "?25h";
      end show;
   end Cursor;

   ---------------------------------------------------------------------
   package body Display is
   ---------------------------------------------------------------------
      -- SU
      function scroll_up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'S';
      end scroll_up;

      -- SD
      function scroll_down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return CSI & L(2..L'Last) & 'T';
      end scroll_down;
   end Display;

   ---------------------------------------------------------------------
   package body Editor is
   ---------------------------------------------------------------------
      -- ED
      function erase_display(Mode: ERASER_MODE:=All_Of) return STRING is
         M : STRING renames ERASER_MODE'Pos(Mode)'Image;
      begin
         return CSI & M(2..M'Last) & 'J';
      end erase_display;

      -- EL
      function erase_line(Mode: ERASER_MODE:=All_Of) return STRING is
         M : STRING renames ERASER_MODE'Pos(Mode)'Image;
      begin
         return CSI & M(2..M'Last) & 'K';
      end erase_line;
   end Editor;
   
   ---------------------------------------------------------------------
   package body Render is
   ---------------------------------------------------------------------
      function fgcolor(Color: Render.COLOR) return STRING is
         C : STRING renames Render.COLOR'Pos(Color)'Image;
      begin
         return '3' & C(2..C'Last);
      end fgcolor;
      
      function bgcolor(Color: Render.COLOR) return STRING is
         C : STRING renames Render.COLOR'Pos(Color)'Image;
      begin
         return '4' & C(2..C'Last);
      end bgcolor;
      
      function Set(p0: STRING) return STRING is
      begin
         return CSI & p0 & 'm';
      end Set;
      function Set(p0, p1: STRING) return STRING is
      begin
         return CSI & p0&';'&p1 & 'm';
      end Set;
      function Set(p0, p1, p2: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2 & 'm';
      end Set;
      function Set(p0, p1, p2, p3: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4, p5: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8 & 'm';
      end Set;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING is
      begin
         return CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8&';'&p9 & 'm';
      end Set;
   end Render;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------
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
