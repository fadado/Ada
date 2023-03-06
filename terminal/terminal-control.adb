------------------------------------------------------------------------
package body Terminal.Control is
------------------------------------------------------------------------
   ---------------------------------------------------------------------
   package C0 is
   ---------------------------------------------------------------------
      BEL   : constant CHARACTER := CHARACTER'Val(7);
      BS    : constant CHARACTER := CHARACTER'Val(8);  -- format effector
      HT    : constant CHARACTER := CHARACTER'Val(9);  -- format effector
      LF    : constant CHARACTER := CHARACTER'Val(10); -- format effector
      VT    : constant CHARACTER := CHARACTER'Val(11); -- format effector
      FF    : constant CHARACTER := CHARACTER'Val(12); -- format effector
      CR    : constant CHARACTER := CHARACTER'Val(13); -- format effector
      LS1   : constant CHARACTER := CHARACTER'Val(14); -- SO
      LS0   : constant CHARACTER := CHARACTER'Val(15); -- SI
      ESC   : constant CHARACTER := CHARACTER'Val(27);
   end C0;

   ---------------------------------------------------------------------
   package C1 is
   ---------------------------------------------------------------------
      CSI   : constant STRING := C0.ESC & '[';
      HTS   : constant STRING := C0.ESC & 'H';
      IND   : constant STRING := C0.ESC & 'D';
      NEL   : constant STRING := C0.ESC & 'E';
      OSC   : constant STRING := C0.ESC & ']';
      RI    : constant STRING := C0.ESC & 'M';
      SS2   : constant STRING := C0.ESC & 'N';
      SS3   : constant STRING := C0.ESC & 'O';
      ST    : constant STRING := C0.ESC & '\';
   end C1;

   ---------------------------------------------------------------------
   package body Cursor is
   ---------------------------------------------------------------------
      -- CUB
      function backward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'D';
      end backward;

      -- CBT
      function backward_tab(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'Z';
      end backward_tab;

      -- CUD
      function down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'B';
      end down;

      -- CUF
      function forward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'C';
      end forward;

      -- DECTCEM reset
      function hide return STRING is
      begin
         return C1.CSI & "?25l";
      end hide;

      -- CHA
      function horizontal_absolute(Column: POSITIVE:=1) return STRING is
         C : STRING renames Column'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'G';
      end horizontal_absolute;

      -- CHT
      function horizontal_tab(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'I';
      end horizontal_tab;

      -- CNL
      function next_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'E';
      end next_line;

      -- CUP
      function position(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return C1.CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
      end position;

      -- CPL
      function preceding_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'F';
      end preceding_line;

      -- DECRC
      function restore return STRING is
      begin
         return C0.ESC & '8';
      end restore;

      -- DECSC
      function save return STRING is
      begin
         return C0.ESC & '7';
      end save;

      -- DECTCEM set
      function show return STRING is
      begin
         return C1.CSI & "?25h";
      end show;

      -- CUU
      function up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'A';
      end up;
   end Cursor;

   ---------------------------------------------------------------------
   package body Display is
   ---------------------------------------------------------------------
      -- SU
      function scroll_up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'S';
      end scroll_up;

      -- SD
      function scroll_down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'T';
      end scroll_down;
      -- ED
      function erase(Mode: ERASER_MODE:=All_Of) return STRING is
         M : STRING renames ERASER_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'J';
      end erase;

      -- EL
      function erase_line(Mode: ERASER_MODE:=All_Of) return STRING is
         M : STRING renames ERASER_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'K';
      end erase_line;
   end Display;
   
   ---------------------------------------------------------------------
   package body Format is
   ---------------------------------------------------------------------
      function backspace return CHARACTER is
      begin
         return C0.BS;
      end backspace;

      function carriage_return return CHARACTER is
      begin
         return C0.CR;
      end carriage_return;

      function form_feed return CHARACTER is
      begin
         return C0.FF;
      end form_feed;

      function horizontal_tab return CHARACTER is
      begin
         return C0.HT;
      end horizontal_tab;

      function line_feed return CHARACTER is
      begin
         return C0.LF;
      end line_feed;

      function vertical_tab return CHARACTER is
      begin
         return C0.VT;
      end vertical_tab;

      function horizontal_tab_set return STRING is
      begin
         return C1.HTS;
      end horizontal_tab_set;

      function index return STRING is
      begin
         return C1.IND;
      end index;

      function next_line return STRING is
      begin
         return C1.NEL;
      end next_line;

      function reverse_index return STRING is
      begin
         return C1.RI;
      end reverse_index;

      ------------------------------------------------------------------
      package body Style is
      ------------------------------------------------------------------
         function fgcolor(Color: COLORS) return STRING is
            C : STRING renames COLORS'Pos(Color)'Image;
         begin
            return '3' & C(2..C'Last);
         end fgcolor;
         
         function bgcolor(Color: COLORS) return STRING is
            C : STRING renames COLORS'Pos(Color)'Image;
         begin
            return '4' & C(2..C'Last);
         end bgcolor;
         
         function Render(p0: STRING) return STRING is
         begin
            return C1.CSI & p0 & 'm';
         end Render;
         function Render(p0, p1: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1 & 'm';
         end Render;
         function Render(p0, p1, p2: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2 & 'm';
         end Render;
         function Render(p0, p1, p2, p3: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4, p5: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8 & 'm';
         end Render;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING is
         begin
            return C1.CSI & p0&';'&p1&';'&p2&';'&p3&';'&p4&';'&p5&';'&p6&';'&p7&';'&p8&';'&p9 & 'm';
         end Render;
      end Style;
   end Format;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------

   function reset_initial_state return STRING is
   begin
      return C0.ESC & 'c';
   end reset_initial_state;

   -- DECALN
   function screen_test return STRING is
   begin
      return C0.ESC & "#8";
   end screen_test;
end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
