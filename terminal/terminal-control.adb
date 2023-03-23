------------------------------------------------------------------------
package body Terminal.Control is
------------------------------------------------------------------------
   ---------------------------------------------------------------------
   package C0 is
   ---------------------------------------------------------------------
      NUL   : constant CHARACTER := CHARACTER'Val(0);
      BEL   : constant CHARACTER := CHARACTER'Val(7);
      BS    : constant CHARACTER := CHARACTER'Val(8);  -- format effector
      HT    : constant CHARACTER := CHARACTER'Val(9);  -- format effector
      LF    : constant CHARACTER := CHARACTER'Val(10); -- format effector
      VT    : constant CHARACTER := CHARACTER'Val(11); -- format effector
      FF    : constant CHARACTER := CHARACTER'Val(12); -- format effector
      CR    : constant CHARACTER := CHARACTER'Val(13); -- format effector
      SO    : constant CHARACTER := CHARACTER'Val(14);
      SI    : constant CHARACTER := CHARACTER'Val(15);
      ESC   : constant CHARACTER := CHARACTER'Val(27);
      DEL   : constant CHARACTER := CHARACTER'Val(127);
   end C0;

   ---------------------------------------------------------------------
   package C1 is
   ---------------------------------------------------------------------
      CSI   : constant STRING := (C0.ESC, '[');
      HTS   : constant STRING := (C0.ESC, 'H');
      NEL   : constant STRING := (C0.ESC, 'E');
      OSC   : constant STRING := (C0.ESC, ']');
      RI    : constant STRING := (C0.ESC, 'M');
      ST    : constant STRING := (C0.ESC, '\');
   end C1;

   ---------------------------------------------------------------------
   package body Setup is
   ---------------------------------------------------------------------
      -- RIS
      function reset_initial_state return STRING is
      begin
         return C0.ESC & 'c';
      end reset_initial_state;

      -- DECALN
      function screen_alignment_test return STRING is
      begin
         return C0.ESC & "#8";
      end screen_alignment_test;

      function alternate_screen_buffer return STRING  is
      begin
         return C1.CSI & "?1049h";
      end alternate_screen_buffer;

      function normal_screen_buffer return STRING  is
      begin
         return C1.CSI & "?1049l";
      end normal_screen_buffer;

      function designate_character_sets return STRING is
         G0 : constant STRING := C0.ESC & "(B"; -- USASCII
         G1 : constant STRING := C0.ESC & ")0"; -- line draw
      begin
         return G0 & G1;
      end designate_character_sets;

      function echo_on return STRING is
         SRM   : constant STRING := "12";
         RESET : constant CHARACTER := 'l';
      begin
         return C1.CSI & SRM & RESET;
      end echo_on;

      function echo_off return STRING is
         SRM : constant STRING := "12";
         SET : constant CHARACTER := 'h';
      begin
         return C1.CSI & SRM & SET;
      end echo_off;

      function seven_bits_controls return STRING  is
      begin
         return C0.ESC & " F";
      end seven_bits_controls;
   end Setup;

   ---------------------------------------------------------------------
   package body Display is
   ---------------------------------------------------------------------
      -- DCH
      function delete_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'P';
      end delete_character;

      -- DL
      function delete_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'M';
      end delete_line;

      -- ECH
      function erase_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'X';
      end erase_character;

      -- ED
      function erase_page(Mode: ERASE_MODE:=All_Of) return STRING is
         M : STRING renames ERASE_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'J';
      end erase_page;

      -- EL
      function erase_line(Mode: ERASE_MODE:=All_Of) return STRING is
         M : STRING renames ERASE_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'K';
      end erase_line;

      -- ICH
      function insert_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & '@';
      end insert_character;

      -- IL
      function insert_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'L';
      end insert_line;

      -- SL
      function scroll_left(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & " @";
      end scroll_left;

      -- SR
      function scroll_right(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & " A";
      end scroll_right;

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

      -- SM & RM (modes)
      function mode_replace return STRING is
         IRM   : constant CHARACTER := '4';
         RESET : constant CHARACTER := 'l';
      begin
         return C1.CSI & IRM & RESET;
      end mode_replace;

      function mode_insert return STRING is
         IRM : constant CHARACTER := '4';
         SET : constant CHARACTER := 'h';
      begin
         return C1.CSI & IRM & SET;
      end mode_insert;

      -- BEL
      function bell return CHARACTER is
      begin
         return C0.BEL;
      end bell;

      -- OSC .. ST
      function window_title(Title: STRING) return STRING is
      begin
         return C1.OSC & "2;" & Title & C1.ST;
      end window_title;
   end Display;
   
   ---------------------------------------------------------------------
   package body Cursor is
   ---------------------------------------------------------------------
      -- CUB
      function left(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'D';
      end left;

      -- CBT
      function backward_tabulation(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'Z';
      end backward_tabulation;

      -- CUD
      function down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'B';
      end down;

      -- CUF
      function right(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'C';
      end right;

      -- DECTCEM reset
      function hide return STRING is
      begin
         return C1.CSI & "?25l";
      end hide;

      -- CHA
      function horizontal(N: POSITIVE:=1) return STRING is
         C : STRING renames N'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'G';
      end horizontal;

      -- CHT
      function forward_tabulation(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'I';
      end forward_tabulation;

      -- CNL
      function next_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'E';
      end next_line;

      -- CUP
      function move(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return C1.CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
      end move;

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
   package body Format is
   ---------------------------------------------------------------------
      -- BS
      function backspace return CHARACTER is
      begin
         return C0.BS;
      end backspace;

      -- CR
      function carriage_return return CHARACTER is
      begin
         return C0.CR;
      end carriage_return;

      -- HT
      function tabulation return CHARACTER is
      begin
         return C0.HT;
      end tabulation;

      -- LF
      function line_feed return CHARACTER is
      begin
         return C0.LF;
      end line_feed;

      -- SI
      function standard_character_set return CHARACTER is
      begin
         return C0.SI;
      end standard_character_set;

      -- SO
      function alternate_character_set return CHARACTER is
      begin
         return C0.SO;
      end alternate_character_set;

      -- NEL
      function next_line return STRING is
      begin
         return C1.NEL;
      end next_line;

      function new_line return STRING is
      begin
         return (C0.CR, C0.LF);
      end new_line;

      -- RI
      function reverse_line_feed return STRING is
      begin
         return C1.RI;
      end reverse_line_feed;

      -- HPA
      function hpa(N: POSITIVE:=1) return STRING is
         C : STRING renames N'Image;
      begin
         return C1.CSI & C(2..C'Last) & '`';
      end hpa;

      -- HPR
      function hpr(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'a';
      end hpr;

      -- VPA
      function vpa(N: POSITIVE:=1) return STRING is
         L : STRING renames N'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'd';
      end vpa;

      -- VPR
      function vpr(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'e';
      end vpr;

      -- HVP
      function hvp(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return C1.CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'f';
      end hvp;

      -- HTS
      function tabulation_set return STRING is
      begin
         return C1.HTS;
      end tabulation_set;

      -- TBC
      function tabulation_clear(Mode: TBC_MODE:=Current_Column) return STRING is
         M : STRING renames TBC_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'g';
      end tabulation_clear;

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
   -- REP
   function repeat(Graphic: CODE; Times: POSITIVE) return STRING is
      x : constant POSITIVE := Times-1;
      T : STRING renames x'Image;
   begin
      return Graphic & C1.CSI & T(2..T'Last) & 'b';
   end repeat;

end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
