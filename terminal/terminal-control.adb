with Terminal.Control.C0;
with Terminal.Control.C1;

------------------------------------------------------------------------
package body Terminal.Control is
------------------------------------------------------------------------
   ---------------------------------------------------------------------
   package body Setup is
   ---------------------------------------------------------------------
      -- RIS
      function reset_initial_state return STRING is
      begin
         return C0.ESC & 'c';
      end reset_initial_state;

      -- DECSTR : W
      function soft_reset return STRING is
      begin
         return C1.CSI & "!p";
      end soft_reset;

      -- DECALN
      function screen_alignment_test return STRING is
      begin
         return C0.ESC & "#8";
      end screen_alignment_test;

      -- : W
      function alternate_screen(Mode: SWITCH:=On) return STRING is
      begin
         case Mode is
            when On  => return C1.CSI & "?1049h";
            when Off => return C1.CSI & "?1049l";
         end case;
      end alternate_screen;

      -- : W
      function designate_character_sets return STRING is
         G0 : constant STRING := C0.ESC & "(B"; -- USASCII
         G1 : constant STRING := C0.ESC & ")0"; -- line draw
      begin
         return G0 & G1;
      end designate_character_sets;

      function echo(Mode: SWITCH:=On) return STRING is
      begin
         case Mode is
            when On  => return C1.CSI & "12l";
            when Off => return C1.CSI & "12h";
         end case;
      end echo;

      function seven_bits_controls return STRING  is
      begin
         return C0.ESC & " F";
      end seven_bits_controls;
   end Setup;

   ---------------------------------------------------------------------
   package body Display is
   ---------------------------------------------------------------------
      -- DCH : W
      function delete_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'P';
      end delete_character;

      -- DL : W
      function delete_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'M';
      end delete_line;

      -- ECH : W
      function erase_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'X';
      end erase_character;

      -- ED : W
      function erase_page(Mode: ERASE_MODE:=All_Of) return STRING is
         M : STRING renames ERASE_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'J';
      end erase_page;

      -- EL : W
      function erase_line(Mode: ERASE_MODE:=All_Of) return STRING is
         M : STRING renames ERASE_MODE'Pos(Mode)'Image;
      begin
         return C1.CSI & M(2..M'Last) & 'K';
      end erase_line;

      -- ICH : W
      function insert_character(Characters: POSITIVE:=1) return STRING is
         C : STRING renames Characters'Image;
      begin
         return C1.CSI & C(2..C'Last) & '@';
      end insert_character;

      -- IL : W
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

      -- SU : W
      function scroll_up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'S';
      end scroll_up;

      -- SD : W
      function scroll_down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'T';
      end scroll_down;

      -- DECSTBM : W
      function scroll_region(Top, Bottom: POSITIVE) return STRING is
         T : STRING renames Top'Image;
         B : STRING renames Bottom'Image;
      begin
         return C1.CSI & T(2..T'Last) & ';' & B(2..B'Last) & 'r';
      end scroll_region;

      --
      function mode_insert(Mode: SWITCH:=On) return STRING is
      begin
         case Mode is
            when On  => return C1.CSI & "4h";
            when Off => return C1.CSI & "4l";
         end case;
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
      -- CUU : W
      function up(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'A';
      end up;

      -- CUD : W
      function down(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'B';
      end down;

      -- CUF : W
      function forward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'C';
      end forward;

      -- CUB : W
      function backward(Columns: POSITIVE:=1) return STRING is
         C : STRING renames Columns'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'D';
      end backward;

      -- CNL : W
      function next_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'E';
      end next_line;

      -- CPL : W
      function preceding_line(Lines: POSITIVE:=1) return STRING is
         L : STRING renames Lines'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'F';
      end preceding_line;

      -- CHA : W
      function column(N: POSITIVE:=1) return STRING is
         C : STRING renames N'Image;
      begin
         return C1.CSI & C(2..C'Last) & 'G';
      end column;

      -- VPA : W
      function line(N: POSITIVE:=1) return STRING is
         L : STRING renames N'Image;
      begin
         return C1.CSI & L(2..L'Last) & 'd';
      end line;

      -- CUP : W
      function move(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return C1.CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'H';
      end move;

      -- DECSC : W
      function save return STRING is
      begin
         return C0.ESC & '7';
      end save;

      -- DECRC : W
      function restore return STRING is
      begin
         return C0.ESC & '8';
      end restore;

      -- DECTCEM : W
      function visible(Mode: SWITCH:=On) return STRING is
      begin
         case Mode is
            when On  => return C1.CSI & "?25h";
            when Off => return C1.CSI & "?25l";
         end case;
      end visible;

      -- ATT160 : W
      function blink(Mode: SWITCH:=On) return STRING is
      begin
         case Mode is
            when On  => return C1.CSI & "?12h";
            when Off => return C1.CSI & "?12l";
         end case;
      end blink;

      -- DECCUSR : W
      function shape(Form: SHAPES:=user_shape) return STRING is
         S : STRING renames SHAPES'Pos(Form)'Image;
      begin
         return C1.CSI & S(2..S'Last) & " q";
      end shape;
   end Cursor;

   ---------------------------------------------------------------------
   package body Tabulator is
   ---------------------------------------------------------------------
      -- HTS
      function set_stop return STRING is
      begin
         return C1.HTS;
      end set_stop;

      -- TBC
      function clear(Mode: TBC_MODE:=Current_Column) return STRING is
         M : constant NATURAL := (if Mode = All_Of then 3 else 0);
         S : STRING renames M'Image;
      begin
         return C1.CSI & S(2..S'Last) & 'g';
      end clear;

      -- CBT
      function backward(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'Z';
      end backward;

      -- CHT
      function forward(Tabs: POSITIVE:=1) return STRING is
         T : STRING renames Tabs'Image;
      begin
         return C1.CSI & T(2..T'Last) & 'I';
      end forward;
   end Tabulator;

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
      function horizontal_tabulation return CHARACTER is
      begin
         return C0.HT;
      end horizontal_tabulation;

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
      function nel return STRING is
      begin
         return C1.NEL;
      end nel;

      function new_line return STRING is
      begin
         return (C0.CR, C0.LF);
      end new_line;

      -- RI : W
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

      -- HVP : W
      function hvp(Line, Column: POSITIVE:=1) return STRING is
         L : STRING renames Line'Image;
         C : STRING renames Column'Image;
      begin
         return C1.CSI & L(2..L'Last) & ';' & C(2..C'Last) & 'f';
      end hvp;

      ------------------------------------------------------------------
      package body Style is
      ------------------------------------------------------------------
         function fgcolor(Color: COLORS; Light: INTENSITY:=dimmed) return STRING is
            C : constant NATURAL := COLORS'Pos(Color);
            I : constant NATURAL := (if Light = bright then 60 else 0);
            P : constant NATURAL := 30 + C + I;
            S : STRING renames P'Image;
         begin
            return S(2..S'Last);
         end fgcolor;
         
         function bgcolor(Color: COLORS; Light: INTENSITY:=dimmed) return STRING is
            C : constant NATURAL := COLORS'Pos(Color);
            I : constant NATURAL := (if Light = bright then 60 else 0);
            P : constant NATURAL := 40 + C + I;
            S : STRING renames P'Image;
         begin
            return S(2..S'Last);
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
