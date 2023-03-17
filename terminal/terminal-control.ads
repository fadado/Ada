------------------------------------------------------------------------
package Terminal.Control is
------------------------------------------------------------------------
   pragma Pure(Terminal.Control);

   ---------------------------------------------------------------------
   package Display is
   ---------------------------------------------------------------------
      type ERASE_MODE is (From_Start, To_End, All_Of);

      function delete_character(Characters: POSITIVE:=1) return STRING with Inline;
      function delete_line(Lines: POSITIVE:=1) return STRING with Inline;
      function erase_character(Characters: POSITIVE:=1) return STRING with Inline;
      function erase_line(Mode: ERASE_MODE:=All_Of) return STRING with Inline;
      function erase_page(Mode: ERASE_MODE:=All_Of) return STRING with Inline;
      function insert_character(Characters: POSITIVE:=1) return STRING with Inline;
      function insert_line(Lines: POSITIVE:=1) return STRING with Inline;
      function scroll_down(Lines: POSITIVE:=1) return STRING with Inline;
      function scroll_left(Columns: POSITIVE:=1) return STRING with Inline;
      function scroll_right(Columns: POSITIVE:=1) return STRING with Inline;
      function scroll_up(Lines: POSITIVE:=1) return STRING with Inline;
      function mode_insert return STRING with Inline;
      function mode_replace return STRING with Inline;
      function echo_on return STRING with Inline;
      function echo_off return STRING with Inline;
   end Display;

   ---------------------------------------------------------------------
   package Cursor is
   ---------------------------------------------------------------------
      function backward_tabulation(Tabs: POSITIVE:=1) return STRING with Inline;
      function column(N: POSITIVE:=1) return STRING with Inline;
      function down(Lines: POSITIVE:=1) return STRING with Inline;
      function forward_tabulation(Tabs: POSITIVE:=1) return STRING with Inline;
      function hide return STRING with Inline;
      function left(Columns: POSITIVE:=1) return STRING with Inline;
      function next_line(Lines: POSITIVE:=1) return STRING with Inline;
      function position(Line, Column: POSITIVE:=1) return STRING with Inline;
      function preceding_line(Lines: POSITIVE:=1) return STRING with Inline;
      function restore return STRING with Inline;
      function right(Columns: POSITIVE:=1) return STRING with Inline;
      function save return STRING with Inline;
      function show return STRING with Inline;
      function up(Lines: POSITIVE:=1) return STRING with Inline;
   end Cursor;

   ---------------------------------------------------------------------
   package Format is
   ---------------------------------------------------------------------
      type TBC_MODE is (Current_Column, All_Of);
      for TBC_MODE use (Current_Column => 0, All_Of => 3);

      function backspace return CHARACTER with Inline;
      function carriage_return return CHARACTER with Inline;
      function column(N: POSITIVE:=1) return STRING with Inline;
      function designate_Gs return STRING with Inline;
      function down(Lines: POSITIVE:=1) return STRING with Inline;
      function form_feed return CHARACTER with Inline;
      function line(N: POSITIVE:=1) return STRING with Inline;
      function line_feed return CHARACTER with Inline;
      function next_line return STRING with Inline;
      function position(Line, Column: POSITIVE:=1) return STRING with Inline;
      function right(Columns: POSITIVE:=1) return STRING with Inline;
      function shift_in return CHARACTER with Inline;
      function shift_out return CHARACTER with Inline;
      function tabulation return CHARACTER with Inline;
      function tabulation_clear(Mode: TBC_MODE:=Current_Column) return STRING with Inline;
      function tabulation_set return STRING with Inline;
      function up return STRING with Inline;
    --function line_tabulation return CHARACTER with Inline;

      ------------------------------------------------------------------
      package DEC is -- DEC Special Graphics Character Set
      ------------------------------------------------------------------
         blank                   : constant CHARACTER := CHARACTER'Val(95);
         diamond                 : constant CHARACTER := CHARACTER'Val(96);
         checkerboard            : constant CHARACTER := CHARACTER'Val(97);
         degree_symbol           : constant CHARACTER := CHARACTER'Val(102);
         lower_right_corner      : constant CHARACTER := CHARACTER'Val(106);
         upper_right_corner      : constant CHARACTER := CHARACTER'Val(107);
         upper_left_corner       : constant CHARACTER := CHARACTER'Val(108);
         lower_left_corner       : constant CHARACTER := CHARACTER'Val(109);
         crossing_lines          : constant CHARACTER := CHARACTER'Val(110);
         left_T                  : constant CHARACTER := CHARACTER'Val(116);
         right_T                 : constant CHARACTER := CHARACTER'Val(117);
         bottom_T                : constant CHARACTER := CHARACTER'Val(118);
         top_T                   : constant CHARACTER := CHARACTER'Val(129);
         vertical_bar            : constant CHARACTER := CHARACTER'Val(120);
         horizontal_bar          : constant CHARACTER := CHARACTER'Val(113);
         pi                      : constant CHARACTER := CHARACTER'Val(123);
         plus_minus              : constant CHARACTER := CHARACTER'Val(103);
         less_than_equal_to      : constant CHARACTER := CHARACTER'Val(121);
         greater_than_equal_to   : constant CHARACTER := CHARACTER'Val(122);
         not_equal_to            : constant CHARACTER := CHARACTER'Val(124);
         UK_pound_symbol         : constant CHARACTER := CHARACTER'Val(125);
         centered_dot            : constant CHARACTER := CHARACTER'Val(126);
         HT_symbol               : constant CHARACTER := CHARACTER'Val(98);
         FF_symbol               : constant CHARACTER := CHARACTER'Val(99);
         CR_symbol               : constant CHARACTER := CHARACTER'Val(100);
         LF_symbol               : constant CHARACTER := CHARACTER'Val(101);
         NL_symbol               : constant CHARACTER := CHARACTER'Val(104);
         VT_symbol               : constant CHARACTER := CHARACTER'Val(105);
         horizontal_line_scan_1  : constant CHARACTER := CHARACTER'Val(111);
         horizontal_line_scan_3  : constant CHARACTER := CHARACTER'Val(112);
         horizontal_line_scan_5  : constant CHARACTER := CHARACTER'Val(113);
         horizontal_line_scan_7  : constant CHARACTER := CHARACTER'Val(114);
         horizontal_line_scan_9  : constant CHARACTER := CHARACTER'Val(115);
      end DEC;

      ------------------------------------------------------------------
      package Style is
      ------------------------------------------------------------------
         reset          : constant STRING := "0";
         bold           : constant STRING := "1";
         faint          : constant STRING := "2";
         normal         : constant STRING := "22"; -- neither bold nor faint
         italic         : constant STRING := "3";
         no_italic      : constant STRING := "23";
         underline      : constant STRING := "4";
         no_underline   : constant STRING := "24";
         blink          : constant STRING := "5";
         no_blink       : constant STRING := "25";
         inverse        : constant STRING := "7";

         type COLORS is (black, red, green, yellow, blue, magenta, cyan, white);
         function fgcolor(Color: COLORS) return STRING with Inline;
         function bgcolor(Color: COLORS) return STRING with Inline;

         function Apply(p0: STRING) return STRING with Inline;
         function Apply(p0, p1: STRING) return STRING with Inline;
         function Apply(p0, p1, p2: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4, p5: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING with Inline;
         function Apply(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING with Inline;
      end Style;
   end Format;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------
   function bell return CHARACTER with Inline;
   function repeat(Graphic: CODE; Times: POSITIVE) return STRING with Inline;
   function reset_initial_state return STRING with Inline;
   function screen_alignment_test return STRING with Inline;
   function window_title(Title: STRING) return STRING with Inline;

end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
