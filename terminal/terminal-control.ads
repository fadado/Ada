------------------------------------------------------------------------
package Terminal.Control is
------------------------------------------------------------------------
   pragma Pure(Terminal.Control);

   ---------------------------------------------------------------------
   package Cursor is
   ---------------------------------------------------------------------
      function backward_tabulation(Tabs: POSITIVE:=1) return STRING with Inline;
      function character_absolute(Column: POSITIVE:=1) return STRING with Inline;
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
   package Display is
   ---------------------------------------------------------------------
      type ERASE_MODE is (From_Start, To_End, All_Of);
      function erase_page(Mode: ERASE_MODE:=All_Of) return STRING with Inline;
      function erase_line(Mode: ERASE_MODE:=All_Of) return STRING with Inline;
      --
      function scroll_up(Lines: POSITIVE:=1) return STRING with Inline;
      function scroll_down(Lines: POSITIVE:=1) return STRING with Inline;
   end Display;

   ---------------------------------------------------------------------
   package Format is
   ---------------------------------------------------------------------
      function backspace return CHARACTER with Inline;
      function carriage_return return CHARACTER with Inline;
      function character_line_position(Line, Column: POSITIVE:=1) return STRING with Inline;
      function character_position_absolute(Column: POSITIVE:=1) return STRING with Inline;
      function character_position_forward(Columns: POSITIVE:=1) return STRING with Inline;
      function character_tabulation return CHARACTER with Inline;
      function character_tabulation_set return STRING with Inline;
      function form_feed return CHARACTER with Inline;
      function line_feed return CHARACTER with Inline;
      function line_position_absolute(Line: POSITIVE:=1) return STRING with Inline;
      function line_position_forward(Lines: POSITIVE:=1) return STRING with Inline;
      function line_tabulation return CHARACTER with Inline;
      function next_line return STRING with Inline;
      function reverse_line_feed return STRING with Inline;

      type TBC_MODE is (Current_Column, ignore_01, ignore_02, All_Of);
      function tabulation_clear(Mode: TBC_MODE:=Current_Column) return STRING with Inline;

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
         --
         type COLORS is (black, red, green, yellow, blue, magenta, cyan, white);
         function fgcolor(Color: COLORS) return STRING with Inline;
         function bgcolor(Color: COLORS) return STRING with Inline;
         --
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
   function repeat(Times: POSITIVE) return STRING with Inline;
   function reset_initial_state return STRING with Inline;
   function screen_alignment_test return STRING with Inline;
   function window_title(Title: STRING) return STRING with Inline;

end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
