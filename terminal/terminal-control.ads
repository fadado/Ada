------------------------------------------------------------------------
package Terminal.Control is
------------------------------------------------------------------------
   pragma Pure(Terminal.Control);

   ---------------------------------------------------------------------
   package Cursor is
   ---------------------------------------------------------------------
      function backward(Columns: POSITIVE:=1) return STRING with Inline;
      function backward_tab(Tabs: POSITIVE:=1) return STRING with Inline;
      function down(Lines: POSITIVE:=1) return STRING with Inline;
      function forward(Columns: POSITIVE:=1) return STRING with Inline;
      function hide return STRING with Inline;
      function horizontal_absolute(Column: POSITIVE:=1) return STRING with Inline;
      function horizontal_tab(Tabs: POSITIVE:=1) return STRING with Inline;
      function next_line(Lines: POSITIVE:=1) return STRING with Inline;
      function position(Line, Column: POSITIVE:=1) return STRING with Inline;
      function preceding_line(Lines: POSITIVE:=1) return STRING with Inline;
      function restore return STRING with Inline;
      function save return STRING with Inline;
      function show return STRING with Inline;
      function up(Lines: POSITIVE:=1) return STRING with Inline;
      --
      function forward_tab(Tabs: POSITIVE:=1) return STRING renames horizontal_tab;
   end Cursor;

   ---------------------------------------------------------------------
   package Display is
   ---------------------------------------------------------------------
      type ERASER_MODE is (From_Start, To_End, All_Of);
      function erase(Mode: ERASER_MODE:=All_Of) return STRING with Inline;
      function erase_line(Mode: ERASER_MODE:=All_Of) return STRING with Inline;
      --
      function scroll_up(Lines: POSITIVE:=1) return STRING with Inline;
      function scroll_down(Lines: POSITIVE:=1) return STRING with Inline;
   end Display;

   ---------------------------------------------------------------------
   package Format is
   ---------------------------------------------------------------------
      function backspace return CHARACTER with Inline;
      function carriage_return return CHARACTER with Inline;
      function form_feed return CHARACTER with Inline;
      function horizontal_tab return CHARACTER with Inline;
      function line_feed return CHARACTER with Inline;
      function vertical_tab return CHARACTER with Inline;
      function horizontal_tab_set return STRING with Inline;
      function index return STRING with Inline;
      function next_line return STRING with Inline;
      function reverse_index return STRING with Inline;

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
         function Render(p0: STRING) return STRING with Inline;
         function Render(p0, p1: STRING) return STRING with Inline;
         function Render(p0, p1, p2: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4, p5: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING with Inline;
         function Render(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING with Inline;
      end Style;
   end Format;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------
   function reset_initial_state return STRING with Inline;
   function screen_test return STRING with Inline;
end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
