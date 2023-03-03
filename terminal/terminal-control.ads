------------------------------------------------------------------------
package Terminal.Control is
------------------------------------------------------------------------
   pragma Pure(Terminal.Control);

   ---------------------------------------------------------------------
   package C0 is
   ---------------------------------------------------------------------
      BEL   : constant CHARACTER := CHARACTER'Val(7);
      BS    : constant CHARACTER := CHARACTER'Val(8);
      HT    : constant CHARACTER := CHARACTER'Val(9);
      LF    : constant CHARACTER := CHARACTER'Val(10);
      VT    : constant CHARACTER := CHARACTER'Val(11);
      FF    : constant CHARACTER := CHARACTER'Val(12);
      CR    : constant CHARACTER := CHARACTER'Val(13);
      LS1   : constant CHARACTER := CHARACTER'Val(14); -- SO
      LS0   : constant CHARACTER := CHARACTER'Val(15); -- SI
   end C0;

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
   package Editor is
   ---------------------------------------------------------------------
      type ERASER_MODE is (From_Start, To_End, All_Of);
      function erase_display(Mode: ERASER_MODE:=All_Of) return STRING with Inline;
      function erase_line(Mode: ERASER_MODE:=All_Of) return STRING with Inline;
      --
      function scroll_up(Lines: POSITIVE:=1) return STRING with Inline;
      function scroll_down(Lines: POSITIVE:=1) return STRING with Inline;
   end Editor;

   ---------------------------------------------------------------------
   package Render is
   ---------------------------------------------------------------------
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
      type COLOR is (black, red, green, yellow, blue, magenta, cyan, white);
      function fgcolor(Color: Render.COLOR) return STRING with Inline;
      function bgcolor(Color: Render.COLOR) return STRING with Inline;
      --
      function Set(p0: STRING) return STRING with Inline;
      function Set(p0, p1: STRING) return STRING with Inline;
      function Set(p0, p1, p2: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4, p5: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4, p5, p6: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7, p8: STRING) return STRING with Inline;
      function Set(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9: STRING) return STRING with Inline;
   end Render;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------
   function reset_initial_state return STRING with Inline;
   function E_test return STRING with Inline;
end Terminal.Control;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
