------------------------------------------------------------------------
package terminal.ANSI is
------------------------------------------------------------------------
   -- C0
   function bell  return CHARACTER with Inline;
   function tab   return CHARACTER with Inline;
   -- CSI cursor
   function cursor_up(Lines: POSITIVE := 1) return STRING with Inline;
   function cursor_down(Lines: POSITIVE := 1) return STRING with Inline;
   function cursor_right(Columns: POSITIVE := 1) return STRING with Inline;
   function cursor_left(Columns: POSITIVE := 1) return STRING with Inline;
   function cursor_down_1st(Lines: POSITIVE := 1) return STRING with Inline;
   function cursor_up_1st(Lines: POSITIVE := 1) return STRING with Inline;
   function cursor_column(Column: POSITIVE := 1) return STRING with Inline;
   function cursor_move(Line, Column: POSITIVE := 1) return STRING with Inline;
   function cursor_save return STRING with Inline;
   function cursor_restore return STRING with Inline;
   function cursor_hide return STRING with Inline;
   function cursor_show return STRING with Inline;
   -- CSI eraser
   type Display_Eraser_Mode is (Down, Up, Full);
   function erase_display(Mode: Display_Eraser_Mode := Full) return STRING with Inline;
   type Line_Eraser_Mode is (Rigth, Left, Full);
   function erase_line(Mode: Line_Eraser_Mode := Full) return STRING with Inline;
   -- CSI scroll
   function scroll_up(Lines: POSITIVE := 1) return STRING with Inline;
   function scroll_down(Lines: POSITIVE := 1) return STRING with Inline;
   -- SGR attributes
   package SGR is
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
      type COLOR is (black, red, green, yellow, blue, magenta, cyan, white);
   end SGR;
   function foreground(Color: SGR.COLOR) return STRING with Inline;
   function background(Color: SGR.COLOR) return STRING with Inline;
   function attributes(s0: STRING) return STRING with Inline;
   function attributes(s0, s1: STRING) return STRING with Inline;
   function attributes(s0, s1, s2: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4, s5: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4, s5, s6: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4, s5, s6, s7: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4, s5, s6, s7, s8: STRING) return STRING with Inline;
   function attributes(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9: STRING) return STRING with Inline;
   -- Fs
   function reset_device return STRING with Inline;
------------------------------------------------------------------------
end terminal.ANSI;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
