------------------------------------------------------------------------
package terminal.ANSI is
   pragma Pure(ANSI);
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
   function cursor_position(Line, Column: POSITIVE := 1) return STRING with Inline;
   -- CSI ...
   function clear_screen return string with Inline;
   -- SGR
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
   end SGR;
   -- Fs
   function reset return STRING with Inline;
------------------------------------------------------------------------
end terminal.ANSI;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
