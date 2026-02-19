------------------------------------------------------------------------
package Terminal.Control is
------------------------------------------------------------------------
   pragma Pure(Terminal.Control);

   type SWITCH is (Off, On);

   ---------------------------------------------------------------------
   package Setup is
   ---------------------------------------------------------------------
      function reset_initial_state                 return STRING with Inline;
      function soft_reset                          return STRING with Inline;
      function alternate_screen(Mode: SWITCH:=On)  return STRING with Inline;
      function designate_character_sets            return STRING with Inline;
      function seven_bits_controls                 return STRING with Inline;
      function echo(Mode: SWITCH:=On)              return STRING with Inline;
      function screen_alignment_test               return STRING with Inline;
   end Setup;

   ---------------------------------------------------------------------
   package Display is
   ---------------------------------------------------------------------
      type ERASE_MODE is (From_Start, To_End, All_Of);

      function bell                                      return CHARACTER with Inline;
      function delete_character(Characters: POSITIVE:=1) return STRING with Inline;
      function delete_line(Lines: POSITIVE:=1)           return STRING with Inline;
      function erase_character(Characters: POSITIVE:=1)  return STRING with Inline;
      function erase_line(Mode: ERASE_MODE:=All_Of)      return STRING with Inline;
      function erase_page(Mode: ERASE_MODE:=All_Of)      return STRING with Inline;
      function insert_character(Characters: POSITIVE:=1) return STRING with Inline;
      function insert_line(Lines: POSITIVE:=1)           return STRING with Inline;
      function scroll_down(Lines: POSITIVE:=1)           return STRING with Inline;
      function scroll_left(Columns: POSITIVE:=1)         return STRING with Inline;
      function scroll_right(Columns: POSITIVE:=1)        return STRING with Inline;
      function scroll_up(Lines: POSITIVE:=1)             return STRING with Inline;
      function mode_insert(Mode: SWITCH:=On)             return STRING with Inline;
      function scroll_region(Top, Bottom: POSITIVE)      return STRING with Inline;
      function window_title(Title: STRING)               return STRING with Inline;
   end Display;

   ---------------------------------------------------------------------
   package Cursor is
   ---------------------------------------------------------------------
      type SHAPES is (
         user_shape,
         blinking_block, steady_block,
         blinking_underline, steady_underline,
         blinking_bar, steady_bar
        );

      function next_line(Lines: POSITIVE:=1)       return STRING with Inline;
      function preceding_line(Lines: POSITIVE:=1)  return STRING with Inline;
      function column(N: POSITIVE:=1)              return STRING with Inline;
      function line(N: POSITIVE:=1)                return STRING with Inline;
      function up(Lines: POSITIVE:=1)              return STRING with Inline;
      function down(Lines: POSITIVE:=1)            return STRING with Inline;
      function forward(Columns: POSITIVE:=1)       return STRING with Inline;
      function backward(Columns: POSITIVE:=1)      return STRING with Inline;
      function move(Line, Column: POSITIVE:=1)     return STRING with Inline;
      function save                                return STRING with Inline;
      function restore                             return STRING with Inline;
      function visible(Mode: SWITCH:=On)           return STRING with Inline;
      function blink(Mode: SWITCH:=On)             return STRING with Inline;
      function shape(Form: SHAPES:=user_shape)     return STRING with Inline;
   end Cursor;

   ---------------------------------------------------------------------
   package Tabulator is
   ---------------------------------------------------------------------
      type TBC_MODE is (Current_Column, All_Of);

      function set_stop                               return STRING with Inline;
      function clear(Mode: TBC_MODE:=Current_Column)  return STRING with Inline;
      function backward(Tabs: POSITIVE:=1)            return STRING with Inline;
      function forward(Tabs: POSITIVE:=1)             return STRING with Inline;
   end Tabulator;

   ---------------------------------------------------------------------
   package Format is
   ---------------------------------------------------------------------
      function backspace                        return CHARACTER with Inline;
      function horizontal_tabulation            return CHARACTER with Inline;
      function carriage_return                  return CHARACTER with Inline;
      function line_feed                        return CHARACTER with Inline;
      function reverse_line_feed                return STRING with Inline;
      function new_line                         return STRING with Inline;
      function nel                              return STRING with Inline;
      function hpa(N: POSITIVE:=1)              return STRING with Inline;
      function hpr(Columns: POSITIVE:=1)        return STRING with Inline;
      function vpa(N: POSITIVE:=1)              return STRING with Inline;
      function vpr(Lines: POSITIVE:=1)          return STRING with Inline;
      function hvp(Line, Column: POSITIVE:=1)   return STRING with Inline;
   end Format;

   ---------------------------------------------------------------------
   package Style is
   ---------------------------------------------------------------------
      type COLORS is (
         black, red, green, yellow, blue, magenta, cyan, white
        );
      type INTENSITY is (dimmed, bright);

      default        : constant STRING := "0";
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
      positive       : constant STRING := "27";
      invisible      : constant STRING := "8";
      visible        : constant STRING := "28";
      crossed        : constant STRING := "9";
      no_crossed     : constant STRING := "29";

      function fgcolor(Color: COLORS; Light: INTENSITY:=dimmed) return STRING with Inline;
      function bgcolor(Color: COLORS; Light: INTENSITY:=dimmed) return STRING with Inline;

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

   ---------------------------------------------------------------------
   package Drawing is -- DEC Special Graphics Character Set
   ---------------------------------------------------------------------
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

      function alternate_character_set return CHARACTER with Inline;
      function standard_character_set  return CHARACTER with Inline;
   end Drawing;

   ---------------------------------------------------------------------
   -- Other
   ---------------------------------------------------------------------
   function repeat(Graphic: CODE; Times: POSITIVE) return STRING with Inline;

end Terminal.Control;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
