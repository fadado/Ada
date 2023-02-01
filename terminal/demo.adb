-- demo.adb

with Terminal.Control;
use  Terminal;
use  Terminal.Control;

with Terminal.Screen;
with Ada.Text_IO;

procedure demo is
   -- to move to screen module
   procedure screen_Clear_Screen is
   begin
      Emit(cursor_move); -- go to home
      Emit(erase_display);
   end;

   procedure test_01 is
   begin
      screen_Clear_Screen;
      Emit(cursor_hide);
      Emit(E_test);
      delay 2.0;
      Emit(cursor_show);
   end;

   procedure test_02 is
   begin
      Emit(attributes(
            foreground(SGR.red),
            background(SGR.yellow),
            SGR.blink,
            SGR.bold,
            SGR.italic)
      );
      screen_Clear_Screen;
      Emit(cursor_hide);
      Emit(cursor_move(1, 7));
      Screen.Print("Hi!");
      delay 2.0;
      Emit(cursor_show);
   end;
begin
   test_01;
   test_02;
   Emit(reset_device);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
