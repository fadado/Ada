-- demo.adb

with Terminal.Control;
use  Terminal;
use  Terminal.Control;

with Terminal.Display;
with Ada.Text_IO;

procedure demo is
   -- to move to display module
   procedure display_Clear_Display is
   begin
      Send(Cursor.home);
      Send(display_erase);
   end;

   procedure test_01 is
   begin
      Send(Cursor.hide);
      display_Clear_Display;
      Send(E_test);
      delay 2.0;
      Send(Cursor.show);
   end;

   procedure test_02 is
   begin
      Send(Style.attributes(
            Style.foreground(Style.red),
            Style.background(Style.yellow),
            Style.blink,
            Style.bold,
            Style.italic)
      );
      Send(Cursor.hide);
      display_Clear_Display;
      Send(Cursor.position(1, 7));
      Terminal.Display.Print("Hi!");
      delay 2.0;
      Send(Cursor.show);
   end;
begin
   test_01;
   test_02;
   Send(reset_device);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
