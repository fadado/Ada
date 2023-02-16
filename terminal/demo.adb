-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal.Control;
   use Terminal.Device;

   -- to move to display module ???
   procedure display_Clear_Display is
   begin
      Write(Cursor.home & erase_display);
   end;

   procedure test_01 is
   begin
      Write(Cursor.hide);
      display_Clear_Display;
      Write(E_test);
      delay 2.0;
      Write(Cursor.show);
   end;

   procedure test_02 is
   begin
      Write(Style.attributes(
            Style.foreground(Style.red),
            Style.background(Style.yellow),
            Style.blink,
            Style.bold,
            Style.italic)
      );
      Write(Cursor.hide);
      display_Clear_Display;
      Write(Cursor.position(1,7) & "Hi!");
      delay 2.0;
      Write(Cursor.show);
   end;
begin
   test_01;
   test_02;
   Write(reset_device);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
