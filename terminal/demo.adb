-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal.Control;
   use Terminal.Device;

   procedure test_01 is
   begin
      Put(Cursor.hide);
      Put(Cursor.home & Editor.erase_display);
      Put(E_test);
      delay 2.0;
      Put(Cursor.show);
   end;

   procedure test_02 is
      package R renames Render;
   begin
      Put(Render.Set(R.fgcolor(R.red), R.bgcolor(R.yellow), R.bold, R.italic));
      Put(Cursor.hide);
      Put(Cursor.home & Editor.erase_display);
      Put(Cursor.position(1,7) & "Hi!");
      for i in 1..20 loop
         Put(Display.scroll_down);
         delay 0.1;
      end loop;
      Put(Cursor.show);
   end;
begin
   --test_01;
   test_02;
   Put(reset_device);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
