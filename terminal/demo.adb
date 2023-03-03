-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal.Control;
   use Terminal.Device;

   procedure test_01 is
   begin
      Emit(Cursor.hide);
      Emit(Cursor.position & Editor.erase_display);
      Emit(E_test);
      delay 2.0;
      Emit(Cursor.show);
   end;

   procedure test_02 is
      package R renames Render;
   begin
      Emit(Render.Set(
         R.fgcolor(R.red),
         R.bgcolor(R.yellow),
         R.bold,
         R.italic));
      Emit(Cursor.hide);
      Emit(Cursor.position & Editor.erase_display);
      Emit(Cursor.position(1,7));
      -- Latin1 => UTF-8
      Print('¿'); Print("¡Hi!"); Print('?');
      for i in 1..20 loop
         Emit(Editor.scroll_down);
         delay 0.1;
      end loop;
      Emit(Cursor.show);
   end;
begin
   --test_01;
   test_02;
   Emit(reset_initial_state);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
