-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal.Control;
   use Terminal.Device;

   procedure test_01 is
   begin
      Emit(Cursor.hide);
      Emit(Cursor.position & Display.erase_page);
      Emit(screen_alignment_test);
      delay 2.0;
      Emit(Cursor.show);
   end;

   procedure test_02 is
      package S renames Format.Style;
   begin
      Emit(S.Apply(
         S.fgcolor(S.red),
         S.bgcolor(S.yellow),
         S.bold,
         S.italic));
      Emit(Cursor.hide);
      Emit(Cursor.position & Display.erase_page);
      Emit(Cursor.position(1,7));
      Emit('=' & repeat(79));
      -- Latin1 => UTF-8
      Emit(Cursor.position(2,7));
      Print('¿'); Print("¡Hi!"); Print('?');
      for i in 1..20 loop
         Emit(Display.scroll_down);
         delay 0.1;
      end loop;
      Emit(Cursor.show);
   end;
begin
   Emit(window_title("testing control functions"));
   --test_01;
   test_02;
   Emit(reset_initial_state);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
