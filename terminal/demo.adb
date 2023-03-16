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
      Emit(Cursor.position(1,1));
      Emit(repeat('=', 80));
      -- Latin1 => UTF-8
      Emit(Cursor.position(2,7));
      Print('¿'); Print("¡Hi!"); Print('?');
      for i in 1..20 loop
         Emit(Display.scroll_down);
         delay 0.1;
      end loop;
      Emit(Cursor.show);
   end;

   procedure test_03 is
      space : constant CHARACTER := ' ';
      tilde : constant CHARACTER := '~';
   begin
      for c in space .. tilde loop
         declare
            ord : constant POSITIVE := CHARACTER'Pos(c);
         begin
            if ord < 100 then
               Emit(' ');
            end if;
            Emit(ord'Image & ' ' & c);
            if (ord+1) mod 16 = 0 then
               Emit(Format.next_line);
            end if;
         end;
      end loop;
      Emit(Format.next_line & Format.next_line);
   end;

   procedure test_04 is
      package D renames Format.DEC;
   begin
      Emit(Cursor.hide);
      Emit(Display.erase_page);
      Emit(Format.shift_out);

      Emit(Cursor.position(1,1)); Emit(D.upper_left_corner);
      Emit(repeat(D.horizontal_bar, 80-2));
      Emit(D.upper_right_corner);

      for i in 2..24 loop
         Emit(Cursor.position(i,1));  Emit(D.vertical_bar);
         Emit(Cursor.position(i,80)); Emit(D.vertical_bar);
      end loop;

      Emit(Cursor.position(25,1));  Emit(D.lower_left_corner);
      Emit(repeat(D.horizontal_bar, 80-2));
      Emit(D.lower_right_corner);

      Emit(Format.shift_in);
      delay 5.5;
      Emit(Cursor.show);
   end;

begin
   Emit(Format.designate_gs);

   Emit(window_title("testing control functions"));

 --test_01;
 --test_02;
 --test_03; Emit(Format.shift_out); test_03; delay 5.0;
   test_04;

   Emit(reset_initial_state);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
