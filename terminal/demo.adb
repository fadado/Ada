-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal.Control;
   use Terminal.Device;

   procedure test_01 is
   begin
      Emit(Cursor.position & Display.erase_page);
      Emit(Tests.screen_alignment);
      delay 2.0;
   end;

   procedure test_02 is
      package S renames Format.Style;
   begin
      Emit(S.Apply(
         S.fgcolor(S.red),
         S.bgcolor(S.yellow),
         S.bold,
         S.italic));
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
      package S renames Format.Style;
   begin
      Emit(S.Apply(S.bgcolor(S.white), S.fgcolor(S.black)));
      Emit(Display.erase_page);
      Emit(Format.alternate_character_set);

      Emit(Cursor.position(1,1));
      Emit(D.upper_left_corner);
      Emit(repeat(D.horizontal_bar, 80-2));
      Emit(D.upper_right_corner);

      for i in 2..24 loop
         Emit(Cursor.position(i,1));  Emit(D.vertical_bar);
         Emit(Cursor.position(i,80)); Emit(D.vertical_bar);
      end loop;

      Emit(Cursor.position(25,1));
      Emit(D.lower_left_corner);
      Emit(repeat(D.horizontal_bar, 80-2));
      Emit(D.lower_right_corner);

      delay 5.5;
      Emit(Format.standard_character_set);
   end;

begin
   -- initialize
   Emit(Display.alternate_screen_buffer   &
        Display.seven_bits_controls       &
        Display.designate_character_sets  &
        Cursor.hide);
   --
   Emit(Display.window_title("testing control functions"));

 --test_01;
 --test_02;
 --test_03; Emit(Format.alternate_character_set); test_03; delay 5.0;
   test_04;

   -- finalize
   Emit(Display.normal_screen_buffer   &
        Cursor.show);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
