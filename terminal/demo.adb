-- demo.adb

with Terminal.Control;
with Terminal.Device;

procedure demo is
   use Terminal;
   use Terminal.Control;

   procedure test_01 is
   begin
      Device.Emit(Cursor.move & Display.erase_page);
      Device.Emit(Setup.screen_alignment_test);
      delay 2.0;
   end;

   procedure test_02 is
      package S renames Format.Style;
   begin
      Device.Emit(S.Render(
         S.fgcolor(S.red),
         S.bgcolor(S.yellow),
         S.bold,
         S.italic));
      Device.Emit(Cursor.move & Display.erase_page);
      Device.Emit(Cursor.move(1,1));
      Device.Emit(repeat('=', 80));
      -- Latin1 => UTF-8
      Device.Emit(Cursor.move(2,7));
      Device.Print('¿'); Device.Print("¡Hi!"); Device.Print('?');
      for i in 1..20 loop
         Device.Emit(Display.scroll_down);
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
               Device.Emit(' ');
            end if;
            Device.Emit(ord'Image & ' ' & c);
            if (ord+1) mod 16 = 0 then
               Device.Emit(Format.new_line);
            end if;
         end;
      end loop;
      Device.Emit(Format.new_line & Format.new_line);
   end;

   procedure test_04 is
      package D renames Format.DEC;
      package S renames Format.Style;
      fc : STRING := S.fgcolor(S.yellow, S.bright);
      bc : STRING := S.bgcolor(S.cyan,   S.dimmed);
   begin
      -- black, red, green, yellow, blue, magenta, cyan, white
      Device.Emit(S.Render(fc, bc));
      Device.Emit(Display.erase_page);
      Device.Emit(Format.alternate_character_set);

      Device.Emit(Cursor.move(1,1));
      Device.Emit(D.upper_left_corner);
      Device.Emit(repeat(D.horizontal_bar, 80-2));
      Device.Emit(D.upper_right_corner);

      for i in 2..24 loop
         Device.Emit(Cursor.move(i,1));  Device.Emit(D.vertical_bar);
         Device.Emit(Cursor.move(i,80)); Device.Emit(D.vertical_bar);
      end loop;

      Device.Emit(Cursor.move(25,1));
      Device.Emit(D.lower_left_corner);
      Device.Emit(repeat(D.horizontal_bar, 80-2));
      Device.Emit(D.lower_right_corner);
      delay 5.5;
      Device.Emit(Format.standard_character_set);
   end;

begin
   -- initialize
   Device.Emit(Setup.alternate_screen(On)     &
               Setup.seven_bits_controls      &
               Setup.designate_character_sets &
               Cursor.visible(Off));
   --
   Device.Emit(Display.window_title("testing control functions"));

 --test_01;
 --test_02;
 --test_03; Device.Emit(Format.alternate_character_set); test_03; delay 5.0;
   test_04;

   -- finalize
   Device.Emit(Setup.alternate_screen(Off)   &
               Cursor.visible(On));
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
