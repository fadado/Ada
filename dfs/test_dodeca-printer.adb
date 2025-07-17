-- test_dodeca-output.adb

with Ada.Text_IO;

separate (test_Dodeca)
task body Printer is
   use Ada.Text_IO;

   serie : TONE_ROW;
begin
   loop
      select
         accept Output(serie : TONE_ROW) do
            Printer.serie := serie;
         end Output;
         for tone of serie loop
            Put(tone'Image);
         end loop;
         New_Line;
      or
         terminate;
      end select;
   end loop;
end Printer;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
