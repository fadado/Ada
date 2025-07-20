-- test_dodeca-output.adb

with Ada.Text_IO;

separate (test_Dodeca)
task body Printer is
   use Ada.Text_IO;

   series : TONE_ROW; -- local reference: Printer.series
begin
   loop
      select
         accept Output(series : TONE_ROW) do
            Printer.series := series;
         end Output;
         for t of series loop
            Put(t'Image);
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
