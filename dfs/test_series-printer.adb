-- test_series-output.adb

with Ada.Text_IO;
use  Ada.Text_IO;

separate (test_Series)
task body Printer is
   series : TONE_ROW; -- local reference: Printer.series
begin
   loop
      select
         accept Output(series : TONE_ROW) do
            Printer.series := series;
         end Output;
         for t of series loop
            if t < 10 then
               Put(t'Image);
            elsif t = 10 then
               Put(" A");
            elsif t = 11 then
               Put(" B");
            end if;
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
