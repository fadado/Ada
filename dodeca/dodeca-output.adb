-- dodeca-output.adb

with Ada.Text_IO;

separate (Dodeca)
procedure Output(Solved: SOLUTION)
is
   use Ada.Text_IO;
begin
   for item of Solved loop
      Put(item'Image);
   end loop;
   New_Line;
end;


-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
