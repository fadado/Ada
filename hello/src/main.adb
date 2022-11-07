with Ada.Text_IO;

procedure Main is
   procedure print(line: String) renames Ada.Text_IO.Put_Line;
begin
   print("¡Hello, José!");
end Main;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
