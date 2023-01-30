-- demo.adb

with Terminal.Screen;
with Terminal.ANSI;

with Ada.Text_IO;

procedure demo is
   package VDU renames Terminal.Screen;
begin
   VDU.Reset;
   VDU.Move_To(1, 3);
   delay 0.5;
   Ada.Text_IO.Put("Hi!");
   delay 0.5;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
