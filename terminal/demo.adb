-- demo.adb

with Terminal.Screen;
with Terminal.ANSI;

with Ada.Text_IO;

procedure demo is
   package VDU    renames Terminal.Screen;
   package ANSI   renames Terminal.ANSI;
   package SGR    renames Terminal.ANSI.SGR;
   use ANSI;
begin
   VDU.Clear;
   VDU.Display(
      foreground(SGR.red),
      background(SGR.yellow),
      SGR.blink,
      SGR.bold,
      SGR.italic
   );
   VDU.Print(ANSI.cursor_hide);
   VDU.Move(1, 7);
   Ada.Text_IO.Put("Hi!");
   delay 2.0;
   VDU.Print(ANSI.reset);
   VDU.Reset;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
