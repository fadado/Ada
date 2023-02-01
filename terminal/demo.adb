-- demo.adb

with Terminal.Screen;
with Terminal.Control;

with Ada.Text_IO;

procedure demo is
   package screen  renames Terminal.Screen;
   package Control renames Terminal.Control;
   package SGR     renames Terminal.Control.SGR;
   use Terminal;
   use Control;
begin
   Emit(erase_display);
   Emit(attributes(
         foreground(SGR.red),
         background(SGR.yellow),
         SGR.blink,
         SGR.bold,
         SGR.italic)
   );
   Emit(cursor_hide);
   Emit(cursor_move(1, 7));

   screen.Print("Hi!");
   delay 2.0;

   Emit(reset_device);
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
