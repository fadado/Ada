------------------------------------------------------------------------
package Terminal.Screen is
------------------------------------------------------------------------
   ScreenHeight : constant Integer := 24;
   ScreenWidth  : constant Integer := 80;

   subtype Height is Integer range 1 .. ScreenHeight;
   subtype Width  is Integer range 1 .. ScreenWidth;

   procedure Reset;
   procedure Clear_Screen; 
   procedure Beep; 
   procedure Move_to(line: Height; column: Width);
end Terminal.Screen;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
