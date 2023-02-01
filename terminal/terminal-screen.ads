------------------------------------------------------------------------
package Terminal.Screen is
------------------------------------------------------------------------
   ScreenHeight : constant Integer := 24;
   ScreenWidth  : constant Integer := 80;

   subtype Height is Integer range 1 .. ScreenHeight;
   subtype Width  is Integer range 1 .. ScreenWidth;

   procedure Reset;
   procedure Clear; 
   procedure Beep; 
   procedure Move(line: Height; column: Width);
   procedure Display(s0: STRING) with Inline;
   procedure Display(s0, s1: STRING) with Inline;
   procedure Display(s0, s1, s2: STRING) with Inline;
   procedure Display(s0, s1, s2, s3: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4, s5: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4, s5, s6: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7, s8: STRING) with Inline;
   procedure Display(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9: STRING) with Inline;

   procedure Print with Inline;
   procedure Print(Item: CHARACTER) with Inline;
   procedure Print(Item: STRING) with Inline;
   procedure Print(Item: INTEGER; Width: INTEGER := 1) with Inline;
end Terminal.Screen;   
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
