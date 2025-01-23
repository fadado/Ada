package Music is
   ---------------------------------
   -- Encoded pitch and intervals --
   ---------------------------------

   type    PITCH is range 0..127;
   type    PITCH_INTERVAL is range -127..+127;
   subtype UNORDERED_INTERVAL is PITCH_INTERVAL range 0..127;

   -- Ordered pitch interval
   function Interval(x, y: PITCH) return PITCH_INTERVAL
      with Inline;

   -- Unordered pitch interval:
   --     abs Interval(x, y)
   -- function "abs"(i: PITCH_INTERVAL) return UNORDERED_INTERVAL;

   -- Pich transposition
   function Transposition(x: PITCH; i: PITCH_INTERVAL) return PITCH
      with Inline;

   -- Pitch inversion
 --function Inversion(x: PITCH; i: PITCH_INTERVAL) return PITCH
 --   with Inline;

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   type PITCH_CLASS is mod 12;
   type DIRECTED_INTERVAL is range 0..11;
   type INTERVAL_CLASS is mod 7;

   -- Standard interval names
   Unison : constant DIRECTED_INTERVAL := 0;
   Minor2 : constant DIRECTED_INTERVAL := 1;
   Major2 : constant DIRECTED_INTERVAL := 2;
   Minor3 : constant DIRECTED_INTERVAL := 3;
   Major3 : constant DIRECTED_INTERVAL := 4;
   Fourth : constant DIRECTED_INTERVAL := 5;
   Tritone: constant DIRECTED_INTERVAL := 6;
   Fifth  : constant DIRECTED_INTERVAL := 7;
   Minor6 : constant DIRECTED_INTERVAL := 8;
   Major6 : constant DIRECTED_INTERVAL := 9;
   Minor7 : constant DIRECTED_INTERVAL := 10;
   Major7 : constant DIRECTED_INTERVAL := 11;

   -- Obtain pitch-class from a pich
   --   PITCH_CLASS'Mod(x)

   function Interval(x, y: PITCH_CLASS) return DIRECTED_INTERVAL
      with Inline;

   function "abs"(i: DIRECTED_INTERVAL) return INTERVAL_CLASS
      with Inline;

   function Transposition(x: PITCH_CLASS; i: DIRECTED_INTERVAL) return PITCH_CLASS
      with Inline;

   function Inversion(x: PITCH_CLASS; i: DIRECTED_INTERVAL) return PITCH_CLASS
      with Inline;

   function Multiplication(x, y: PITCH_CLASS) return PITCH_CLASS
      with Inline;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
