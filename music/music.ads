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
   function Inversion(x: PITCH; i: PITCH_INTERVAL) return PITCH
      with Inline;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
