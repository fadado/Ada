package body Music is
   ---------------------------------
   -- Encoded pitch and intervals --
   ---------------------------------

   function Interval(x, y: PITCH) return PITCH_INTERVAL is
   begin
      return PITCH_INTERVAL(y - x);
   end Interval;

   -- Unordered pitch interval:
   --     abs Interval(x, y)
   -- function "abs"(i: PITCH_INTERVAL) return UNORDERED_INTERVAL;

   function Transposition(x: PITCH; i: PITCH_INTERVAL) return PITCH is
   begin
      return x + PITCH'Base(i);
   end Transposition;

   function Inversion(x: PITCH; i: PITCH_INTERVAL) return PITCH is
   begin
      return PITCH'Base(i) - x;
   end Inversion;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
