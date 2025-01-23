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

 --function Inversion(x: PITCH; i: PITCH_INTERVAL) return PITCH is
 --begin
 --   return PITCH'Base(i) - x;
 --end Inversion;

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   -- Ordered pitch-class interval ("directed" interval)
   function Interval(x, y: PITCH_CLASS) return DIRECTED_INTERVAL is
   begin
      return DIRECTED_INTERVAL(y - x);
   end;

   -- Unordered pitch-class interval
   function "abs"(i: DIRECTED_INTERVAL) return INTERVAL_CLASS is
   begin
      return INTERVAL_CLASS(if i <= Tritone then i else 12-i);
   end;

   -- Transposition
   function Transposition(x: PITCH_CLASS; i: DIRECTED_INTERVAL) return PITCH_CLASS is
   begin
      return x + PITCH_CLASS(i);
   end;

   -- Inversion
   function Inversion(x: PITCH_CLASS; i: DIRECTED_INTERVAL) return PITCH_CLASS is
   begin
      return PITCH_CLASS(i) - x;
   end;

   -- Multiplication
   function Multiplication(x, y: PITCH_CLASS) return PITCH_CLASS is
   begin
      return (x * y);
   end;
end Music;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
