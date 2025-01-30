package body Music is
   ---------------------------------
   -- Encoded pitch and intervals --
   ---------------------------------

   function Distance(x, y: PITCH) return PITCH_INTERVAL is
   begin
      return PITCH_INTERVAL(y - x);
   end Distance;

   function Transposition(x: PITCH; i: PITCH_INTERVAL) return PITCH is
   begin
      return x + PITCH'Base(i);
   end Transposition;

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   -- Ordered pitch-class interval ("directed" interval)
   function Distance(x, y: PITCH_CLASS) return PC_INTERVAL is
   begin
      return y - x;
   end;

   function Transposition(x: PITCH_CLASS; i: PC_INTERVAL) return PITCH_CLASS is
   begin
      return x + i;
   end;

   function Inversion(x: PITCH_CLASS; i: PC_INTERVAL) return PITCH_CLASS is
   begin
      return i - x;
   end;

   -- Unordered pitch-class interval
   function "abs"(i: PC_INTERVAL) return INTERVAL_CLASS is
   begin
      return INTERVAL_CLASS(if i < 7 then i else -i);
   end;
end Music;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
