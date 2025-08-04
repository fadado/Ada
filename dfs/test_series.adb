-- Generates all-interval twelve-tone rows
--    https://en.wikipedia.org/wiki/All-interval_twelve-tone_row

pragma Assertion_Policy(Ignore); -- Check / Ignore

with Backtracker;

procedure test_Series is
   type PITCH_CLASS is mod 12;
   -- 12 chromatic tones

   type TUPLE_INDEX is range 1..12;
   -- Position inside the series

   type TONE_ROW is array (TUPLE_INDEX) of PITCH_CLASS;
   -- dodecaphonic series

   type INTERVAL is range 1..11;
   -- Constraining unique intervals

   Used_Tones     : array (PITCH_CLASS) of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of tones and intervals in use

   -- Compute last interval
   function last_interval
     (series : TONE_ROW;
      index  : TUPLE_INDEX;
      tone   : PITCH_CLASS) return INTERVAL
   with Inline
   is
      tone_up : PITCH_CLASS renames series(TUPLE_INDEX'Pred(index));
      -- previous tone
   begin
      -- ordered pitch-class interval
      return INTERVAL(tone_up - tone);

    -- buggy old version using unordered pitch interval
    --if tone > tone_up then
    --   return INTERVAL(tone - tone_up);
    --else
    --   return INTERVAL(tone_up - tone);
    --end if;
   end;

   -- Reasons to prune?
   function Rejected
     (series : TONE_ROW;
      index  : TUPLE_INDEX;
      tone   : PITCH_CLASS) return BOOLEAN
   is
   begin
      pragma Assert(index > TUPLE_INDEX'First);

      if Used_Tones(tone) then
         return TRUE;
      elsif Used_Intervals(last_interval(series, index, tone)) then
         return TRUE;
      else
         return FALSE;
      end if;
   end;

   -- Hook-before when tone is accepted at index in series
   procedure Enter
     (series : TONE_ROW;
      index  : TUPLE_INDEX;
      tone   : PITCH_CLASS)
   is
   begin
      Used_Tones(tone) := TRUE;
      if index > TUPLE_INDEX'First then
         Used_Intervals(last_interval(series, index, tone)) := TRUE;
      end if;
   end;

   -- Hook-after when tone is accepted at index in series
   procedure Leave
     (series : TONE_ROW;
      index  : TUPLE_INDEX;
      tone   : PITCH_CLASS)
   is
   begin
      Used_Tones(tone) := FALSE;
      if index > TUPLE_INDEX'First then
         Used_Intervals(last_interval(series, index, tone)) := FALSE;
      end if;
   end;

   -- Accept calls to print series
   task Printer is
      entry Output(series: TONE_ROW);
   end Printer;

   task body Printer is separate;

begin
   declare
      package All_Intervals_Twelve_Tone_Rows is
         new Backtracker (
           NODE_VALUE      => PITCH_CLASS,
           VECTOR_INDEX    => TUPLE_INDEX,
           VECTOR_SOLUTION => TONE_ROW,
           Found           => Printer.Output,
           Rejected        => Rejected,
           Enter           => Enter,
           Leave           => Leave
         );
      use All_Intervals_Twelve_Tone_Rows;
   begin
    --Traverse(FOREST_SET'(0 => TRUE, others => FALSE));
      --
      Traverse;
   end;
end test_Series;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
