-- Generates all-interval twelve-tone rows
--    https://en.wikipedia.org/wiki/All-interval_twelve-tone_row

with Backtracker;

procedure test_Dodeca is

   ---------------------------------------------------------------------
   -- Client side types
   ---------------------------------------------------------------------

   type TONE is range 1..12;
   -- 12 chromatic notes

   type ORDER is range 1..12;
   -- Position inside the series

   type TONE_ROW is array (ORDER) of TONE;
   -- dodecaphonic series

   task Printer is
      entry Output(series: TONE_ROW);
   end Printer;

   task body Printer is separate;
   -- Accept calls to print series

   ---------------------------------------------------------------------
   -- Constraints
   ---------------------------------------------------------------------

   type INTERVAL is range 1..11;
   -- Constraining unique intervals

   Used_Tones     : array (TONE)     of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   -- Compute last interval
   function last_interval
     (series : TONE_ROW;
      index  : ORDER;
      note   : TONE) return INTERVAL
   with Inline
   is
      note_up : TONE renames series(ORDER'Pred(index));
      -- previous tone
   begin
      return INTERVAL(abs(note - note_up));
   end;

   -- Reasons to prune
   function Rejected
     (series : TONE_ROW;
      index  : ORDER;
      note   : TONE) return BOOLEAN
   is
   begin
      if index = ORDER'First then
         return FALSE;
      elsif Used_Tones(note) then
         return TRUE;
      elsif Used_Intervals(last_interval(series, index, note)) then
         return TRUE;
      else
         return FALSE;
      end if;
   end;

   -- Wrap the recursive calls
   procedure Enter
     (series : TONE_ROW;
      index  : ORDER;
      note   : TONE)
   is
   begin
      Used_Tones(note) := TRUE;
      if index > ORDER'First then
         Used_Intervals(last_interval(series, index, note)) := TRUE;
      end if;
   end;

   procedure Leave
     (series : TONE_ROW;
      index  : ORDER;
      note   : TONE)
   is
   begin
      Used_Tones(note) := FALSE;
      if index > ORDER'First then
         Used_Intervals(last_interval(series, index, note)) := FALSE;
      end if;
   end;

begin
   ---------------------------------------------------------------------
   -- Generate all panintervalic twelve-tone tone-rows
   ---------------------------------------------------------------------
   declare
      package Dodecaphonic_Panintervalic_Series is
         new Backtracker (
           NODE_VALUE      => TONE,
           VECTOR_INDEX    => ORDER,
           VECTOR_SOLUTION => TONE_ROW,
           Found           => Printer.Output,
           Rejected        => Rejected,
           Enter           => Enter,
           Leave           => Leave
         );
   begin
      Dodecaphonic_Panintervalic_Series.Traverse;
   end;

end test_Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
