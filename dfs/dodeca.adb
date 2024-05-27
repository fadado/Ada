-- dodeca.adb

with DFS;

procedure Dodeca is
   pragma Optimize(Time);

   ---------------------------------------------------------------------
   -- Client side types
   ---------------------------------------------------------------------

   type TONE is range 1..12;
   -- 12 chromatic notes

   type ORDER is range 1..12;
   -- Position inside the serie

   type TONE_ROW is array (ORDER) of TONE;
   -- dodecaphonic serie

   package Printer is
      procedure On is null;
      procedure Off is null;
      procedure Output(serie: TONE_ROW);
   end Printer;
   package body Printer is separate;
   -- Called for each solution

   ---------------------------------------------------------------------
   -- Constraints
   ---------------------------------------------------------------------

   type INTERVAL is range 1..11;
   -- Constraining unique intervals

   Used_Tones     : array (TONE)     of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   -- Compute interval
   function last_interval (
         path  : TONE_ROW;
         depth : ORDER;
         item  : TONE
     )
     return INTERVAL
     with Inline
   is
      -- previous tone
      item_up : TONE renames path(ORDER'Pred(depth));
   begin
      return INTERVAL(abs(item - item_up));
   end;

   -- Reasons to prune
   function Reject (
         path  : TONE_ROW;
         depth : ORDER;
         item  : TONE
     )
     return BOOLEAN is
   begin
      return  Used_Tones(item)
      or else Used_Intervals(last_interval(path, depth, item));
   end;

   -- Wrap the recursive calls
   procedure Enter (
         path  : TONE_ROW;
         depth : ORDER;
         item  : TONE
     ) is
   begin
      Used_Tones(item) := TRUE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := TRUE;
      end if;
   end;

   procedure Leave (
         path  : TONE_ROW;
         depth : ORDER;
         item  : TONE
     ) is
   begin
      Used_Tones(item) := FALSE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := FALSE;
      end if;
   end;

begin
   ---------------------------------------------------------------------
   -- Generate all panintervalic twelve-tone tone-rows
   ---------------------------------------------------------------------
   declare
      package Dodecaphonic_Panintervalic_Series is
         new DFS (
           CHOICE   => TONE,
           LEVEL    => ORDER,
           SOLUTION => TONE_ROW,
           Output   => Printer.Output
         --Reject   => Reject,
         --Enter    => Enter,
         --Leave    => Leave
         );
   begin
      Printer.On;
      Dodecaphonic_Panintervalic_Series.Search;
      Printer.Off;
   end;

end Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
