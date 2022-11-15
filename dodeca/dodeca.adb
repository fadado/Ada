--dodeca.adb

with Ada.Text_IO;
use  Ada.Text_IO;

procedure DODECA is
   SERIES_LENGTH : constant := 12;

   type Note_Type     is range 1..SERIES_LENGTH;
   type Interval_Type is range 1..SERIES_LENGTH-1;
   type Level_Type    is range 1..SERIES_LENGTH;

   type Solution_Type is array (Level_Type) of Note_Type;

   -- Notes and intervals not yet used
   Notes     : array (Note_Type) of Boolean := (others => True);
   Intervals : array (Interval_Type) of Boolean := (others => True);

   -- Vector with solution
   Solution  : Solution_Type;

   procedure Advance(level: Level_Type) is
      interval : Interval_Type;
   begin
      for note in Note_Type loop
         -- is note free to take?
         if Notes(note) then
            interval := Interval_Type(abs (note - Solution(level-1)));
            -- is interval free to take?
            if Intervals(interval) then
               Solution(level) := note;
               if level = SERIES_LENGTH then
                  -- one complete solution found
                  for note of Solution loop
                     put(note'Image);
                  end loop;
                  New_Line;
               else
                  Notes(note)         := False;
                  Intervals(interval) := False;
                  Advance(level + 1);
                  Intervals(interval) := True;
                  Notes(note)         := True;
               end if;
            end if;
         end if;
      end loop;
   end Advance;
begin
   for note in Note_Type loop
      Solution(1) := note;
      Notes(note) := False;
      Advance(2);
      Notes(note) := True;
   end loop;
end DODECA;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
