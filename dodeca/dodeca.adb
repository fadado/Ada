--dodeca.adb


with Ada.Text_IO;
use  Ada.Text_IO;

procedure DODECA is
   N : constant := 3;   -- 12 when debuged!

   type Note_Type     is range 1..N;
   type Interval_Type is range 1..N-1;

   notes     : array (Note_Type) of Boolean := (others => False);
   intervals : array (Interval_Type) of Boolean := (others => False);

   interval  : Interval_Type;

   subtype Level_Type is Integer range 1..N;
   series : array (Level_Type) of Note_Type;

   procedure Advance(level: Level_Type) is
   begin
      for note in Note_Type loop
         if not notes(note) then
            notes(note) := True;
            interval := Interval_Type(abs (note - series(level-1)));
            if not intervals(interval) then
               intervals(interval) := True;
               series(level) := note;

               if level < N then
                  Advance(level + 1);
               else
                  -- one complete series found
                  for note of series loop
                     put(note'Image);
                  end loop;
                  New_Line;
               end if;

               intervals(interval) := False;
            end if;
            notes(note) := False;
         end if;
      end loop;
   end Advance;
begin
   for note in Note_Type loop
      notes(note) := True;
      series(1) := note;
      Advance(2);
      notes(note) := False;
   end loop;
end DODECA;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
