-- dodeca.adb

with Ada.Text_IO;

procedure DODECA is
   SIZE : constant := 12;

   -- Set of available choices
   type Choice_Type   is range 1..SIZE;

   -- Node's level in the in decision tree
   type Level_Type    is range 1..SIZE;

   -- Ordered set of choices
   type Solution_Type is array (Level_Type) of Choice_Type;

   -- Sets of notes and intervals used
   type Interval_Type is range 1..SIZE-1;
   Used_Notes     : array (Choice_Type) of Boolean := (others => False);
   Used_Intervals : array (Interval_Type) of Boolean := (others => False);

   -- Vector with current (partial) solution
   Solution : Solution_Type;

   -- Output current complete solution
   procedure Output is
      use Ada.Text_IO;
   begin
      for choice of Solution loop
         Put(choice'Image);
      end loop;
      New_Line;
   end Output;

   -- Solve the search problem
   procedure Solve(level: Level_Type);

   procedure Solve is
   begin
      for choice in Choice_Type loop
         Solution(1) := choice;
         Used_Notes(choice) := True;
         Solve(2);
         Used_Notes(choice) := False;
      end loop;
   end Solve;

   procedure Solve(level: Level_Type) is
      interval : Interval_Type;
   begin
      for choice in Choice_Type loop
         if Used_Notes(choice) then
            null;    -- fail
         else
            interval := Interval_Type(abs (choice - Solution(level-1)));
            if Used_Intervals(interval) then
               null; -- fail
            else
               Solution(level) := choice;
               Used_Notes(choice) := True;
               Used_Intervals(interval) := True;
               if level = SIZE then
                  Output;
               else
                  Solve(level + 1);
               end if;
               Used_Intervals(interval) := False;
               Used_Notes(choice) := False;
            end if;
         end if;
      end loop;
   end Solve;
begin
   Solve;
end DODECA;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
