-- dodeca.adb

with Ada.Text_IO;

procedure DODECA is
   SIZE : constant := 12;

   -- Set of available choices
   type Choice_Type   is range 1..SIZE;

   -- Node's level in the decision tree
   type Level_Type    is range 1..SIZE;

   -- Ordered set of choices
   type Solution_Type is array (Level_Type) of Choice_Type;

   --
   task Series is
      entry Next(solution : out Solution_Type);
   end;
   task body Series is
      -- Vector with current (partial) solution
      Solution : Solution_Type;

      -- Sets of notes and intervals used
      type Interval_Type is range 1..SIZE-1;
      Used_Notes     : array (Choice_Type) of Boolean := (others => False);
      Used_Intervals : array (Interval_Type) of Boolean := (others => False);

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
      procedure Extend(level: Level_Type);

      procedure Solve is
         -- start on level 1
         level : constant := 1;
         -- to wrap the recursive call
         procedure enter(choice: Choice_Type) with Inline is
         begin
            Used_Notes(choice) := True;
         end;
         procedure leave(choice: Choice_Type) with Inline is
         begin
            Used_Notes(choice) := False;
         end;
      begin
         -- try starting with each choice
         for choice in Choice_Type loop
            Solution(level) := choice;
            enter(choice);
            Extend(level => level+1);
            leave(choice);
         end loop;
      end Solve;

      procedure Extend(level: Level_Type) is
         -- `choice` corresponds to current level
         function interval(choice: Choice_Type) return Interval_Type with Inline is
         begin
            return Interval_Type(abs (choice - Solution(level-1)));
         end;
         -- reasons to prune
         function reject(choice: Choice_Type) return Boolean with Inline is
         begin
            return Used_Notes(choice) or else Used_Intervals(interval(choice));
         end;
         -- to wrap the recursive call
         procedure enter(choice: Choice_Type) with Inline is
         begin
            Used_Notes(choice) := True;
            Used_Intervals(interval(choice)) := True;
         end;
         procedure leave(choice: Choice_Type) with Inline is
         begin
            Used_Intervals(interval(choice)) := False;
            Used_Notes(choice) := False;
         end;
      begin
         -- try to extend solution with each choice
         for choice in Choice_Type loop
            if reject(choice) then
               null; -- fail
            else
               Solution(level) := choice;
               if level = SIZE then
                  Output;
--                  accept Next(solution : out Solution_Type) do
--                     solution := Series.Solution;
--                  end Next;
               else
                  enter(choice);
                  Extend(level => level+1);
                  leave(choice);
               end if;
            end if;
         end loop;
      end Extend;
   begin
      Solve;
   end Series;
begin
   null;
end DODECA;

   -- ¡ISO-8859-1!
   -- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
   -- vim:fileformat=dos:fileencoding=latin1:syntax=ada
