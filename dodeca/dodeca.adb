-- dodeca.adb

procedure Dodeca is
   pragma Optimize(Time);

   Chromatic_Notes: constant := 12;

   M: constant := Chromatic_Notes;
   -- Number of items to choose

   N: constant := Chromatic_Notes;
   -- Solution vector size

   type LEVEL is range 1..N;
   -- Search tree levels

   type CHOICE is range 1..M;
   -- Set of available choices

   type SOLUTION is array (LEVEL) of CHOICE;
   -- Ordered set of choices

   procedure Output(Solved: SOLUTION) is separate;
   -- Called for each solution

   ---------------------------------------------------------------------
   -- Constraints
   ---------------------------------------------------------------------

   type INTERVAL is range 1..(N-1);
   -- Constraining unique intervals

   Used_Notes     : array (CHOICE)   of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   Path: SOLUTION;
   -- Vector with (partial) solution

   ---------------------------------------------------------------------
   -- Walk the tree
   ---------------------------------------------------------------------

   -- Try to add one step to the partial solution
   procedure Descend(L: in LEVEL) is
      -- compute interval
      function last_interval(item: in CHOICE) return INTERVAL is
         item_up: CHOICE renames Path(LEVEL'Pred(L));
      begin
         return INTERVAL(abs(item - item_up));
      end;

      -- reasons to prune
      function reject(item: in CHOICE) return BOOLEAN is
      begin
         return  Used_Notes(item)
         or else Used_Intervals(last_interval(item));
      end;

      -- to wrap the recursive calls
      procedure enter(item: in CHOICE) is
      begin
         Used_Notes(item) := TRUE;
         Used_Intervals(last_interval(item)) := TRUE;
      end;

      procedure leave(item: in CHOICE) is
      begin
         Used_Intervals(last_interval(item)) := FALSE;
         Used_Notes(item) := FALSE;
      end;

   begin
      -- try to extend the solution with each choice
      for item in CHOICE loop
         if reject(item) then
            null; -- fail
         else
            Path(L) := item;
            if L = LEVEL'Last then
               -- Path is completed: one solution found
               Output(Path);
            else
               -- descend one level
               enter(item);
               Descend(LEVEL'Succ(L));
               leave(item);
            end if;
         end if;
      end loop;
   end Descend;

   procedure Walk is
      root: constant := LEVEL'First;
   begin
      for item in CHOICE loop
         Path(root) := item;
         Used_Notes(item) := TRUE;
         Descend(LEVEL'Succ(root));
         Used_Notes(item) := FALSE;
      end loop;
   end Walk;

begin
   Walk;
end Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
