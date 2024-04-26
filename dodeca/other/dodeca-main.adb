-- dodeca.adb

procedure Dodeca is
   pragma Optimize(Time);

   Chromatic_Notes: constant := 12;

   M: constant := Chromatic_Notes;
   -- Number of items to choose from

   N: constant := Chromatic_Notes;
   -- Vector solution size

   ---------------------------------------------------------------------
   -- Parameters
   ---------------------------------------------------------------------

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

   type INTERVAL is range 1..(CHOICE'Last-1);
   -- Constraining unique intervals

   Used_Notes     : array (CHOICE)   of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   -- Compute interval
   function last_interval(path:  in SOLUTION;
                          depth: in LEVEL;
                          item:  in CHOICE) return INTERVAL with Inline
   is
      item_up: CHOICE renames path(LEVEL'Pred(depth));
   begin
      return INTERVAL(abs(item - item_up));
   end;

   -- Reasons to prune
   function Reject(path:  in SOLUTION;
                   depth: in LEVEL;
                   item:  in CHOICE) return BOOLEAN is
   begin
      return  Used_Notes(item)
      or else Used_Intervals(last_interval(path, depth, item));
   end;

   -- Wrap the recursive calls
   procedure Enter(path:  in SOLUTION;
                   depth: in LEVEL;
                   item:  in CHOICE) is
   begin
      Used_Notes(item) := TRUE;
      if depth > LEVEL'First then
         Used_Intervals(last_interval(path, depth, item)) := TRUE;
      end if;
   end;

   procedure Leave(path:  in SOLUTION;
                   depth: in LEVEL;
                   item:  in CHOICE) is
   begin
      Used_Notes(item) := FALSE;
      if depth > LEVEL'First then
         Used_Intervals(last_interval(path, depth, item)) := FALSE;
      end if;
   end;

   ---------------------------------------------------------------------
   -- Walk the tree
   ---------------------------------------------------------------------

   path: SOLUTION; -- Vector with (partial) solution

   -- Try to add one step to the partial solution
   procedure Extend(depth: in LEVEL) is
   begin
      -- try to extend the solution with each choice
      for item in CHOICE loop
         if Reject(path, depth, item) then
            null; -- fail
         else
            path(depth) := item;
            if depth = LEVEL'Last then
               -- path is completed: one solution found
               Output(path);
            else
               -- descend one level
               Enter(path, depth, item);
               Extend(LEVEL'Succ(depth));
               Leave(path, depth, item);
            end if;
         end if;
      end loop;
   end Extend;

   procedure Walk is
   begin
      for item in CHOICE loop
         path(LEVEL'First) := item;

         Enter(path, LEVEL'First, item);
         Extend(LEVEL'Succ(LEVEL'First));
         Leave(path, LEVEL'First, item);
      end loop;
   end Walk;

begin

   Walk;

end Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
