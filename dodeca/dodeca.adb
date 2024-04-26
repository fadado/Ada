-- dodeca.adb

procedure Dodeca is
   pragma Optimize(Time);

   ---------------------------------------------------------------------
   -- Walk the tree
   ---------------------------------------------------------------------

   generic
      type CHOICE is (<>);
      -- Set of available choices

      type LEVEL is (<>);
      -- Search tree levels

      type SOLUTION is array (LEVEL) of CHOICE;
      -- Vector of choices

      with procedure Output(goal: SOLUTION) is <>;
      -- Called for each solution found

      with function Reject(path: SOLUTION; depth: LEVEL; item: CHOICE)
         return BOOLEAN is <>;
      -- Check constraints for the current node

      with procedure Enter(path: SOLUTION; depth: LEVEL; item: CHOICE) is <>;
      -- Hook to run before entering one level down

      with procedure Leave(path: SOLUTION; depth: LEVEL; item: CHOICE) is <>;
      -- Hook to run after exiting one level down

   package DFS is
      procedure Search;
   end DFS;

   package body DFS is

      path: SOLUTION; -- Vector with (partial) solution

      -- Try to add one step to the partial solution
      procedure Extend(depth: LEVEL) is
      begin
         -- try to extend the solution with each choice
         for item in CHOICE loop
            if not Reject(path, depth, item) then
               -- accept item for the current level
               path(depth) := item;
               -- if path is completed: one solution found
               if depth = LEVEL'Last then
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

      procedure Search is
      begin
         for item in CHOICE loop
            path(LEVEL'First) := item;

            Enter(path, LEVEL'First, item);
            Extend(LEVEL'Succ(LEVEL'First));
            Leave(path, LEVEL'First, item);
         end loop;
      end Search;

   end DFS ;

   ---------------------------------------------------------------------
   -- Client side types
   ---------------------------------------------------------------------

   type TONE is range 1..12;
   -- 12 chromatic notes

   type ORDER is range 1..12;
   -- Position inside the serie

   type TONE_ROW is array (ORDER) of TONE;
   -- dodecaphonic serie

   procedure Output(serie: TONE_ROW) is separate;
   -- Called for each solution

   ---------------------------------------------------------------------
   -- Constraints
   ---------------------------------------------------------------------

   type INTERVAL is range 1..11;
   -- Constraining unique intervals

   Used_Notes     : array (TONE)     of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   -- Compute interval
   function last_interval(path:  TONE_ROW;
                          depth: ORDER;
                          item:  TONE) return INTERVAL with Inline
   is
      -- previous tone
      item_up: TONE renames path(ORDER'Pred(depth));
   begin
      return INTERVAL(abs(item - item_up));
   end;

   -- Reasons to prune
   function Reject(path:  TONE_ROW;
                   depth: ORDER;
                   item:  TONE) return BOOLEAN is
   begin
      return  Used_Notes(item)
      or else Used_Intervals(last_interval(path, depth, item));
   end;

   -- Wrap the recursive calls
   procedure Enter(path:  TONE_ROW;
                   depth: ORDER;
                   item:  TONE) is
   begin
      Used_Notes(item) := TRUE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := TRUE;
      end if;
   end;

   procedure Leave(path:  TONE_ROW;
                   depth: ORDER;
                   item:  TONE) is
   begin
      Used_Notes(item) := FALSE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := FALSE;
      end if;
   end;

begin
   ---------------------------------------------------------------------
   -- Generate all panintervalic twelve-tone tone-rows
   ---------------------------------------------------------------------
   declare
      package Dodecaphonic_Panintervalic_Series is new DFS (
         CHOICE   => TONE,
         LEVEL    => ORDER,
         SOLUTION => TONE_ROW
       --Output   => Dodeca.Output,
       --Reject   => Dodeca.Reject,
       --Enter    => Dodeca.Enter,
       --Leave    => Dodeca.Leave
      );
   begin
      Dodecaphonic_Panintervalic_Series.Search;
   end;

end Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
