-- dodeca.adb

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
   function last_interval(path:  in TONE_ROW;
                          depth: in ORDER;
                          item:  in TONE) return INTERVAL with Inline
   is
      -- previous tone
      item_up: TONE renames path(ORDER'Pred(depth));
   begin
      return INTERVAL(abs(item - item_up));
   end;

   -- Reasons to prune
   function Reject(path:  in TONE_ROW;
                   depth: in ORDER;
                   item:  in TONE) return BOOLEAN is
   begin
      return  Used_Notes(item)
      or else Used_Intervals(last_interval(path, depth, item));
   end;

   -- Wrap the recursive calls
   procedure Enter(path:  in TONE_ROW;
                   depth: in ORDER;
                   item:  in TONE) is
   begin
      Used_Notes(item) := TRUE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := TRUE;
      end if;
   end;

   procedure Leave(path:  in TONE_ROW;
                   depth: in ORDER;
                   item:  in TONE) is
   begin
      Used_Notes(item) := FALSE;
      if depth > ORDER'First then
         Used_Intervals(last_interval(path, depth, item)) := FALSE;
      end if;
   end;

   ---------------------------------------------------------------------
   -- Walk the tree
   ---------------------------------------------------------------------

   subtype CHOICE is TONE;
   -- Set of available choices

   subtype LEVEL is ORDER;
   -- Search tree levels

   subtype SOLUTION is TONE_ROW;
   -- Ordered set of choices

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
