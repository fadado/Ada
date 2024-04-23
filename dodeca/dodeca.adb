-- dodeca.adb

procedure Dodeca is
   pragma Optimize(Time);

   Chromatic_Notes: constant := 12;

   type CHOICE is range 1 .. Chromatic_Notes;
   -- Set of available choices

   type LEVEL is new CHOICE;
   -- Search tree level

   type SOLUTION is array (LEVEL) of CHOICE;
   -- Ordered set of choices

   procedure Output(Full: SOLUTION) is separate;
   -- Called for each solution

   type INTERVAL is range CHOICE'First .. CHOICE'Pred(CHOICE'Last);
   -- Constraining intervals (must be unique)

   Used_Notes     : array (CHOICE)   of BOOLEAN := (others => FALSE);
   Used_Intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);
   -- Sets of notes and intervals in use

   Vector: SOLUTION;
   -- Vector with current (partial) solution

   -- Try to add one step to the partial solution
   procedure Extend(Current: LEVEL) is
      function  last_interval(c: CHOICE) return INTERVAL with Inline;
      function  reject(c: CHOICE) return BOOLEAN with Inline;
      procedure enter(c: CHOICE) with Inline;
      procedure leave(c: CHOICE) with Inline;

      -- compute choice/previous interval
      function last_interval(c: CHOICE) return INTERVAL is
         previous: CHOICE renames Vector(LEVEL'Pred(Current));
      begin
         return INTERVAL(abs(c - previous));
      end;

      -- reasons to prune
      function reject(c: CHOICE) return BOOLEAN is
      begin
         return Used_Notes(c)
            or else Used_Intervals(last_interval(c));
      end;

      -- to wrap the recursive calls
      procedure enter(c: CHOICE) is
      begin
         Used_Notes(c) := TRUE;
         Used_Intervals(last_interval(c)) := TRUE;
      end;

      procedure leave(c: CHOICE) is
      begin
         Used_Intervals(last_interval(c)) := FALSE;
         Used_Notes(c) := FALSE;
      end;

   begin
      -- try to extend the solution with each choice
      for c in CHOICE loop
         if reject(c) then
            null; -- fail
         else
            Vector(Current) := c;
            -- at the bottom?
            if Current = LEVEL'Last then
               Output(Vector);
            else
               -- descend one level
               enter(c);
               Extend(Current => LEVEL'Succ(Current));
               leave(c);
            end if;
         end if;
      end loop;
   end Extend;

begin
   declare
      first: constant := LEVEL'First;
   begin
      for c in CHOICE loop
         Vector(first) := c;
         Used_Notes(c) := TRUE;
         Extend(Current => LEVEL'Succ(first));
         Used_Notes(c) := FALSE;
      end loop;
   end;
end Dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
