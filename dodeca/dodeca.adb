-- dodeca.adb

with Ada.Text_IO;

procedure dodeca is
   pragma Optimize(Time);

   Chromatic_Notes: constant := 12;

   -- Set of available choices
   type CHOICE   is range 1 .. Chromatic_Notes;
   type LEVEL    is new CHOICE;

   -- Ordered set of choices
   type SOLUTION is array (LEVEL) of CHOICE;

------------------------------------------------------------------------
   -- Consumer
------------------------------------------------------------------------
   procedure output(vector: SOLUTION)
   is
      use Ada.Text_IO;
   begin
      for item of vector loop
         Put(item'Image);
      end loop;
      New_Line;
   end;
------------------------------------------------------------------------
   -- Producer
------------------------------------------------------------------------
   -- Vector with current (partial) solution
   serie: SOLUTION;

   -- Sets of notes and intervals used
   type INTERVAL is range CHOICE'First .. CHOICE'Pred(CHOICE'Last);
   used_notes     : array (CHOICE)   of BOOLEAN := (others => FALSE);
   used_intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);

   -- Try to add one partial solution
   procedure extend(current: LEVEL) is
      function  last_interval(c: CHOICE) return INTERVAL with Inline;
      function  reject(c: CHOICE) return BOOLEAN with Inline;
      procedure enter(c: CHOICE) with Inline;
      procedure leave(c: CHOICE) with Inline;

      -- compute choice/previous interval
      function last_interval(c: CHOICE) return INTERVAL is
         previous: CHOICE renames serie(LEVEL'Pred(current));
      begin
         return INTERVAL(abs(c - previous));
      end;

      -- reasons to prune
      function reject(c: CHOICE) return BOOLEAN is
      begin
         return used_notes(c)
            or else used_intervals(last_interval(c));
      end;

      -- to wrap the recursive calls
      procedure enter(c: CHOICE) is
      begin
         used_notes(c) := TRUE;
         used_intervals(last_interval(c)) := TRUE;
      end;

      procedure leave(c: CHOICE) is
      begin
         used_intervals(last_interval(c)) := FALSE;
         used_notes(c) := FALSE;
      end;

   begin
      -- try to extend solution with each choice
      for c in CHOICE loop
         if reject(c) then
            null; -- fail
         else
            serie(current) := c;
            -- at the bottom?
            if current = LEVEL'Last then
               output(serie);
            else
               -- descend one level
               enter(c);
               extend(current => LEVEL'Succ(current));
               leave(c);
            end if;
         end if;
      end loop;
   end extend;

begin
   declare
      first: constant := LEVEL'First;
   begin
      for c in CHOICE loop
         serie(first) := c;
         -- descend one level
         used_notes(c) := TRUE;
         extend(current => LEVEL'Succ(first));
         used_notes(c) := FALSE;
      end loop;
   end;
end dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
