-- dodeca.adb

with Ada.Text_IO;

procedure dodeca is
   Chromatic_Notes : constant := 12;

   -- Set of available choices
   type CHOICE   is range 1..Chromatic_Notes;
   type LEVEL    is range CHOICE'First..CHOICE'Last;
   -- Ordered set of choices
   type SOLUTION is array (LEVEL) of CHOICE;

   -- Consumer
   task player is
      entry output(vector: in SOLUTION);
      entry finish;
   end player;

   task body player is
      use Ada.Text_IO;
      vector : SOLUTION;
   begin
      loop
         select
            accept output(vector: in SOLUTION)
            do
               player.vector := vector; -- local copy
            end output;
            for item of vector loop
               Put(item'Image);
            end loop;
            New_Line;
         or
            accept finish;
            exit;
         end select;
      end loop;
   end player;

   -- Producer
   task solver;
   task body solver is
      -- Vector with current (partial) solution
      serie : SOLUTION;

      -- Sets of notes and intervals used
      type INTERVAL is range CHOICE'First..CHOICE'Pred(CHOICE'Last);
      used_notes     : array (CHOICE) of BOOLEAN := (others => FALSE);
      used_intervals : array (INTERVAL) of BOOLEAN := (others => FALSE);

      -- Solve the search problem
      procedure extend(current: in LEVEL) is
         -- `c` is the choice in the currrent level
         function last_interval(c: in CHOICE) return INTERVAL with Inline is
            previous : CHOICE renames serie(LEVEL'Pred(current));
         begin
            return INTERVAL(abs (c - previous));
         end;
         -- reasons to prune
         function reject(c: in CHOICE) return BOOLEAN with Inline is
         begin
            return used_notes(c) or else used_intervals(last_interval(c));
         end;
         -- to wrap the recursive call
         procedure enter(c: in CHOICE) with Inline is
         begin
            used_notes(c) := TRUE;
            used_intervals(last_interval(c)) := TRUE;
         end;
         procedure leave(c: in CHOICE) with Inline is
         begin
            used_intervals(last_interval(c)) := FALSE;
            used_notes(c) := FALSE;
         end;
      begin
         -- try to extend solution with each choice
         for c in CHOICE'Range loop
            if reject(c) then
               null; -- fail
            else
               serie(current) := c;
               if current = LEVEL'Last then
                  player.output(serie);
               else
                  enter(c);
                  extend(current => LEVEL'Succ(current));
                  leave(c);
               end if;
            end if;
         end loop;
      end extend;
   begin
      declare
         first  : constant := LEVEL'First;
         second : constant := LEVEL'Succ(LEVEL'First);
      begin
         for c in CHOICE'Range loop
            -- start with each choice in turn
            serie(first) := c;
            -- enter next level
            used_notes(c) := TRUE;
            -- extend solution
            extend(current => second);
            -- return from next level
            used_notes(c) := FALSE;
         end loop;
         player.finish;
      end;
   end solver;
begin
   -- wait until player and solver end
   null;
end dodeca;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
