-- dfs.adb

package body DFS is

   path : SOLUTION; -- Vector with (partial) solution

   -- Try to add one step to the partial solution
   procedure extend(depth: LEVEL) is
   begin
      -- try to extend the solution with each choice
      Each_Level_Loop:
         for item in CHOICE loop
            if not Reject(path, depth, item) then
               -- accept item for the current level
               path(depth) := item;
               -- if path is completed: one solution found
               if depth = LEVEL'Last then
                  Output(path);
                  exit Each_Level_Loop; -- not really necessary
               else
                  -- descend one level
                  Enter(path, depth, item);
                  extend(LEVEL'Succ(depth));
                  Leave(path, depth, item);
               end if;
            end if;
         end loop Each_Level_Loop;
   end extend;

   -- Walk the tree prunning when a node is rejected
   procedure Search is
   begin
      Top_Level_Loop:
         for item in CHOICE loop
            path(LEVEL'First) := item;

            Enter(path, LEVEL'First, item);
            extend(LEVEL'Succ(LEVEL'First));
            Leave(path, LEVEL'First, item);
         end loop Top_Level_Loop;
   end Search;

end DFS ;

-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
