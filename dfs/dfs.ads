-- dfs.ads

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
   -- Walk the tree prunning when a node is rejected

end DFS;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
