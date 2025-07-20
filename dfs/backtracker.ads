-- backtracker.ads

generic
   type NODE_VALUE is (<>);
   -- Set of available choices

   type VECTOR_INDEX is (<>);
   -- Search tree levels

   type VECTOR_SOLUTION is array (VECTOR_INDEX) of NODE_VALUE;
   -- Vector of choices

   with procedure Found
     (solution : VECTOR_SOLUTION)
   is <>;
   -- Called for each solution found

   with function Rejected
     (solution : VECTOR_SOLUTION;
      index    : VECTOR_INDEX;
      value    : NODE_VALUE) return BOOLEAN
   is <>;
   -- Check constraints for the current node

   with procedure Enter
     (solution : VECTOR_SOLUTION;
      index    : VECTOR_INDEX;
      value    : NODE_VALUE)
   is <>;
   -- Hook to run before entering one level down

   with procedure Leave
     (solution : VECTOR_SOLUTION;
      index    : VECTOR_INDEX;
      value    : NODE_VALUE)
   is <>;
   -- Hook to run after exiting one level down

package Backtracker is

   procedure Traverse with Inline;
   -- Walk the tree prunning when a node is rejected

end Backtracker;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
