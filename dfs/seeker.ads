-- seeker.ads

pragma Assertion_Policy(Check); -- Check / Ignore

generic
   type NODE_VALUE is (<>);
   -- Set of available choices

   type VECTOR_INDEX is (<>);
   -- Search tree levels

   type VECTOR_SOLUTION is array (VECTOR_INDEX) of NODE_VALUE;
   -- Vector of choices

   with procedure Goal
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

package Seeker is
   type FOREST_SET is array (NODE_VALUE) of BOOLEAN;

   procedure Seek
     (forest : FOREST_SET := (others => TRUE))
   with Pre => VECTOR_SOLUTION'Length > 1;
   -- Walk the indicated trees, prunning when a node is rejected
end Seeker;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
