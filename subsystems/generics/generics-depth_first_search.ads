------------------------------------------------------------------------------
--  Generics . Depth_First_Search specification (generic)
------------------------------------------------------------------------------

generic
   ---------------------------------------------------------------------
   -- Vector solution types (like a signature)
   ---------------------------------------------------------------------

   type NODE_VALUES is (<>);
   -- Set of available choices

   type INDEX_TYPE is (<>);
   -- Search tree levels

   type VECTOR_SOLUTION is array (INDEX_TYPE) of NODE_VALUES;
   -- Vector of choices

   ---------------------------------------------------------------------
   -- Vector solution hooks
   ---------------------------------------------------------------------

   with procedure Goal
     (solution : in VECTOR_SOLUTION)
   is <>;
   -- Called for each solution found

   with function Rejected
     (solution : in VECTOR_SOLUTION;
      index    : in INDEX_TYPE;
      element  : in NODE_VALUES) return BOOLEAN
   is <>;
   -- Check constraints for the current node

   with procedure Enter
     (solution : in VECTOR_SOLUTION;
      index    : in INDEX_TYPE;
      element  : in NODE_VALUES)
   is <>;
   -- Hook to run before entering one level down

   with procedure Leave
     (solution : in VECTOR_SOLUTION;
      index    : in INDEX_TYPE;
      element  : in NODE_VALUES)
   is <>;
   -- Hook to run after exiting one level down

package Generics . Depth_First_Search is

   pragma Assertion_Policy (Check); -- Check / Ignore

   type FOREST_SET is array (NODE_VALUES) of BOOLEAN;
   -- (sub)set of NODE_VALUES values used as tree root node

   procedure Seek
     (forest : in FOREST_SET := (others => TRUE))
   with Pre => VECTOR_SOLUTION'Length > 1;
   -- Walk the indicated trees, prunning when a node is rejected
end Generics . Depth_First_Search;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
