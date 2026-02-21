pragma Assertion_Policy(Check); -- Check / Ignore

generic
   ---------------------------------------------------------------------
   -- Vector solution types (signature like)
   ---------------------------------------------------------------------

   type ELEMENT_TYPE is (<>);
   -- Set of available choices

   type INDEX_TYPE is (<>);
   -- Search tree levels

   type ARRAY_TYPE is array (INDEX_TYPE) of ELEMENT_TYPE;
   -- Vector of choices

   ---------------------------------------------------------------------
   -- Vector solution hooks (primitive operations like)
   ---------------------------------------------------------------------

   with procedure Goal
     (solution : in ARRAY_TYPE)
   is <>;
   -- Called for each solution found

   with function Rejected
     (solution : in ARRAY_TYPE;
      index    : in INDEX_TYPE;
      element  : in ELEMENT_TYPE) return BOOLEAN
   is <>;
   -- Check constraints for the current node

   with procedure Enter
     (solution : in ARRAY_TYPE;
      index    : in INDEX_TYPE;
      element  : in ELEMENT_TYPE)
   is <>;
   -- Hook to run before entering one level down

   with procedure Leave
     (solution : in ARRAY_TYPE;
      index    : in INDEX_TYPE;
      element  : in ELEMENT_TYPE)
   is <>;
   -- Hook to run after exiting one level down

package Depth_First_Search is
   type FOREST_SET is array (ELEMENT_TYPE) of BOOLEAN;
   -- (sub)set of ELEMENT_TYPE values used as tree root node

   procedure Seek
     (forest : in FOREST_SET := (others => TRUE))
   with Pre => ARRAY_TYPE'Length > 1;
   -- Walk the indicated trees, prunning when a node is rejected
end Depth_First_Search;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
