------------------------------------------------------------------------------
--  Generics . Depth_First_Search implementation
------------------------------------------------------------------------------

-- generic
--    type NODE_VALUES is (<>);
--    type INDEX_TYPE is (<>);
--    type VECTOR_SOLUTION is array (INDEX_TYPE) of NODE_VALUES;
--    with procedure Goal
--    with function  Rejected
--    with procedure Enter
--    with procedure Leave

package body Generics . Depth_First_Search is

   solution : VECTOR_SOLUTION;
   -- Vector to fill with NODE_VALUES

   -- Try to add one step to the partial solution
   procedure traverse
     (index : in INDEX_TYPE)
   is
   begin
      -- try to extend the solution with each choice
      for element in NODE_VALUES loop
         if not Rejected(solution, index, element) then
            -- accept element for the current level
            solution(index) := element;

            if index /= INDEX_TYPE'Last then
               -- recurse if solution is not completed
               Enter(solution, index, element);
               traverse(INDEX_TYPE'Succ(index));
               Leave(solution, index, element);
            else
               Goal(solution);
            end if;
         end if;
      end loop;
   end traverse;

   -- Walk the tree, prunning when a node is rejected
   procedure Seek
     (forest : in FOREST_SET := (others => TRUE))
   is
      first : constant INDEX_TYPE := INDEX_TYPE'First;
   begin
      for element in NODE_VALUES loop
         if forest(element) then
            solution(first) := element;

            Enter(solution, first, element);
            traverse(INDEX_TYPE'Succ(first));
            Leave(solution, first, element);
         end if;
      end loop;
   end Seek;
end Generics . Depth_First_Search;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
