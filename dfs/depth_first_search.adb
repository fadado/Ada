pragma Assertion_Policy(Check); -- Check / Ignore

package body Depth_First_Search is
   subtype NODE_VALUES     is ELEMENT_TYPE;
   subtype VECTOR_SOLUTION is ARRAY_TYPE;
   -- fancy renaming

   solution : VECTOR_SOLUTION;
   -- (partial) solution vector to fill with NODE_VALUES

   -- Try to add one step to the partial solution
   procedure traverse
     (index : in INDEX_TYPE)
   is
   begin
      -- try to extend the solution with each choice
      for element in NODE_VALUES loop
         if not Rejected(solution, index, element) then
            solution(index) := element;
            -- accept element for the current level

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

   -- Walk the tree prunning when a node is rejected
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
end Depth_First_Search;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
