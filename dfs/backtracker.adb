-- backtracker.adb

package body Backtracker is
   solution : VECTOR_SOLUTION; -- (partial) solution

   -- Try to add one step to the partial solution
   procedure traverse
     (index : VECTOR_INDEX)
   is
   begin
      -- try to extend the solution with each choice
      for value in NODE_VALUE loop
         if not Rejected(solution, index, value) then
            solution(index) := value;
            -- accept value for the current level

            if index /= VECTOR_INDEX'Last then
               -- recurse if solution is not completed
               Enter(solution, index, value);
               traverse(VECTOR_INDEX'Succ(index));
               Leave(solution, index, value);
            else
               Found(solution);
            end if;
         end if;
      end loop;
   end traverse;

   -- Walk the tree prunning when a node is rejected
   procedure Traverse
   is
   begin
      traverse(VECTOR_INDEX'First);
   end Traverse;
end Backtracker;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
